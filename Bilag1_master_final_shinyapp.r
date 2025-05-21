# Installere pakker
pacman::p_load(tidyverse, tidymodels, skimr)

# Arbejdsmappe
setwd("C:/Users/andre/OneDrive - EaDania/eksamen_sem2/2_semesters_eksamensopgave2025_kode")

# Datasæt der skal indlæses
datasets <- c(
  "all_companies", "meetings", "events", 
  "all_contact", "company_contacts", "event_participants", "old_projects"
)

# Indlæs alle datasæt på én gang
for (dataset in datasets) {
  assign(dataset, readRDS(paste0(dataset, ".rds")))
}

# Overblik over hoveddata
all_companies %>% skim() %>% print()

# Rens dubletter i CVRix m. keep_map 
keep_map <- tibble::tribble(
  ~CVRix, ~keep_CompanyId,
  "29189846", 93890,
  "29287511", 168800,
  "30773047", 95641,
  "39670038", 158472,
  "10268702", 157160,
  "10465893", 74160,
  "11995403", 181658,
  "13824606", 169388,
  "17033875", 19982,
  "17616617", 94582,
  "17888749", 169090,
  "17912828", 94654,
  "18103206", 54491,
  "18593602", 74165,
  "18936984", 95873,
  "21320080", 54823,
  "21522783", 95840,
  "26564336", 148461,
  "27623549", 144562,
  "28127863", 95550,
  "28860420", 89136,
  "29190925", 62614,
  "30778065", 42371,
  "31777879", 160192,
  "31843219", 94665,
  "32326706", 44563,
  "32709923", 157199,
  "33621477", 68403,
  "34335869", 50984,
  "34354332", 194491,
  "34546789", 57532,
  "35954716", 95509,
  "38488244", 93824,
  "41375124", 169278,
  "42630314", 178564,
  "44662116", 93356,
  "45724719", 95519,
  "55828415", 111315,
  "87683710", 193184
)

# Filtrér companies på virksomhedsstatus og fravælg kolonner:
companies_clean <- all_companies %>%
  filter(z_companies_1_Virksomhedsstatus_1 %in% c("Aktiv", "NORMAL")) %>%
  select(
    -CompanyType,
    -`z_companies_1_Firmanavn_1`,
    -`z_companies_1_CVR-nummer_1`,
    - `z_companies_1_Type_1`,
    -`z_companies_1_P-Nummer_1`,
    -`z_companies_1_Land_1`,
    -`z_companies_1_Reklamebeskyttet_1`
  ) %>% 
  
  # Opret churn-variabel som factor
  mutate(
    churnet      = factor(
      z_companies_121_1Erhvervsrådet_121,
      levels = c(FALSE, TRUE),
      labels = c("yes", "no")
    ),
    
    # Omdan "antal ansatte" til virksomheds-str.
    ansatte      = z_companies_1_Ansatte_1,
    virksomhedsstr = case_when(
      ansatte == 0              ~ "Gratis_Medlem",       
      ansatte  < 50             ~ "Mindre_Virksomhed",
      ansatte  < 250            ~ "Mellem_Str._Virksomhed",
      ansatte >= 250            ~ "Større_Virksomhed",
      TRUE                       ~ NA_character_
    ) %>%
      factor(levels = c("Gratis_Medlem","Mindre_Virksomhed","Mellem_Str._Virksomhed","Større_Virksomhed"))
  ) %>%
  
  # Fjern original-kolonnen med churn-info
  select(-z_companies_121_1Erhvervsrådet_121) %>%
  
  # Join keep_map og filtrér dubletter væk:
  left_join(keep_map, by = "CVRix") %>%
  filter(
    is.na(keep_CompanyId) | CompanyId == keep_CompanyId
  ) %>%
  select(-keep_CompanyId) %>%
  
  # Fjern rækker med manglende post nr. eller nace-kode:
  drop_na(
    `z_companies_1_Post nr._1`,
    `z_companies_1_Nace kode_1`
  )

# RENSNING AF MEETINGS
# Udvælg kolonner fra meetings
meetings_clean <- meetings %>%
  select(CompanyId, StartDate, mødelængde)

# find dubletter
dubletter <- meetings_clean %>%
  group_by(CompanyId, StartDate) %>%
  filter(n() > 1) %>%       # behold kun de grupper med >1 række
  arrange(CompanyId, StartDate)

dubletter

# Fjern dubletter på samme møde
dedupped <- meetings_clean %>%
  distinct(CompanyId, StartDate, .keep_all = TRUE)
# .keep_all = TRUE sikrer at vi stadig har mødelængde med

# Tæl antal møder pr. virksomhed/medlem samt mødelængde
meeting_counts <- dedupped %>%
  group_by(CompanyId) %>%
  summarise(
    Antal_Møder               = n(),                           # nu 1 række per unikt StartDate
    total_mødelængde          = sum(mødelængde, na.rm = TRUE),  # sum af varigheden
    gnsn_mødelængde_pr_møde   = mean(mødelængde, na.rm = TRUE)  # gennemsnit
  ) %>%
  ungroup() %>%
  select(-total_mødelængde)   # <— fjern total_mødelængde (ikke brug for længere)

# Meeting‐buckets 
meeting_counts_bucked <- meeting_counts %>%
  mutate(
    meeting_bucket = case_when(
      Antal_Møder == 0          ~ "IngenMøder",
      Antal_Møder == 1          ~ "EtMøde",
      Antal_Møder <= 5          ~ "HøjstFemMøder",
      TRUE                      ~ "FlereEndFemMøder"
    ),
    meeting_bucket = factor(meeting_bucket, levels = c("IngenMøder","EtMøde","HøjstFemMøder","FlereEndFemMøder"))
  )

# RENSNING AF EVENTS
# Definér cutoff‐dato og find gyldige CVR‐numre
cutoff <- as_date("2025-02-25")
valid_cvr <- companies_clean %>%
  mutate(CompanyDate = as_date(CompanyDateStamp)) %>%
  filter(CompanyDate <= cutoff) %>%
  pull(CVRix)

# Udvælg kolonner, konverter StartDate og normaliser "Title" til en ny kolonne "EventName" samt fjern dubletter
valid_events_grouped <- events %>%
  select(Cvr, Title, StartDate) %>%
  mutate(
    StartDate = as_date(StartDate),
    # Fjern årstal + alt efter (fx " 2022 - En hyldest…") og trim
    EventName = str_remove(Title, " \\d{4}.*") %>% str_trim()
  ) %>%
  distinct(Cvr, Title, StartDate, .keep_all = TRUE) %>%      #Fjern dubletter
  filter(Cvr %in% valid_cvr)     # Filtrer kun events fra gyldige CVR-numre

# Tæl hvor mange gange en virksomhed har deltaget i events
event_counts_for_model <- valid_events_grouped %>%
  count(Cvr, name = "Antal_Events") %>%
  rename(CVRix = Cvr)

# Lav bucket
event_counts_bucked <- event_counts_for_model %>%
  mutate(
    event_bucket = case_when(
      Antal_Events == 0      ~ "IngenEvents",
      Antal_Events <= 2      ~ "HøjstToEvents",
      Antal_Events <= 5      ~ "HøjstFemEvents",
      TRUE                   ~ "FlereEndFemEvents"
    ),
    event_bucket = factor(event_bucket,
                          levels = c("IngenEvents","HøjstToEvents","HøjstFemEvents","FlereEndFemEvents"))
  )

# Saml alt i master-datasæt (Merge stepwise med left_join)
master_data <- companies_clean %>%
  left_join(meeting_counts_bucked, by = "CompanyId") %>%
  left_join(event_counts_bucked,   by = "CVRix") %>%   # her får du event_bucket
  replace_na(list(
    Antal_Møder             = 0,
    gnsn_mødelængde_pr_møde = dminutes(0),
    Antal_Events            = 0
  ))

#....
master_final <- master_data %>%
  # Omdøb kolonner
  rename(
    MedlemsRegistreringsDato = CompanyDateStamp,
    `CVR-nr.`                = CVRix,
    PostNr.                  = `z_companies_1_Post nr._1`
  ) %>%
  
  # Flyt churnet frem som første kolonne
  relocate(churnet, .before = 1) %>%
  
  # BRANCHE‐KLASSIFIKATIONEN
  mutate(
    # Ekstraher de to første cifre fra den nye, enkle kolonne
    nace_div     = str_sub(`z_companies_1_Nace kode_1`, 1, 2),
    
    # Map til dine funktionelle kategorier
    branche_type = case_when(
      nace_div %in% sprintf("%02d", 10:33) |
        nace_div %in% c("08", "35")               ~ "Produktionsvirksomheder",
      
      nace_div == "46"                          ~ "Engroshandel",
      nace_div == "47"                          ~ "Detailhandel",
      
      nace_div %in% sprintf("%02d", 41:43)      ~ "Bygge & anlæg",
      nace_div %in% c("49","52","53")           ~ "Transport & logistik",
      nace_div %in% c("55","56")                ~ "Hotel & restauration",
      nace_div %in% sprintf("%02d", 58:63)      ~ "Information & kommunikation",
      nace_div %in% sprintf("%02d", 64:66)      ~ "Finans & forsikring",
      nace_div %in% sprintf("%02d", 69:82)      ~ "Forretningsservice",
      nace_div %in% sprintf("%02d", 84:88)      ~ "Offentlig administration, uddannelse, sundhed mv.",
      nace_div %in% sprintf("%02d", 90:96)      ~ "Øvrige serviceaktiviteter",
      
      TRUE                                      ~ "Andet/Ukendt"
    )
  ) %>%
  select(
    -nace_div,
    -CompanyId,
    -z_companies_1_Ansatte_1,
    -z_companies_1_Virksomhedsstatus_1,
    -`z_companies_1_Nace kode_1`,   # fjern den oprindelige kolonne
    -Antal_Møder,
    -Antal_Events
  )

# Flere rettelser
master_final <- master_final %>%
  # Gør missing i buckets til "ingen"
  mutate(
    meeting_bucket = fct_na_value_to_level(meeting_bucket, "IngenMøder"),
    event_bucket   = fct_na_value_to_level(event_bucket,   "IngenEvents")
  )

# De allersidste detaljer
master_final <- master_final %>%
  
# Omregn til minutter og lav dummy-variabler ... Binære indikator...
mutate(
  duration_min = as.numeric(gnsn_mødelængde_pr_møde, units = "mins"),
  Ingen_Mødelængde      = if_else(duration_min == 0,              1L, 0L),
  Møde_Længde_Under60min     = if_else(duration_min > 0    & duration_min <= 60, 1L, 0L),
  Møde_Længde_60min     = if_else(duration_min == 60, 1L, 0L),
  Møde_Længde_75min     = if_else(duration_min > 60   & duration_min <= 75, 1L, 0L),
  Møde_Længde_90min     = if_else(duration_min > 75   & duration_min <= 90, 1L, 0L),
  Møde_Længde_Over90min = if_else(duration_min > 90,             1L, 0L)
) %>%
  select(-duration_min) %>%    # ← husk %>% her
  # Size‐indikatorer
  mutate(
    Gratis_Medlem          = if_else(virksomhedsstr == "Gratis_Medlem",         1L, 0L),
    Mindre_Virksomhed      = if_else(virksomhedsstr == "Mindre_Virksomhed",     1L, 0L),
    Mellem_Str._Virksomhed = if_else(virksomhedsstr == "Mellem_Str._Virksomhed",1L, 0L),
    Større_Virksomhed      = if_else(virksomhedsstr == "Større_Virksomhed",     1L, 0L),
  # # meeting_bucket-indikatorer
    Ingen_Møder            = if_else(meeting_bucket  == "IngenMøder",          1L, 0L),
    Et_Møde                = if_else(meeting_bucket  == "EtMøde",              1L, 0L),
    Højst_Fem_Møder        = if_else(meeting_bucket  == "HøjstFemMøder",       1L, 0L),
    Flere_End_Fem_Møder    = if_else(meeting_bucket  == "FlereEndFemMøder",    1L, 0L),
    # event_bucket-indikatorer
    Ingen_Events_Deltagelse         = if_else(event_bucket == "IngenEvents",       1L, 0L),
    Højst_To_Events_Deltagelse      = if_else(event_bucket == "HøjstToEvents",     1L, 0L),
    Højst_Fem_Events_Deltagelse     = if_else(event_bucket == "HøjstFemEvents",    1L, 0L),
    Flere_End_Fem_Events_Deltagelse = if_else(event_bucket == "FlereEndFemEvents", 1L, 0L),
    # branche_type-indikatorer
    Branche_Produktionsvirksomhed   = if_else(branche_type == "Produktionsvirksomheder",                           1L, 0L),
    Branche_Engroshandel            = if_else(branche_type == "Engroshandel",                                      1L, 0L),
    Branche_Detailhandel            = if_else(branche_type == "Detailhandel",                                      1L, 0L),
    Branche_ByggeAnlæg              = if_else(branche_type == "Bygge & anlæg",                                     1L, 0L),
    Branche_TransportLogistik       = if_else(branche_type == "Transport & logistik",                              1L, 0L),
    Branche_HotelRestauration       = if_else(branche_type == "Hotel & restauration",                              1L, 0L),
    Branche_InfoKommunikation       = if_else(branche_type == "Information & kommunikation",                       1L, 0L),
    Branche_FinansForsikring        = if_else(branche_type == "Finans & forsikring",                               1L, 0L),
    Branche_Forretningsservice      = if_else(branche_type == "Forretningsservice",                                1L, 0L),
    Branche_OffentligUddSundhed     = if_else(branche_type == "Offentlig administration, uddannelse, sundhed mv.", 1L, 0L),
    Branche_ØvrigeServiceaktiviteter= if_else(branche_type == "Øvrige serviceaktiviteter",                         1L, 0L),
    Branche_Anden                   = if_else(branche_type == "Andet/Ukendt",                                      1L, 0L)
  ) %>%
  # Fjern de gamle kolonner
  select(
    -virksomhedsstr,
    -meeting_bucket,
    -event_bucket,
    -branche_type,
    -PostNr.,
    -ansatte,
    -Gratis_Medlem, 
    -gnsn_mødelængde_pr_møde
  ) %>%   
  # Fjern missing i churnet-kolonnen
  filter(!is.na(churnet)) %>%  
  mutate(churnet = factor(churnet, levels = c("yes", "no"))) 

# Overblik over master_final
master_final %>% skim() %>% print()

View(master_final)
write_csv(master_final, "Bilag2_master_final_shinyapp.csv")

