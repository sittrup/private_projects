# Installer pacman
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load pacman og installer
pacman::p_load(tidyverse, readxl, lubridate, httr, jsonlite, 
               rvest, caret, car, randomForest, glmnet, 
               fastDummies, cowplot, car)

# Loader libraries
library(tidyverse) 
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(rvest)
library(caret)
library(car)         
library(randomForest)
library(glmnet)
library(fastDummies)
library(cowplot)    
library(car)

# ------------------------------------------------------------------------------
# NYT LØSNINGSFORSLAG
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# INDLÆSNING AF GULD-DATA
# ------------------------------------------------------------------------------

guld_data <- readxl::read_excel("Guld.xlsx")

# Her konverteres dato, så det står i korrekt datoformat
# dplyr::mutate bruges til at oprette en ny kolonne 'Dato' med korrekt datoformat
# dplyr::case_when bruges til at konvertere tal, f.eks. 
# Hvis Dato kan konverteres til et tal, omdannes det til en dato med startpunkt "1899-12-30"
# Hvis Dato matcher mønsteret "dd.mm.yyyy", omdannes det til en dato ved hjælp af lubridate::dmy
# Hvis ingen af de to ovenstående betingelser er opfyldt, omdannes det til NA
guld_data <- guld_data |>
  dplyr::mutate(
    Dato = dplyr::case_when(
      suppressWarnings(!is.na(as.numeric(Dato))) ~ as.Date(as.numeric(Dato), origin="1899-12-30"),
      grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", Dato)   ~ lubridate::dmy(Dato),
      TRUE ~ as.Date(NA)
    )
  )

# Fjerner Gule_poletter_stk, laver Kamp om til en factor, fjerner NA
cleaned_data <- guld_data |>
  dplyr::select(-Gule_poletter_stk) |>
  dplyr::mutate(Kamp = as.factor(Kamp)) |>
  tidyr::drop_na()

# Beregner udnyttelsesgraden for hver række og tilføjer den som en ny kolonne i cleaned_data2
cleaned_data2 <- cleaned_data |>
  dplyr::mutate(
    udnyttelsesgraden_bestilte = Guld_menu_stk / Antal_bestilte,
    udnyttelsesgraden_max = Guld_menu_stk / Antal_max
  )

# ------------------------------------------------------------------------------

# HENT/GEM API-DATA (DMI)
# ------------------------------------------------------------------------------

# Her hentes data fra DMI's API og gemmes i en lokal RDS-fil, så vi ikke behøver at hente dataen hver gang
r_data_file <- "APIdata.rds"
APIdata     <- NULL

if (file.exists(r_data_file)) {
  message("Indlæser DMI-data fra lokal RDS-fil: ", r_data_file)
  APIdata <- readRDS(r_data_file)
} else {
  message("Ingen lokal APIdata fundet. Henter data fra DMI API...")
  
  base_url   <- "https://dmigw.govcloud.dk/v2/"
  info_url   <- "metObs/collections/observation/items?"
  api_key    <- "f21f4434-06af-4b0d-a00f-209da7929822"
  station_id <- "06060"
  
  unique_dates <- sort(unique(as.Date(cleaned_data2$Dato)))
  date_chunks  <- split(unique_dates, lubridate::floor_date(unique_dates, "month"))
  
  fetch_data_chunk <- function(dates) {
    chunk_responses <- list()
    for (this_date in dates) {
      start_datetime <- format(as.Date(this_date),    "%Y-%m-%dT00:00:00Z")
      end_datetime   <- format(as.Date(this_date)+1, "%Y-%m-%dT00:00:00Z")
      
      req_url  <- paste0("stationId=", station_id, "&datetime=", start_datetime, "/", end_datetime, "&limit=100000")
      full_url <- paste0(base_url, info_url, req_url, "&api-key=", api_key)
      
      api_call <- httr::GET(full_url)
      if (api_call$status_code != 200) {
        warning("API-kald mislykkedes [", api_call$status_code, "] for dato ", this_date)
        Sys.sleep(1)
        next
      }
      
      api_char <- rawToChar(api_call$content)
      api_JSON <- jsonlite::fromJSON(api_char, flatten=TRUE)
      if (is.null(api_JSON$features) || length(api_JSON$features) == 0) {
        warning("Ingen data for dato ", this_date)
        Sys.sleep(1)
        next
      }
      
      needed_cols <- c("properties.observed", "properties.parameterId", "properties.value")
      if (!all(needed_cols %in% colnames(api_JSON$features))) {
        warning("Forventede kolonner findes ikke for dato ", this_date)
        Sys.sleep(1)
        next
      }
      
      observations <- api_JSON$features |>
        dplyr::select(
          observed    = properties.observed,
          parameterId = properties.parameterId,
          Value       = properties.value
        ) |>
        # Tving Value til character fra start
        dplyr::mutate(Value = as.character(Value)) |>
        dplyr::mutate(
          Value = dplyr::if_else(
            grepl("^-?\\d+(\\.\\d+)?$", Value),
            Value,
            NA_character_   # NA af typen character
          ),
          Value = as.numeric(Value)
        )
      
      dmi <- observations |>
        dplyr::mutate(Dato = as.Date(substr(observed, 1, 10))) |>
        dplyr::group_by(Dato, parameterId) |>
        dplyr::summarise(Value = mean(Value, na.rm=TRUE), .groups="drop") |>
        tidyr::pivot_wider(names_from=parameterId, values_from=Value)
      
      chunk_responses <- append(chunk_responses, list(dmi))
      Sys.sleep(1)
    }
    if (length(chunk_responses) > 0) {
      dplyr::bind_rows(chunk_responses)
    } else {
      NULL
    }
  }
  
  api_responses <- list()
  for (chunk in date_chunks) {
    chunk_data <- fetch_data_chunk(chunk)
    if (!is.null(chunk_data)) {
      api_responses <- append(api_responses, list(chunk_data))
    }
  }
  
  if (length(api_responses) > 0) {
    APIdata <- dplyr::bind_rows(api_responses) |>
      dplyr::mutate(Dato = as.Date(Dato))
    message("API-data hentet fra DMI – gemmer i fil: ", r_data_file)
    saveRDS(APIdata, r_data_file)
  } else {
    warning("Ingen API-data hentet. Bruger cleaned_data2 som fallback.")
  }
}

if (!is.null(APIdata)) {
  final_data <- dplyr::left_join(cleaned_data2, APIdata, by="Dato")
} else {
  final_data <- cleaned_data2
}

# ------------------------------------------------------------------------------

# WEBSCRAPING (superstats.dk) + JOIN
# ------------------------------------------------------------------------------

# Opretter en mappe til dataen
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

# Opretter en vektor med sæsoner og en tom liste til at gemme dataen i
seasons <- paste0(2013:2024, "/", 2014:2025)
match_list <- list()

# Vi gør her det samme som med API, vi scraper for data på superstats og gemmer dem i en lokal CSV-fil
for (season_year in seasons) {
  file_season <- gsub("/", "-", season_year)
  local_file  <- file.path(data_dir, paste0(file_season, ".csv"))
  
  if (file.exists(local_file)) {
    message("Læser lokal CSV: ", local_file)
    table_data <- readr::read_csv(local_file, show_col_types=FALSE)
    match_list[[season_year]] <- table_data
  } else {
    match_url <- paste0("https://superstats.dk/program?season=", season_year)
    message("Scraper data for: ", season_year, " => ", match_url)
    
    table_data <- NULL
    try({
      html <- rvest::read_html(match_url)
      table_data_html <- html |>
        rvest::html_nodes("div#content") |>
        rvest::html_table()
      
      if (length(table_data_html) > 0 && nrow(table_data_html[[1]]) > 0) {
        if (is.null(colnames(table_data_html[[1]]) == "")) {
          colnames(table_data_html[[1]]) <- c("day", "date_time", "opponent", "goals", "audience", "referee", "channels")
        }
        
        table_data_processed <- table_data_html[[1]] |>
          dplyr::mutate(
            runde = dplyr::if_else(grepl("^Runde", day), gsub("Runde ", "", day), as.character(NA))
          ) |>
          tidyr::fill(runde, .direction="down") |>
          dplyr::filter(!grepl("^Runde", day)) |>
          dplyr::mutate(season=season_year)
        
        table_data <- table_data_processed
        readr::write_csv(table_data, local_file)
        message("Gemte scraping-resultat i: ", local_file)
        match_list[[season_year]] <- table_data
      } else {
        message("Ingen data fundet for sæson: ", season_year)
      }
    }, silent=FALSE)
    
    if (!is.null(table_data)) {
      match_list[[season_year]] <- table_data
    }
  }
}

# Formatering af dataen - vi splitter dataen i date og time og ændrer f.eks. "Ã¦" til "æ"
combined_match <- dplyr::bind_rows(match_list, .id="season")

combined_match <- combined_match |>
  dplyr::mutate(runde = dplyr::if_else(is.na(runde), 1L, as.integer(runde))) |>
  tidyr::separate(date_time, into=c("date", "time"), sep=" ") |>
  dplyr::mutate(across(where(is.character), ~ iconv(., from="UTF-8", to="latin1"))) |>
  dplyr::mutate(across(where(is.character), ~ stringr::str_replace_all(.,
                                                                       c("Ã¦"="æ", "Ã¸"="ø", "Ã…"="Å", "LÃ¸r"="Lør", "SÃ¸n"="Søn"))))

vff_matches <- combined_match |>
  dplyr::filter(grepl("VFF", opponent) | grepl("VFF", day)) |>
  dplyr::select(-day, -channels)

season_dates <- tibble::tibble(
  season = c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018",
             "2018/2019", "2019/2020", "2020/2021", "2021/2022", "2022/2023",
             "2023/2024", "2024/2025"),
  start_date = as.Date(c("2013-07-19", "2014-07-18", "2015-07-17", "2016-07-15",
                         "2017-07-14", "2018-07-13", "2019-07-12", "2020-09-11",
                         "2021-07-16", "2022-07-15", "2023-07-21", "2024-07-20")),
  end_date   = as.Date(c("2014-05-18", "2015-06-07", "2016-05-29", "2017-06-04",
                         "2018-05-27", "2019-05-26", "2020-07-26", "2021-05-28",
                         "2022-05-22", "2023-06-04", "2024-05-26", "2025-05-25"))
)

# Adskil kolonnerne 'day' og 'month', tilføj 'year' og 'dato' i korrekt format
# Konverter 'dato' til Date-format i både vff_matches og final_data
# Left join final_data og vff_matches på 'dato'
vff_matches <- vff_matches |>
  dplyr::mutate(
    day   = as.integer(stringr::str_extract(date, "^[0-9]+")),
    month = as.integer(stringr::str_extract(date, "(?<=/)[0-9]+"))
  ) |>
  dplyr::left_join(season_dates, by="season") |>
  dplyr::mutate(
    year = dplyr::if_else(
      (month > lubridate::month(start_date)) |
        (month == lubridate::month(start_date) & day >= lubridate::day(start_date)),
      lubridate::year(start_date),
      lubridate::year(end_date)
    ),
    dato = as.Date(paste(year, month, day, sep="-"), format="%Y-%m-%d")
  ) |>
  dplyr::select(date, time, opponent, goals, audience, referee, runde, season, dato)

final_data <- final_data |>
  dplyr::rename(dato = Dato) |>
  dplyr::mutate(dato = as.Date(dato))

vff_matches <- vff_matches |>
  dplyr::mutate(dato = as.Date(dato))

final_data_joined <- dplyr::left_join(final_data, vff_matches, by="dato")



###############################
###############################
### Det nye løsningsforslag ###
###############################
###############################

# Indlæs farveblinds-pakke
library(viridis)  

# Opret cleaned_data3
cleaned_data3 <- final_data_joined |>
  dplyr::filter(!is.na(Guld_menu_stk) & Guld_menu_stk != 0,
                !is.na(Antal_bestilte) & Antal_bestilte != 0,
                !is.na(Antal_max) & Antal_max != 0,
                !is.na(audience) & audience != 0) |>
  dplyr::mutate(
    udnyttelsesgraden_bestilte = Guld_menu_stk / Antal_bestilte,
    udnyttelsesgraden_max = Guld_menu_stk / Antal_max,
    udnyttelsesgraden_stadion = audience / 10
  ) |>
  dplyr::select(udnyttelsesgraden_bestilte, udnyttelsesgraden_max, udnyttelsesgraden_stadion)

# Konverter data til langt format for at gøre det nemt at plotte
cleaned_data3_long <- cleaned_data3 |> 
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Kategori", 
                      values_to = "Værdi")

# Opret søjlediagram og vis det med farveblinde-venlige farver
ggplot(cleaned_data3_long, aes(x = Kategori, y = Værdi, fill = Kategori)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Søjlediagram over udnyttelsesgrader",
       x = "Kategori",
       y = "Gennemsnitlig udnyttelsesgrad") +
  theme_minimal()


# Opret boxplot og vis det med farveblinde-venlige farver
ggplot(cleaned_data3_long, aes(x = Kategori, y = Værdi, fill = Kategori)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Boxplot over udnyttelsesgrader",
       x = "Kategori",
       y = "Udnyttelsesgrad") +
  theme_minimal()

# Opret violinplot og vis det med farveblinde-venlige farver
ggplot(cleaned_data3_long, aes(x = Kategori, y = Værdi, fill = Kategori)) +
  geom_violin() +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Violinplot over udnyttelsesgrader",
       x = "Kategori",
       y = "Udnyttelsesgrad") +
  theme_minimal()


# Opret cleaned_data4
cleaned_data4 <- final_data_joined |>
  dplyr::filter(!is.na(Guld_menu_stk) & Guld_menu_stk != 0,
                !is.na(Antal_bestilte) & Antal_bestilte != 0,
                !is.na(Antal_max) & Antal_max != 0,
                !is.na(audience) & audience != 0) |>
  dplyr::mutate(
    udnyttelsesgraden_bestilte = Guld_menu_stk / Antal_bestilte,
    udnyttelsesgraden_max = Guld_menu_stk / Antal_max,
    udnyttelsesgraden_stadion = audience / 10
  ) |>
  dplyr::select(udnyttelsesgraden_bestilte, udnyttelsesgraden_max, udnyttelsesgraden_stadion)

# Konverter data til langt format for at gøre det nemt at plotte
cleaned_data3_long <- cleaned_data3 |> 
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Kategori", 
                      values_to = "Værdi")

# Opret cleaned_data4 (forudsætter at final_data_joined allerede eksisterer)
cleaned_data4 <- final_data_joined |>
  dplyr::filter(!is.na(Guld_menu_stk) & Guld_menu_stk != 0,
                !is.na(Antal_bestilte) & Antal_bestilte != 0,
                !is.na(Antal_max) & Antal_max != 0,
                !is.na(audience) & audience != 0) |>
  dplyr::mutate(
    udnyttelsesgraden_bestilte = Guld_menu_stk / Antal_bestilte,
    udnyttelsesgraden_max = Guld_menu_stk / Antal_max,
    udnyttelsesgraden_stadion = audience / 10
  ) |>
  dplyr::select(Kamp, udnyttelsesgraden_bestilte, udnyttelsesgraden_max, udnyttelsesgraden_stadion)

# Beregn gennemsnittet af udnyttelsesgrader for hvert hold
average_data <- cleaned_data4 |>
  dplyr::group_by(Kamp) |>
  dplyr::summarise(
    gennemsnit_udnyttelsesgraden_bestilte = mean(udnyttelsesgraden_bestilte, na.rm = TRUE),
    gennemsnit_udnyttelsesgraden_max = mean(udnyttelsesgraden_max, na.rm = TRUE),
    gennemsnit_udnyttelsesgraden_stadion = mean(udnyttelsesgraden_stadion, na.rm = TRUE)
  )

# Konverter data til langt format for at gøre det nemt at plotte
average_data_long <- average_data |> 
  tidyr::pivot_longer(cols = -Kamp, 
                      names_to = "Kategori", 
                      values_to = "Værdi")

# Opret søjlediagram og vis det med farveblinde-venlige farver
ggplot(average_data_long, aes(x = Kamp, y = Værdi, fill = Kategori)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Gennemsnitlig udnyttelsesgrad for hvert hold",
       x = "Hold",
       y = "Gennemsnitlig udnyttelsesgrad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Roterer x-aksen etiketter for bedre læsbarhed

######
# Her laves der cirkeldiagram på udnyttelsesgraderne
######

# Funktion til at kategorisere data i under 70, 70-80 og over 80% udnyttelsesgrad
kategoriser_udnyttelse <- function(udnyttelse) {
  dplyr::case_when(
    udnyttelse < 0.7 ~ "Under 70%",
    udnyttelse > 0.8 ~ "Over 80%",
    TRUE ~ "70-80%"
  )
}

# Kategoriser dataene for udnyttelsesgraden_bestilte, udnyttelsesgraden_max og udnyttelsesgraden_stadion
cleaned_data4 <- cleaned_data4 |>
  dplyr::mutate(
    kategori_bestilte = kategoriser_udnyttelse(udnyttelsesgraden_bestilte),
    kategori_max = kategoriser_udnyttelse(udnyttelsesgraden_max),
    kategori_stadion = kategoriser_udnyttelse(udnyttelsesgraden_stadion)
  )

# Beregn andelen af hver kategori for udnyttelsesgraden_bestilte
andel_bestilte <- cleaned_data4 |>
  dplyr::count(kategori_bestilte) |>
  dplyr::mutate(procent = n / sum(n) * 100)

# Beregn andelen af hver kategori for udnyttelsesgraden_max
andel_max <- cleaned_data4 |>
  dplyr::count(kategori_max) |>
  dplyr::mutate(procent = n / sum(n) * 100)

# Beregn andelen af hver kategori for udnyttelsesgraden_stadion
andel_stadion <- cleaned_data4 |>
  dplyr::count(kategori_stadion) |>
  dplyr::mutate(procent = n / sum(n) * 100)

# Opret cirkeldiagram for udnyttelsesgraden_bestilte
ggplot(andel_bestilte, aes(x = "", y = procent, fill = kategori_bestilte)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Cirkeldiagram over udnyttelsesgraden_bestilte",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Fjerner x-aksen etiketter
        axis.ticks = element_blank(),   # Fjerner x-aksen ticks
        panel.grid = element_blank())   # Fjerner grid linjer

# Opret cirkeldiagram for udnyttelsesgraden_max
ggplot(andel_max, aes(x = "", y = procent, fill = kategori_max)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Cirkeldiagram over udnyttelsesgraden_max",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Fjerner x-aksen etiketter
        axis.ticks = element_blank(),   # Fjerner x-aksen ticks
        panel.grid = element_blank())   # Fjerner grid linjer

# Opret cirkeldiagram for udnyttelsesgraden_stadion
ggplot(andel_stadion, aes(x = "", y = procent, fill = kategori_stadion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Cirkeldiagram over udnyttelsesgraden_stadion",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Fjerner x-aksen etiketter
        axis.ticks = element_blank(),   # Fjerner x-aksen ticks
        panel.grid = element_blank())   # Fjerner grid linjer

########
######
####
###
# Opret cleaned_data5
cleaned_data5 <- final_data_joined |>
  dplyr::filter(!is.na(Guld_menu_stk) & Guld_menu_stk != 0,
                !is.na(Antal_bestilte) & Antal_bestilte != 0,
                !is.na(Antal_max) & Antal_max != 0,
                !is.na(audience) & audience != 0) |>
  dplyr::mutate(
    udnyttelsesgraden_bestilte = Guld_menu_stk / Antal_bestilte,
    udnyttelsesgraden_max = Guld_menu_stk / Antal_max,
    udnyttelsesgraden_stadion = audience / 10
  ) |>
  dplyr::select(precip_dur_past1h, precip_past1h, udnyttelsesgraden_bestilte, udnyttelsesgraden_max, udnyttelsesgraden_stadion, dato)

# Funktion til at bestemme sæson
bestem_saeson <- function(dato) {
  måned <- lubridate::month(dato)
  dplyr::case_when(
    måned %in% c(12, 1, 2) ~ "Vinter",
    måned %in% c(3, 4, 5) ~ "Forår",
    måned %in% c(6, 7, 8) ~ "Sommer",
    måned %in% c(9, 10, 11) ~ "Efterår"
  )
}

# Tilføj sæson kolonne
cleaned_data5 <- cleaned_data5 |>
  dplyr::mutate(saeson = bestem_saeson(dato))

# Konverter data til langt format for at gøre det nemt at plotte
cleaned_data5_long <- cleaned_data5 |> 
  tidyr::pivot_longer(cols = c(precip_dur_past1h, precip_past1h, udnyttelsesgraden_bestilte, udnyttelsesgraden_max, udnyttelsesgraden_stadion), 
                      names_to = "Kategori", 
                      values_to = "Værdi")

# Beregn gennemsnittet for udnyttelsesgraderne
gennemsnit_udnyttelse <- cleaned_data5 |>
  dplyr::group_by(saeson) |>
  dplyr::summarise(
    gennemsnit_bestilte = mean(udnyttelsesgraden_bestilte, na.rm = TRUE),
    gennemsnit_max = mean(udnyttelsesgraden_max, na.rm = TRUE),
    gennemsnit_stadion = mean(udnyttelsesgraden_stadion, na.rm = TRUE)
  )

# Opret kombinationsdiagram
ggplot(cleaned_data5_long, aes(x = saeson, y = Værdi, fill = Kategori)) +
  geom_bar(data = subset(cleaned_data5_long, Kategori %in% c("precip_dur_past1h", "precip_past1h")), 
           stat = "summary", fun = "mean", position = "dodge") +
  geom_line(data = subset(cleaned_data5_long, Kategori %in% c("udnyttelsesgraden_bestilte", "udnyttelsesgraden_max", "udnyttelsesgraden_stadion")), 
            aes(group = Kategori, color = Kategori), stat = "summary", fun = "mean", size = 1) +
  geom_point(data = subset(cleaned_data5_long, Kategori %in% c("udnyttelsesgraden_bestilte", "udnyttelsesgraden_max", "udnyttelsesgraden_stadion")), 
             aes(group = Kategori, color = Kategori), stat = "summary", fun = "mean", size = 2) +
  scale_fill_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  scale_color_viridis_d(option = "D") +  # Bruger en farveblindvenlig palette
  labs(title = "Kombinationsdiagram over udnyttelsesgrader og nedbør",
       x = "Sæson",
       y = "Gennemsnitlig værdi",
       fill = "Kategori",
       color = "Kategori") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


     