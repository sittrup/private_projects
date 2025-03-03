# de 6 steps for begyndere
# explore
# clean
# manipulate
# describe and summarise
# visualise
# analyse

# pacman til at loade pakker
pacman::p_load("tidyverse")

# se starwars filen
view(starwars)

# her ses alle data-filerne der er indbygget i R
data()

# her ser man hele datasættet i glimpse og en specifik
glimpse(starwars)
class(starwars$gender)
unique(starwars$gender)

# starwars$gender laves om til en factor
starwars$gender <- as.factor(starwars$gender)
class(starwars$gender)

# udvælgelse af værdier feks mand eller kvinde eller noget helt trejde
levels(starwars$gender)

# her vil vi vende om på masculine og feminine
starwars$gender <- factor(starwars$gender, levels = c("masculine", "feminine"))

# tjek i levels
levels(starwars$gender)

### brug select

names(starwars)

# udvælg kolonner feks name, height
starwars |> 
  select(name, height, ends_with("color"))

unique(starwars$hair_color)

# her trækkes der data fra blond eller brown hår og personer under 180 cm
starwars |> 
  select(name, height, ends_with("color")) |> 
  filter(hair_color %in% c("blond", "brown") &
           height < 180)

### hvor der ingen data er (NA)


# mean beregner gennemsnittet af højden (height) for alle karakterer
# i starwars-datasættet, uden at tage manglende værdier (NA) med.
mean(starwars$height, na.rm = TRUE)









