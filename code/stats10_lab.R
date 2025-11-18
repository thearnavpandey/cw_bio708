pacman::p_load(tidyverse, patchwork, janitor, palmerpenguins, here)

penguins_raw

# data manipulations ------------------------------------------------------

colnames(penguins_raw)

## clean column names
## -use clean_names ()
?clean_names

penguins_clean <- clean_names(penguins_raw)
colnames(penguins_clean)


#change input in clutch_completion
#- use ifelse() 
# combine it with mutate()

unique(penguins_clean$clutch_completion)

penguins_clean <- penguins_clean %>% 
  mutate(clutch_completion = if_else(clutch_completion == "Yes", 1, 0))

unique(penguins_clean$clutch_completion)

# change species name input
# use case_when()
# use mutate()
sp <- unique(penguins_clean$species)

# ?case_when
# penguins_clean <- penguins_clean %>% 
#   mutate(species = case_when (
#          species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
#          species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap",
#          species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo"))

  
penguins_clean <- penguins_clean %>% 
  mutate(species = case_when (
    species == sp[1] ~ "adelie",
    species == sp[3] ~ "chinstrap",
    species == sp[2] ~ "gentoo"))

unique(penguins_clean$species)

view(penguins_clean)

## remove NAs from the data
colnames(penguins_clean)

penguins_clean <- penguins_clean %>% 
  drop_na(culmen_length_mm, 
          culmen_depth_mm, 
          flipper_length_mm, 
          body_mass_g,
          sex)

view(penguins_clean)

# analyze penguin data ----------------------------------------------------

## develop a full model
m_full <- glm(clutch_completion ~ species + 
           culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g + sex,
    data = penguins_clean,
    family = "binomial")

summary (m_full)

# moedl selection with dredge
library(MuMIn)
options(na.action = "na.fail")
m <- m_full
m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 2)

nrow(m_set)


