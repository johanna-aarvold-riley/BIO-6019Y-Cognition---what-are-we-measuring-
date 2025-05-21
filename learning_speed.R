# packages ----
library(lme4)
library(lmerTest)
library(performance)
library(sjPlot)
library(patchwork)
library(modelsummary)

# learning speed from AL to RL1 ----

AL_RL1_learning <- learning %>%
  filter(set == "1", phase %in% c("AL", "RL1")) %>%
  group_by(TagID_hex) %>%
  filter(all(!is.na(criterion)) & n() == 2) %>%
  ungroup() %>%
  left_join(birds %>% select(TagID_hex, species), by = "TagID_hex")

AL_RL1_learning <- AL_RL1_learning %>%
  filter(!(TagID_hex == "01103FA593")) # remove extreme outlier

AL_RL1_log <- AL_RL1_learning %>%
  mutate(log_criterion = log(criterion)) # log transform to fit assumptions better

AL_RL1_lm <- lm(log_criterion ~ phase, data = AL_RL1_log)

check_model(AL_RL1_lm)
AIC(AL_RL1_lm) # other models used in comparison when fitting have been removed so code is more readable (this applies without)
summary(AL_RL1_lm)

tab_model(AL_RL1_lm, file = "AL_RL1_lm_table.doc")

# AL RL1 with species term ----

AL_RL1_species <- AL_RL1_log %>%
  left_join(birds %>% select(TagID_hex, species), by = "TagID_hex")

AL_RL1_species_lm <- lm(log_criterion ~ phase * species, data = AL_RL1_species)
check_model(AL_RL1_species_lm)
AIC(AL_RL1_species_lm)
summary(AL_RL1_species_lm)

tab_model(AL_RL1_species_lm, file = "AL_RL1_species_lm_table.doc")

# learning speed across reversals (RL1-4) ----

reversals_learning <- learning %>% # create phase_continuous column which treats phases in set 2 as continuous reversals from set 1
  filter(set %in% c("1", "2")) %>%
  mutate(phase_continuous = case_when(
    set == "1" & phase == "AL" ~ "AL",
    set == "1" & phase == "RL1" ~ "RL1",
    set == "1" & phase == "RL2" ~ "RL2",
    set == "2" & phase == "AL" ~ "RL3",
    set == "2" & phase == "RL1" ~ "RL4")) 

reversals_learning <- reversals_learning %>%
  group_by(TagID_hex) %>%
  filter(all(!is.na(criterion)),           
         n_distinct(phase_continuous) == 5) %>%  # keep only birds that learned in every phase
  ungroup() %>%
  filter(phase_continuous != "AL")         # remove AL phase (only interested in reversals in this model)


head(reversals_learning)

reversals_learning %>%
  summarise(unique_birds = n_distinct(TagID_hex)) %>%
  pull(unique_birds) %>%
  print() # 35 birds remain

reversals_learning_log <- reversals_learning %>%
  mutate(log_criterion = log(criterion + 1))  # log(criterion + 1) to avoid log(0)

reversals_lm <- lm(log_criterion ~ phase_continuous, data = reversals_learning_log)

check_model(reversals_lm)
AIC(reversals_lm)
summary(reversals_lm)

tab_model(reversals_lm, file = "reversals_lm_table.doc")

# reversals with species term ----

reversals_species <- reversals_learning_log %>%
  left_join(birds %>% select(TagID_hex, species), by = "TagID_hex")

reversals_species_lm <- lm(log_criterion ~ phase_continuous * species, data = reversals_species)

check_model(reversals_species_lm)
AIC(reversals_lm)
summary(reversals_species_lm)

tab_model(reversals_species_lm, file = "reversals_species_lm_table.doc")
