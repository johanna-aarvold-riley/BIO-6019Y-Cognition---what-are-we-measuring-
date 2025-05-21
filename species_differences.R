# investigating species differences post-criterion ----
# packages ----

# general data exploration ----

# create subset
set1_visits <- ind_visits %>%
  filter(set == "1") %>% # filter for only set 1
  select(TagID_hex, date_time, feeder, feeder_correct, phase, set, species) %>% # take some columns from ind_visits
  left_join(
    learning %>% filter(set == "1"), # add criterion column from learning speed df
    by = c("TagID_hex", "phase", "set")) %>%
  mutate(phase = as.factor(phase)) %>%  # convert phase to factor
  arrange(date_time, TagID_hex, phase) # arrange in chronological order

# create a filtered version where each bird has one row each
set1_per_bird <- set1_visits %>%
  group_by(TagID_hex, species, phase) %>%
  summarise(
    criterion = first(criterion), 
    .groups = "drop")

# summary of no. visits before criterion met per species per phase
set1_per_bird %>%
  filter(!is.na(criterion)) %>%
  group_by(species, phase) %>%
  summarise(
    avg_visits = mean(criterion),
    sd_visits = sd(criterion),
    n = n()) 

# summary of n birds which met vs didnt meet criterion & proportion
set1_per_bird %>%
  group_by(species, phase) %>%
  summarise(
    total_birds = n_distinct(TagID_hex),
    met_criterion = sum(!is.na(criterion)),
    not_met_criterion = sum(is.na(criterion)),
    proportion_met = met_criterion / total_birds,
    .groups = "drop") 

# summary of engagement (total n visits per bird) per species per phase
set1_visits %>%
  group_by(TagID_hex, species, phase) %>%
  summarise(visits = n(), .groups = "drop") %>%
  group_by(species, phase) %>%
  summarise(
    avg_visits = mean(visits),
    sd_visits = sd(visits),
    n_birds = n(),
    .groups = "drop") 

# n birds which engaged with feeders at some point per species
set1_visits %>%
  distinct(TagID_hex, species) %>%
  count(species) 


# comparison of feeder visits after criterion ----

# create subset of set 1 which only contains visits from birds after criterion is met

set1_post_criterion <- set1_visits %>%
  semi_join(
    set1_per_bird %>%
      filter(!is.na(criterion)) %>% # keep only birds which meet criterion in all 3 phases
      count(TagID_hex) %>%
      filter(n == 3),
    by = "TagID_hex") %>%
  filter(!is.na(criterion)) %>%
  group_by(TagID_hex, phase) %>% # order visits chronologically within each phase for each bird
  arrange(date_time, .by_group = TRUE) %>%
  mutate( visit_number_total = row_number(), # create total visit count (inc all visits in the experiment)
    visit_number = visit_number_total - criterion) %>% # minus criterion number from visit count to make count of visits after criterion is met
  filter(visit_number > 0) %>% # keep only visits after criterion is met
  ungroup()

# summary of number of visits, correct, incorrect, and proportion of correct vs incorrect grouped by phase and species
set1_post_criterion %>%
  group_by(species, phase) %>%
  summarise(
    total_visits = n(),
    correct_visits = sum(feeder_correct == 1),
    incorrect_visits = sum(feeder_correct == 0),
    proportion_correct = correct_visits / total_visits,
    .groups = "drop") %>%
  print(width= Inf)

# density histogram to visualise distribution of visit numbers post-criterion
ggplot(set1_post_criterion, aes(x = visit_number)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Visit Numbers Post-Criterion", x = "Visit Number", y = "Count") +
  theme_minimal()


## model ----

# scale visit_number
set1_post_criterion <- set1_post_criterion %>%
  mutate(visit_number_scaled = scale(visit_number))

set1_post_c_model3 <- glmer(
  feeder_correct ~ phase + species * visit_number_scaled + (1 | TagID_hex),
  family = binomial,
  data = set1_post_criterion,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

check_model(set1_post_c_model3)
AIC(set1_post_c_model3)
summary(set1_post_c_model3)

tab_model(set1_post_c_model3, file = "set1_post_c_glmm_table.doc")
## plots ----
### violin plot ----

ggplot(set1_post_criterion, aes(x = species, y = feeder_correct, fill = species)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) + 
  labs(title = "Proportion of Correct Visits Post-Criterion by Species",
       x = "Species",
       y = "Proportion Correct Visits") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "olivedrab")) +
  theme(legend.position = "none")

### model-predicted learning curves by phase and species ----

prediction_data <- expand.grid(
  species = unique(set1_post_criterion$species),
  phase = unique(set1_post_criterion$phase),
  visit_number_scaled = seq(0, 1, length.out = 100))  # 100 points from scaled 0 to 1

# predict probabilities using the fitted model (minus random effects)
prediction_data$predicted_prob <- predict(set1_post_c_model3, newdata = prediction_data, type = "response", re.form = NA)

set1_model_predicted_plot1 <- ggplot(prediction_data, aes(x = visit_number_scaled, y = predicted_prob, color = species, linetype = phase)) +
  geom_line() + 
  labs(
    x = "Visit Number (scaled)", 
    y = "Predicted Proportion of Correct Visits", 
    title = "Model Predicted Learning Curves over visits after meeting criterion") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "olivedrab")) +
  theme(legend.title = element_blank())

### proportion of correct feeder visit by phase and species ----

set1_post_criterion %>%
  group_by(TagID_hex, species, phase) %>%
  summarise(
    total_visits = n(),
    correct_visits = sum(feeder_correct == 1),
    incorrect_visits = sum(feeder_correct == 0),
    prop_correct = correct_visits / total_visits,
    .groups = "drop") %>%
  ggplot(aes(x = phase, y = prop_correct, color = species)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "line", aes(group = species), size = 1.2) +
  scale_color_manual(values = c("BLUTI" = "blue", "GRETI" = "grey40")) +
  labs(
    x = "Phase",
    y = "Proportion of Correct Visits",
    title = "Proportion of Correct Feeder Visits After Learning",
    color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### random sample of individuals tracking plot ----
# shows variation between individuals

# calculate proportion correct per individual per phase
set1_individual_props <- set1_post_criterion %>%
  group_by(TagID_hex, species, phase) %>%
  summarise(
    total_visits = n(),
    correct_visits = sum(feeder_correct == 1),
    prop_correct = correct_visits / total_visits,
    .groups = "drop")

# sample 10 individuals from each species
set.seed(123)  # sets the start of the random number generator to 123 so i'll get the same 10 individuals every time i run this code (for reproducible plot)
sampled_ids <- set1_individual_props %>%
  distinct(TagID_hex, species) %>%
  group_by(species) %>%
  sample_n(10) %>%
  pull(TagID_hex)

# create df with just sampled individuals
set1_sampled <- set1_individual_props %>%
  filter(TagID_hex %in% sampled_ids)

# plot lines for sampled individuals, points for all individuals
ggplot(set1_individual_props, aes(x = phase, y = prop_correct, color = species)) +
  geom_line(data = set1_sampled, aes(group = TagID_hex), color = "black", size = 0.4, alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "line", aes(group = species), size = 1.2) +
  scale_color_manual(values = c("BLUTI" = "blue", "GRETI" = "grey40")) +
  labs(
    x = "Phase",
    y = "Proportion of Correct Visits",
    title = "Individual Learning Trajectories After Criterion Met",
    color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# proportion of incorrect visits which are to previously assigned feeder ----

# pivot feeder assignments to wide format
assignments_wide_set1 <- assigned_feeders %>%
  filter(phase %in% c("AL", "RL1", "RL2"), set == 1) %>%  # filter for only set 1
  select(-set) %>%
  pivot_wider(
    names_from = phase,
    values_from = assigned_feeder,
    names_prefix = "assigned_")

# left join assignments to h2 incorrect visits data
set1_post_criterion <- set1_post_criterion %>%
  left_join(assignments_wide_set1, by = "TagID_hex")

## RL1 ----

rl1_incorrect_visits <- set1_post_criterion %>%
  filter(phase == "RL1", feeder_correct == 0) %>% # filter for only phase RL1 incorrect visits
  mutate(
    visit_type = case_when(
      feeder == assigned_AL ~ "Previously assigned (AL)",  # categorise types of incorrect visit
      feeder == assigned_RL2 ~ "Unassigned feeder", 
      TRUE ~ "Other site"))

# summary of n and proportion to each incorrect visit type by species
rl1_incorrect_visits %>%
  group_by(species, visit_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(prop = n / sum(n)) %>%
  print()

### model ----

rl1_filtered <- rl1_incorrect_visits %>%
  filter(visit_type %in% c("Previously assigned (AL)", "Unassigned feeder")) %>% # remove any visits to other sites
  mutate(
    visit_type_binary = ifelse(visit_type == "Previously assigned (AL)", 1, 0)) # convert to binary for modelling (previously assigned AL = 1, unassigned = 0)


rl1_scaled <- rl1_filtered %>%
  mutate(visit_number_z = scale(visit_number)) # scale visit number for generalised mixed effects model

rl1_model2 <- glmer(visit_type_binary ~ species * visit_number_z + (1 | TagID_hex),
                    family = binomial, data = rl1_scaled)

summary(rl1_model2)
AIC(rl1_model2)
anova(rl1_model2)

tab_model(rl1_model2, file = "rl1_glmm_table.doc")

### plot ----

# predict values using model
rl1_scaled$rl1_predicted_visit_type <- predict(rl1_model2, newdata = rl1_scaled, type = "response", re.form = NA)

set1_model_predicted_plot2 <- ggplot(rl1_scaled, aes(x = visit_number_z, y = rl1_predicted_visit_type, color = species)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("BLUTI" = "blue", "GRETI" = "olivedrab"),
    labels = c("BLUTI" = "Blue tit", "GRETI" = "Great tit")) +
  scale_x_continuous(
    breaks = seq(min(rl1_scaled$visit_number_z), max(rl1_scaled$visit_number_z), by = 1),
    minor_breaks = seq(min(rl1_scaled$visit_number_z), max(rl1_scaled$visit_number_z), by = 0.5)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05)) +
  theme_minimal() +
  labs(
    title = "Predicted Probability of Visiting Previously Assigned Feeder in Incorrect Visit",
    x = "Visit Number (scaled)",
    y = "Predicted Probability",
    color = "Species") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey85", size = 0.3),
    panel.grid.minor.y = element_line(color = "grey85", size = 0.3))


## RL2 ----
# same as above, adjusted for additional previous assignment

rl2_incorrect_visits <- set1_post_criterion %>%
  filter(phase == "RL2", feeder_correct == 0) %>%
  mutate(
    visit_type = case_when(
      feeder == assigned_AL ~ "Previously assigned (AL)",
      feeder == assigned_RL1 ~ "Previously assigned (RL1)",
      TRUE ~ "Other site"))

rl2_incorrect_visits %>%
  group_by(species, visit_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(prop = n / sum(n)) %>%
  print()

### model ----

rl2_filtered <- rl2_incorrect_visits %>%
  filter(visit_type %in% c("Previously assigned (AL)", "Previously assigned (RL1)")) %>% # remove any visits to other site
  mutate(
    visit_type_binary = ifelse(visit_type == "Previously assigned (RL1)", 1, 0))  # convert to binary (RL = 1, AL = 0)


rl2_scaled <- rl2_filtered %>%
  mutate(visit_number_z = scale(visit_number)) # scale for model

rl2_model <- glmer(visit_type_binary ~ species * visit_number_z + (1 | TagID_hex),
                   family = binomial, data = rl2_scaled)

summary(rl2_model)
AIC(rl2_model)
anova(rl2_model)

tab_model(rl2_model, file = "rl2_model_table.doc")

### plot ----

# model-predicted values
rl2_scaled$rl2_predicted_visit_type <- predict(rl2_model, newdata = rl2_scaled, type = "response", re.form = NA)

set1_model_predicted_plot3 <- ggplot(rl2_scaled, aes(x = visit_number_z, y = rl2_predicted_visit_type, color = species)) +
  geom_line(size = 1) +  # Line plot for predictions
  scale_color_manual(values = c("BLUTI" = "blue", "GRETI" = "olivedrab"),
  labels = c("BLUTI" = "Blue tit", "GRETI" = "Great tit")) +
  labs(
    title = "Predicted Probability of Visiting Most Recent (RL1) Previously Assigned Feeder",
    x = "Visit Number (scaled)",
    y = "Predicted Probability",
    color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey85", size = 0.3),
    panel.grid.minor.y = element_line(color = "grey85", size = 0.3))
