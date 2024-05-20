# PSYCH 434: Example script for a causal inference analysis
# May 2024
# questions: joseph.bulbulia@vuw.ac.nz

# Causal Question Does a one unit shift in perfectionism affect anxiety and depression
# compare NZ Born (YES vs NO)
# Population: Residents of New Zealand in 2018-2021

# set path to save models
push_mods <-
  here::here('/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/saved')

# load packages
library("tidyverse")


# get devtools
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# get 'margot' from my github (make sure to update)
devtools::install_github("go-bayes/margot")


# Check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

library(margot)

# use p_load to load / install the packages
pacman::p_load(
  skimr,
  naniar,
  WeightIt,
  clarify,
  MatchThem,
  cobalt,
  MatchIt,
  kableExtra,
  janitor,
  lmtp,
  SuperLearner,
  ranger,
  margot,
  xgboost,
  glmnet,
  doParallel,
  ggplot2,
  here,
  naniar,
  gtsummary,
  grf,
  progressr,
  tidyverse,
  ggplot2,
  parameters,
  kableExtra
)

# wrangling ---------------------------------------------------------------
# set seed for reproducability
set.seed(123)

library(margot)

# eliminate haven labels
df_nz <- as.data.frame(df_nz)
df_nz <- haven::zap_formats(df_nz)
df_nz <- haven::zap_label(df_nz)
df_nz <- haven::zap_widths(df_nz)


# test total n in the data
# total nzavs participants
n_total <- skimr::n_unique(df_nz$id)

# get comma in number
n_total <- prettyNum(n_total, big.mark = ",")

# check n total
n_total

# save n for manuscript
margot::here_save(n_total, "n_total")

# name of exposure
name_exposure <-  "perfectionism"

# get names
colnames(df_nz)
summary(df_nz)

str(df_nz)
# check missing values so that you can figure out which to select.
skimr::skim(df_nz) |> 
  arrange(n_missing)

# obtain ids for individuals who participated in 2018 and have no missing baseline exposure
ids_2018 <- df_nz |>
  dplyr::filter(year_measured == 1, wave == 2018) |>
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing in baseline exposure
  #  dplyr::filter(!is.na(eth_cat)) |> # criteria, no missing
  pull(id)


# if you decide to include the exposure in the treatment wave,
# if you decide to include the exposure in the treatment wave,
# obtain ids for individuals who participated in 2019
# ids_2019 <- df_nz |>
#   dplyr::filter(year_measured == 1, wave == 2019) |>
#   dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing
#   pull(id)

# intersect IDs from 2018 and 2019 to ensure participation in both years
# ids_2018_2019 <- intersect(ids_2018, ids_2019)

colnames(df_nz)
# data wrangling
dat_long <- df_nz |>
  # dplyr::filter(id %in% ids_2018_2019 &
  #                 wave %in% c(2018, 2019, 2020)) |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  arrange(id, wave) |>
  select(
    "id",
    "wave",
    "year_measured",
    "age",
    "male",
    "born_nz",
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # are you currently employed? (this includes self-employment or casual work)
    "edu",
    # "gen_cohort",
    "household_inc",
    "partner",
    # 0 = no, 1 = yes
    "parent",
    #"alert_level_combined", # see bibliography
    # 0 = no, 1 = yes
    "political_conservative",
    # see nzavs sheet
    "hours_exercise",
    # see nzavs sheet
    "agreeableness",
    # Mini-IPIP6 Agreeableness (also modelled as empathy facet)
    # Sympathize with others' feelings.
    # Am not interested in other people's problems.
    # Feel others' emotions.
    # Am not really interested in others.
    "conscientiousness",
    # see mini ipip6
    # Get chores done right away.
    # Like order.
    # Make a mess of things.
    # Often forget to put things back in their proper place.
    "extraversion",
    # Mini-IPIP6 Extraversion
    # Am the life of the party.
    # Don't talk a lot.
    # Keep in the background.
    # Talk to a lot of different people at parties.
    "honesty_humility",
    # see mini ipip6
    # Would like to be seen driving around in a very expensive car.
    # Would get a lot of pleasure from owning expensive luxury goods.
    # Feel entitled to more of everything.
    # Deserve more things in life.
    "openness",
    # see mini ipip6
    # Have a vivid imagination.
    # Have difficulty understanding abstract ideas.
    # Do not have a good imagination.
    # Am not interested in abstract ideas.
    "neuroticism",
    # see mini ipip6
    # Have frequent mood swings.
    # Am relaxed most of the time.
    # Get upset easily.
    # Seldom feel blue.
    "modesty",
    # # see mini ipip6
    # # I want people to know that I am an important person of high status,
    # # I am an ordinary person who is no better than others.
    # # I wouldn’t want people to treat me as though I were superior to them.
    # # I think that I am entitled to more respect than the average person is
    #"w_gend_age_ethnic",
    "neighbourhood_community",
    # #I feel a sense of community with others in my local neighbourhood.
    "belong",
    # see nzavs sheet
    "rural_gch_2018_l",
    # see nzavs sheet
    "support",
    # "support_help",
    # # 'There are people I can depend on to help me if I really need it.
    # "support_turnto",
    # # There is no one I can turn to for guidance in times of stress.
    # "support_rnoguidance",
    #There is no one I can turn to for guidance in times of stress.
    "perfectionism",
    "religion_religious",
    "kessler_latent_depression",
    "kessler_latent_anxiety", 
    hours_
  ) |>
  mutate(
    #initialize 'censored'
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) &
                         year_measured == 1, 1, censored),
    # create urban binary variable
    
    urban = ifelse(rural_gch_2018_l == 1, 1, 0)
    
  ) |>
  select(-c(year_measured, rural_gch_2018_l)) |>
  dplyr::mutate(
    # rescale these variables, to get all variables on a similar scale
    # otherwise your models can blow up, or become uninterpretable.
    household_inc_log = log(household_inc + 1),
    hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::select(-c(household_inc, hours_exercise)) |>
  droplevels() |>
  # dplyr::rename(sample_weights = w_gend_age_ethnic,
  #               sample_origin =  sample_origin_names_combined) |>
  arrange(id, wave) |>
  mutate(
    urban = as.numeric(as.character(urban)),
    #   parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    censored = as.numeric(as.character(censored)),
    employed = as.numeric(as.character(employed))
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()


n_participants <- skimr::n_unique(dat_long$id)

# get comma in number
n_participants <- prettyNum(n_participants, big.mark = ",")

# check
n_participants

# save number for manuscrip
margot::here_save(n_participants, "n_participants")

# inspect data
skimr::skim(dat_long)


# set baseline variables --------------------------------------------------

# for confounding control
baseline_vars = c(
  "age",
  "male",
  "edu",
  "eth_cat",
  "partner",
  "employed",
  "born_nz",
  "neighbourhood_community",
  "household_inc_log",
  "parent",
  "religion_religious",
  "urban",
 # "sample_weights",
  "employed"
)


# treatment
exposure_var = c("perfectionism", "censored") # we will use the censored variable later

# outcome, can be many
outcome_vars = c("kessler_latent_anxiety", "kessler_latent_depression")


# sample weights balanced male / female-------------------------------------# balance on gender weights
# calculate gender weights assuming male is coded as 1 and female as 0
prop_male_population <-
  0.5  # target proportion of males in the population
prop_female_population <-
  0.5  # target proportion of females in the population

prop_male_sample <- mean(dat_long$male)
prop_female_sample <- 1 - prop_male_sample

gender_weight_male <- prop_male_population / prop_male_sample
gender_weight_female <- prop_female_population / prop_female_sample

dat_long$sample_weights <-
  ifelse(dat_long$male == 1, gender_weight_male, gender_weight_female)

# we will upweight males and down weight non-males to obtain a balance of gender in the *target* population
table(round(dat_long$sample_weights, 3))


# make your tables --------------------------------------------------------
dt_18 <- dat_long |>
  filter(wave == 2018)

# variables for the table
base_vars <- setdiff(baseline_vars, c("censored", "sample_weights", outcome_vars))


# get baseline cols
selected_base_cols <-
  dt_18 |> select(all_of(base_vars))

# missing values visualisation
viss_miss_baseline <- naniar::vis_miss(dt_18, warn_large_data = F)
here_save(viss_miss_baseline, "base_var")


# check missing values a baseline
viss_miss_baseline


# the setdiff command allows us to remove names from the baseline vars list that we do not 
table_baseline <- selected_base_cols %>%
  janitor::clean_names(case = "title") %>%
  tbl_summary(
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")
    ),
    type = list(all_continuous() ~ "continuous2")
  ) %>%
  modify_header(label = "**Exposure + Demographic Variables**") %>%
  bold_labels()

# baseline
table_baseline

# save your baseline table
margot::here_save(table_baseline, "table_baseline")

# exposure table ----------------------------------------------------------
# get first and second wave
dt_18_19 <- dat_long |>
  dplyr::filter(wave == 2018 | wave == 2019) |>
  # we need to drop unused levels of the wave
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_18_19 %>% select(c("perfectionism", "wave"))

# check
#str(selected_exposure_cols)

table_exposures <- selected_exposure_cols %>%
  janitor::clean_names(case = "title") %>%
  labelled::to_factor() %>%
  tbl_summary(by = "Wave",
              # Specify the grouping variable. Adjust "Wave" to match the cleaned column name.
              # Uncomment the following line and adjust if you have continuous variables to summarize
              # statistic = list(all_continuous() ~ "{mean} ({sd})"),
              # add_n(),  # Add a column with total number of non-missing observations (uncomment if needed))
              missing = "always",
              percent = "column") %>%
  modify_header(label = "**Exposure Variables by Wave**") %>%
  bold_labels()

# save baseline
here_save(table_exposures, "table_exposures")

# check
table_exposures


# outcome table -----------------------------------------------------------
dt_18_20 <- dat_long |>
  dplyr::filter(wave == 2018 | wave == 2020) |>
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_18_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

# better names if desirable
selected_outcome_cols <-
  dt_18_20 %>% select(all_of(names_outcomes_final), wave)
# |> # example if you want to rename your variables for the table
#   rename(
#     Social_belonging = belong,
#     Annual_charity = charity_donate,
#     Volunteering_hours = hours_charity,
#     Community_gives_money_binary = community_money_binary,
#     Community_gives_time_binary = community_time_binary,
#     Family_gives_money_binary = family_money_binary,
#     Family_gives_time_binary = family_time_binary,
#     Friends_give_money_binary = friends_money_binary,
#     Friends_give_time = friends_time_binary,
#     Social_support = support,
#     Sense_neighbourhood_community = neighbourhood_community
#   )

# order names correctly
selected_outcome_cols <- selected_outcome_cols %>%
  select(sort(names(selected_outcome_cols)))

# checks
# str(selected_outcome_cols)
# colnames(selected_outcome_cols)

table_outcomes <- selected_outcome_cols %>%
  janitor::clean_names(case = "title") %>%
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(by = "Wave",
              #specify the grouping variable. Adjust "Wave" to match the cleaned column name
              # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables) %>%
              #  add_n()
              missing = "always",
              percent = "column") %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

# save
margot::here_save(table_outcomes, "table_outcomes")

# read if needed
table_outcomes <- margot::here_read("table_outcomes")
table_outcomes

# histogram of the exposure -----------------------------------------------
# select 2019 wave
dt_19 <- dat_long |> dplyr::filter(wave == 2019)

# mean of exposure
mean_exposure <- mean(dt_19$perfectionism, na.rm = TRUE)

# view
mean_exposure

# save
here_save(mean_exposure, "mean_exposure")

# check
mean_exposure

# sd of exposure
sd_exposure <- sd(dt_19$perfectionism, na.rm = TRUE)

# save
here_save(sd_exposure, "sd_exposure")

# check
# sd_exposure


# median
median_exposure <- median(dt_19$perfectionism, na.rm = TRUE)

median_exposure

# check if you like
# median_exposure
# mean_exposure
graph_density_shift_function <- margot::coloured_histogram(
  dt_19,
  col_name = "perfectionism",
  binwidth = .1,
  unit_of_change = 1,
  scale_min = 1,
  scale_max = 7,
  highlight_range = "hightest" # "lowest" if you want the lowest, "both" if both
)
graph_density_shift_function
margot::here_save(graph_density_shift_function,
                  "graph_density_shift_function")

# change in exposure ------------------------------------------------------
dt_18_19_positivity <- dat_long |>
  dplyr::filter(wave == 2018 |
                  wave == 2019) |>
  dplyr::mutate(perfectionism_round = round(perfectionism, digits = 0)) |>
  dplyr::select(perfectionism_round, id, wave) |>
  droplevels()

out <- margot::create_transition_matrix(data = dt_18_19_positivity,
                                        state_var = "perfectionism_round",
                                        id_var = "id")


# t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table
transition_table  <- margot::transition_table(out)

# for import later
margot::here_save(transition_table, "transition_table")


# view
print(transition_table$table)
print(transition_table$explanation)



# example of a cross sectional analysis -----------------------------------

dt_18_regress <- dat_long |>
  filter(wave == 2018) |>
  mutate(
    #perfectionism_z = scale(perfectionism),
    kessler_latent_anxiety_z = scale(kessler_latent_anxiety),
    kessler_latent_depression_z = scale(kessler_latent_depression)
  )

# check
hist(dt_18_regress$kessler_latent_depression_z)
# check
hist(dt_18_regress$kessler_latent_anxiety_z)


# code from above
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights", outcome_vars))

# fit model
fit_kessler_latent_anxiety <-
  margot::regress_with_covariates(
    dt_18_regress,
    outcome = "kessler_latent_anxiety_z",
    exposure = "perfectionism",
    baseline_vars = base_var
  )

# view
parameters::model_parameters(fit_kessler_latent_anxiety, ci_method = "wald")[2, ]

# save results
here_save(fit_kessler_latent_anxiety, "fit_kessler_latent_anxiety")

# view
fit_kessler_latent_depression <-
  regress_with_covariates(
    dt_18_regress,
    outcome = "kessler_latent_depression_z",
    exposure = "perfectionism",
    baseline_vars = base_var
  )
# view model
parameters::model_parameters(fit_kessler_latent_depression, ci_method =
                               "wald")[2, ]

# save results
here_save(fit_kessler_latent_depression,
          "fit_kessler_latent_depression")


# calculate betas anxiety
lm_fit_kessler_latent_anxiety <- tbl_regression(fit_kessler_latent_anxiety)
here_save(lm_fit_kessler_latent_anxiety,
          "lm_fit_kessler_latent_anxiety")

# calculate betas depression
lm_fit_kessler_latent_depression
lm_fit_kessler_latent_depression <- tbl_regression(fit_kessler_latent_depression)
here_save(lm_fit_kessler_latent_depression,
          "lm_fit_kessler_latent_depression")

#
# 
b_lm_fit_kessler_latent_anxiety <- inline_text(lm_fit_kessler_latent_anxiety,
                                               variable = perfectionism,
                                               pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# view
b_lm_fit_kessler_latent_anxiety

here_save(b_lm_fit_kessler_latent_anxiety,
          "b_lm_fit_kessler_latent_anxiety")

# #
b_lm_fit_kessler_latent_depression <-
  inline_text(lm_fit_kessler_latent_depression,
              variable = perfectionism,
              pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

here_save(b_lm_fit_kessler_latent_depression,
          "b_lm_fit_kessler_latent_depression")


# impute missing values at baseline ---------------------------------------

exposure_vars <- c("perfectionism", "censored")
baseline_vars <- setdiff(baseline_vars, "sample_weights")

# check
baseline_vars

# here we imput the baseline
df_impute_base <- margot::margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_vars,
  outcome_vars = outcome_vars
)

# save

# get sample weights back to data
dt_18 <- dat_long |> filter(wave == 2018)

# add sample weights
df_impute_base$t0_sample_weights = dt_18$sample_weights

# save
here_save(df_impute_base, "df_impute_base")


# data wrangling for censoring weights ------------------------------------
df_impute_base <- here_read("df_impute_base")

# check data types
str(df_impute_base)

df_wide_censored <- df_impute_base |>
  relocate("t0_censored", .before = starts_with("t1_")) |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))


# save
here_save(df_wide_censored, "df_wide_censored")


# read if needed
df_wide_censored <- here_read("df_wide_censored")

# check missing values
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)

# people lost at wave 2
table(df_wide_censored$t0_censored)

# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0

# used if you did not censor at 2019
#t1_na_condition <- rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0

df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  # mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%. # use if censored at t1
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_censored &
        !t0_sample_weights &
        !t0_born_nz,
        #   !t1_perfectionism &  we will standardise perfectionism and use it later!t1_censored,
        .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_sample_weights,
    t0_born_nz,
    t0_censored,
    t1_perfectionism,
    t1_censored,
    ends_with("_z")
  ) |>
  mutate(t0_lost = 1 - t0_censored) |>
  mutate(t1_lost = 1 - t1_censored) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

# check what we have
colnames(df_clean) # note this  "t1_perfectionism"               "t1_perfectionism_z"

# check again
naniar::vis_miss(df_clean, warn_large_data = FALSE)

# checks
table(df_clean$t1_lost)
table(df_clean$t0_lost)

# checks
table(df_clean$t0_censored)

# checks
test <- df_wide_censored |> filter(t0_censored == 1)
nrow(test)

#
# df_impute_base$t1_perfectionism_z = scale(df_impute_base$t1_perfectionism)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)

# checks
str(df_clean)

# checks
nrow(df_clean)


# weights for treatment ----------------------------------------------------
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0"), -t0_censored, -t0_lost, -t0_sample_weights) |> colnames() # note

# check this is correct.
baseline_vars_models <- c(baseline_vars_models)

# create fresh dataset
df_clean_pre <- df_clean[baseline_vars_models]

# checks
str(df_clean_pre)

# if this variable were not a factor, make sure it is
# df_clean_pre$t0_eth_cat <- as.factor(df_clean_pre$t0_eth_cat)


# perform one-hot encoding using model.matrix
# we need factors to be 0 or 1
encoded_vars <- model.matrix(~ t0_eth_cat  - 1, data = df_clean_pre)
head(encoded_vars)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df %>%
  janitor::clean_names()

# View the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe

# ensure to remove original categorical variables to avoid duplication
df_clean_hot <- df_clean %>%
  select(-c(t0_eth_cat)) %>%
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# get baseline variable set without factors
baseline_vars_set <- setdiff(names(df_clean_pre), c("t0_lost", "id", "t0_eth_cat"))

# check
baseline_vars_set

# add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)

# check
full_predictor_vars

# check
str(df_clean_hot)

# set up super learner

library(SuperLearner)

# library for multicore processing
library(doParallel)

# learners
listWrappers()

# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet"
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")


# run super learner
sl <- SuperLearner(
  Y = df_clean_hot$t0_lost,
  X = df_clean_hot[full_predictor_vars],
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)

# stop the cluster
stopCluster(cl)

# save your super learner model
here_save(sl, "sl")


sl <- here_read("sl")

# check outputs
# summary of the SuperLearner output
print(sl)

#a detailed summary, including cross-validated risks
summary(sl)                #

# examination of cross-validated performance
# cross-validated risks for each learner
sl$cvRisk

# weights assigned to each learner in the final ensemble
sl$coef

# generate predictions
predictions <- predict(sl, newdata = df_clean_hot[full_predictor_vars], type = "response")

# extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot$pscore <- predictions$pred[, 1]

# check the structure of the predictions
str(df_clean_hot$pscore)

# check pscore
hist(df_clean_hot$pscore)

# make censoring weights
df_clean_hot$weights <- ifelse(df_clean_hot$t0_lost == 1,
                               1 / df_clean_hot$pscore,
                               1 / (1 - df_clean_hot$pscore))

# check
hist(df_clean_hot$weights)

# obtain stablise weights
marginal_censored <- mean(df_clean_hot$t0_lost)

# check (fyi)
marginal_censored


# stabalised weights
df_clean_hot$weights_stabilised <- ifelse(
  df_clean_hot$t0_lost == 1,
  marginal_censored / df_clean_hot$pscore,
  (1 - marginal_censored) / (1 - df_clean_hot$pscore)
)

# checks
hist(df_clean_hot$weights_stabilised)
max(df_clean_hot$weights_stabilised)
min(df_clean_hot$weights_stabilised)

# save output of hot code dataset
here_save(df_clean_hot, "df_clean_hot")

# get weights into the model
# new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights_stabilised * df_clean$t0_sample_weights

# checks
min(df_clean$t0_combo_weights)
max(df_clean$t0_combo_weights)

# check distrobution of weights
hist(df_clean$t0_combo_weights)

# next remove those who were lost between t0 and t1
df_clean_t1 <- df_clean |> filter(t0_lost == 0) |>
  select(-t1_perfectionism_z, -t1_lost, -t0_lost, -t0_sample_weights) |>
  relocate("t0_combo_weights", .before = starts_with("t1_"))

# check
hist(df_clean_t1$t0_combo_weights)

# checks
max(df_clean_t1$t0_combo_weights)
min(df_clean_t1$t0_combo_weights)

# number of weighted sample at t1, again check
n_censored_sample <- nrow(df_clean_t1)
n_censored_sample <- prettyNum(n_censored_sample, big.mark = ",")

# save output for manuscript
here_save(n_censored_sample, "n_censored_sample")

# check
n_censored_sample

# no one missing in exposure
# check
table(is.na(df_clean_t1$n_censored_sample)) # none

# gets us the correct df for weights

# check column oder and missing ness
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

#check
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

# get correct censoring -----------------------------------------
# THIS CODE IS redundant but NO HARM DONE
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0
# baseline_vars
# df_impute_base$t0_sample_weights


df_clean_t2 <- df_clean_t1 %>%
  # select(-t0_alert_level_combined_lead) |>
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  # mutate(t0_lost = 1 - t0_censored) |>
  mutate(t1_lost = 1 - t1_censored) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  select(-t1_lost, -t0_censored)

## END REDUNDANT
# test
nrow(df_clean_t2)
colnames(df_clean_t2)
# checks
hist(df_clean_t2$t0_combo_weights)

# outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)

# save
here_save(df_clean_t2, "df_clean_t2")



# check propensity scores -------------------------------------------------
# imbalance plot ----------------------------------------------------------
df_clean_t2 <- here_read("df_clean_t2")

# view
hist(df_clean_t2$t0_combo_weights)

# if you are comparing 2 x subgroups out of n > 2 groups,  do this
# df_subgroup <-df_clean_t2 |> filter(t0_eth_cat == "maori" | t0_eth_cat == "euro") |> droplevels()
#
# # save
# here_save(df_subgroup, "df_subgroup")

# copy data and make the group to be compared a factor
df_sub <- df_clean_t2

# make factor
df_sub$t0_born_nz <- as.factor(df_clean_t2$t0_born_nz)

# make propensity score model. Need correct covariates
baseline_vars_models = df_clean_t2 |>
  dplyr::select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
baseline_vars_models

# needed for subgroup ps
baseline_vars_models_sans_group <- setdiff(baseline_vars_models, "t0_born_nz")


# equation string
string <- formula_str <- as.formula(paste(
  "t1_perfectionism",
  "~",
  paste(baseline_vars_models, collapse = "+")
))

# equation string for subgroup analysis
string_sans <- formula_str <- as.formula(paste(
  "t1_perfectionism",
  "~",
  paste(baseline_vars_models_sans_group, collapse = "+")
))


# iptw marginal analysis
iptw_marginal  <- WeightIt::weightit(
  string,
  method = "ebal",
  estimand = "ATE",
  weights = "t0_combo_weights",
  #focal = "set",
  data = df_clean_t2
)
summary_iptw_marginal <- summary(iptw_marginal)
here_save(summary_iptw_marginal, "summary_iptw_marginal")


# note any extreme weights
plot(summary(iptw_marginal))

# save model
here_save(iptw_marginal, "iptw_marginal")


# iptw conditional analysis  won't work
# no diff in the groups
# iptw_conditional <- weightit(
#   string_sans,
#   method = "ebal",
#   estimand = "ATE",
#   by = "t0_born_nz",
#   weights = "t0_combo_weights",
#   #focal = "set", # if att
#   data = df_sub
# )
#
# # view
# summary(iptw_conditional)

# save model
# here_save(iptw_conditional, "iptw_conditional")
# summary_iptw_conditional <- summary(iptw_conditional)
# here_save(summary_iptw_conditional, "summary_iptw_conditional")



colnames(df_nz)

# visualise imbalance
love_plot_marginal <-
  love.plot(
    iptw_marginal,
    binary = "std",
    thresholds = c(m = .1),
    wrap = 50,
    position = "bottom",
    size = 3
  )

# view
love_plot_marginal

# save for manuscript
here_save(love_plot_marginal, "love_plot_marginal")
#
#love_plot_conditional
# love_plot_conditional <-
#   love.plot(
#     iptw_conditional,
#     cluster = "t0_born_nz",
#     binary = "std",
#     thresholds = c(m = .1),
#     wrap = 50,
#     position = "bottom",
#     size = 2
#   )
# love_plot_conditional



# START ANALYSIS HERE --------------------------------------------------------------
# read data --  start here if previous work already done
df_clean_t2 <- here_read("df_clean_t2")

# check
colnames(df_clean_t2)

# check
str(df_clean_t2)

# names of vars for modelling
names_base <-
  df_clean_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
names_base

# get outcome names for checks
names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

# check
names_outcomes

# obsessively check
names(df_clean_t2)

# check against this
names_base

# df_final_base  <- df_clean_t2[names_base]
str(df_clean_t2)


# lets one hot encode any categorical vars, here only t0_eth_cat

# this code is the same as above
encoded_vars <- model.matrix(~ t0_eth_cat  - 1, data = df_clean_t2)


# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df %>%
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe

# ensure to remove original categorical variables to avoid duplication

# note new data `df_clean_t2`
df_clean_hot_t2 <- df_clean_t2 %>%
  select(-c(t0_eth_cat)) %>%
  bind_cols(encoded_df) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  #  relocate("t0_censored", .before = starts_with("t1_"))  |>
  relocate("t1_censored", .before = starts_with("t2_"))

# check names
colnames(df_clean_hot_t2)

# # extract and print the new column names for encoded variables
# new_encoded_colnames_t2 <- colnames(encoded_df)
# print(new_encoded_colnames_t2)
#
# print(new_encoded_colnames_t2)
# # get baseline variable set without factors
#
# baseline_vars_set_t2 <- setdiff(names(df_clean_hot_t2), c("id","t0_eth_cat"))
# set_final_names <- c(baseline_vars_set_t2, new_encoded_colnames_t2)

# check
set_final_names

# set names for analysis
set_final_names <-
  df_clean_hot_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()


# add the new encoded column names

# check
full_predictor_vars_t2

# check
colnames(df_clean_hot_t2)


# model estimation --------------------------------------------------------


# estimate models ---------------------------------------------------------
library(lmtp)
library(SuperLearner)
library(xgboost)
library(ranger)
library(future)
# model charitable giving
# this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
set.seed(0112358)
plan(multisession)

# hopefully you have 10 :). if not, consider decreasing the number of folds (for this assessment)
n_cores <- parallel::detectCores() - 2


#### SET VARIABLE NAMES
#  model
A <- c("t1_perfectionism")
C <- c("t1_censored")
W <- set_final_names


# get max value of data
# make data_final
df_final <- df_clean_hot_t2


# for later use
here_save(df_final, "df_final")
here_save(W, "W")


# last checks
colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)



#  write function(s) for shift(s) ------------------------------------------------------
# get enpoints
max_data <- max(df_final$t1_perfectionism)


# shift function
gain_A <- function(data, trt) {
  ifelse(data[[trt]] < max_data - 1, data[[trt]] + 1, max_data)
}

# loss_A <- function(data, trt) {
#   ifelse(data[[trt]] > min_scale + 1, data[[trt]] - 1, min_scale)
# }



# changing your function to be fixed at 7 if you like...
fixed_shift_to_7 <- function(data, trt) {
  ifelse(data[[trt]] != 7,  7, data[[trt]])
}

# changing your function to be fixed at 0 if you like...
fixed_shift_to_0 <- function(data, trt) {
  ifelse(data[[trt]] != 0,  0, data[[trt]])
}


# set libraries
sl_lib <- c("SL.glmnet", "SL.ranger", #
            "SL.xgboost") #

# view superlearners
listWrappers()

# test data
df_clean_slice <- df_final |>
  slice_head(n = 1000) |>
  as.data.frame()



# Models!
t2_kessler_latent_anxiety_z_null_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = NULL,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# evaluate predictive performance of the models
# however prediction isn't always best
t2_kessler_latent_anxiety_z_null_test$fits_r
t2_kessler_latent_anxiety_z_null_test$fits_m
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")


# test gain
t2_kessler_latent_anxiety_z_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = gain_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_kessler_latent_anxiety_z_test

# view learner performance
t2_kessler_latent_anxiety_z_test$fits_r
t2_kessler_latent_anxiety_z_test$fits_m

# save
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")

# test contrast
test_contrast_anxiety <- lmtp::lmtp_contrast(t2_kessler_latent_anxiety_z_test , ref = t2_kessler_latent_anxiety_z_null_test)

# check
test_contrast_anxiety


# tests look good, let's run the model ------------------------------------
# models ------------------------------------------------------------------


# anxiety -- marginal -----------------------------------------------------
t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# checks
t2_kessler_latent_anxiety_z_gain$fits_r
t2_kessler_latent_anxiety_z_gain$fits_m

# view
t2_kessler_latent_anxiety_z_gain

# save model
here_save(t2_kessler_latent_anxiety_z_gain,
          "t2_kessler_latent_anxiety_z_gain")


# null model
t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = NULL,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_kessler_latent_anxiety_z_null

# save model
here_save(t2_kessler_latent_anxiety_z_null,
          "t2_kessler_latent_anxiety_z_null")

# depression marginal  ----------------------------------------------------
t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W,
  shift = gain_A,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
t2_kessler_latent_depression_z_gain

# save model
here_save(t2_kessler_latent_depression_z_gain,
          "t2_kessler_latent_depression_z_gain")


# null model
t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W,
  shift = NULL,
  data = df_final,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_kessler_latent_depression_z_null

# save model
here_save(t2_kessler_latent_depression_z_null,
          "t2_kessler_latent_depression_z_null")



# subgroup models ---------------------------------------------------------
# df_final <- here_read("df_final") # if needed


dt_18 <- dat_long |> 
  filter(wave == 2018)

## in baseline sample 
df_18 |> dplyr::filter( dplyr::filter(born_nz == 0)) |> droplevels()

# select participant n at basel
n_baseline_participants_born_nz_no<- dt_18 |> dplyr::filter(born_nz == 0) |> droplevels()
n_baseline_participants_born_nz_yes<- dt_18 |> dplyr::filter(born_nz == 1) |> droplevels()

n_baseline_participants_born_overseas<- nrow( n_baseline_participants_born_nz_no)
n_baseline_participants_born_nz <- nrow ( n_baseline_participants_born_nz_yes )
       
       
# make pretty
n_baseline_participants_born_overseas <-   prettyNum(n_baseline_participants_born_overseas, big.mark = ",")
n_baseline_participants_born_nz <-
  prettyNum(n_baseline_participants_born_nz, big.mark = ",")

# save
here_save(n_baseline_participants_born_overseas, "n_baseline_participants_born_overseas")
here_save(n_baseline_participants_born_nz, "n_baseline_participants_born_nz")


# sanity checks
n_participants_born_nz
n_participants_born_overseas

here_save(n_participants_born_nz, "n_participants_born_nz")
here_save(n_participants_born_overseas,
          "n_participants_born_overseas")



##
df_born_nz_no  <- df_final |> dplyr::filter(t0_born_nz == 0) |> droplevels()
df_born_nz_yes <- df_final |> dplyr::filter(t0_born_nz == 1)  |> droplevels()

# checks
n_participants_born_nz <- nrow(df_born_nz_yes)
n_participants_born_overseas <- nrow(df_born_nz_no)



# make pretty
n_participants_born_nz <-
  prettyNum(n_participants_born_nz, big.mark = ",")
n_participants_born_overseas <-
  prettyNum(n_participants_born_overseas, big.mark = ",")

# sanity checks
n_participants_born_nz
n_participants_born_overseas

here_save(n_participants_born_nz, "n_participants_born_nz")
here_save(n_participants_born_overseas,
          "n_participants_born_overseas")

# need to remove the "born nz" name as we are stratifying
W_sub <- setdiff(W, "t0_born_nz")

# check
W_sub



# subgroup-  born overseas ------------------------------------------------

# anxiety -- born overseas -----------------------------------------------------
df_born_nz_no_t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
# save model
here_save(
  df_born_nz_no_t2_kessler_latent_anxiety_z_gain,
  "df_born_nz_no_t2_kessler_latent_anxiety_z_gain"
)


# null model
df_born_nz_no_t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_no_t2_kessler_latent_anxiety_z_null

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_anxiety_z_null,
  "df_born_nz_no_t2_kessler_latent_anxiety_z_null"
)

# depression born overseas  ----------------------------------------------------
df_born_nz_no_t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_no_t2_kessler_latent_depression_z_gain

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_depression_z_gain,
  "df_born_nz_no_t2_kessler_latent_depression_z_gain"
)


# null model
df_born_nz_no_t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_no,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_no$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_no_t2_kessler_latent_depression_z_null

# save model
here_save(
  df_born_nz_no_t2_kessler_latent_depression_z_null,
  "df_born_nz_no_t2_kessler_latent_depression_z_null"
)


# subgroup born nz --------------------------------------------------------
# subgroup-  born nz ------------------------------------------------

# anxiety -- born nz -----------------------------------------------------
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_anxiety_z_gain,
  "df_born_nz_yes_t2_kessler_latent_anxiety_z_gain"
)


# null model
df_born_nz_yes_t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_yes_t2_kessler_latent_anxiety_z_null

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_anxiety_z_null,
  "df_born_nz_yes_t2_kessler_latent_anxiety_z_null"
)



# depression born nz  ----------------------------------------------------
df_born_nz_yes_t2_kessler_latent_depression_z_gain <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = gain_A,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)


# view
df_born_nz_yes_t2_kessler_latent_depression_z_gain

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_depression_z_gain,
  "df_born_nz_yes_t2_kessler_latent_depression_z_gain"
)


# null model
df_born_nz_yes_t2_kessler_latent_depression_z_null <- lmtp_tmle(
  outcome = "t2_kessler_latent_depression_z",
  baseline = W_sub,
  shift = NULL,
  data = df_born_nz_yes,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_born_nz_yes$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
df_born_nz_yes_t2_kessler_latent_depression_z_null

# save model
here_save(
  df_born_nz_yes_t2_kessler_latent_depression_z_null,
  "df_born_nz_yes_t2_kessler_latent_depression_z_null"
)




# compute contrasts -------------------------------------------------------

# import marginal models
t2_kessler_latent_anxiety_z_gain <- margot::here_read("t2_kessler_latent_anxiety_z_gain")
t2_kessler_latent_anxiety_z_null <- margot::here_read("t2_kessler_latent_anxiety_z_null")
t2_kessler_latent_depression_z_gain <- margot::here_read("t2_kessler_latent_depression_z_gain")
t2_kessler_latent_depression_z_null <- margot::here_read("t2_kessler_latent_depression_z_null")


# contrast marginal anxiety
contrast_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_kessler_latent_anxiety_z_gain,
                ref =  t2_kessler_latent_anxiety_z_null,
                type = "additive")

# save for manuscript
here_save(contrast_t2_kessler_latent_anxiety_z,
          "contrast_t2_kessler_latent_anxiety_z")

# make table
tab_contrast_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z,
                                                                       scale = "RD",
                                                                       new_name = "marginal: anxiety")

#view
tab_contrast_t2_kessler_latent_anxiety_z

# contrast marginal depression
contrast_t2_kessler_latent_depression_z <-
  lmtp_contrast(t2_kessler_latent_depression_z_gain,
                ref =  t2_kessler_latent_depression_z_null,
                type = "additive")

# save for manuscript

here_save(
  contrast_t2_kessler_latent_depression_z,
  "contrast_t2_kessler_latent_depression_z"
)



# make table
tab_contrast_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z,
                                                                          scale = "RD",
                                                                          new_name = "marginal: depression")

# view
tab_contrast_t2_kessler_latent_depression_z

# import born overseas models
df_born_nz_no_t2_kessler_latent_anxiety_z_gain <- margot::here_read("df_born_nz_no_t2_kessler_latent_anxiety_z_gain")
df_born_nz_no_t2_kessler_latent_anxiety_z_null <- margot::here_read("df_born_nz_no_t2_kessler_latent_anxiety_z_null")
df_born_nz_no_t2_kessler_latent_depression_z_gain <- margot::here_read("df_born_nz_no_t2_kessler_latent_depression_z_gain")
df_born_nz_no_t2_kessler_latent_depression_z_null <- margot::here_read("df_born_nz_no_t2_kessler_latent_depression_z_null")

# contrast born overseas anxiety
contrast_df_born_nz_no_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(df_born_nz_no_t2_kessler_latent_anxiety_z_gain,
                ref =  df_born_nz_no_t2_kessler_latent_anxiety_z_null,
                type = "additive")

# make table
tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_no_t2_kessler_latent_anxiety_z,
  scale = "RD",
  new_name = "born overseas: anxiety"
)

# view
tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z

# contrast marginal depression
contrast_df_born_nz_no_t2_kessler_latent_depression_z <-
  lmtp_contrast(
    df_born_nz_no_t2_kessler_latent_depression_z_gain,
    ref =  df_born_nz_no_t2_kessler_latent_depression_z_null,
    type = "additive"
  )

# make table
tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_no_t2_kessler_latent_depression_z,
  scale = "RD",
  new_name = "born overseas: depression"
)


#view
tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z

# import born nz models
df_born_nz_yes_t2_kessler_latent_anxiety_z_gain <- margot::here_read("df_born_nz_yes_t2_kessler_latent_anxiety_z_gain")
df_born_nz_yes_t2_kessler_latent_anxiety_z_null <- margot::here_read("df_born_nz_yes_t2_kessler_latent_anxiety_z_null")
df_born_nz_yes_t2_kessler_latent_depression_z_gain <- margot::here_read("df_born_nz_yes_t2_kessler_latent_depression_z_gain")
df_born_nz_yes_t2_kessler_latent_depression_z_null <- margot::here_read("df_born_nz_yes_t2_kessler_latent_depression_z_null")


# contrast born nz anxiety
contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(df_born_nz_yes_t2_kessler_latent_anxiety_z_gain,
                ref =  df_born_nz_yes_t2_kessler_latent_anxiety_z_null,
                type = "additive")

# make table
tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z,
  scale = "RD",
  new_name = "born NZ: anxiety"
)

# view
tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z

# contrast born nz depression
contrast_df_born_nz_yes_t2_kessler_latent_depression_z <-
  lmtp_contrast(
    df_born_nz_yes_t2_kessler_latent_depression_z_gain,
    ref =  df_born_nz_yes_t2_kessler_latent_depression_z_null,
    type = "additive"
  )

# make table
tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z <- margot::margot_lmtp_evalue(
  contrast_df_born_nz_yes_t2_kessler_latent_depression_z,
  scale = "RD",
  new_name = "born NZ: depression"
)

# view
tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z


# make tables -------------------------------------------------------------

# marginal tables
tab_marginal_outcomes <- rbind(
  tab_contrast_t2_kessler_latent_anxiety_z,
  tab_contrast_t2_kessler_latent_depression_z
)

# save
margot::here_save(tab_marginal_outcomes, "tab_marginal_outcomes")
# born overseas tables

# table with evalues for the graph
group_tab_marginal_outcomes <- margot::group_tab(tab_marginal_outcomes, type = "RD")

# view
group_tab_marginal_outcomes

# save
margot::here_save(group_tab_marginal_outcomes, "group_tab_marginal_outcomes")


# subgroups
tab_born_overseas_outcomes <-
  rbind(
    tab_contrast_df_born_nz_no_t2_kessler_latent_anxiety_z,
    tab_contrast_df_born_nz_no_t2_kessler_latent_depression_z
  )

# save
margot::here_save(tab_born_overseas_outcomes, "tab_born_overseas_outcomes")
tab_born_overseas_outcomes

# table with evalues for the graph
group_tab_born_overseas_outcomes <- margot::group_tab(tab_born_overseas_outcomes, type = "RD")

# view
group_tab_born_overseas_outcomes

# save
margot::here_save(group_tab_born_overseas_outcomes,
                  "group_tab_born_overseas_outcomes")



# born nz tables
tab_born_nz_outcomes <-
  rbind(
    tab_contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z,
    tab_contrast_df_born_nz_yes_t2_kessler_latent_depression_z
  )

# save
margot::here_save(tab_born_nz_outcomes , "tab_born_nz_outcomes")


# table with evalues for the graph
group_tab_born_nz_outcomes <- margot::group_tab(tab_born_nz_outcomes, type = "RD")

# save
margot::here_save(group_tab_born_nz_outcomes, "group_tab_born_nz_outcomes")


# view all
group_tab_marginal_outcomes
group_tab_born_overseas_outcomes
group_tab_born_nz_outcomes




# make graphs -------------------------------------------------------------

group_tab_marginal_outcomes
group_tab_not_born_overseas_outcomes
group_tab_born_nz_outcomes


n_participants

title = "Marginal Effect of One Point Increase in Perfectionism on Distress"
title_null = "N = 19,735, New Zealand Attitudes and Values Study (synthetic data)"


plot_group_tab_marginal_outcomes <- margot_plot(
  group_tab_marginal_outcomes,
  type = "RD",
  title =  "Perfectionism + One vs No Intervention ",
  subtitle = "on Depression Anxiety",
  estimate_scale = 1,
  base_size = 18,
  text_size = 4.5,
  point_size = 4,
  title_size = 20,
  subtitle_size = 14,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -.5,
  x_lim_lo = -.5,
  x_lim_hi = .5
)

plot_group_tab_marginal_outcomes

push_mods
ggsave(
  plot_group_tab_all_warm,
  path = here::here(here::here(push_mods)),
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_group_tab_all_warm.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 300
)


# do the subgroup graphs here: --------------------------------------------





# subgroup comparisons ----------------------------------------------------

contrast_anxiety_yes <- contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z
contrast_anxiety_no <- contrast_df_born_nz_no_t2_kessler_latent_anxiety_z

contrast_anxiety_yes
contrast_anxiety_no

contrast_depression_yes <- contrast_df_born_nz_yes_t2_kessler_latent_anxiety_z
contrast_depression_no <- contrast_df_born_nz_no_t2_kessler_latent_anxiety_z
# first calculate the difference in the means
g_hat_theta <- contrast_yes$vals$theta - contrast_no$vals$theta

# then calculate the se_diff

sqrt((contrast_yes$vals$std.error ^ 2) + (contrast_no$vals$std.error ^ 2))


# obtain difference
se_diff = sqrt((contrast_yes$vals$std.error ^ 2) + (contrast_no$vals$std.error ^ 2))

# compute confidence intervals
conf_low = g_hat_theta - (1.97 * se_diff)
conf_high = g_hat_theta + (1.97 * se_diff)


out <- cbind.data.frame(g_hat_theta, se_diff, conf_low, conf_high)

out <- round(out, 4)

out

# we can write a function to do this for us

compute_difference_means <- function(group1, group2) {
  # extract means and standard errors from each group
  mean_A <- group1$vals$theta
  mean_B <- group2$vals$theta
  se_A <- group1$vals$std.error
  se_B <- group2$vals$std.error
  
  #compute difference in means and standard error of the difference
  mean_difference <- mean_A - mean_B
  se_diff <- sqrt(se_A ^ 2 + se_B ^ 2)
  
  # compute 95% confidence intervals (using 1.96 for Z-value)
  conf_low <- mean_difference - (1.96 * se_diff)
  conf_high <- mean_difference + (1.96 * se_diff)
  
  # create output data frame and round the results
  out <- data.frame(
    mean_difference = round(mean_difference, 4),
    std_error = round(se_diff, 4),
    conf_low = round(conf_low, 4),
    conf_high = round(conf_high, 4)
  )
  
  return(out)
}


# use function
difference_in_group_means_anxiety  <- margot::compute_difference_means(contrast_anxiety_yes, contrast_anxiety_no)
difference_in_group_means_anxiety
here_save(difference_in_group_means_anxiety,
          "difference_in_group_means_anxiety")


difference_in_group_means_depression  <- margot::compute_difference_means(contrast_depression_yes, contrast_depression_no)
difference_in_group_means_depression
here_save(difference_in_group_means_depression,
          "difference_in_group_means_depression")




## use glue in your document as follows
glue::glue(
  "The difference in means is {difference_in_group_means$mean_difference} with a 95% CI of [{difference_in_group_means$conf_low}, {difference_in_group_means$conf_high}]."
)






# HETEROGENEITY -----------------------------------------------------------




# IGNORE BELOW -- THIS IS EXTRA FOR INVESTIGATING HETEROGENEITY -----------

# EXTRA: heterogeneity with GRF -------------------------------------------
# see: https://grf-labs.github.io/grf/
#devtools::install_github("grf-labs/grf", subdir = "r-package/grf")
library(grf)

# read data
colnames (df_final)

# read data
df_grf <- here_read("df_final")

# get baseline names
names_grf <- here_read("W")


# check all indicators are numeric or binary
colnames(df_grf)
str(df_grf). # we won't use "id"

# sample weights
t0_combo_weights <- df_grf$t0_combo_weights

table(df_grf$t1_censored)

# get censoring indicator, note that "censored" has the
# **opposite meaning in lmtp models!  we need to make D = "not_lost"
t1_lost = 1 - df_grf$t1_censored


#check
table(t1_lost)

# add to grf
df_grf$t1_lost <- t1_lost

# label this D
D <- as.factor (1 - df_grf$t1_censored)

# get key data features
nrow(df_grf)

#names_grf

# use standard deviation units for exposure, so that +1 = +1 SD units
t1_perfectionism_z <- scale(df_grf$t1_perfectionism)


# select exposure
selected_A = matrix(df_grf$t1_perfectionism_z) # standard deviation of exposure
selected_Y = matrix(df_clean_hot$t2_kessler_latent_anxiety_z)


# select covariates, make sure to remove attributes (we did this above)
cen_X <- cbind(df_grf[names_grf], t1_perfectionism_z)


# predict censoring
cen_forest <- probability_forest(cen_X, D)


# generate predictions
predictions_grf <- predict(cen_forest, newdata = cen_X, type = "response")
predictions_grf
# extract predictions from the 'pred' component and ensure it's a vector
pscore <- predictions_grf$pred[, 2]

hist(pscore)
mean(pscore)
sd(pscore)

df_grf$pscore <- pscore

# make censoring weights
df_grf$cen_weights <- ifelse(t1_lost == 1, 1 / pscore, 1 / (1 - pscore))

# view
hist(df_grf$cen_weights, breaks = 50)
# check
hist(df_grf$cen_weights)

# obtain stablise weights
marginal_censored <- mean(df_grf$t1_lost)

marginal_censored

df_grf$t1_lost

# stabalised weights
df_grf$weights_stabilised <- ifelse(
  df_grf$t1_lost == 1,
  marginal_censored / df_grf$pscore,
  (1 - marginal_censored) / (1 - df_grf$pscore)
)


# checks
hist(df_grf$weights_stabilised, breaks = 50)

max(df_grf$weights_stabilised)
min(df_grf$weights_stabilised)



# check

# set up data
df_grf$t1_not_lost = 1 - df_grf$t1_censored


# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# you can probably just use "SL.glmnet"
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")


sl_2 <- SuperLearner(
  Y = df_grf$t1_not_lost,
  X = cen_X,
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)



# save your super learner model
here_save(sl_2, "sl_2")


# stop the cluster
stopCluster(cl)


# check outputs
print(sl_2)                  # summary of the SuperLearner output
summary(sl_2)                # a detailed summary, including cross-validated risks

# examination of cross-validated performance
sl_2$cvRisk                  # cross-validated risks for each learner
sl_2$coef                    # weights assigned to each learner in the final ensemble



# generate predictions
predictions_super <- predict(sl_2, newdata = cen_X, type = "response")


# very similar to grf
mean(predictions_super$pred[, 1])
sd(predictions_super$pred[, 1])


# extract predictions from the 'pred' component and ensure it's a vector
df_grf$super_pscore <- predictions_super$pred[, 1]

# check the structure of the predictions
str(df_grf$super_pscore)

# check pscore
hist(df_grf$super_pscore)

# make censoring weights
df_grf$super_weights <- ifelse(t1_lost == 1, 1 / df_grf$super_pscore, 1 / (1 - df_grf$super_pscore))

# check
hist(df_grf$super_weights, breaks = 50)


# stabalise
df_grf$weights_stabilised_super <- ifelse(
  df_grf$t1_lost == 1,
  marginal_censored / df_grf$super_pscore,
  (1 - marginal_censored) / (1 - df_grf$super_pscore)
)



# checks
hist(df_grf$weights_stabilised_super , breaks = 50)
max(df_grf$weights_stabilised_super)
min(df_grf$weights_stabilised_super)

# compare with causal forest
hist(df_grf$weights_stabilised , breaks = 50)
max(df_grf$weights_stabilised)
min(df_grf$weights_stabilised)


# lets use superlearner (it gives us forests and more)


# ok for combo weights to be labelled t0 because we just have a point estimate
df_grf$t0_combo_weights_w2 <- df_grf$weights_stabilised_super  * df_grf$t0_combo_weights

hist(df_grf$t0_combo_weights_w2 , breaks = 50)
max(df_grf$t0_combo_weights_w2)
min(df_grf$t0_combo_weights_w2)

colnames(df_grf)
here_save(df_grf, "df_grf")

df_grf_t2 <- df_grf |>
  filter(t1_censored == 1) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
  relocate("t1_censored", .before = starts_with("t2_"))

colnames(df_grf_t2)


# save data with weights
here_save(df_grf, "df_grf")

summary(df_grf$t1_perfectionism)
#  make treatment binary
df_grf_t2 <- df_grf_t2 |>
  mutate(t1_perfectionism_binary = as.integer(ifelse(t1_perfectionism > 4, 1, 0)))

# variables need to be in matrix form

# make it so that large is good
g_W  = matrix(1 - df_grf_t2$t1_perfectionism_z)
t2_kessler_latent_depression_z = matrix(df_grf_t2$t2_kessler_latent_depression_z)

# make it so that large is good
g_Y = matrix(1 - df_grf_t2$t2_kessler_latent_anxiety_z)

# set binary exposure so that large is absence of perfectionism
g_W_binary = matrix(1 - df_grf_t2$t1_perfectionism_binary)


g_weights <- df_grf_t2$t0_combo_weights_w2

# checks
str(g_W)
str(g_X)
str(g_W)
str(g_Y)
str(g_weights)
g_Y
# model anxiety
tau_forest_t2_kessler_latent_anxiety_z <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W,
  sample.weights = g_weights
)

# save
here_save(
  tau_forest_t2_kessler_latent_anxiety_z,
  'tau_forest_t2_kessler_latent_anxiety_z'
)


# view
tau_forest_t2_kessler_latent_anxiety_z

# ATE
anxiety_forest_ate <- average_treatment_effect(tau_forest_t2_kessler_latent_anxiety_z, target.sample = "all")


# save
here_save(anxiety_forest_ate, "anxiety_forest_ate")

# check out
anxiety_forest_ate

# model anxiety
tau_forest_anxiety_binary <- grf::causal_forest(
  X = g_X,
  Y = g_Y,
  W = g_W_binary,
  sample.weights = g_weights
)

# save
here_save(tau_forest_anxiety_binary, 'tau_forest_anxiety_binary')


# ATE
bin_anxiety_forest_ate <- average_treatment_effect(tau_forest_anxiety_binary, target.sample = "overlap")
bin_anxiety_forest_ate

# save
here_save(bin_anxiety_forest_ate, "bin_anxiety_forest_ate")


# not possible with a continuous treatement, but this is the code for binary treatments
bin_anxiety_att <- average_treatment_effect(tau_forest_anxiety_binary, target.sample = "treated")
bin_anxiety_att

# get a histogram that shows heterogeniety
tau.hat.oob <- predict(tau_forest_anxiety_binary)

# show
hist(tau.hat.oob$predictions)

# description of heterogeneity
best_linear_projection(tau_forest_anxiety_binary, g_X)

names_grf
# this only works for binary treatments
rate <- rank_average_treatment_effect(tau_forest_anxiety_binary, g_X[, "t0_eth_cateuro"])

#
plot(rate, ylab = "Euro", main = "TOC: ranked by decreasing weight")

# #
# forest.W <- regression_forest(g_X, g_W, tune.parameters = "all")
# #
# W.hat <- predict(forest.W)$predictions
#
# #
# forest.Y <- regression_forest(g_X, g_Y, tune.parameters = "all")
#
# #
# Y.hat <- predict(forest.Y)$predictions


forest.Y.varimp <- variable_importance(tau_forest_anxiety_binary)
#forest.Y.varimp
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.99)
selected.vars
colnames(g_X)

# obtain treatment effect in the most important predictors
tau.forest <- causal_forest(
  g_X[, selected.vars],
  g_Y,
  g_W_binary,
  W.hat = W.hat,
  Y.hat = Y.hat,
  tune.parameters = "all",
  sample.weights = g_weights
)

# not must different
average_treatment_effect(tau.forest, target.sample = "all")


# training sample
n <- nrow(g_X) # n in sample

set.seed(123)
train <- sample(1:n, n / 2) # get half sampl
train

# training sample
train.forest <- causal_forest(g_X[train, ], g_Y[train], g_W_binary[train], sample.weights = g_weights[train])

# eavaluation sample
eval.forest <- causal_forest(g_X[-train, ], g_Y[-train], g_W_binary[-train], sample.weights = g_weights[-train])

# rank on new data (ony supports binary treatment effects)
rate <- rank_average_treatment_effect(eval.forest, predict(train.forest, g_X[-train, ])$predictions)
plot(rate)

average_treatment_effect(train.forest, target.sample = "treated")
average_treatment_effect(eval.forest, target.sample = "treated")


#tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
# paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


##
library(policytree)
library(DiagrammeR)

# get ate
ate <- average_treatment_effect(tau_forest_anxiety_binary)

# check for overlap
hist(tau_forest_anxiety_binary$W.hat)

# quick eval
varimp <- variable_importance(tau_forest_anxiety_binary)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
ranked.vars


# access the column names from your data frame using these indices
ranked.cols <- colnames(g_X)[ranked.vars]

# display the ordered
ranked.cols


# not much evidence for heterogeneity!
best_linear_projection(tau_forest_t2_kessler_latent_anxiety_z, g_X[ranked.vars[1:5]])


# Compute doubly robust scores
# dr.scores <- grf::get_scores(tau_forest_anxiety_binary)


# will only work for binary variables
dr.scores <- double_robust_scores(tau_forest_anxiety_binary)
dr.scores

# # Use as the ATE as a "cost" of program treatment to find something non-trivial
# cost <- ate[["estimate"]]
# cost
# -dr.scores
#
# dr.rewards <- cbind.data.frame(control = -dr.scores,
#                      treat = dr.scores - cost)
# dr.rewards
# # plot overlap
use_X <- g_X[, selected.vars]
head(use_X)
tree <- policy_tree(use_X, dr.scores, depth = 2)
tree_full <- policy_tree(g_X, dr.scores, depth = 2)

#save
here_save(tree, "tree")
here_save(tree_full, "tree_full")

print(tree)
plot(tree)
dev.off()
print(tree_full)
plot(tree_full)

# Predict the treatment assignment {1, 2} for each sample.
predicted <- predict(tree_full, g_X)
plot(X[, 1], X[, 2], col = predicted)
legend("topright",
       c("control", "treat"),
       col = c(1, 2),
       pch = 19)
abline(0, -1, lty = 2)
dev.off()
node.id <- predict(tree_full, g_X, type = "node.id")

values <- aggregate(
  dr.scores,
  by = list(leaf.node = node.id),
  FUN = function(x)
    c(mean = mean(x), se = sd(x) / sqrt(length(x)))
)
print(values, digits = 2)


# eval grf fit ------------------------------------------------------------


# eval fit

# The overlap assumption requires a positive probability of treatment for each 𝑋𝑖
# . We should not be able to deterministically decide the treatment status of an individual based on its covariates, meaning none of the estimated propensity scores should be close to one or zero. One can check this with a histogram:
hist(e.hat <- tau.forest$W.hat)

W = g_W
# One can also check that the covariates are balanced across the treated and control group by plotting the inverse-propensity weighted histograms of all samples, overlaid here for each feature (done with ggplot2 which supports weighted histograms):
IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

min(IPW)

#Make long

df <- cbind.data.frame(g_W, g_X_binary, IPW)
df
head(df)
table(df$g_W)

# Load the necessary library
library(tidyr)

# Reshape the dataframe
df_long <- df %>%
  pivot_longer(cols = starts_with("t0_"),
               names_to = "variable",
               values_to = "value") |>
  mutate(W = factor(g_W))

df_long$value

ggplot(df_long, aes(x = value, weight = IPW, fill = W)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30) +
  facet_wrap( ~ variable, ncol = 2)


ggplot(df,
       aes(
         x = t0_religion_church_round_z,
         weight = IPW,
         fill = as.factor(g_W)
       )) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = 30)




n <- 2000
p <- 10
X <- matrix(rnorm(n * p), n, p)
dim(X)
X
X.test <- matrix(0, 101, p)

dim(X.test)

X.test[, 1] <- seq(-2, 2, length.out = 101)
dim(X.test)
