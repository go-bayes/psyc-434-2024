# test function
library(margot)
library(dplyr)
# get ids

ids_2018 <- df_nz |>
  filter(year_measured == 1,
         wave == 2018) |>
  pull(id)

dat_long_full <- df_nz |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  arrange(id, wave) |>
  select(
    "id",
    "wave",
    "year_measured",
    # "edu",
    "religion_believe_god",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    # 0 = female, 0.5 = neither female nor male, 1 = male.
    "age",
    "born_nz",
    #   "hlth_disability",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    "edu",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei_13_l",
    # see nzavs materials
    "partner")

# set up
baseline_vars <- colnames(dat_long_full)
baseline_vars <- setdiff(baseline_vars, "id")
exposure_var <- c("partner")
outcome_vars <- c("religion_believe_god")

#devtools::install_github("go-bayes/margot")
df <- data.frame( dat_long_full) 

df_impute <- margot::margot_wide_impute_baseline(dat_long_full, 
                                         baseline_vars = baseline_vars, 
                                         exposure_var = exposure_var, 

                                         #                                         outcome_vars = outcome_vars)
naniar::vis_miss( 
  df_impute
)
margot_wide_impute_baseline()
