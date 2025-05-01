suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(haven)
  library(mice)
  library(lmtest)
  library(sandwich)
  library(fs)
  library(knitr)     # kable()
  library(broom)
  library(grf)
  library(randomForest)
  library(ggplot2)
  
  
})




## ------------------------------------------------------------------
## 1.  Utility functions
## ------------------------------------------------------------------

clean_dataset_name <- function(path, root) {
  path %>%
    path_rel(start = root) %>%                  # 1) relative path
    path_ext_remove() %>%                       # 2) drop extension
    str_replace_all("[/\\s\\-]+", "_") %>%      # 3) tidy punctuation
    path_sanitize() %>%
    str_to_lower()
}

load_stata_folder <- function(root = here("Datasets")) {
  dir_ls(root, recurse = TRUE, type = "file", glob = "*.dta") |>
    set_names(~ clean_dataset_name(.x, root)) |>
    map(read_dta)
}

if (!exists("datasets")) {
  message("Loading datasets…")
  datasets <- load_stata_folder()
} else {
  message("`datasets` already in memory; skipping load.")
}

replace_missing <- function(data, codes = missing_codes) {
  data %>%
    mutate(across(
      any_of(names(codes)),
      ~ replace(.x, .x %in% codes[[cur_column()]], NA_real_)
    ))
}

## ------------------------------------------------------------------
## 2.  Key variables
## ------------------------------------------------------------------

# Education 
education_subset_29 <- datasets$sweep_6_age_29_stata_stata13_se_bcs6derived %>%
  select(BCSID, Highest_Academic_Level = HIACA00) %>%
  mutate(Highest_Academic_Level = na_if(Highest_Academic_Level, -9))

# Sex 
sex_subset_26 <- datasets$sweep_5_age_26_stata_stata13_bcs96x %>%
  transmute(
    BCSID = bcsid,
    Sex   = sex %>%
      haven::zap_labels() %>%
      na_if(-1) %>%
      factor(levels = c(1, 2), labels = c("Male", "Female"))
  )

hourly_wage_bcs6 <- function(df) {
  missing_codes <- list(
    cgropay   = c(9999998, 9999999),
    cgroprd   = c(6, 8, 9),
    cgropred  = 20:24,
    chours1   = c(98, 99),
    chours2   = c(998, 999),
    chours3   = c(998, 999),
    chours4   = c(998, 999)
  )
  
  period_multipliers <- c(
    `1`  = 52,   `2`  = 26,  `3`  = 13,  `4`  = 12,
    `5`  = 1,    `6`  = 52/3, `7` = 52/5, `8` = 52/6,
    `9`  = 52/7, `10` = 52/8, `11` = 6,    `12` = 8,
    `13` = 9,    `14` = 10,   `15` = 4,    `16` = 2
  )
  
  # 2) inline helper that zaps any code → NA
  replace_missing <- function(data) {
    data %>%
      mutate(across(
        all_of(names(missing_codes)),
        ~ replace(.x, .x %in% missing_codes[[cur_column()]], NA_real_)
      ))
  }
  
  # 3) the main pipeline
  df %>%
    select(
      bcsid, econact,
      cgropay, cgroprd, cgropred,
      chours1:chours4
    ) %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    replace_missing() %>%                                         # zap bad codes
    mutate(
      ## pay period
      period_code = coalesce(cgropred, cgroprd),
      multiplier  = unname(period_multipliers[as.character(period_code)]),
      
      ## money & hours
      annual_pay   = cgropay * multiplier,
      Weekly_hours = coalesce(
        chours1 + chours3,
        chours2 + chours3,
        chours1,
        chours2
      ),
      Weekly_hours = if_else(between(Weekly_hours, 1, 99),
                             Weekly_hours, NA_real_),
      
      ## final wage
      Hourly_pay = annual_pay / (Weekly_hours * 52),
      Hourly_pay = if_else(between(Hourly_pay, 0, 500),
                           Hourly_pay, NA_real_),
      ln_wage    = log(Hourly_pay)
    ) %>%
    select(
      BCSID        = bcsid,
      Employment_Status     = econact,
      Hourly_pay,
      Weekly_hours,
      ln_wage
    )
}

salary_subset_29 <- hourly_wage_bcs6(
  datasets$sweep_6_age_29_stata_stata13_se_bcs2000
)

## ------------------------------------------------------------------
## 3. Control variables
## ------------------------------------------------------------------

missing_codes <- list(
  Region10        = c(-2, -1),
  Social_class10  = c(-2, -1),
  Family_income10 = c(-1,  8),
  ReadingScore10  = c(-1),
  MathsScore10    = c(-1),
  FinalSchoolType = c(-1),
  OrgType29       = c(8,  9),
  FirmSize29     = c(8,  9),
  UnionMem29      = c(8,  9),
  HouseOwnership10           = c(-1),
  SchoolDaysMissed10         = c(-1),
  SibCode10       = c(-1)
)



## 1. Background variables at age 10

background_subset_10 <- datasets$sweep_3_age_10_stata_stata13_bcs3derived %>%
  select(
    BCSID           = bcsid,
    Region10        = bd3regn,
    Social_class10  = bd3psoc,
    Family_income10 = bd3inc
  ) %>%
  replace_missing()


## 2. Cognitive scores at age 10

cognitive_subset_10 <- datasets$sweep_3_age_10_stata_stata13_bcs3derived %>%
  select(
    BCSID          = bcsid,
    ReadingScore10 = bd3read,
    MathsScore10   = bd3maths
  ) %>%
  replace_missing()


## 3. School type at age 10

school_subset_10 <- datasets$sweep_4_headteacher_stata_stata13_bcs70_htq %>%
  select(
    BCSID           = bcsid,
    FinalSchoolType = schtype
  ) %>%
  replace_missing() %>%
  mutate(
    PrivateSchool10 = if_else(FinalSchoolType == 2, 1, 0)
  )


## 4. Employment at age 29

employment_subset_29 <- datasets$sweep_6_age_29_stata_stata13_se_bcs2000 %>%
  select(
    BCSID        = bcsid,
    OrgType29    = cjorg,
    FirmSize29  = cjemps2,
    UnionMem29   = unionmem
  ) %>%
  replace_missing()


## 5. Additional background at age 10

background2_subset_10 <- datasets$sweep_3_age_10_stata_stata13_se_sn3723 %>%
  select(
    BCSID     = bcsid,
    HouseOwnership10     = d2,
    SchoolDaysMissed10   = j111,
    SibCode10 = a4a_42
  ) %>%
  replace_missing() %>%
  mutate(
    Siblings10 = SibCode10 - 1
  ) %>%
  select(BCSID, HouseOwnership10, SchoolDaysMissed10, Siblings10)

## Exam Results at age 26
# turn any negative or 88 into NA, then return zero if still NA
na_to_zero <- function(x) {
  x <- ifelse(x < 0 | x == 88, NA, x)
  replace(x, is.na(x), 0)
}

## 2. mutate the data ----------------------------------------------------
exams_subset_26 <- datasets$sweep_5_age_26_stata_stata13_bcs96x %>%
  # ---- GCSE / O-level / CSE block (age-16) ----
mutate(
  ## CSEs
  cse_gr1      = na_to_zero(b960148),  # grade 1 (equivalent to O-level/GCSE A-C)
  cse_gr25     = na_to_zero(b960151),  # grades 2-5
  
  ## O-levels
  olevel_ac    = na_to_zero(b960154),  # A-C
  olevel_de    = na_to_zero(b960157),  # D-E
  
  ## GCSEs
  gcse_ac      = na_to_zero(b960160),  # A-C
  gcse_de      = na_to_zero(b960163),  # D-G/“other”
  
  ## Scottish equivalents (optional – drop if you prefer England & Wales only)
  scot_o_ac    = na_to_zero(b960169),  # ‘O’ Grade A-C
  scot_o_de    = na_to_zero(b960172),  # ‘O’ Grade D-E
  ssg_13       = na_to_zero(b960175),  # Standard Grade 1-3  (≈ A-C)
  ssg_4plus    = na_to_zero(b960178),  # Standard Grade ≥4   (≈ D-G)
  
  ## aggregate HIGH (A-C / grade 1 / SG 1-3) and LOW (D-E etc.)
  gcs_block_hi = cse_gr1 + olevel_ac + gcse_ac + scot_o_ac + ssg_13,
  gcs_block_lo = cse_gr25 + olevel_de + gcse_de + scot_o_de + ssg_4plus,
  
  
  # ---- A-/S-/AS-level block (age-18) ----
  alevel_tot   = na_to_zero(b960166),
  a_block_tot   = alevel_tot,         # plus any slevel_ac / aslevel_ac
) %>%
  select(
    BCSID = bcsid,
    GCS_hi_26 = gcs_block_hi,
    GCS_lo_26 = gcs_block_lo,
    A_levels_26 = a_block_tot
  )



## ------------------------------------------------------------------
## 4.  Assemble the master file
## ------------------------------------------------------------------

base_variables <- reduce(
  list(salary_subset_29, sex_subset_26, education_subset_29),
  full_join, by = "BCSID"
)

master_data <- reduce(
  list(
    base_variables,
    background_subset_10,
    cognitive_subset_10,
    school_subset_10,
    employment_subset_29,
    background2_subset_10,
    exams_subset_26
  ),
  full_join,
  by = "BCSID"
)

names(master_data)


analysis <- master_data %>% 
  filter(
    Employment_Status %in% c(1,2),
    Weekly_hours > 0,
    Hourly_pay > 1, #Filtering to remove results below minimum wage
    Highest_Academic_Level > 3,
    !is.na(ln_wage),
    !is.na(Sex)          
  )

lower_bound <- quantile(analysis$Hourly_pay, 0.01, na.rm = TRUE)
upper_bound <- quantile(analysis$Hourly_pay, 0.99, na.rm = TRUE)

# now filter the analysis set to only those observations within those bounds
analysis <- analysis %>%
  filter(
    Hourly_pay >= lower_bound,
    Hourly_pay <= upper_bound
  )

## Factorising Categorical Variables
num_cols <- c(
  "Hourly_pay", "Weekly_hours", "ln_wage",
  "ReadingScore10", "MathsScore10",
  "SchoolDaysMissed10", "Siblings10",
  "GCS_hi_26", "GCS_lo_26", "A_levels_26"
)
fac_cols <- setdiff(names(analysis), c("BCSID", num_cols))

analysis <- analysis %>%
  mutate(
    # convert all labelled or integer codes to factors
    across(all_of(fac_cols), ~ as_factor(.x)),
    # ensure truly continuous vars are naked numeric
    across(all_of(num_cols), as.numeric)
  )

analysis$Highest_Academic_Level <- droplevels(analysis$Highest_Academic_Level)
analysis <- analysis %>%
  mutate(
    Highest_Academic_Level = fct_recode(
      Highest_Academic_Level,
      "1 A level"             = "1 A level or more than 1 AS level at gr",
      "2 or more A-levels"    = "2 or more A-levels",
      "Diploma of HE"         = "Diploma of HE",
      "Degree"                = "Degree, other degree level",
      "Higher degree"         = "Higher degree"
    )
  )
levels(analysis$Highest_Academic_Level)

analysis
library(forcats) 
control_vars <- c(
  "Region10",
  "ReadingScore10",
  "MathsScore10",
  "PrivateSchool10",
  "Social_class10",
  "Family_income10",
  "HouseOwnership10",
  "SchoolDaysMissed10",
  "Siblings10",
  "OrgType29",
  "FirmSize29",
  "UnionMem29",
  "GCS_hi_26",
  "GCS_lo_26",
  "A_levels_26"
)

add_na_flags <- function(df, vars){
  df %>% mutate(across(
    all_of(vars),
    .fns = list(na = ~ as.integer(is.na(.x))),
    .names = "{.col}_NA"
  ))
}


analysis2 <- analysis %>%
  add_na_flags(control_vars) %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),                    # cheap numeric fill
    across(where(is.factor),  ~ fct_na_value_to_level(.x, "Missing"))  # NEW name
  )

## ------------------------------------------------------------------
## 5.  Regressions - Blundell et al Specification
## ------------------------------------------------------------------


spec1 <- ln_wage ~ Highest_Academic_Level

spec2 <- update(spec1, . ~ . +
                  Region10 + ReadingScore10 + MathsScore10 +
                  PrivateSchool10 +
                  Region10_NA + ReadingScore10_NA + MathsScore10_NA +
                  PrivateSchool10_NA)

spec3 <- update(spec2, . ~ . +
                  OrgType29 + UnionMem29 +
                  HouseOwnership10 + SchoolDaysMissed10 + Siblings10 +
                  OrgType29_NA + UnionMem29_NA +
                  HouseOwnership10_NA + SchoolDaysMissed10_NA + Siblings10_NA)

run_lm <- function(df, fml, sex, digits = 4){
  mod <- lm(fml, data = df %>% filter(Sex == sex))
  ct  <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
  printCoefmat(ct, digits = digits, format = "f")   # <-- added
}

# --------- MEN ------------------------------------------------------------
cat("\n*** Men – specification 1 ***\n"); run_lm(analysis2, spec1, "Male")
cat("\n*** Men – specification 2 ***\n"); run_lm(analysis2, spec2, "Male")
cat("\n*** Men – specification 3 ***\n"); run_lm(analysis2, spec3, "Male")

# --------- WOMEN ----------------------------------------------------------
cat("\n*** Women – specification 1 ***\n"); run_lm(analysis2, spec1, "Female")
cat("\n*** Women – specification 2 ***\n"); run_lm(analysis2, spec2, "Female")
cat("\n*** Women – specification 3 ***\n"); run_lm(analysis2, spec3, "Female")


## ------------------------------------------------------------------
## 5.  Regressions – Preferred Specification 
## ------------------------------------------------------------------

## 1. SPECIFICATION I – “parsimonious”: pre-degree controls only
spec_I <- ln_wage ~ Highest_Academic_Level +
  Region10 + ReadingScore10 + MathsScore10 +
  PrivateSchool10 + Social_class10 +
  Region10_NA + ReadingScore10_NA + MathsScore10_NA +
  PrivateSchool10_NA + Social_class10_NA

cat("\n*** Men – Spec I ***\n");    spec_I_men   <- run_lm(analysis2, spec_I, "Male")
cat("\n*** Women – Spec I ***\n");  spec_I_women <- run_lm(analysis2, spec_I, "Female")

## 2. SPECIFICATION II – adds GCSE & A-level attainment
spec_II <- update(
  spec_I, . ~ . +
    GCS_hi_26 + GCS_lo_26 + A_levels_26 +
    GCS_hi_26_NA + GCS_lo_26_NA + A_levels_26_NA
)

cat("\n*** Men – Spec II ***\n");   spec_II_men   <- run_lm(analysis2, spec_II, "Male")
cat("\n*** Women – Spec II ***\n"); spec_II_women <- run_lm(analysis2, spec_II, "Female")

## ------------------------------------------------------------------
## 5.  Regressions Results Comparison
## ------------------------------------------------------------------



models <- list(
  men_spec1       = run_lm(analysis2, spec1,   "Male"),
  men_spec2       = run_lm(analysis2, spec2,   "Male"),
  men_spec3       = run_lm(analysis2, spec3,   "Male"),
  women_spec1     = run_lm(analysis2, spec1,   "Female"),
  women_spec2     = run_lm(analysis2, spec2,   "Female"),
  women_spec3     = run_lm(analysis2, spec3,   "Female"),
  men_spec_I      = run_lm(analysis2, spec_I,  "Male"),
  women_spec_I    = run_lm(analysis2, spec_I,  "Female"),
  men_spec_II     = run_lm(analysis2, spec_II, "Male"),
  women_spec_II   = run_lm(analysis2, spec_II, "Female")
)

extract_degree <- function(fit, model_name) {
  tidy(fit) %>%                                    # term / estimate / std.error / statistic / p.value
    filter(term == "Highest_Academic_LevelDegree") %>%
    transmute(
      model   = model_name,
      coef    = estimate,
      p_value = p.value
    )
}
my_results <- imap_dfr(models, ~
                         tidy(.x) %>%
                         filter(term=="Highest_Academic_LevelDegree") %>%
                         transmute(
                           sex          = ifelse(grepl("^men", .y), "Men", "Women"),
                           specification= sub("^(men|women)_", "", .y),
                           my_coef      = signif(estimate, 3),
                           my_p_stars   = case_when(
                             p.value < 0.01 ~ "***",
                             p.value < 0.05 ~ "**",
                             p.value < 0.10 ~ "*",
                             TRUE           ~ ""
                           )
                         )
)

paper_results <- tribble(
  ~sex,  ~specification, ~paper_coef, ~paper_sig,  ~source,
  "Men",   "spec1",       0.202,        "***",     "Table 2",
  "Men",   "spec2",       0.198,        "***",     "Table 2",
  "Men",   "spec3",       0.154,        "***",     "Table 2",
  "Women", "spec1",       0.274,        "***",     "Table 3",
  "Women", "spec2",       0.245,        "***",     "Table 3",
  "Women", "spec3",       0.232,        "***",     "Table 3",
  "Men",   "spec_I",      0.189,        "***",     "Table 4",
  "Women", "spec_I",      0.229,        "***",     "Table 4",
  "Men",   "spec_II",     0.142,        "***",     "Table 4",
  "Women", "spec_II",     0.180,        "***",     "Table 4"
)

comparison_tbl <- my_results %>%
  left_join(paper_results, by = c("sex","specification")) %>%
  select(
    Sex            = sex,
    Specification  = specification,
    `My coef`      = my_coef,
    `Paper coef`   = paper_coef,
    `My signif.`   = my_p_stars,
    `Paper signif.`= paper_sig,
    Source         = source
  ) %>%
  arrange(Sex, Specification)

# Overall correlation
overall_cor <- comparison_tbl %>%
  summarise(
    corr = cor(`My coef`, `Paper coef`)
  ) %>%
  pull(corr)

# Correlation by sex
by_sex_cor <- comparison_tbl %>%
  group_by(Sex) %>%
  summarise(
    corr = cor(`My coef`, `Paper coef`)
  )
overall_cor


### ------------------------------------------------------------------------------------
### PROXYING AND MATCHING
### ------------------------------------------------------------------------------------

# ---------- 1.  sample and treatment indicator --------------------
analysis_sub <- analysis2 %>%
  filter(Highest_Academic_Level %in% c("Degree", "1 A level", "2 or more A-levels")) %>%
  mutate(UG_degree = Highest_Academic_Level == "Degree")

# ---------- 2.  variables used as proxies ------------------------
proxy_vars <- c(
  "Region10", "ReadingScore10", "MathsScore10",
  "PrivateSchool10", "Social_class10", "Family_income10",
  "HouseOwnership10", "SchoolDaysMissed10", "Siblings10",
  "GCS_hi_26", "GCS_lo_26", "A_levels_26"
)

# ---------- 4.  regression formula --------------------------------
full_rhs <- c(
  "UG_degree",
  proxy_vars,
  paste0(proxy_vars, "_NA")   # add the flag dummies
)
reg_formula <- reformulate(full_rhs, response = "ln_wage")

run_lm <- function(df, sex) {
  mod <- lm(reg_formula, data = df %>% filter(Sex == sex))
  coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
}

cat("\n===========  MEN  ===========\n")
print(run_lm(analysis_sub, "Male"))

cat("\n===========  WOMEN  =========\n")
print(run_lm(analysis_sub, "Female"))



# ---------------------------------------------------------------
#  PROPENSITY-SCORE MATCHING  (nearest neighbour, ATT on treated)
# ---------------------------------------------------------------
library(MatchIt)     # matching
library(lmtest); library(sandwich)   # robust SE


flag_vars    <- paste0(proxy_vars, "_NA")
ps_formula   <- reformulate(c(proxy_vars, flag_vars), response = "UG_degree")


run_psm <- function(df_sex, sex_lbl){
  # ----- 2. propensity-score model ------------------------------
  ps_mod <- glm(ps_formula, data = df_sex, family = binomial)
  
  # ----- 3. nearest-neighbour match -----------------------------
  m_out <- matchit(
    ps_formula, data = df_sex,
    method   = "nearest",
    distance = "logit",          # use linear predictor
    replace  = TRUE,             # as in the paper
    caliper  = .2,               # ≈ 0.2·σ(logit-PS)
    ratio    = 1
  )
  cat("\n=== Balance for", sex_lbl, "===\n")
  print(summary(m_out, standardize = TRUE))   # balance table
  
  # ----- 4. ATT estimate on matched sample ----------------------
  mdat   <- match.data(m_out)
  att_lm <- lm(ln_wage ~ UG_degree, data = mdat, weights = weights)
  cat("\n*** ATT (robust HC1) –", sex_lbl, "***\n")
  print(coeftest(att_lm, vcovHC(att_lm, type = "HC1")))
  invisible(att_lm)
}

att_men   <- run_psm(filter(analysis_sub, Sex == "Male"),   "Men")
att_women <- run_psm(filter(analysis_sub, Sex == "Female"), "Women")


# ---------------------------------------------------------------
#  Random FOREST
# ---------------------------------------------------------------

lm_model <- lm(ln_wage ~ . - UG_degree - Highest_Academic_Level - BCSID, data = analysis_sub)
summary(lm_model)

analysis_sub$predicted_lm <- predict(lm_model)

set.seed(123)  # for reproducibility

rf_model <- randomForest(
  ln_wage ~ . - UG_degree - Highest_Academic_Level - BCSID,
  data = analysis_sub,
  ntree = 500,       # 500 trees
  mtry = floor(sqrt(ncol(analysis_sub) - 3))  # √p rule (excluding BCSID, UG_degree, Highest_Academic_Level)
)

print(rf_model)
analysis_sub$predicted_rf <- predict(rf_model)


ggplot(analysis_sub, aes(x = ln_wage, y = predicted_lm)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  ggtitle("OLS Predictions vs Actual ln_wage")

ggplot(analysis_sub, aes(x = ln_wage, y = predicted_rf)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, colour = "blue") +
  ggtitle("Random Forest Predictions vs Actual ln_wage")


# R-squared for OLS
cor(analysis_sub$ln_wage, analysis_sub$predicted_lm)^2

# R-squared for Random Forest
cor(analysis_sub$ln_wage, analysis_sub$predicted_rf)^2

#───────────────────────────────────────────────────────────────────────────────#
# CAUSAL FORESTS
#───────────────────────────────────────────────────────────────────────────────#

#───────────────────────────────────────────────────────────────────────────────#
# 1.  PRE-TREATMENT COVARIATES
#───────────────────────────────────────────────────────────────────────────────#
baseline_vars <- c(
  "Region10",
  "ReadingScore10", "MathsScore10",
  "PrivateSchool10",
  "Social_class10", "Family_income10",
  "HouseOwnership10",
  "SchoolDaysMissed10", "Siblings10",
  "GCS_hi_26", "GCS_lo_26", "A_levels_26"
)

#───────────────────────────────────────────────────────────────────────────────#
# 2.  BUILD OUTCOME, TREATMENT, AND COVARIATE MATRICES
#───────────────────────────────────────────────────────────────────────────────#
df <- analysis_sub %>% 
  mutate(W = as.numeric(UG_degree))    # ensure 0/1

Y <- df$ln_wage          # outcome
W <- df$W                # treatment

X <- df %>% 
  select(all_of(baseline_vars)) %>%    # *only* baseline covariates
  mutate(across(where(is.factor), as.numeric)) %>% 
  as.data.frame()

# drop any constant columns (safety)
X <- X[, vapply(X, function(z) length(unique(z)) > 1, logical(1))]

#───────────────────────────────────────────────────────────────────────────────#
# 3.  FIT HONEST CAUSAL FOREST
#───────────────────────────────────────────────────────────────────────────────#
set.seed(2025)
cf <- causal_forest(
  X, Y, W,
  honesty       = TRUE,
  num.trees     = 2000,
  sample.fraction = 0.5
)

# quick diagnostic plot
hist(cf$W.hat, 30,
     main = "Propensity estimates (post-cleaning)",
     xlab = "Internal p̂")

#───────────────────────────────────────────────────────────────────────────────#
# 4.  OVERALL ATE (OVERLAP ESTIMAND)
#───────────────────────────────────────────────────────────────────────────────#
ate_overlap <- average_treatment_effect(cf, target.sample = "overlap")
print(ate_overlap)      # log-point effect and s.e.

#───────────────────────────────────────────────────────────────────────────────#
# 5.  ROBUSTNESS: EXTERNAL, CLIPPED PROPENSITY SCORES
#───────────────────────────────────────────────────────────────────────────────#
p_ext <- glm(W ~ ., data = data.frame(W, X), family = binomial)$fitted
p_ext <- pmin(pmax(p_ext, 0.01), 0.99)  # clip to [0.01,0.99]

cf_clip <- causal_forest(X, Y, W,
                         W.hat   = p_ext,
                         honesty = TRUE,
                         num.trees = 2000)

ate_clip <- average_treatment_effect(cf_clip)
print(ate_clip)

#───────────────────────────────────────────────────────────────────────────────#
# 6.  INDIVIDUAL CATEs + DISTRIBUTION PLOT
#───────────────────────────────────────────────────────────────────────────────#
analysis_sub$CATE <- predict(cf)$predictions

ggplot(analysis_sub, aes(CATE)) +
  geom_histogram(bins = 40, colour = "white") +
  labs(title = "Distribution of individual CATEs",
       x = "Estimated log-wage effect of a degree",
       y = "Count")

#───────────────────────────────────────────────────────────────────────────────#
# 7.  SUBGROUP EFFECTS: FEMALE vs MALE (OVERLAP POPULATION)
#───────────────────────────────────────────────────────────────────────────────#
idx_f <- which(analysis_sub$Sex == "Female")
idx_m <- which(analysis_sub$Sex == "Male")

subgrp <- rbind(
  Female = unlist(average_treatment_effect(cf, subset = idx_f,
                                           target.sample = "overlap")[1:2]),
  Male   = unlist(average_treatment_effect(cf, subset = idx_m,
                                           target.sample = "overlap")[1:2])
)
print(subgrp)



# 1. Gender
gender_cates <- analysis_sub %>%
  group_by(Sex) %>%
  summarise(
    mean_cate = mean(CATE, na.rm = TRUE),
    se_cate   = sd(CATE, na.rm = TRUE)/sqrt(n()),
    n         = n()
  )


print(subgrp)

kable(gender_cates, digits = 3,
      col.names = c("Group", "Mean CATE (log‐points)", "s.e.", "N"))

# 2. SES quartiles
analysis_sub <- analysis_sub %>%
  mutate(SES_q = ntile(Family_income10, 4))

ses_cates <- analysis_sub %>%
  group_by(SES_q) %>%
  summarise(
    mean_cate = mean(CATE, na.rm = TRUE),
    se_cate   = sd(CATE, na.rm = TRUE)/sqrt(n()),
    n         = n()
  )

# 3. Maths-ability quartiles
analysis_sub <- analysis_sub %>%
  mutate(Math_q = ntile(MathsScore10, 4))

math_cates <- analysis_sub %>%
  group_by(Math_q) %>%
  summarise(
    mean_cate = mean(CATE, na.rm = TRUE),
    se_cate   = sd(CATE, na.rm = TRUE)/sqrt(n()),
    n         = n()
  )


diff_est <- subgrp["Female","estimate"] - subgrp["Male","estimate"]
diff_se  <- sqrt(subgrp["Female","std.err"]^2 + subgrp["Male","std.err"]^2)
z        <- diff_est / diff_se
pval     <- 2 * (1 - pnorm(abs(z)))

