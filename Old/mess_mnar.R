library(mice)
library(estimatr)
library(tictoc)
library(furrr)
library(scico)
library(summarytools)


# 1. TRY PATTERN MIXTURE WITH DIFFERENT PARAMETERS FOR DIFFERENCE IN HEIGHT IN 4 COHORTS
# 2. GET DIFFERENCES IN ESTIMATES AND PLOT ACCORDING TO PARAMETERS
# 3. REGRESS HEIGHT_T ON COG_T & OBS_HEIGHT_T+1 & MALE TO SEE REASONABLE DIFFERENCE IN HEIGHT
# 4. OR COMPARE AGAINST A GROWTH CHART TO GET AN IDEA HOW MUCH SMALLER THEY MUST BE

rm(list = ls())

# 1. Load Data ----
load("Data/df_raw.Rdata")
load("Data/mice.Rdata")

df_mnar <- map_dfr(df_raw,
                   ~ .x %>%
                     select(id, cohort, matches("height_chart")) %>%
                     pivot_longer(-c(id, cohort), 
                                  names_to = "height_var",
                                  values_to = "height")) %>%
  mutate(age = str_sub(height_var, -2),
         obs = ifelse(is.na(height), 0, 1)) %>%
  select(-height)

imp_long <- map(imp,
                ~ complete(.x, "long", TRUE) %>%
                  as_tibble() %>%
                  rename(imp = .imp) %>%
                  select(-.id))
rm(imp, df_raw)


# 2. Model Functions ----
mod_covars <- list(basic = "1",
                   sep = c("father_class", "mother_edu_level"),
                   height = c("father_class", "mother_edu_level", 
                              "mother_height", "father_height")) %>%
  map(glue_collapse, " + ")

mod_sex <- list(all = c(0, 1), male = 1, female = 0)

mod_specs <- df_mnar %>%
  distinct(cohort, age) %>%
  mutate(cog_var = map2(cohort, age,
                        ~ names(imp_long[[.x]]) %>%
                          str_subset(glue("resid_{.y}")))) %>%
  unnest(cog_var) %>%
  expand_grid(sex = c("all", "male", "female"),
              mod = names(mod_covars),
              diff = seq(from = -1, to = 1, length.out = 21)) %>%
  filter(!str_detect(cog_var, "comp")) %>%
  mutate(spec_id = row_number(), .before = 1)

get_mnar <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(imp > 0,
           male %in% mod_sex[[spec$sex]]) %>%
    rename(height_mnar = any_of(glue("height_chart_{spec$age}")),
           cog_var = any_of(spec$cog_var)) %>%
    left_join(df_mnar %>%
                filter(age == !!spec$age),
              by = c("id", "cohort")) %>%
    mutate(height_mnar = ifelse(obs == 0, height_mnar + spec$diff, height_mnar)) %>%
    group_split(imp)
  
  covars <- mod_covars[[spec$mod]]
  mod_form <- glue("height_mnar ~ cog_var + {covars}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  map(df_reg, 
      ~ lm_robust(as.formula(mod_form), .x, survey_weight)) %>%
    pool() %>%
    tidy(conf.int = TRUE) %>%
    as_tibble() %>%
    filter(term == "cog_var") %>%
    select(beta = estimate, se = std.error,
           lci = conf.low, uci = conf.high)
}


set.seed(1)
tic()
plan(multisession, workers = 4)

df_res <- mod_specs %>%
  filter(mod == "basic", sex == "all") %>%
  sample_frac() %>%
  mutate(future_map_dfr(spec_id, get_mnar, .progress = TRUE)) %>%
  arrange(spec_id)

future:::ClusterRegistry("stop")
toc()

df_heat <- df_res %>%
  mutate(age = ifelse(as.numeric(age) < 14, 11, 16),
         cog_var = str_sub(cog_var, 1, -10),
         cog_var = glue("{cog_var} @ {age}")) %>%
  select(cohort, cog_var, diff, beta, se)

get_diff <- function(beta_x, se_x, beta_y, se_y){
  beta <- beta_y - beta_x
  z <- beta / sqrt((se_x^2)+(se_y^2))
  p <- pnorm(z)
  
  tibble(beta = beta, p = p)
}

df_clean <- expand_grid(rename_with(df_heat, ~ glue("{.x}_x")),
                        rename_with(df_heat, ~ glue("{.x}_y"))) %>%
  filter(cog_var_x == cog_var_y,
         cohort_x < cohort_y) %>%
  mutate(get_diff(beta_x, se_x, beta_y, se_y),
         across(c(diff_x, diff_y), factor)) %>%
  select(-beta_x, -beta_y, -se_x, -se_y) %>%
  filter(cog_var_x == "verbal @ 11",
         cohort_x != "2001c_white",
         cohort_y != "2001c_white") %>%
  mutate(cohort_y = glue("yo {cohort_y}"))

limits <- max(abs(df_clean$beta)) * c(-1, 1)

ggplot(df_clean) +
  aes(x = diff_x, y = diff_y, fill = beta) + # alpha = 1-p) +
  facet_grid(cohort_y ~ cohort_x) +
  geom_tile() +
  geom_abline() +
  theme_bw() +
  # scale_fill_scico(palette = "imola", limit = limits)  
  scale_fill_distiller(type = "div", limit = limits)


# Height Regression
attrit_specs <- mod_specs %>%
  distinct(cohort, age) %>%
  rename(age_fup = age) %>%
  left_join(mod_specs %>%
              distinct(cohort, age, cog_var), 
            by = "cohort") %>%
  filter(age < age_fup) %>%
  mutate(spec_id = row_number(), .before = 1)


get_attrit <- function(spec_id){
  spec <- slice(attrit_specs, !!spec_id)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(imp == 0) %>%
    left_join(df_mnar %>%
                filter(cohort == !!spec$cohort,
                       age == !!spec$age_fup),
              by = c("id", "cohort")) %>%
    mutate(diff = 1 - obs)
  
  glue("height_chart_{spec$age} ~ {spec$cog_var} + male + diff") %>%
    as.formula() %>%
    lm(df_reg) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "diff") %>%
    select(beta = 2, lci = 6, uci = 7) %>%
    mutate(across())
}

df_attrit <- attrit_specs %>%
  sample_frac() %>%
  mutate(map_dfr(spec_id, get_attrit)) %>%
  arrange(spec_id) %>%
  mutate(age = ifelse(as.numeric(age) < 14, 11, 16),
         cog_var = str_sub(cog_var, 1, -10),
         cog_var = glue("{cog_var} @ {age}"))

ggplot(df_attrit) +
  aes(x = cog_var, y = beta, ymin = lci, ymax = uci) +
  facet_wrap(cohort ~ age_fup, scales = "free_x") +
  geom_hline(yintercept = 0) +
  geom_pointrange()


map_dfr(imp_long,
        ~ .x %>%
          filter(imp == 0) %>%
          select(id, cohort, matches("height_chart")) %>%
          pivot_longer(-c(id, cohort), 
                       names_to = "height_var",
                       values_to = "height")) %>%
  drop_na() %>%
  group_by(cohort, height_var) %>%
  descr() %>%
  tb() %>%
  select(cohort, height_var, mean, sd) %>%
  mutate(age = str_sub(height_var, -2)) %>%
  distinct(cohort, age)
