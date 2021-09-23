library(tidyverse)
library(haven)
library(glue)
library(gallimaufr)
library(ridittools)
library(broom)
library(furrr)
library(tictoc)
library(mice)
library(quantreg)
library(magrittr)
library(estimatr)

rm(list = ls())

# 1. Load Data ----
load("Data/df_raw.Rdata")
load("Data/df_obs.Rdata")

# 2. Multiple Imputation ----
to_ridit <- function(x){
  x_dense <- dense_rank(x)
  
  x_ridit <- toridit(table(x_dense))[x_dense]
  
  as.numeric(x_ridit)
}

tic()
imp <- list()
for(cohort in names(df_raw)){
  df <- df_raw[[cohort]] %>%
    drop_na(male) %>%
    select(id, cohort, survey_weight, male,
           matches("chart"), matches("resid"),
           mother_height, father_height,
           father_class, mother_edu_level, father_edu_level) %>%
    mutate(across(matches("resid"), to_ridit))
  
  pred <- make.predictorMatrix(df)
  pred[, c("id", "cohort", "male")] <- 0
  
  temp <- list()
  for (male in c(0, 1)){
    df_mice <- filter(df, male == !!male)
    
    temp[[male + 1]] <- parlmice(df_mice, n.core = 4, n.imp.core = 8,
                                 predictorMatrix = pred)
  }
  
  imp[[cohort]] <- rbind(temp[[1]], temp[[2]])
}
toc()

rm(df, df_mice, temp, pred, male, cohort)

save(imp, file = "Data/mice.Rdata")

# 3. Run Regressions ----
load("Data/mice.Rdata")
imp_long <- map(imp,
                ~ complete(.x, "long", TRUE) %>%
                  as_tibble() %>%
                  rename(imp = .imp) %>%
                  select(-.id))
rm(imp)

mod_covars <- list(basic = "1",
                   sep = c("father_class", "mother_edu_level"),
                   height = c("father_class", "mother_edu_level", 
                              "mother_height", "father_height")) %>%
  map(glue_collapse, " + ")

mod_sex <- list(all = c(0, 1), male = 1, female = 0)

mod_specs <- df_obs %>%
  distinct(cohort, age) %>%
  mutate(x_var = map2(cohort, age,
                      ~ names(df_raw[[.x]]) %>%
                        str_subset(glue("resid_{.y}")))) %>%
  unnest(x_var) %>%
  expand_grid(sex = c("all", "male", "female"),
              mod = names(mod_covars),
              type = c("cc", "mi"),
              weights = c(TRUE, FALSE)) %>%
  filter(weights == TRUE | str_detect(cohort, "(1946|2001)"),
         !str_detect(x_var, "comp")) %>%
  mutate(spec_id = row_number(), .before = 1)

get_result <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  # obs <- df_obs %>%
  #   filter(cohort == spec$cohort,
  #          age == spec$age)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(male %in% mod_sex[[spec$sex]]) %>%
    # semi_join(obs, by = "id") %>%
    rename(x_var = all_of(spec$x_var))
  
  if (spec$weights == FALSE) df_reg$survey_weight <- 1
  
  mod_form <- glue("height_chart_{spec$age} ~ x_var + {mod_covars[[spec$mod]]}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  run_mod <- function(df) lm_robust(as.formula(mod_form), df, survey_weight)
  
  if (spec$type == "cc"){
    
    mod <- df_reg %>% 
      filter(imp == 0) %>%
      run_mod()
    
  } else{
    
    mod <- df_reg %>% 
      filter(imp > 0) %>%
      group_split(imp) %>%
      map(run_mod) %>%
      pool()
    
  }
  
  tidy(mod, conf.int = TRUE) %>%
    filter(term == "x_var") %>%
    select(beta = estimate, se = std.error,
           lci = conf.low, uci = conf.high)
}

mod_dict <- c(basic = "Sex-Adjusted",
              sep = "+ SEP",
              height = "+ Parental Height")

df_res <- mod_specs %>%
  mutate(map_dfr(spec_id, get_result)) %>%
  mutate(cohort_clean = fct_rev(cohort),
         mod_clean = factor(mod_dict[mod], mod_dict)) %>%
  mutate(x_var = str_sub(x_var, 1, -10),
         age = ifelse(as.integer(age) < 14, 11, 16),
         x_var = glue("{str_to_title(x_var)} @ age {age}"),
         string = glue("{round(beta, 2)} ({round(lci, 2)}, {round(uci, 2)})")) %>%
  uncount(ifelse(cohort %in% c("1958c", "1970c"), 2, 1), .id = "uncount") %>%
  mutate(weights = ifelse(uncount == 2, FALSE, weights)) %>%
  select(-uncount)

plot_main <- function(type, weights){
  p <- df_res %>%
    filter(sex == "all", type == !!type, weights == !!weights) %>%
    ggplot() +
    aes(x = cohort_clean, y = beta, ymin = lci, 
        ymax = uci, label = string) +
    facet_grid(x_var ~ mod_clean, switch = "y", scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(position = position_dodge(0.5)) +
    geom_text(aes(as.numeric(cohort_clean) + 0.2),
              size = 4, y = Inf, vjust = 0, hjust = 1.1) +
    scale_y_continuous(expand = c(0.05, 0.5)) +
    coord_flip() +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank(),
          axis.title = element_text(size = rel(0.8)),
          legend.position = "none") +
    labs(x = NULL, y = "Marginal Effect")
  
  weight_stub <- ifelse(weights == TRUE, "weighted", "unweighted")
  
  glue("Images/main_{type}_{weight_stub}.png") %>%
    ggsave(plot = p, width = 29.7, height = 21, units = "cm")
  
  return(p)
}

df_res %>%
  distinct(type, weights) %$%
  map2(type, weights, plot_main)

plot_sex <- function(type, weights){
  p <- df_res %>%
    filter(sex != "all", type == !!type, weights == !!weights) %>%
    mutate(sex_clean = str_to_title(sex)) %>%
    ggplot() +
    aes(x = cohort_clean, y = beta, ymin = lci, 
        ymax = uci, label = string, color = sex_clean) +
    facet_grid(x_var ~ mod_clean, switch = "y", scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_pointrange(position = position_dodge(0.5))  +
    scale_color_brewer(palette = "Dark2") +
    coord_flip() +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank(),
          axis.title = element_text(size = rel(0.8)),
          legend.position = "bottom") +
    labs(x = NULL, y = "Marginal Effect", color = NULL)
  
  weight_stub <- ifelse(weights == TRUE, "weighted", "unweighted")
  
  glue("Images/sex_{type}_{weight_stub}.png") %>%
    ggsave(plot = p, width = 29.7, height = 21, units = "cm")
  
  return(p)
}

df_res %>%
  distinct(type, weights) %$%
  map2(type, weights, plot_sex)


# 4. Get Quantile Regressions ----
get_qreg <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(imp == 0,
           male %in% mod_sex[[spec$sex]]) %>%
    rename(x_var = all_of(spec$x_var))
  
  if (spec$weights == FALSE) df_reg$survey_weight <- 1
  
  mod_form <- glue("height_chart_{spec$age} ~ x_var + {mod_covars[[spec$mod]]}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  as.formula(mod_form) %>%
    rq(tau = 1:9/10, data = df_reg, weights = survey_weight) %>%
    tidy() %>%
    filter(term == "x_var") %>%
    select(tau, beta = estimate, lci = conf.low, uci = conf.high)
}

df_q <- mod_specs %>%
  filter(type == "cc", sex == "all") %>%
  filter(mod == "basic", weights = TRUE) %>%
  mutate(res = map(spec_id, get_qreg)) %>%
  unnest(res)

df_clean <- df_q %>%
  mutate(x_var = str_sub(x_var, 1, -10),
         age = ifelse(as.integer(age) < 14, 11, 16),
         x_var = glue("{str_to_title(x_var)} @ age {age}")) %>%
  filter(type == "cc", sex == "all", 
         x_var != "Comp @ age 16", mod == "basic") %>%
  mutate(tau_clean = glue("{tau*100}th"),
         cohort_f = cohort)

df_clean %>%
  filter(weights == TRUE) %>%
  ggplot() +
  aes(x = tau_clean, y = beta, ymin = lci, ymax = uci, group = cohort) +
  facet_grid(x_var ~ cohort, switch = "y", scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line() +
  geom_line(aes(group = cohort_f),
            data = select(df_clean, -cohort),
            linetype = "dotted") +
  guides(color = "none", fill = "none") +
  labs(x = "Percentile", y = NULL) +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_blank(),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none")
ggsave("Images/quantile.png", width = 29.7, height = 21, units = "cm")


# Multiply Imputed Data
get_mi <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  mod_form <- glue("height_chart_{spec$age} ~ x_var + {mod_covars[[spec$mod]]}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  get_df <- function(imp){
    obs <- df_obs %>%
      filter(cohort == spec$cohort,
             age == spec$age)
    
    df_reg <- imp_long[[spec$cohort]] %>%
      filter(imp == !!imp,
             male %in% mod_sex[[spec$sex]]) %>%
      semi_join(obs, by = "id") %>%
      rename(x_var = all_of(spec$x_var))
    
    if (spec$weights == FALSE) df_reg$survey_weight <- 1
    
    return(df_reg)
  }
  
  run_mods <- function(imp){
    df <- get_df(imp)
    
    run_mod <- function(boot, df){
      set.seed(boot)
      as.formula(mod_form) %>%
        rq(tau = 1:9/10, data = sample_frac(df), 
           weights = survey_weight) %>%
        tidy() %>%
        filter(term == "x_var") %>%
        select(tau, estimate)
    }
    
    map_dfr(1:20, run_mod, df)
  }
  
  get_ci <- function(estimate){
    quantile(estimate, probs = c(.5, .025, .975)) %>% 
      set_names(c("beta", "lci", "uci")) %>%
      as_tibble_row()
  }
  
  map_dfr(1:3, run_mods) %>%
    group_by(tau) %>%
    summarise(get_ci(estimate))
}




mod_specs %>%
  filter(mod == "basic", type == "mi", sex == "all", weights == TRUE)
