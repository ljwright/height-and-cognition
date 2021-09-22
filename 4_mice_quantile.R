library(tidyverse)
library(haven)
library(glue)
library(gallimaufr)
library(ridittools)
library(DescTools)
library(broom)
library(furrr)
library(tictoc)
library(specr)
library(lme4)
library(mice)
library(quantreg)
library(magrittr)

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
           matches("height_chart"), matches("resid"),
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

load("Data/mice.Rdata")
imp_long <- map(imp,
                ~ complete(.x, "long", TRUE) %>%
                  as_tibble() %>%
                  rename(imp = .imp) %>%
                  select(-.id))
rm(imp)

# 3. Run Regressions ----
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
              type = c("cc", "mi")) %>%
  mutate(spec_id = row_number(), .before = 1)

get_result <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  obs <- df_obs %>%
    filter(cohort == spec$cohort,
           age == spec$age)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(male %in% mod_sex[[spec$sex]]) %>%
    semi_join(obs, by = "id") %>%
    rename(x_var = all_of(spec$x_var))
  
  mod_form <- glue("height_chart_{spec$age} ~ x_var + {mod_covars[[spec$mod]]}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  run_mod <- function(df) lm(as.formula(mod_form), df)
  
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

cohort_dict <- c("46c" = "1946c", "58c" = "1958c",
                 "70c" = "1970c", "mcs" = "2001c")
mod_dict <- c(basic = "Sex-Adjusted",
              sep = "+ SEP",
              height = "+ Parental Height")

df_res <- mod_specs %>%
  mutate(map_dfr(spec_id, get_result)) %>%
  mutate(cohort_clean = factor(cohort_dict[cohort], cohort_dict) %>%
           fct_rev(),
         mod_clean = factor(mod_dict[mod], mod_dict)) %>%
  mutate(cohort_clean = factor(cohort_dict[cohort], cohort_dict) %>%
           fct_rev(),
         mod_clean = factor(mod_dict[mod], mod_dict)) %>%
  filter(!str_detect(x_var, "comp")) %>%
  mutate(x_var = str_sub(x_var, 1, -10),
         age = ifelse(as.integer(age) < 14, 11, 16),
         x_var = glue("{str_to_title(x_var)} @ age {age}"),
         string = glue("{round(beta, 2)} ({round(lci, 2)}, {round(uci, 2)})"))

plot_res <- function(sex, type){
  p <- df_res %>%
    filter(sex == !!sex, type == !!type) %>%
    ggplot() +
    aes(x = cohort_clean, y = beta, ymin = lci, 
        ymax = uci, label = string) +
    facet_grid(x_var ~ mod_clean, switch = "y", scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(position = position_dodge(0.5)) +
    geom_text(size = 4, y = Inf, vjust = 0, hjust = 1.1) +
    scale_y_continuous(expand = c(0.05, 0.4)) +
    coord_flip() +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank(),
          axis.title = element_text(size = rel(0.8)),
          legend.position = "none") +
    labs(x = NULL, y = "Marginal Effect")
  
  glue("Images/main_{type}_{sex}.png") %>%
    ggsave(plot = p, width = 29.7, height = 21, units = "cm")
  
  return(p)
}

df_res %>%
  distinct(sex, type) %$%
  map2(sex, type, plot_res)


# 4. Get Quantile Regressions ----
get_qreg <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  df_reg <- imp_long[[spec$cohort]] %>%
    filter(imp == 0,
           male %in% mod_sex[[spec$sex]]) %>%
    rename(x_var = all_of(spec$x_var))
  
  mod_form <- glue("height_chart_{spec$age} ~ x_var + {mod_covars[[spec$mod]]}")
  if (spec$sex == "all") mod_form <- glue("{mod_form} + male")
  
  as.formula(mod_form) %>%
    rq(tau = 1:9/10, data = df_reg) %>%
    tidy() %>%
    filter(term == "x_var") %>%
    select(tau, beta = estimate, lci = conf.low, uci = conf.high)
}

df_q <- mod_specs %>%
  filter(type == "cc") %>%
  mutate(res = map(spec_id, get_qreg)) %>%
  unnest(res)

df_clean <- df_q %>%
  mutate(x_var = str_sub(x_var, 1, -10),
         age = ifelse(as.integer(age) < 14, 11, 16),
         x_var = glue("{str_to_title(x_var)} @ age {age}")) %>%
  filter(type == "cc", sex == "all", 
         x_var != "Comp @ age 16", mod == "sep") %>%
  mutate(tau_clean = glue("{tau*100}th"),
         cohort_clean = factor(cohort_dict[cohort], cohort_dict),
         cohort_f = cohort)

ggplot(df_clean) +
  aes(x = tau_clean, y = beta, ymin = lci, ymax = uci, group = cohort) +
  facet_grid(x_var ~ cohort_clean, switch = "y", scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line() +
  geom_line(aes(group = cohort_f),
            data = select(df_clean, -cohort_clean),
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
  
