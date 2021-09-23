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
library(patchwork)

rm(list = ls())

# 1. Load Data ----
load("Data/df_raw.Rdata")
load("Data/df_obs.Rdata")


# 2. Add Variables ----
# Time Invariant
to_ridit <- function(x){
  x_dense <- dense_rank(x)
  
  x_ridit <- toridit(table(x_dense))[x_dense]
  
  as.numeric(x_ridit)
}

df_inv <- map_dfr(df_raw, select, -matches("_..$")) %>%
  select(-matches("^age"), -father_household) %>%
  relocate(cohort, .after = 1) %>%
  group_by(cohort) %>%
  mutate(across(c(father_class, mother_edu_years, mother_edu_level, 
                  father_edu_years, father_edu_level),
                list(ridit = to_ridit))) %>%
  ungroup()

# Time Variant
make_long <- function(df){
  df %>%
    pivot_longer(cols = matches("_[0-9][0-9]$"),
                 names_to = c(".value", "fup"),
                 names_pattern = "(.*)_(..)") %>%
    mutate(fup = as.numeric(fup))
}

df_height <- df_raw %>%
  map_dfr(~ .x %>%
            select(id, cohort, male, matches("^height.*_..$")) %>%
            make_long()) %>%
  rename(height_raw = height) %>%
  group_by(cohort, male, fup) %>%
  mutate(height_rank = 100*percent_rank(height_raw),
         height_ridit = to_ridit(height_raw),
         height_z = wtd_scale(height_raw)) %>%
  ungroup() %>%
  drop_na()

df_cog <- df_raw %>%
  map_dfr(~ .x %>%
            select(id, cohort, male, matches("^(maths|verbal|vocab)")) %>%
            make_long())  %>%
  rename_with(~ glue("{.x}_raw"), .cols = c(maths, verbal, vocab)) %>%
  pivot_longer(cols = matches("^(maths|verbal|vocab)"),
               names_to = "test", values_to = "score") %>%
  separate(test, c("test", "type"), sep = "_") %>%
  drop_na() %>%
  semi_join(df_height, by = c("id", "cohort", "fup")) %>%
  group_by(cohort, male, fup, test, type) %>%
  mutate(rank = percent_rank(score),
         z = wtd_scale(score),
         ridit = to_ridit(score)) %>%
  ungroup()

save(df_inv, df_cog, df_height,
     file = "Data/df_analysis.Rdata")

# 3. Set-Up Model Objects ----
covars <- df_inv %>%
  select(male, matches("^(mother|father)"),
         -matches("_(bmi|weight)"), -father_class_alt) %>%
  names()

num_detect <- function(x, pattern){
  str_detect(x, pattern) %>% sum()
}

mod_covars <- map(1:length(covars), ~ combn(covars, .x, simplify = FALSE)) %>%
  flatten() %>%
  keep(~ num_detect(.x, "male") > 0) %>%
  keep(~ num_detect(.x, "father_class") <= 1) %>%
  keep(~ num_detect(.x, "father_edu") <= 1) %>%
  keep(~ num_detect(.x, "mother_edu") <= 1) %>%
  keep(~ num_detect(.x, "_height") %in% c(0, 2)) %>%
  keep(~ num_detect(.x, "_edu_years") %in% c(0, 2)) %>%
  keep(~ num_detect(.x, "_edu_level") %in% c(0, 2))

mod_sexes <- list(all = c(0, 1), female = 0, male = 1)

mod_specs <- df_cog %>%
  distinct(cohort, fup, test, type) %>%
  filter(type == "resid") %>%
  expand_grid(cog_score = c("rank", "z", "ridit"),
              height_score = glue("height_{c('raw', 'chart', 'rank', 'ridit', 'z')}"),
              sex = c("all", "male", "female"),
              weights = c(TRUE, FALSE),
              covars = 1:length(mod_covars)) %>%
  mutate(spec_id = row_number(), .before = 1)


# Model Functions
get_spec <- function(spec_id){
  mod_specs %>%
    filter(spec_id == !!spec_id)
}

get_df <- function(spec_id){
  spec <- get_spec(spec_id)
  
  inv <- df_inv %>%
    filter(cohort == spec$cohort, 
           male %in% mod_sexes[[spec$sex]]) %>%
    select(id, all_of(mod_covars[[spec$covars]]), survey_weight)
  
  if (spec$weights == FALSE) inv$survey_weight <- 1
  
  ht <- df_height %>%
    filter(cohort == spec$cohort, fup == spec$fup) %>%
    select(id, height = all_of(spec$height_score))
  
  cog <- df_cog %>%
    filter(cohort == spec$cohort, fup == spec$fup, 
           test == spec$test, type == spec$type) %>%
    select(id, cog = all_of(spec$cog_score))
  
  inv %>%
    full_join(ht, by = "id") %>%
    full_join(cog, by = "id")
}

get_func <- function(spec_id){
  spec <- get_spec(spec_id)
  
  mod_func <- c("height ~ cog", mod_covars[[spec$covars]]) %>%
    glue_collapse(" + ")
  
  if (spec$sex != "all"){
    mod_func <- str_replace(mod_func, " male ", "1")
  }
  
  return(mod_func)
}

run_mod <- function(spec_id){
  df <- get_df(spec_id)
  
  mod <- get_func(spec_id) %>%
    as.formula() %>%
    lm_robust(df, weights = survey_weight)
  
  broom::tidy(mod, conf.int = TRUE) %>%
    filter(term == "cog") %>%
    select(beta = estimate, lci = conf.low, uci = conf.high) %>%
    bind_cols(broom::glance(mod) %>%
                select(r2 = r.squared, n = nobs))
}

run_safe <- safely(run_mod)

# 4. Run Models ----
set.seed(1)
tic()
plan(multisession, workers = 4)
mod_res <- mod_specs %>%
  select(spec_id) %>%
  sample_frac() %>%
  # slice(1:1000) %>%
  mutate(res = future_map(spec_id, run_safe, .progress = TRUE)) %>%
  left_join(mod_specs, by = "spec_id") %>%
  arrange(spec_id)
future:::ClusterRegistry("stop")
toc()

mod_res %>%
  filter(map_lgl(res, ~ !is.null(.x$error)))

save(mod_res, file = "Data/sca_results.Rdata")

# 5. Plot Results ----
# Clean Results
cohort_dict <- c(NSHD = "1946c", NCDS = "1958c", BCS70 = "1970c", MCS = "2001c")
height_dict <- c(height_chart = "Growth Chart", height_z = "Z-Score", height_rank = "Rank",
                 height_ridit = "Ridit Score", height_raw = "Raw Height")
cog_dict <- c(ridit = "Ridit", rank = "Rank", z = "Z-Score")

mod_clean <- mod_res %>%
  mutate(res = map(res, ~ .x$result)) %>%
  unnest(res) %>%
  mutate(test_clean = case_when(test == "maths" & between(fup, 7, 11) ~ "Maths @ 11 years",
                                test == "maths" & between(fup, 12, 17) ~ "Maths @ 16 years",
                                test == "vocab"  ~ "Vocabulary @ 16 years",
                                test == "verbal" ~ "Verbal @ 11 years"),
         cohort_clean = ordered(cohort_dict[cohort], cohort_dict),
         cog_clean = factor(cog_dict[cog_score], cog_dict),
         height_clean = factor(height_dict[height_score], height_dict)) %>%
  group_by(cohort, cog_score, height_score) %>%
  mutate(cohort_rank = percent_rank(beta)) %>%
  group_by(cohort, test_clean, cog_score, height_score) %>%
  mutate(test_rank = percent_rank(beta)) %>%
  ungroup()

# Plot, Cognitive Scoring in Columns
plot_sca <- function(height_score, position = "middle"){
  
  p <- mod_clean %>%
    filter(height_score == !!height_score) %>%
    ggplot() +
    aes(x = cohort_rank, y = beta, color = cohort_clean) +
    facet_wrap(~ cog_clean, nrow = 1, scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(size = 0.5) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = NULL, y = height_dict[height_score]) +
    theme_bw() +
    theme(strip.placement = "outside",
          axis.title.y = element_text(size = rel(0.8)),
          # axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(0.8)),
          legend.position = "none")
  
  if (position != "top"){
    p <- p + theme(strip.text.x = element_blank() , 
                   strip.background.x = element_blank())
  }
  if (position == "bottom"){
    p <- p +
      labs(x = "Rank", color = NULL) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(size = 1)))
  }
  
  return(p)
}

p <- list()
p[[1]] <- plot_sca("height_chart", "top")
p[[2]] <- plot_sca("height_z")
p[[3]] <- plot_sca("height_rank")
p[[4]] <- plot_sca("height_ridit")
p[[5]] <- plot_sca("height_raw", "bottom")

p[[1]] / p[[2]] / p[[3]] /
  p[[4]] / p[[5]]

ggsave("Images/sca_cog_facet.png", 
       width = 29.7, height = 21,
       units = "cm", dpi = 600)


# Plot, Test in Columns
plot_sca_cog <- function(cog_score){
  
  p <- mod_clean %>%
    filter(cog_score == !!cog_score) %>%
    ggplot() +
    aes(x = test_rank, y = beta, color = cohort_clean) +
    facet_grid(height_clean ~ test_clean, scales = "free_y", switch = "y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(size = 0.5) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Rank", y = NULL) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank(),
          axis.title.y = element_text(size = rel(0.8)),
          legend.position = "none")
  
  glue("Images/sca_cog_{cog_score}.png") %>%
    ggsave(plot = p, width = 29.7, height = 21,
           units = "cm", dpi = 600)
  
  return(p)
}


unique(mod_clean$cog_score) %>%
  map(plot_sca_cog)
