library(tidyverse)
library(haven)
library(glue)
library(gallimaufr)
library(ridittools)
library(broom)
library(summarytools)
library(magrittr)
library(officer)
library(flextable)
library(furrr)
library(tictoc)
library(childsds)
library(mice)

rm(list = ls())

# 1. Load Data ----
load("Data/df_raw.Rdata")
load("Data/mice.Rdata")

imp_long <- map(imp,
                ~ complete(.x, "long", TRUE) %>%
                  as_tibble() %>%
                  rename(imp = .imp) %>%
                  select(-.id, -matches("bmi_"), -matches("_report"),
                         -father_edu_level))
rm(imp)


# 2. Add Variables ----
to_ridit <- function(x){
  x_dense <- dense_rank(x)
  
  x_ridit <- toridit(table(x_dense))[x_dense]
  
  as.numeric(x_ridit)
}

df_inv <- map_dfr(imp_long, select, -matches("_..$"))

df_height <- map_dfr(imp_long, 
                     ~ .x %>%
                       select(id, cohort, imp, male, survey_weight, matches("(height|age)_")) %>%
                       pivot_longer(matches("(height|age)_")) %>%
                       separate(name, c("variable", "fup"), sep = "_") %>%
                       pivot_wider(names_from = variable, values_from = value)) %>%
  mutate(height_chart = sds(height, age, male, "height",
                            uk1990.ref, male = 1, female = 0)) %>%
  group_by(cohort, imp, male, fup) %>%
  mutate(height_ridit = to_ridit(height),
         height_z = wtd_scale(height), #survey_weight),
         height_rank = percent_rank(height)) %>%
  ungroup() %>%
  rename(height_raw = height) %>%
  select(-male, -survey_weight, -age)

df_cog <- map_dfr(imp_long, 
                  ~ .x %>%
                    select(id, cohort, imp, survey_weight, matches("_resid_")) %>%
                    pivot_longer(matches("_resid_"), 
                                 names_to = "test", 
                                 values_to = "cog")) %>%
  group_by(cohort, imp, test) %>%
  mutate(cog_ridit = to_ridit(cog),
         cog_z = wtd_scale(cog, survey_weight),
         cog_rank = percent_rank(cog)) %>%
  ungroup() %>%
  select(-cog, -survey_weight)

save(df_inv, df_cog, df_height,
     file = "Data/df_analysis.Rdata")





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

rm(imp_long)
