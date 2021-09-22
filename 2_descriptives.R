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
library(summarytools)
library(patchwork)
library(magrittr)
library(officer)
library(flextable)

rm(list = ls())

# 1. Load Data ----
load_dta <- function(cohort){
  glue("Data/{cohort}_cleaned.dta") %>%
    read_dta() %>%
    as_factor() %>%
    zap_label() %>%
    zap_formats()
}

df_raw <- c("mcs", "70c", "58c", "46c") %>%
  set_names(., .) %>%
  map(load_dta)

save(df_raw, file = "Data/df_raw.Rdata")
rm(load_dta)


# 2. Descriptive Table ----
df_desc <- map(df_raw,
               ~ .x %>%
                 select(id, cohort, male, survey_weight,
                        matches("(chart|maths|verbal|vocab)_"),
                        father_class, mother_edu_level,
                        father_height, mother_height))

df_inv <- map(df_desc,
              ~ .x %>%
                select(-matches("_..$")))

df_obs <- map(df_desc,
              ~ names(.x) %>%
                str_subset("_..$") %>%
                str_sub(-2) %>%
                unique()) %>%
  map_dfr(~ tibble(age = .x), .id = "cohort") %>%
  mutate(id = map2(cohort, age,
                   ~ df_desc[[.x]] %>%
                     drop_na(matches(glue("height_chart_{.y}"))) %>%
                     pull(id))) %>%
  unnest(id)

save(df_obs, file = "Data/df_obs.Rdata")

# Make Tables
table_1 <- function(cohort, age){
  df_desc[[cohort]] %>%
    select(id, all_of(glue("height_chart_{age}"))) %>%
    drop_na() %>%
    left_join(df_inv[[cohort]], by = "id") %>%
    mutate(male = factor(male, labels = c("Female", "Male"))) %>%
    select(-survey_weight) %>%
    get_desc("id") %>%
    select(-group_var) %>%
    mutate(across(c(var, cat),
                  ~ ifelse(str_detect(.x, "height_chart"), "height", .x)))
}

desc <- list()
desc$tbl1_ord <- df_obs %>%
  distinct(age, cohort) %>%
  arrange(cohort, age) %>%
  uncount(2) %>%
  group_by(cohort, age) %>%
  mutate(var = ifelse(row_number() == 1, "string", "miss")) %>%
  unite("var", cohort, age, var) %>%
  pull()

desc$var_dict <- c(n = "Sample Size", male = "Sex", height = "Height",
                   mother_height = "Maternal Height (cm)", 
                   father_height = "Paternal Height (cm)",
                   father_class = "Father's Social Class",
                   mother_edu_level = "Mother's Education")

desc$df_1 <- df_obs %>%
  distinct(cohort, age) %>%
  mutate(tbl = map2(cohort, age, table_1)) %>%
  unnest(tbl) %>%
  unite("cohort", cohort, age) %>% 
  arrange(cohort, var, cat) %>%
  group_by(cohort, var) %>%
  mutate(miss = ifelse(row_number() == 1 & !(var %in% c("height", "n")), miss, "")) %>%
  ungroup() %>%
  pivot_wider(names_from = cohort, values_from = c(string, miss), 
              names_glue = "{cohort}_{.value}") %>%
  mutate(var_clean = factor(desc$var_dict[var], desc$var_dict),
         cat_clean = ifelse(var == cat, as.character(var_clean), cat),
         .before = 1) %>%
  arrange(var_clean, cat_clean) %>%
  group_by(var_clean) %>%
  mutate(var_clean = ifelse(var_clean == cat_clean, "", as.character(var_clean))) %>%
  select(var_clean, cat_clean, all_of(desc$tbl1_ord))

desc$tbl1_names <- names(desc$df_1)[-(1:2)]

desc$tbl1_header <- ifelse(str_detect(desc$tbl1_names, "string"),
                           "Mean (SD) /\n n (%)", "Missing %") %>%
  set_names(desc$tbl1_names) %>%
  c(var_clean = "", cat_clean = "Variable") %>%
  as.list()

desc$tbl1_span1 <- str_sub(desc$tbl1_names, 5, 6) %>%
  paste("Age", .) %>%
  set_names(desc$tbl1_names) %>%
  c(var_clean = "", cat_clean = "") %>%
  as.list()

desc$tbl1_stub <- c("46c" = "1946c", "58c" = "1958c",
                    "70c" = "1970c", "mcs" = "2001c")

desc$tbl1_span2 <- desc$tbl1_stub[str_sub(desc$tbl1_names, 1, 3)] %>%
  set_names(desc$tbl1_names) %>%
  c(var_clean = "", cat_clean = "") %>%
  as.list()

desc$flx_1 <- flextable(desc$df_1) %>%
  set_header_labels(values = desc$tbl1_header) %>% 
  add_header(., values = desc$tbl1_span1) %>%
  add_header(., values = desc$tbl1_span2) %>%
  border_remove() %>%
  merge_v(1) %>%
  merge_h(part = "header") %>% 
  border(j = c(2, 6, 10, 14), 
         border.right = fp_border(color = "grey50", width = 1, style = "dashed")) %>%
  border_inner_h(border = fp_border(color = "grey50", width = 1, style = "dashed"),
                 part = "body") %>% 
  hline_top(border = fp_border(color = "black", width = 2), part = "all") %>% 
  hline_bottom(border = fp_border(color = "black", width = 2), part = "all") %>%
  fix_border_issues(part = "all") %>% 
  align(j = 1:2, align = "right", part = "all") %>% 
  align(j = 3:ncol(desc$df_1), align = "center", part = "all") %>% 
  valign(j = 1, valign = "center") %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>% 
  autofit()

desc$flx_1 
save_as_docx(desc$flx_1, path = "Tables/table_1.docx")


# Table 2
table_2 <- function(cohort, age){
  df_desc[[cohort]] %>%
    select(matches(age)) %>%
    drop_na(matches("height_chart")) %>%
    select(-matches("height_chart")) %>%
    descr() %>%
    tb() %>%
    mutate(test = str_sub(variable, 1, -4),
           missing = round(100 - pct.valid, 1) %>% paste0("%")) %>%
    select(test, mean, min, max, sd, cv, missing) %>%
    mutate(across(mean:cv, round, 1))
}

desc$test_dict <- c(maths = "Maths", verbal = "Verbal Similarities",
                    vocab = "Vocabularly/Comprehension")

desc$flx_2 <- df_obs %>%
  distinct(cohort, age) %>%
  mutate(tbl = map2(cohort, age, table_2)) %>%
  unnest(tbl) %>%
  filter(!str_detect(test, "resid")) %>%
  mutate(age = ifelse(as.integer(age) < 14, 11, 16)) %>%
  mutate(cohort = desc$tbl1_stub[cohort]) %>%
  mutate(test = desc$test_dict[test],
         test = glue("{test} @ Age {age}")) %>%
  relocate(test, age, .before = 1) %>%
  arrange(age, test, cohort) %>%
  select(-age) %>%
  rename_with(str_to_title) %>%
  rename_with(str_to_upper, c(Sd, Cv)) %>%
  make_flx()

desc$flx_2
save_as_docx(desc$flx_2, path = "Tables/table_1.docx")


# Cognition Density Plots
df_dens <- map_dfr(df_desc,
                   ~ .x %>%
                     select(id, matches("_..$"), -matches("height"),
                            -matches("_resid")) %>%
                     pivot_longer(-id),
                   .id = "cohort") %>%
  drop_na() %>%
  separate(name, c("test", "age"), sep = "_") %>%
  inner_join(df_obs, by = c("cohort", "id", "age")) %>%
  mutate(age = ifelse(as.integer(age) < 14, 11, 16),
         test = glue("{str_to_title(test)} @ Age {age}"),
         cohort = desc$tbl1_stub[cohort])

df_ind <- df_dens %>%
  group_by(cohort, test) %>%
  descr(value) %>%
  tb() %>%
  mutate(missing = 100 - pct.valid) %>%
  select(cohort, test, mean, min, max, sd, cv, missing) %>%
  pivot_longer(-c(cohort, test)) %>%
  mutate(value = round(value, 2),
         name = ifelse(str_length(name) == 2, 
                       str_to_upper(name),
                       str_to_title(name)),
         string = glue("{name} = {value}"),
         string = ifelse(name == "Missing", glue("{string}%"), string)) %>%
  group_by(cohort, test) %>%
  summarise(string = glue_collapse(string, sep = "\n"),
            .groups = "drop")

plot_dens <- function(test){
  p <- df_dens %>%
    filter(test == !!test) %>%
    ggplot() +
    aes(x = value) +
    facet_grid(test ~ cohort, scales = "free", 
               switch = "y", space = "free_y") +
    geom_density(color = "grey50", fill = "grey70", alpha = 0.3) +
    geom_text(aes(x = -Inf, y = Inf, label = string), 
              data = df_ind %>%  filter(test == !!test),
              hjust = -0.1, vjust = 1.1, size = 2.5) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank())
  
  if (test != "Maths @ Age 11"){
    p <- p + theme(strip.text.x = element_blank() , 
                   strip.background.x = element_blank())
  }
  
  return(p)
}

dens_p <- df_dens %>%
  distinct(test) %>%
  pull(test) %>%
  map(plot_dens)

dens_p[[1]] + dens_p[[2]] +
  dens_p[[3]] + dens_p[[4]] +
  plot_layout(nrow = 4)
ggsave("Images/figure_1.png", width = 29.7, 
       height = 21, dpi = 600, units = "cm")

# 3. Correlations ----
df_cor <- map(df_desc,
              ~ .x %>%
                select(matches("(height|resid|mother|father)")) %>%
                mutate(father_class = as.numeric(father_class),
                       mother_edu_level = as.numeric(mother_edu_level)))

get_correlation <- function(df){
  
  get_pair <- function(x, y){
    df %>%
      select(all_of(c(x, y))) %>%
      drop_na()
  }
  
  get_cor <- function(df_pair){
    cor(df_pair, method = "spearman") %>%
      as.numeric() %>% 
      pluck(2)
  }
  
  get_ci <- function(x, y){
    df_pair <- get_pair(x, y)
    
    cors <- map_dbl(1:200, 
                    ~ sample_frac(df_pair, replace = TRUE) %>%
                      get_cor())
    
    tibble(beta = get_cor(df_pair),
           lci = quantile(cors, .025),
           uci = quantile(cors, .975))
  }
  
  expand_grid(x = names(df),
              y = names(df)) %>%
    filter(x != y) %>%
    mutate(map2_dfr(x, y, get_ci))
  
}

cors <- list()
cors$df <- map_dfr(df_cor, get_correlation, .id = "cohort")

cors$var_dict <- c(height_chart_11 = "Height @ Age 11", height_chart_16 = "Height @ Age 16",
                   maths_resid_11 = "Maths @ Age 11", verbal_resid_11 = "Verbal @ Age 11",
                   maths_resid_16 = "Maths @ Age 16", vocab_resid_16 = "Vocab @ Age 16",
                   mother_height = "Maternal Height", father_height = "Paternal Height",
                   father_class = "Father's Social Class", mother_edu_level = "Mother's Education")

cors$df_clean <- cors$df %>%
  mutate(across(c(x, y), list(age = ~ str_sub(.x, -2) %>% as.numeric())),
         across(c(x_age, y_age),
                ~ case_when(between(.x, 7, 11) ~ "11",
                            between(.x, 14, 17) ~ "16"))) %>%
  mutate(x_clean = ifelse(!is.na(x_age), glue("{str_sub(x, 1, -3)}{x_age}"), x),
         y_clean = ifelse(!is.na(y_age), glue("{str_sub(y, 1, -3)}{y_age}"), y),
         across(c(x_clean, y_clean), ~ ordered(cors$var_dict[.x], cors$var_dict)),
         cohort = desc$tbl1_stub[cohort]) %>%
  arrange(cohort, x, y) %>%
  distinct(cohort, x_clean, y_clean, .keep_all = TRUE) %>%
  filter(x != "comp_resid_16", y != "comp_resid_16",
         x_clean < y_clean)

plot_cor <- function(df){
  df_string <- df %>%
    mutate(across(beta:uci, round, 2))
  
  ggplot(df) +
    aes(x = cohort, y = beta, ymin = lci, ymax = uci) +
    facet_grid(y_clean ~ x_clean, switch = "y", 
               labeller = labeller(x_clean = label_wrap_gen(16),
                                   y_clean = label_wrap_gen(16))) +
    geom_col(alpha = 0.7) +
    geom_linerange() +
    geom_text(data = df_string, aes(label = beta),
              y = 0.5, color = "red", size = 2.5) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background = element_rect(fill = NA, color = NA))
}

plot_cor(cors$df_clean)
ggsave("Images/figure_2.png", height = 21, width = 29.7,
       units = "cm", dpi = 300)


cors$df_clean %>%
  filter(str_detect(x, "(maths|vocab|verbal)"),
         str_detect(y, "(maths|vocab|verbal)")) %>%
  plot_cor()
ggsave("Images/corr_test.png", height = 16, width = 21,
       units = "cm", dpi = 300)

cors$df_clean %>%
  filter(str_detect(x, "_height"),
         str_detect(y, "_height")) %>%
  plot_cor()
ggsave("Images/corr_parent_height.png", height = 9.9, width = 16,
       units = "cm", dpi = 300)