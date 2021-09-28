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
load_dta <- function(cohort){
  glue("Data/{cohort}_cleaned.dta") %>%
    read_dta() %>%
    as_factor() %>%
    zap_label() %>%
    zap_formats() %>%
    select(-matches("_(chart|cog)_")) %>%
    rename_with(~ str_replace(.x, "age_anth_", "age_"))
}

df_raw <- c("1946c", "1958c", "1970c", "2001c") %>%
  set_names(., .) %>%
  map(load_dta)

rm(load_dta)

# Number with Parent Height < 1.4m
map_dbl(df_raw, ~ sum(.x$parent_height))
map_dbl(df_raw, ~ sum(is.na(.x$male)))

df_raw <- map(df_raw,
              ~ .x %>%
                drop_na(male) %>%
                filter(is.na(parent_height) | parent_height == 0))

# Add 2001c White Data
df_raw$`2001c_white` <- df_raw$`2001c` %>%
  filter(ethnic_group == "White") %>%
  mutate(cohort = "2001c_white")

df_raw$`1970c_measure` <- df_raw$`1970c` %>%
  filter(height_16_report == 1) %>%
  mutate(cohort = "1970c_measure")

save(df_raw, file = "Data/df_raw.Rdata")


# 2. Multiple Imputation ----
tic()
imp <- list()
for(cohort in names(df_raw)){
  df <- df_raw[[cohort]] %>%
    select(id, cohort, survey_weight, male,
           matches("^age_..$"), matches("^(bmi|height)"), matches("resid"),
           mother_height, father_height,
           father_class, mother_edu_level, father_edu_level)
  
  pred <- make.predictorMatrix(df)
  pred[, c("id", "cohort", "male")] <- 0
  
  temp <- list()
  for (male in c(0, 1)){
    df_mice <- filter(df, male == !!male)
    
    temp[[male + 1]] <- parlmice(df_mice, n.core = 4, n.imp.core = 8,
                                 predictorMatrix = pred,
                                 defaultMethod = c("rf", "logreg", "polyreg", "polr"))
  }
  
  imp[[cohort]] <- rbind(temp[[1]], temp[[2]])
}
toc()

rm(df, df_mice, temp, pred, male, cohort)

save(imp, file = "Data/mice.Rdata")


# 3. Descriptive Tables ----
load("Data/df_raw.Rdata")
load("Data/mice.Rdata")

imp_long <- map(imp,
                ~ complete(.x, "long", TRUE) %>%
                  as_tibble() %>%
                  rename(imp = .imp) %>%
                  select(-.id))
rm(imp)


# Time Invariant
make_desc <- function(string_var = "cc", imp_var = NULL, weight_var = NULL){
  if (is.null(imp_var)){
    n_imp <- 0
  } else{
    n_imp <- 1:32
  }
  
  map_dfr(imp_long,
          ~ .x %>%
            filter(imp %in% n_imp) %>%
            select(id, any_of(weight_var), any_of(!!imp_var), 
                   male, father_class, mother_edu_level,
                   father_height, mother_height) %>%
            mutate(male = factor(male, labels = c("Female", "Male"))) %>%
            get_desc("id", imp_var = imp_var, weight_var = weight_var),
          .id = "cohort") %>%
    select(-group_var) %>%
    rename(!!string_var := string)
}

desc <- list()

desc$var_dict <- c(n = "Sample Size", male = "Sex",
                   mother_height = "Maternal Height (cm)", 
                   father_height = "Paternal Height (cm)",
                   father_class = "Father's Social Class",
                   mother_edu_level = "Mother's Education")

desc$col_order <- paste(
  rep(c("cc", "miss", "mi"), times = 3),
  rep(c("1946c", "1958c", "1970c", "2001c"), each = 3),
  sep = "_"
)

desc$df_1 <- full_join(
  make_desc(),
  make_desc("mi", "imp", "survey_weight") %>%
    select(-miss),
  by = c("cohort", "var", "cat")
) %>%
  filter(str_length(cohort) == 5) %>%
  arrange(cohort, var, cat) %>%
  group_by(cohort, var) %>%
  mutate(miss = ifelse(row_number() == 1, miss, NA)) %>%
  ungroup() %>%
  pivot_wider(names_from = cohort, values_from = c(cc, miss, mi)) %>%
  mutate(cat_clean = ifelse(var == cat, desc$var_dict[var], cat),
         var_clean = ifelse(var == cat, NA, desc$var_dict[var]),
         var = factor(var, names(desc$var_dict))) %>%
  arrange(var) %>%
  select(var_clean, cat_clean, all_of(desc$col_order))

desc$tbl1_names <- names(desc$df_1)[-(1:2)]

desc$tbl1_header <- case_when(str_detect(desc$tbl1_names, "cc_") ~ "Complete Cases",
                              str_detect(desc$tbl1_names, "miss_") ~ "Missing %",
                              str_detect(desc$tbl1_names, "mi_") ~ "Imputed Data") %>%
  set_names(desc$tbl1_names) %>%
  c(var_clean = "", cat_clean = "Variable") %>%
  as.list()

desc$tbl1_span <- str_sub(desc$tbl1_names, -5) %>%
  set_names(desc$tbl1_names) %>%
  c(var_clean = "", cat_clean = "") %>%
  as.list()

desc$flx_1 <- make_flx(desc$df_1, desc$tbl1_header, desc$tbl1_span) %>%
  border(j = c(2, 5, 8, 11), 
         border.right = fp_border(color = "grey50", width = 1, style = "dashed"))

desc$flx_1 
save_as_docx(desc$flx_1, path = "Tables/table_1.docx")

# Height
desc$df_2 <- map_dfr(df_raw,
                     ~ .x %>%
                       select(matches("(maths|vocab|verbal)_..$")) %>%
                       descr() %>%
                       tb(),
                     .id = "cohort") %>%
  filter(str_length(cohort) == 5) %>%
  mutate(test = str_sub(variable, 1, -4) %>% str_to_title(),
         age = str_sub(variable, -2) %>% as.numeric(),
         age_ref = ifelse(age < 14, 11, 16),
         test = glue("{test} @ Age {age_ref}"),
         n = format(n.valid, big.mark = ",") %>% trimws(),
         missing = 100 - round(pct.valid, 1),
         missing = glue("{missing}%")) %>%
  select(test, cohort, n, missing, 
         mean, min, max, sd, cv) %>%
  mutate(across(mean:cv, round, 1)) %>%
  rename_with(str_to_title) %>%
  rename_with(str_to_upper, c(Sd, Cv)) %>%
  arrange(Test, Cohort)

desc$flx_2 <- make_flx(desc$df_2)

desc$flx_2
save_as_docx(desc$flx_2, path = "Tables/table_2.docx")


# 4. Descriptive Plots ----
# Cognition and Raw Height Density Plots
df_dens <- map_dfr(df_raw,
                   ~ .x %>%
                     select(id, matches("(maths|vocab|verbal|height)_..$")) %>%
                     pivot_longer(-id),
                   .id = "cohort") %>%
  filter(str_length(cohort) == 5) %>%
  separate(name, c("test", "age"), sep = "_") %>%
  # mutate(age = case_when(test == "height" ~ as.double(age),
  #                        as.double(age) < 14 ~ 11,
  #                        TRUE ~ 16)) %>%
  mutate(test = glue("{cohort}: {str_to_title(test)} @ Age {age}"))

get_stats <- function(df){
  df %>%
    group_by(cohort, test) %>%
    descr(value) %>%
    tb() %>%
    mutate(missing = 100 - pct.valid) %>%
    select(cohort, test, mean, min, max, sd, missing) %>%
    pivot_longer(-c(cohort, test)) %>%
    mutate(value = round(value, 1),
           name = ifelse(str_length(name) == 2, 
                         str_to_upper(name),
                         str_to_title(name)),
           string = glue("{name} = {value}"),
           string = ifelse(name == "Missing", glue("{string}%"), string)) %>%
    group_by(cohort, test) %>%
    summarise(string = glue_collapse(string, sep = "\n"),
              .groups = "drop")
}

df_stat <- get_stats(df_dens)


plot_dens <- function(height){
  df_dens %>%
    filter(str_detect(test, "Height") == !!height) %>%
    ggplot() +
    aes(x = value) +
    facet_wrap(test ~ ., ncol = ifelse(height, 5, 4),
               scales = ifelse(height, "fixed", "free")) +
    geom_density(color = "grey50", fill = "grey70", alpha = 0.3) +
    geom_text(aes(x = -Inf, y = Inf, label = string), 
              data = filter(df_stat, str_detect(test, "Height") == !!height),
              hjust = -0.1, vjust = 1.1,
              size = 2.5) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          strip.background.y = element_blank())
}

plot_dens(FALSE)
ggsave("Images/figure_1.png", width = 29.7, 
       height = 21, dpi = 600, units = "cm")


plot_dens(TRUE)
ggsave("Images/figure_2.png", width = 29.7, 
       height = 21, dpi = 600, units = "cm")

rm(df_dens, df_stat)


# Height Chart
df_chart <- map_dfr(df_raw,
                ~ .x %>%
                  select(id, male, matches("^(age|height)_..$")) %>%
                  pivot_longer(-c(id, male)) %>%
                  separate(name, c("test", "fup"), sep = "_") %>%
                  pivot_wider(names_from = test, values_from = value),
                .id = "cohort") %>%
  filter(str_length(cohort) == 5) %>%
  mutate(value = sds(height, age, male, "height",
                     uk1990.ref, male = 1, female = 0),
         test = glue("{cohort}: Height (Z-Score) @ Age {fup}"))

df_height <- get_stats(df_chart)


ggplot(df_chart) +
  aes(x = value) +
  facet_wrap(test ~ ., ncol = 5, scales = "fixed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density(color = "grey50", fill = "grey70", alpha = 0.3) +
  geom_text(aes(x = -Inf, y = Inf, label = string), 
            data = df_height,
            hjust = -0.1, vjust = 1.1,
            size = 2.5) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_blank())
ggsave("Images/figure_3.png", width = 29.7, 
       height = 21, dpi = 600, units = "cm")

rm(df_chart, df_height)


# 5. Correlations ----
df_cor <- map(df_raw,
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

set.seed(1)
tic()
plan(multisession, workers = 4)
cors$df <- future_map_dfr(df_cor, get_correlation, .id = "cohort", .progress = TRUE)
future:::ClusterRegistry("stop")
toc()

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
         across(c(x_clean, y_clean), ~ ordered(cors$var_dict[.x], cors$var_dict))) %>%
  arrange(cohort, x, y) %>%
  distinct(cohort, x_clean, y_clean, .keep_all = TRUE) %>%
  filter(x != "comp_resid_16", y != "comp_resid_16",
         x_clean < y_clean, cohort != "2001c_white")

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


# Correlation between Parental Height by Ethnicity
df_raw$`2001c`%>%
  select(ethnic_group, mother_height, father_height) %>%
  nest(data = -ethnic_group) %>%
  drop_na(ethnic_group) %>%
  mutate(cor = map(data, 
                   ~ get_correlation(.x) %>%
                     filter(x > y))) %>%
  unnest(cor) %>%
  select(-data) %>%
  mutate(beta_clean = round(beta, 2)) %>%
  ggplot() +
  aes(x = ethnic_group, y = beta, ymin = lci, ymax = uci, label = beta_clean) +
  geom_col(alpha = 0.7) +
  geom_linerange() +
  geom_text(y = 0.5, color = "red", size = 2.5) +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill = NA, color = NA))
ggsave("Images/mcs_assortative.png", height = 9.9, width = 16,
       units = "cm", dpi = 300)



# 6. Attrition ----
df_att <- map_dfr(df_raw,
                  ~ .x %>%
                    select(id, cohort, matches("^height_chart")) %>%
                    pivot_longer(matches("height")) %>%
                    mutate(age_y = str_sub(name, -2) %>% as.numeric(),
                           obs = ifelse(is.na(value), 0, 1)) %>%
                    select(-name, -value))

to_ridit <- function(x){
  x_dense <- dense_rank(x)
  
  x_ridit <- toridit(table(x_dense))[x_dense]
  
  as.numeric(x_ridit)
}

get_glm <- function(df){
  glm(obs ~ value, binomial, df) %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    slice(2) %>%
    select(beta = estimate, lci = conf.low, uci = conf.high)
}


df_p <- map_dfr(df_raw,
                ~ .x %>%
                  select(id, cohort, matches("(height_chart|resid)")) %>%
                  pivot_longer(-c(id, cohort))) %>%
  full_join(df_att, by = c("cohort", "id")) %>%
  mutate(age_x = str_sub(name, -2) %>% as.numeric()) %>%
  filter(age_x < age_y) %>%
  drop_na() %>%
  group_by(cohort, name, age_y) %>%
  mutate(value = to_ridit(value)) %>%
  ungroup() %>%
  nest(data = -c(cohort, name, age_y)) %>%
  mutate(res = map(data, get_glm)) %>%
  unnest(res) %>%
  select(-data) %>%
  filter(cohort != "2001c_white") %>%
  mutate(name = str_replace(name, "_(resid|chart)_", " @ Age") %>%
           str_to_title(),
         age_y = glue("Observed @ Age {age_y}"))


ggplot(df_p) +
  aes(x = name, y = beta, ymin = lci, ymax = uci) +
  facet_wrap(cohort ~ age_y, scale = "free_x") +
  geom_hline(yintercept = 1) +
  geom_pointrange() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill = NA, color = NA),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = NULL)
ggsave("Images/attrition.png", height = 16, width = 16,
       units = "cm", dpi = 300)


map_dfr(df_raw, 
        ~ .x %>%
          select(matches("edu_level")),
        .id = "cohort") %>%
  drop_na() %>%
  count(cohort, mother_edu_level, father_edu_level) %>%
  add_count(cohort, father_edu_level, wt = n, name = "total") %>%
  mutate(p = n/total) %>%
  filter(mother_edu_level == "Low") %>%
  ggplot() +
  aes(x = father_edu_level, y = p, color = cohort) +
  geom_point(position = position_dodge(0.8))



# 1. TRY PATTERN MIXTURE WITH DIFFERENT PARAMETERS FOR DIFFERENCE IN HEIGHT IN 4 COHORTS
# 2. GET DIFFERENCES IN ESTIMATES AND PLOT ACCORDING TO PARAMETERS
# 3. REGRESS HEIGHT_T ON COG_T & OBS_HEIGHT_T+1 & MALE TO SEE REASONABLE DIFFERENCE IN HEIGHT
df_height %>%
  distinct(cohort, age_height) %>%
  mutate(age_x = age_height) %>%
  complete(age_height, age_x) %>%
  drop_na(cohort) %>%
  filter(age_height < age_x)
