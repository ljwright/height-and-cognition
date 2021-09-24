map_dfr(df_raw,
    ~ .x %>%
      select(id, cohort, matches("^height_..$")) %>%
      pivot_longer(matches("height")) %>%
      group_by(id, cohort) %>%
      summarise(obs = ifelse(sum(!is.na(value)) > 0, 1, 0),
                .groups = "drop")) %>%
  count(cohort, obs) %>%
  add_count(cohort, wt = n, name = "total") %>%
  mutate(p = n*100/total)
      drop_na())


df_x <- df_raw$`1946c`

df_height <- map_dfr(df_raw,
        ~ .x %>%
          select(id, cohort, matches("^height_..$")) %>%
          pivot_longer(matches("height"), 
                       names_to = "height_var",
                       values_to = "height"))  %>%
  mutate(obs = ifelse(is.na(height), 0, 1),
         age_height = str_sub(height_var, -2) %>% as.numeric()) %>%
  select(-height_var) %>%
  group_by(id, cohort) %>%
  mutate(any_obs = ifelse(sum(obs) == 0, 0, 1)) %>%
  ungroup()


df_cog <- map_dfr(df_raw,
        ~ .x %>%
          select(id, cohort, male, mother_edu_level, matches("_resid_")) %>%
          pivot_longer(matches("_resid_"), 
                       names_to = "cog_var",
                       values_to = "cog")) %>%
  mutate(age_cog = str_sub(cog_var, -2) %>% as.numeric())



get_attrit <- function(cohort, cog_var, age_cog, age_height){
  df_cog %>%
    filter(cohort == !!cohort,
           cog_var == !!cog_var,
           age_cog == !!age_cog) %>%
    left_join(df_height %>%
                select(id, cohort, age_height, height), 
              by = c("id", "cohort", "age_cog" = "age_height")) %>%
    left_join(df_height %>%
                filter(age_height == !!age_height) %>%
                select(id, cohort, obs),
              by = c("id", "cohort")) %>%
    mutate(across(c(cog, height), scales::rescale)) %>%
    glm(obs ~ cog + height + male + mother_edu_level, binomial, .) %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    slice(-1) %>%
    select(term, beta = estimate, lci = conf.low, uci = conf.high) %>%
    list()
}

df_attrit <- df_cog %>%
  distinct(cohort, cog_var, age_cog) %>%
  left_join(df_height %>%
              distinct(cohort, age_height),
            by = "cohort") %>%
  filter(age_cog < age_height) %>%
  rowwise() %>%
  mutate(res = get_attrit(cohort, cog_var, age_cog, age_height)) %>%
  ungroup() %>%
  unnest(res)

ggplot(df_attrit) +
  aes(x = cog_var, y = beta, ymin = lci, ymax = uci, 
      color = term) +
  facet_wrap(cohort ~ age_height, scales = "free_x") +
  geom_hline(yintercept = 1) +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_y_log10() +
  theme(legend.position = "bottom")


tibble(x = runif(1e4),
       y = x + rnorm(1e4, 0, 0.5),
       y_obs = ifelse(percent_rank(y) < 0.5, y, NA)) %>%
  ggplot() +
  aes(x = x, y = y) +
  geom_point(aes(color = is.na(y_obs))) +
  geom_smooth(method = "lm", color = "blue") +
  geom_smooth(aes(y = y_obs), method = "lm", color = "red")


df_height %>%
  filter(obs == 1) %>%
  arrange(id, cohort, age_height) %>%
  group_by(id, cohort) %>%
  slice(1) %>%
  ungroup() %>%
  count(cohort, age_height)




