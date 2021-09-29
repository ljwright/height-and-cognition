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
library(estimatr)

rm(list = ls())


# 1. Load Data ----
load("Data/main_results.Rdata")
load("Data/mnar_results.Rdata")
load("Data/attrition_results.Rdata")


# 2. Main ----



# 3. MNAR ----
plot_mnar <- function(test){
  res_g <- res_heat %>%
    filter(test == !!test,
           str_length(cohort_x) == 5,
           str_length(cohort_y) == 5)
  
  limits <- max(abs(res_g$beta)) * c(-1, 1)
  
  p <- ggplot(res_g) +
    aes(x = diff_x, y = diff_y, fill = beta) + # alpha = 1-p) +
    facet_grid(cohort_y ~ cohort_x, switch = "both") +
    geom_tile() +
    geom_abline(linetype = "dashed", color = "grey60") +
    theme_bw() +
    scale_fill_distiller(type = "div", limit = limits) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          legend.position = "bottom",
          strip.background = element_rect(fill = "white", colour = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    guides(fill = guide_colorbar(title.position = 'top', 
                                 title.hjust = .5,                                
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(.5, 'lines'))) +
    labs(x = NULL, y = NULL, fill = "Effect Size Difference")
  
  glue("Images/mnar_{test}.png") %>%
    ggsave(plot = p, width = 29.7, height = 21, dpi = 600, units = "cm")
  
  return(p)
}

unique(res_heat$test) %>%
  set_names(., .) %>%
  map(plot_mnar)


# Attrition ----
ggplot(res_att) + 
  aes(x = fup_miss, y = beta, ymin = lci, ymax = uci, color = sex) +
  facet_wrap(~ cohort_clean, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  labs(x = "Attrition By Age", color = NULL,
       y = "Difference in\nHeight Chart\nZ-Score")
ggsave("Images/attrition_height.png", width = 21,
       height = 16, dpi = 600, units = "cm")