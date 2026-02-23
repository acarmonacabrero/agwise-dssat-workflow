library(tidyverse)
library(purrr)
library(yardstick)
library(DescTools)  # for Lin's CCC
library(arrow)
select <- dplyr::select

Country <- 'Nigeria'
useCaseName <- 'Example'
all_data <- read_parquet(paste0(
  '/home/jovyan/rs-soil-comparison-africa/Data/useCase_', Country, "_", 
  useCaseName, '/all_data_', Country, '.parquet'))

vars_samples <- c("N_0-20_sample", "N_20-50_sample")
vars_isric <- c("N_0-20_SG", "N_20-50_SG")
vars_isda <- c("N_0-20_ISDA", "N_20-50_ISDA")

###############################################
### Boxplots for soil parameters and depths ###
###############################################
# TODO: Make a function out of all of this. Move functions to another script

### Prepare data in long format
data_long <- all_data %>%
  pivot_longer(cols = c("N_0-20_sample", "N_20-50_sample",
                        "N_0-20_SG", "N_20-50_SG",
                        "N_0-20_ISDA", "N_20-50_ISDA"),
               names_to = c("Source", "Depth"),
               names_pattern = "N_(.*)_(\\d+_\\d+)",
               values_to = "Value") %>%
  mutate(
    Source = if_else(Source == "sample", "Sample", Source),
    Depth = if_else(Depth == "0_20", "0-20 cm", "20-50 cm")
  )


### Select only columns from one parameter
plot_cols <- grep("^N_", colnames(all_data), value = TRUE)
plot_title <- "Total Nitrogen"
# plot_cols <- grep("^Sand_", colnames(all_data), value = TRUE)
# plot_title <- "Sand"

data_long <- all_data %>%
  select(all_of(plot_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "name",      # temporarily keep full column name
    values_to = "Value"
  ) %>%
  mutate(
    # Source: SG, ISDA, or Sample
    Source = case_when(
      str_detect(name, "SG") ~ "SG",
      str_detect(name, "ISDA") ~ "ISDA",
      str_detect(name, "sample") ~ "Sample",
      TRUE ~ NA_character_
    ),
    # Depth: extract numbers before cm
    Depth = str_extract(name, "\\d+-\\d+"),
    Depth = paste0(Depth, " cm")
  ) %>%
  filter(
    Depth %in% c("0-20 cm", "20-50 cm")
  )

counts <- data_long %>%
  filter(is.finite(Value)) %>%
  group_by(Source, Depth) %>%
  summarise(n = n(), .groups = "drop")

ggplot(data_long %>% filter(is.finite(Value)), 
       aes(x = Source, y = Value, fill = Source)) +
  geom_boxplot(notch = TRUE) +
  geom_text(
    data = counts,
    aes(x = Source, y = max(data_long$Value, na.rm = TRUE) * 1.05, label = paste0("N = ", n)),
    inherit.aes = FALSE,
    vjust = 0
  ) +
  facet_wrap(~Depth, nrow = 1) +
  scale_fill_manual(values = c("Sample" = "grey", "SG" = "blue", "ISDA" = "red")) +
  labs(title = "Total Nitrogen", x = "", y = "Value") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

##########################################################################
### Pair comparison metrics (ISDA vs SG, ISDA vs Sample, SG vs Sample) ###
##########################################################################
#######################################################################
### Comparison plots (Scatter plot, Hexbin, Bland-Altman, Boxplots) ###
#######################################################################
library(dplyr)
library(ggplot2)
library(hexbin)
library(purrr)
library(gridExtra)

plot_comparisons <- function(data, truth_var, estimate_var) {
  
  df <- data %>% 
    select(truth = {{truth_var}}, estimate = {{estimate_var}}) %>%
    filter(!is.na(truth), !is.na(estimate))
  
  comp_name <- paste(estimate_var, "vs", truth_var)
  
  # --- A. Scatter + loess + 1:1 line ---
  p1 <- ggplot(df, aes(x = truth, y = estimate)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = paste("Scatter:", comp_name))
  
  # --- B. Hexbin scatter (if many points) ---
  p2 <- ggplot(df, aes(x = truth, y = estimate)) +
    geom_hex() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = paste("Hexbin:", comp_name)) +
    theme_bw()
  
  # --- C. Bland–Altman ---
  df <- df %>%
    mutate(mean_val = (truth + estimate)/2,
           diff_val = estimate - truth)
  
  p3 <- ggplot(df, aes(x = mean_val, y = diff_val)) +
    geom_point(alpha = 0.3) +
    geom_hline(yintercept = mean(df$diff_val), color = "blue") +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = paste("Bland–Altman:", comp_name),
         x = "Mean of two estimates",
         y = "Difference (estimate - truth)") +
    theme_bw()
  
  # --- D. Distribution comparison ---
  df2 <- data.frame(
    value = c(df$truth, df$estimate),
    source = rep(c("Truth", "Estimate"), each = nrow(df))
  )
  
  p4 <- ggplot(df2, aes(x = source, y = value, fill = source)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.2, outlier.alpha = 0.2) +
    labs(title = paste("Distributions:", comp_name)) +
    theme_bw()
  
  # Combine into a grid
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}


# Loop for each pair
walk2(vars_samples, vars_isric,
      ~plot_comparisons(all_data, .x, .y))
