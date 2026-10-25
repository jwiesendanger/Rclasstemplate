# =====================================================================
# M9A: Basic Regression -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m9a-basic-regression
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# Plots show in the Plots pane and are also saved to m9a_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m9a_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m9a_plots", paste0(name, ".png")), p,
                  width = 8, height = 5, dpi = 100, bg = "white")
  invisible(p)
}

# ---------------------------------------------------------------------
# Needed packages
# ---------------------------------------------------------------------
status("Loading packages")
library(tidyverse)
library(moderndive)

set.seed(76)   # so the random samples below are the same each run

# =====================================================================
# 27.2 One numerical explanatory variable
# 27.2.1 Exploratory data analysis
# =====================================================================
status("27.2.1 EDA: fertility rate vs. life expectancy")
UN_data_m9 <- un_member_states_2024 |>
  select(iso,
         life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         obes_rate = obesity_rate_2016) |>
  na.omit()

glimpse(UN_data_m9)

UN_data_m9 |>
  slice_sample(n = 5)

UN_data_m9 |>
  summarize(mean_life_exp = mean(life_exp),
            mean_fert_rate = mean(fert_rate),
            median_life_exp = median(life_exp),
            median_fert_rate = median(fert_rate))

UN_data_m9 |>
  select(fert_rate, life_exp) |>
  tidy_summary()

UN_data_m9 |>
  tidy_summary(columns = c(fert_rate, life_exp))

# Correlation coefficient (textbook: -0.812)
UN_data_m9 |>
  get_correlation(formula = fert_rate ~ life_exp)

UN_data_m9 |>
  summarize(correlation = cor(fert_rate, life_exp))

show(ggplot(UN_data_m9,
            aes(x = life_exp, y = fert_rate)) +
       geom_point(alpha = 0.1) +
       labs(x = "Life Expectancy", y = "Fertility Rate"),
     "27-02_scatter")

show(ggplot(UN_data_m9, aes(x = life_exp, y = fert_rate)) +
       geom_point(alpha = 0.1) +
       labs(x = "Life Expectancy",
            y = "Fertility Rate",
            title = "Relationship of life expectancy and fertility rate") +
       geom_smooth(method = "lm", se = FALSE),
     "27-03_scatter_regression_line")

# =====================================================================
# 27.2.2 Simple linear regression
# =====================================================================
status("27.2.2 Simple linear regression with lm()")
# Fit regression model:
demographics_model <- lm(fert_rate ~ life_exp, data = UN_data_m9)
# Get regression coefficients (textbook: intercept 12.599, slope -0.137)
coef(demographics_model)

# =====================================================================
# 27.2.3 Observed/fitted values and residuals
# =====================================================================
status("27.2.3 Fitted values and residuals")
regression_points <- get_regression_points(demographics_model)
regression_points
regression_points |> slice(21:24)   # the rows shown in Table 27.3

# =====================================================================
# 27.3 One categorical explanatory variable
# 27.3.1 Exploratory data analysis
# =====================================================================
status("27.3.1 EDA: life expectancy by continent")
gapminder2022 <- un_member_states_2024 |>
  select(country, life_exp = life_expectancy_2022, continent, gdp_per_capita) |>
  na.omit()

glimpse(gapminder2022)

gapminder2022 |> sample_n(size = 3)

gapminder2022 |> select(life_exp, continent) |> tidy_summary()

show(ggplot(gapminder2022, aes(x = life_exp)) +
       geom_histogram(binwidth = 5, color = "white") +
       labs(x = "Life expectancy",
            y = "Number of countries",
            title = "Histogram of distribution of worldwide life expectancies"),
     "27-06_hist_life_exp")

show(ggplot(gapminder2022, aes(x = life_exp)) +
       geom_histogram(binwidth = 5, color = "white") +
       labs(x = "Life expectancy",
            y = "Number of countries",
            title = "Histogram of distribution of worldwide life expectancies") +
       facet_wrap(~ continent, nrow = 2),
     "27-07_hist_by_continent")

show(ggplot(gapminder2022, aes(x = continent, y = life_exp)) +
       geom_boxplot() +
       labs(x = "Continent", y = "Life expectancy",
            title = "Life expectancy by continent"),
     "27-08_box_by_continent")

life_exp_by_continent <- gapminder2022 |>
  group_by(continent) |>
  summarize(median = median(life_exp), mean = mean(life_exp))
life_exp_by_continent

# Table 27.7: difference from Africa's mean
life_exp_by_continent |>
  mutate(difference_vs_africa = mean - mean[continent == "Africa"])

# =====================================================================
# 27.3.2 Linear regression with a categorical explanatory variable
# =====================================================================
status("27.3.2 Regression on continent")
life_exp_model <- lm(life_exp ~ continent, data = gapminder2022)
coef(life_exp_model)        # intercept = Africa's mean; others = offsets

# =====================================================================
# 27.3.3 Observed/fitted values and residuals
# =====================================================================
status("27.3.3 Regression points by country")
get_regression_points(life_exp_model, ID = "country")

# =====================================================================
# 27.4.1 Correlation is not necessarily causation
# =====================================================================
status("27.4.1 Confounding: Human Development Index")
show(ggplot(data = un_member_states_2024,
            aes(x = hdi_2022, y = life_expectancy_2022)) +
       geom_point() +
       labs(x = "Human Development Index (HDI)", y = "Life Expectancy"),
     "27-hdi_vs_life_exp")

show(ggplot(data = un_member_states_2024,
            aes(x = hdi_2022, y = fertility_rate_2022)) +
       geom_point() +
       labs(x = "Human Development Index (HDI)", y = "Fertility Rate"),
     "27-hdi_vs_fert_rate")

un_member_states_2024 |>
  get_correlation(life_expectancy_2022 ~ hdi_2022, na.rm = TRUE)   # textbook: 0.889

un_member_states_2024 |>
  get_correlation(fertility_rate_2022 ~ hdi_2022, na.rm = TRUE)    # textbook: -0.849

# =====================================================================
# 27.4.2 Best-fitting line: sum of squared residuals
# =====================================================================
status("27.4.2 Sum of squared residuals")
demographics_model <- lm(fert_rate ~ life_exp, data = UN_data_m9)
regression_points <- get_regression_points(demographics_model)

regression_points |>
  mutate(squared_residuals = residual^2) |>
  summarize(sum_of_squared_residuals = sum(squared_residuals))     # textbook: 81.3

# =====================================================================
# 27.4.3 Under the hood of get_regression_points()
# =====================================================================
status("27.4.3 get_regression_points() under the hood")
library(broom)
library(janitor)
demographics_model |>
  augment() |>
  mutate_if(is.numeric, round, digits = 3) |>
  clean_names() |>
  select(-c("std_resid", "hat", "sigma", "cooksd", "std_resid"))

status(paste("Whole M9A walkthrough ran.", length(list.files("m9a_plots")),
             "plots saved in m9a_plots/."), state = "COMPLETE")
