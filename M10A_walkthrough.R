# =====================================================================
# M10A: Multiple Regression -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m10a-multiple-regression
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Run line by line (Ctrl+Enter) or all at once (Ctrl+Shift+S).
# Plots show in the Plots pane and are also saved to m10a_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

dir.create("m10a_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggplot2::ggsave(file.path("m10a_plots", paste0(name, ".png")), p,
                  width = 8, height = 5, dpi = 100, bg = "white")
  invisible(p)
}

# ---------------------------------------------------------------------
# Needed packages
# ---------------------------------------------------------------------
status("Loading packages")
library(tidyverse)
library(moderndive)
library(ISLR2)

set.seed(76)   # so the random samples below are the same each run

# =====================================================================
# 30.1 One numerical and one categorical explanatory variable
# 30.1.1 Exploratory data analysis
# =====================================================================
status("30.1.1 EDA: fertility rate, life expectancy, income group")
UN_data_m10 <- un_member_states_2024 |>
  select(country,
         life_expectancy_2022,
         fertility_rate_2022,
         income_group_2024) |>
  na.omit() |>
  rename(life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         income = income_group_2024) |>
  mutate(income = factor(income,
                         levels = c("Low income", "Lower middle income",
                                    "Upper middle income", "High income")))

glimpse(UN_data_m10)                       # textbook: 182 rows

UN_data_m10 |> sample_n(size = 10)

UN_data_m10 |>
  select(life_exp, fert_rate, income) |>
  tidy_summary()

UN_data_m10 |>
  get_correlation(formula = fert_rate ~ life_exp)   # textbook: -0.815

show(ggplot(UN_data_m10, aes(x = life_exp, y = fert_rate, color = income)) +
       geom_point() +
       labs(x = "Life Expectancy", y = "Fertility Rate", color = "Income group") +
       geom_smooth(method = "lm", se = FALSE),
     "30-01_interaction_lines")

# =====================================================================
# 30.1.2 Model with interactions
# =====================================================================
status("30.1.2 One-factor model, then interaction model")
# One categorical explanatory variable (review)
one_factor_model <- lm(fert_rate ~ income, data = UN_data_m10)
coef(one_factor_model)                     # textbook: 4.28, -1.30, -2.25, -2.65

# Interaction model: y ~ x1 * x2  (same as x1 + x2 + x1:x2)
model_int <- lm(fert_rate ~ life_exp * income, data = UN_data_m10)
coef(model_int)                            # textbook Table 30.2

# Intercept and slope for each income group
b <- coef(model_int)
tibble(income    = levels(UN_data_m10$income),
       intercept = b["(Intercept)"] + c(0, b["incomeLower middle income"],
                                        b["incomeUpper middle income"], b["incomeHigh income"]),
       slope     = b["life_exp"] + c(0, b["life_exp:incomeLower middle income"],
                                     b["life_exp:incomeUpper middle income"],
                                     b["life_exp:incomeHigh income"]))

# =====================================================================
# 30.1.3 Model without interactions (parallel slopes)
# =====================================================================
status("30.1.3 Parallel slopes model")
show(ggplot(UN_data_m10, aes(x = life_exp, y = fert_rate, color = income)) +
       geom_point() +
       labs(x = "Life expectancy", y = "Fertility rate", color = "Income group") +
       geom_parallel_slopes(se = FALSE),
     "30-02_parallel_slopes")

# Fit regression model:
model_no_int <- lm(fert_rate ~ life_exp + income, data = UN_data_m10)
# Get the coefficients of the model
coef(model_no_int)                         # textbook Table 30.3: slope -0.101

# =====================================================================
# 30.1.4 Observed responses, fitted values, and residuals
# =====================================================================
status("30.1.4 Fitted values and residuals (interaction model)")
regression_points <- get_regression_points(model_int)
regression_points

# =====================================================================
# 30.2 Two numerical explanatory variables
# 30.2.1 Exploratory data analysis
# =====================================================================
status("30.2.1 EDA: credit card debt, credit limit, income")
credit_m10 <- Credit |> as_tibble() |>
  select(debt = Balance, credit_limit = Limit,
         income = Income, credit_rating = Rating, age = Age)

glimpse(credit_m10)                        # textbook: 400 rows

credit_m10 |> sample_n(size = 5)

credit_m10 |> select(debt, credit_limit, income) |> tidy_summary()

show(ggplot(credit_m10, aes(x = credit_limit, y = debt)) +
       geom_point() +
       labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
            title = "Debt and credit limit") +
       geom_smooth(method = "lm", se = FALSE),
     "30-05a_debt_vs_limit")

show(ggplot(credit_m10, aes(x = income, y = debt)) +
       geom_point() +
       labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
            title = "Debt and income") +
       geom_smooth(method = "lm", se = FALSE),
     "30-05b_debt_vs_income")

credit_m10 |> get_correlation(debt ~ credit_limit)
credit_m10 |> get_correlation(debt ~ income)

# Correlation matrix (textbook Table 30.8)
credit_m10 |> select(debt, credit_limit, income) |> cor()

# Correlation doesn't change under a linear transformation
credit_m10 |> get_correlation(debt ~ 1000 * income)   # textbook: 0.464

# =====================================================================
# 30.2.2 Multiple regression with two numerical regressors
# =====================================================================
status("30.2.2 Regression plane: debt ~ credit_limit + income")
debt_model <- lm(debt ~ credit_limit + income, data = credit_m10)
coef(debt_model)                           # textbook: -385.179, 0.264, -7.663

# Partial vs. simple slope for income
simple_model <- lm(debt ~ income, data = credit_m10)
coef(simple_model)                         # textbook: 246.51, 6.05

# =====================================================================
# 30.2.3 Observed/fitted values and residuals
# =====================================================================
status("30.2.3 Fitted values and residuals (regression plane)")
get_regression_points(debt_model)

status(paste("Whole M10A walkthrough ran.", length(list.files("m10a_plots")),
             "plots saved in m10a_plots/."), state = "COMPLETE")
