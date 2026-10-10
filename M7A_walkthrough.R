# =====================================================================
# M7A: Data Visualization -- textbook walkthrough
# https://spencergreenhalgh.com/ict_lis_661_textbook_2025_fall/_book/m7a-data-visualization
#
# Open Rclasstemplate_ALWAYS_OPEN_ME.Rproj first.
# Run line by line (Ctrl+Enter) to see each plot in the Plots pane,
# or source the whole file -- every plot is also saved to m7a_plots/.
# =====================================================================

status <- function(msg, state = "WORKING") {
  cat("\n####################################################\n")
  cat("#### STATUS:", state, "--", msg, "\n")
  cat("####################################################\n\n")
  flush.console()
}

# show() prints a plot to the Plots pane AND saves it as a PNG
dir.create("m7a_plots", showWarnings = FALSE)
show <- function(p, name) {
  print(p)
  ggsave(file.path("m7a_plots", paste0(name, ".png")), p,
         width = 8, height = 5, dpi = 100)
  invisible(p)
}

# ---------------------------------------------------------------------
# Needed packages
# ---------------------------------------------------------------------
status("Loading packages")
library(nycflights23)
library(ggplot2)
library(moderndive)
library(tibble)
library(dplyr)   # for filter() below

# The chapter uses envoy_flights from the M6A walkthrough.
# Envoy Air's carrier code is "MQ".
envoy_flights <- flights |>
  filter(carrier == "MQ")

# =====================================================================
# 21.3 5NG#1: Scatterplots via geom_point
# =====================================================================
status("21.3.1 Scatterplot (geom_point)")
show(ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
       geom_point(),
     "21-02_scatter")

# A plot with no layers -- blank!
show(ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)),
     "21-03_no_layers")

# 21.3.2 Overplotting -- Method 1: transparency
status("21.3.2 Overplotting: alpha and jitter")
show(ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
       geom_point(alpha = 0.2),
     "21-04_alpha")

# Method 2: jitter
show(ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
       geom_jitter(width = 30, height = 30),
     "21-06_jitter")

# =====================================================================
# 21.4 5NG#2: Linegraphs via geom_line
# =====================================================================
status("21.4 Linegraph (geom_line)")
glimpse(weather)
glimpse(early_january_2023_weather)

show(ggplot(data = early_january_2023_weather,
            mapping = aes(x = time_hour, y = wind_speed)) +
       geom_line(),
     "21-07_line_wind")

# =====================================================================
# 21.5 5NG#3: Histograms via geom_histogram
# =====================================================================
status("21.5 Histograms (geom_histogram)")
show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(),
     "21-10_hist_default")

show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(color = "white"),
     "21-11_hist_white")

show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(color = "white", fill = "steelblue"),
     "21-11b_hist_steelblue")

# 21.5.2 Adjusting the bins
show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(bins = 20, color = "white"),
     "21-12a_hist_bins20")

show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(binwidth = 5, color = "white"),
     "21-12b_hist_binwidth5")

# =====================================================================
# 21.6 Facets
# =====================================================================
status("21.6 Facets (facet_wrap)")
show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(binwidth = 5, color = "white") +
       facet_wrap(~ month),
     "21-13_facet_month")

show(ggplot(data = weather, mapping = aes(x = wind_speed)) +
       geom_histogram(binwidth = 5, color = "white") +
       facet_wrap(~ month, nrow = 4),
     "21-14_facet_nrow4")

# =====================================================================
# 21.7 5NG#4: Boxplots via geom_boxplot
# =====================================================================
status("21.7 Boxplots (geom_boxplot)")
# April five-number summary from the text
weather |> filter(month == 4) |> pull(wind_speed) |> summary()

# Invalid: month is numeric, so you get ONE box
show(ggplot(data = weather, mapping = aes(x = month, y = wind_speed)) +
       geom_boxplot(),
     "21-17_box_invalid")

# Fixed: factor(month) makes month categorical
show(ggplot(data = weather, mapping = aes(x = factor(month), y = wind_speed)) +
       geom_boxplot(),
     "21-18_box_by_month")

# =====================================================================
# 21.8 5NG#5: Barplots via geom_bar or geom_col
# =====================================================================
status("21.8 Barplots (geom_bar / geom_col)")
fruits <- tibble(fruit = c("apple", "apple", "orange", "apple", "orange"))
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2))
fruits
fruits_counted

# Not pre-counted -> geom_bar()
show(ggplot(data = fruits, mapping = aes(x = fruit)) +
       geom_bar(),
     "21-19_bar_fruits")

# Pre-counted -> geom_col()
show(ggplot(data = fruits_counted, mapping = aes(x = fruit, y = number)) +
       geom_col(),
     "21-20_col_fruits")

show(ggplot(data = flights, mapping = aes(x = carrier)) +
       geom_bar(),
     "21-21_bar_carrier")

# Table 21.3: the same counts, pre-counted
flights |> count(carrier, name = "number")

# 21.8.2 Avoid pie charts! (code not shown in the text; this recreates Fig 21.22)
status("21.8.2 The dreaded pie chart")
show(ggplot(flights, aes(x = factor(1), fill = carrier)) +
       geom_bar(width = 1) +
       coord_polar(theta = "y") +
       theme_void(),
     "21-22_pie_avoid")

# 21.8.3 Two categorical variables
status("21.8.3 Two categorical variables")
show(ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
       geom_bar(),
     "21-23_bar_stacked")

# color = outline, not fill
show(ggplot(data = flights, mapping = aes(x = carrier, color = origin)) +
       geom_bar(),
     "21-24_bar_color_outline")

# Common mistake: fill outside aes() -- origin isn't applied
try(print(ggplot(data = flights, mapping = aes(x = carrier), fill = origin) +
            geom_bar()))

show(ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
       geom_bar(position = "dodge"),
     "21-25_bar_dodge")

show(ggplot(data = flights, mapping = aes(x = carrier)) +
       geom_bar() +
       facet_wrap(~ origin, ncol = 1),
     "21-26_bar_facet_origin")

# =====================================================================
# 21.9.2 Function argument specification
# =====================================================================
status("21.9.2 Argument names can be dropped")
# Segment 1:
ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar()

# Segment 2 -- identical plot:
ggplot(flights, aes(x = carrier)) +
  geom_bar()

status(paste("Whole M7A walkthrough ran.", length(list.files("m7a_plots")),
             "plots saved in m7a_plots/ -- use the arrows in the Plots pane to page through."),
       state = "COMPLETE")
