# Install and load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, readr, readxl, fitdistrplus, lubridate, purrr, stringdist)

# Set working directory
setwd("C:/Users/Admin/Downloads/Bootcamp Assignement/A1b")
getwd()
# Load the datasets
ipl_bbb <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
ipl_salary <- read_excel("IPL SALARIES 2024.xlsx")

# Data processing
grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Top 3 highest scorers and top 3 wicket takers in the last 3 seasons
last_three_years <- c(2022, 2023, 2024)
top_run_getters <- player_runs %>%
  filter(Season %in% last_three_years) %>%
  group_by(Season) %>%
  slice_max(order_by = runs_scored, n = 3) %>%
  ungroup()

top_wicket_takers <- player_wickets %>%
  filter(Season %in% last_three_years) %>%
  group_by(Season) %>%
  slice_max(order_by = wicket_confirmation, n = 3) %>%
  ungroup()

print("Top Three Run Getters in the Last 3 Seasons:")
print(top_run_getters)

print("Top Three Wicket Takers in the Last 3 Seasons:")
print(top_wicket_takers)

# Get the best distribution for list_top_batsman_last_three_year
list_top_batsman_last_three_year <- split(top_run_getters, top_run_getters$Season)

get_best_distribution <- function(data) {
  # Exclude 'beta' from distribution names
  dist_names <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp', 'logis', 'cauchy')
  fit_list <- map(dist_names, ~tryCatch(fitdist(data, .x), error = function(e) NULL))
  gof_stats <- map_dbl(fit_list, ~ifelse(!is.null(.x), gofstat(.x)$aic, Inf))
  best_fit <- fit_list[[which.min(gof_stats)]]
  return(best_fit)
}

for (key in names(list_top_batsman_last_three_year)) {
  for (Striker in list_top_batsman_last_three_year[[key]]$Striker) {
    cat("************************\n")
    cat("Year:", key, "Batsman:", Striker, "\n")
    data <- top_run_getters %>% filter(Season == key & Striker == Striker) %>% pull(runs_scored)
    best_fit <- get_best_distribution(data)
    print(best_fit)
  }
}

# Get the best distribution for top_bowler_last_three_year
list_top_bowler_last_three_year <- split(top_wicket_takers, top_wicket_takers$Season)

for (key in names(list_top_bowler_last_three_year)) {
  for (Bowler in list_top_bowler_last_three_year[[key]]$Bowler) {
    cat("************************\n")
    cat("Year:", key, "Bowler:", Bowler, "\n")
    data <- top_wicket_takers %>% filter(Season == key & Bowler == Bowler) %>% pull(wicket_confirmation)
    best_fit <- get_best_distribution(data)
    print(best_fit)
  }
}

# Function to fit and plot normal distribution
fit_and_plot_normal <- function(data, title, xlabel) {
  if(length(data) == 0) {
    cat(paste("No data available for", title, "\n"))
    return(NULL)
  }
  
  mu <- mean(data)
  std <- sd(data)
  
  # Adjust plot margins
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Resize the plotting window
  windows(width = 10, height = 6)  # For Windows
  
  # Plot histogram
  hist(data, breaks = 10, freq = FALSE, col = 'blue', main = title, xlab = xlabel, ylab = 'Density')
  
  # Plot PDF
  x <- seq(min(data), max(data), length = 100)
  lines(x, dnorm(x, mean = mu, sd = std), col = 'black', lwd = 2)
  
  cat(paste(title, "\nFit results: mu =", round(mu, 2), ", std =", round(std, 2), "\n"))
}


# Calculate the Correlation between Salary and Runs
R2024 <- player_runs %>% filter(Season == 2024)

match_names <- function(name, names_list) {
  match <- stringdist::amatch(name, names_list, maxDist = 0.2)
  if (is.na(match)) return(NA)
  return(names_list[match])
}

df_salary <- ipl_salary %>%
  mutate(Matched_Player = map_chr(Player, ~match_names(.x, R2024$Striker)))

df_merged <- df_salary %>%
  inner_join(R2024, by = c("Matched_Player" = "Striker"))

correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")
cat("Correlation between Salary and Runs:", correlation, "\n")

# Visualize the Correlation between Salary and Runs
ggplot(df_merged, aes(x = Rs, y = runs_scored)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Salary and Runs",
       x = "Salary (Rs)",
       y = "Runs Scored") +
  theme_minimal()

# Get the best distribution for one specific player: DA Warner
runs_daw <- player_runs %>% filter(Striker == 'DA Warner')

if (nrow(runs_daw) > 0) {
  cat("************************\n")
  cat("Fitting distribution for player: DA Warner\n")
  best_fit_daw <- get_best_distribution(runs_daw$runs_scored)
  print(best_fit_daw)
} else {
  cat("No data available for player: DA Warner\n")
}

# Get top 10 batsmen based on runs scored in the last 3 years
top_batsmen <- player_runs %>%
  group_by(Striker) %>%
  summarise(total_runs = sum(runs_scored)) %>%
  top_n(10, total_runs) %>%
  pull(Striker)
print(top_batsmen)

# Get top 10 bowlers based on wickets taken in the last 3 years
top_bowlers <- player_wickets %>%
  group_by(Bowler) %>%
  summarise(total_wickets = sum(wicket_confirmation)) %>%
  top_n(10, total_wickets) %>%
  pull(Bowler)
print(top_bowlers)

# Function for fuzzy matching
match_names <- function(names_to_match, reference_names) {
  matches <- stringdist::amatch(tolower(names_to_match), tolower(reference_names), method = "lv")
  return(reference_names[matches])
}


# Apply string matching to get exact names for salaries
matched_batsmen <- match_names(top_batsmen, ipl_salary$Player)
print(matched_batsmen)

ipl_salary$Player
matched_bowlers <- match_names(top_bowlers, ipl_salary$Player)
print(matched_bowlers)

# Filter salaries for the matched players
top_batsmen_salaries <- ipl_salary %>%
  filter(Player %in% matched_batsmen)

top_bowlers_salaries <- ipl_salary %>%
  filter(Player %in% matched_bowlers)

# Print matched salaries for verification
print(top_batsmen_salaries)
print(top_bowlers_salaries)

# Perform t-test
t_test_result <- t.test(top_batsmen_salaries$Rs, top_bowlers_salaries$Rs)

# Print t-test results
cat("T-statistic:", t_test_result$statistic, "\n")
cat("P-value:", t_test_result$p.value, "\n")

# Interpret t-test results
if (t_test_result$p.value < 0.05) {
  cat("There is a significant difference between the salaries of the top 10 batsmen and the top 10 bowlers.\n")
} else {
  cat("There is no significant difference between the salaries of the top 10 batsmen and the top 10 bowlers.\n")
}
