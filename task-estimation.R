library(readr)
library(dplyr)
library(ggplot2)

setwd('~/projects/task-estimation')

to_hrs <- function(x) {
  as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 4, 5))/60
}

tasks <- read_csv('tasks.csv') |>
  filter(!is.na(`Time Estimate (hours)`) & !is.na(`Time Logged (hours)`)) |>
  mutate(
    `Time Logged (hours)` = to_hrs(`Time Logged (hours)`),
    `Time Estimate (hours)` = to_hrs(`Time Estimate (hours)`)
  )

tasks |>
  ggplot(aes(x = `Time Estimate (hours)`, y = `Time Logged (hours)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = 'Logged', linetype = 'Logged')) +
  geom_segment(aes(x = 0.25, xend = 2, y = 0.25, yend = 2, col = 'Ideal', linetype = 'Ideal'), lwd = 1) +
  scale_discrete_manual(
    c("color", "linetype"),
    name = NULL,
    breaks = c("Logged", "Ideal"),
    values = c("Logged" = 4, "Ideal"= 1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")