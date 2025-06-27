library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 100  # Number of observations per group per time period
treatment_effect <- 5  # Effect of treatment

# Simulate data
group <- rep(c(0, 1), each = n)  # 0 = Control, 1 = Treatment
time <- rep(c(0, 1), times = n)  # 0 = Pre-treatment, 1 = Post-treatment
error <- rnorm(2 * n, mean = 0, sd = 2)  # Random noise

id <- rep(1:100, each = 2)
radio <- ifelse(id %% 2 == 0, 1, 0)

# Generate outcome variable
outcome <- 10 + 2 * group + 3 * time + treatment_effect * group * time * radio + error

# Create data frame
data <- data.frame(id = id, group = factor(group), time = factor(time), radio = factor(radio), outcome = outcome)

# View first few rows
head(data)

# Plot the data
ggplot(data, aes(x = time, y = outcome, color = group, group = group)) +
  geom_line(stat = "summary", fun = "mean") +
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  labs(title = "Difference-in-Differences Simulation", x = "Time", y = "Outcome") +
  theme_classic()

m <- lm(outcome ~ time*group*radio, data = data)
summary(m)

data2 <- data %>%
  group_by(id) %>%  # Group by the 'group' variable
  arrange(time) %>%    # Ensure data is ordered by time within each group
  mutate(outcome_lead = lead(outcome, n = 1)) %>%
  ungroup()

data2$outcome_change <- data2$outcome_lead - data2$outcome

m <- lm(outcome_change ~ group*radio, data = data2)
summary(m)


