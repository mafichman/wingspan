library(tidyverse)
library(readr)
library(caret)


# Specify the file name or ID on Google Drive
dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRBgbwCxlrcDk20UUfUPetEnWqGARNeOOzlqqW1hftq8LiAIeuSi51qbPYCOou7Rd4THgh90MoAgz2q/pub?gid=0&single=true&output=csv")

# Clean the data to create standings

standings <- 
  dat %>%
  group_by(player, outcome) %>%
  tally() %>%
  pivot_wider(
    names_from = outcome,  # Spread based on 'outcome'
    values_from = n,       # Fill values from 'n'
    values_fill = 0        # Replace missing combinations with 0
  ) %>%
  select(player, W, L, T) %>%
  mutate(Pct = W/(W+L+T)) %>%
  arrange(-Pct)

# Game records

records <-
  dat %>%
  select(-n_players) %>%
  mutate(game_id = as.character(game_id)) %>%
  pivot_longer(
    cols = where(is.numeric),   # Select numeric columns for reshaping
    names_to = "statistic",     # Create a column for statistic names
    values_to = "value"         # Create a column for values
  ) %>%
  group_by(statistic) %>%       # Group by each statistic
  slice_max(value, with_ties = FALSE) %>%  # Keep only the max value row for each statistic
  ungroup() %>%
  select(statistic, player, value, date, game_id, outcome)

# Calculate everybody's stats for the various categories

stats <-
  dat %>%
  group_by(player) %>%
  summarize(Avg_Score = mean(total),
            Max_Score = max(total),
            Avg_Birds = mean(birds),
            Max_Birds = max(birds) )


# Victory factors

reg <- glm(win ~ birds + eggs + tucked + food, 
                   family="binomial" (link="logit"),
                   data = dat %>%
                     mutate(win = ifelse(outcome == "W", 1, 0)) %>%
                     filter(is.na(bonus) == FALSE & 
                              is.na(goals) == FALSE) %>%
                     mutate(birds_pct = birds / total,
                            eggs_pct = eggs/total,
                            tucked_pct = tucked/total,
                            food_pct = food/total,
                            bonus_dummy = ifelse(bonus != 0, "Bonus", "No Bonus")))

summary(reg)

exp_coef <- reg$coefficients %>%
  as.data.frame() %>%
  rename(coefficient = ".") %>%
  mutate(exponentiated = exp(coefficient))

dat %>%
  mutate(win = ifelse(outcome == "W", 1, 0)) %>%
  filter(is.na(bonus) == FALSE & 
           is.na(goals) == FALSE) %>%
  mutate(estimate = predict(reg, type="response"),
         prediction = ifelse(estimate >= 0.5, 1, 0)) %>%
  ggplot()+
  geom_density(aes(estimate))+
  facet_wrap(~as.factor(win), nrow = 2)+
  theme_bw()

matrix_data <- dat %>%
  mutate(win = ifelse(outcome == "W", 1, 0)) %>%
  filter(is.na(bonus) == FALSE & 
           is.na(goals) == FALSE) %>%
  mutate(estimate = predict(reg, type="response"),
         prediction = as.factor(ifelse(estimate >= 0.4, 1, 0))) %>%
  select(prediction, win) %>%
  mutate(win = as.factor(as.character(win))) 

  caret::confusionMatrix(matrix_data$prediction, matrix_data$win, positive = "1")


# Playing styles

dat %>%
  filter(player %in% c("Michael", "Isaac", "Erica")) %>%
  ggplot()+
  geom_density(aes(total, color = player))+
  theme_bw()
