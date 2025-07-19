## Analysis of ESPN's Fantasy Football Projections for the 2023 NFL season

# Author: John Incantalupo
# Date Completed: ~December 2023

library(tidyverse)
library(lme4)

#Set working directory to where files are

#Load files
scoring <- read.csv("scoring.csv")
week13 <- read.csv("week13.csv")

#Factors
scoring$Week <- factor(scoring$Week)
scoring$Name <- factor(scoring$Name)
scoring$Pos <- factor(scoring$Pos)
scoring$Team <- factor(scoring$Team)
scoring$Opp <- factor(scoring$Opp)
scoring$Injury. <- factor(scoring$Injury.)

week13$Week <- factor(week13$Week)
week13$Name <- factor(week13$Name)
week13$Pos <- factor(week13$Pos)
week13$Team <- factor(week13$Team)
week13$Opp <- factor(week13$Opp)

#Add projection errors to each week
scoring %>% 
  mutate(Proj_error = Actual - Projected) -> scoring

#Eliminate injured players
scoring %>%
  filter(Injury. != 'Y') -> scoring_no_inj
scoring_no_inj[, -8] -> scoring_no_inj #Injury column is now redundant
scoring_no_inj$Name <- factor(scoring_no_inj$Name)

#Will be used for some of the plots
team_colors = c("red", "black", "purple4", "blue",
                "deepskyblue2", "navy", "darkorange", "orangered",
                "blue4", "darkorange", "deepskyblue", "green4",
                "navyblue", "blue", "turquoise4", "red",
                "dodgerblue", "blue", "gray40", "turquoise3",
                "purple", "blue4", "darkgoldenrod", "blue",
                "darkgreen", "springgreen4", "yellow", "midnightblue",
                "red", "orangered4", "deepskyblue2", "red4")


## Exploratory data analysis


#Correlation between projected points and actual points
ggplot(scoring_no_inj, aes(Projected, Actual, color = Pos)) +
  geom_point() +
  labs(x = "Projected Points", y = "Actual Points",
       color = "Position") +
  ggtitle("ESPN Fantasy Football Projected vs Actual Fantasy Points",
          subtitle = "Weeks 1-12, 2023") +
  geom_abline(slope = 1, intercept = 0) #Expected pts line

cor(scoring_no_inj$Projected, scoring_no_inj$Actual) #0.347976

#Grouping by position
ggplot(scoring_no_inj, aes(Projected, Actual, color = Pos)) +
  geom_point() + facet_wrap( ~ Pos)

#Correlation by position
lapply(split(scoring_no_inj, scoring_no_inj$Pos),
       function(X) cor(X$Projected, X$Actual))
#QB: 0.3620392
#RB: 0.2960868
#WR: 0.3356166
#TE: 0.2447473

#Distribution of projection error
ggplot(scoring_no_inj, aes(Proj_error)) + geom_histogram()

#Grouping by week
ggplot(scoring_no_inj, aes(Proj_error)) + geom_density() +
  facet_wrap( ~ Week)

#Grouping by opponent
ggplot(scoring_no_inj, aes(x = Proj_error, color = Opp)) + geom_density() +
  facet_wrap( ~ Opp, nrow = 4) +
  labs(x = "Projection Error (Actual - Projected)",
       y = "Density") +
  ggtitle("Distribution of Projection Error, Grouped by Opposing Team") +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_color_manual(values = team_colors) +
  theme(legend.position = "none")

#Overlay density curves for each position
ggplot(scoring_no_inj, aes(x = Proj_error, color = Pos)) +
  geom_density() +
  labs(x = "Projection Error (Actual - Projected)",
       y = "Density", color = "Position") +
  ggtitle("Distribution of Projection Error, Grouped by Position")

#Averages for each position
pos_avgs <- data.frame(Pos = aggregate(Projected ~ Pos,
                                       data = scoring_no_inj,
                                       FUN = mean)$Pos,
                       Avg_Projected = aggregate(Projected ~ Pos,
                                                 data = scoring_no_inj,
                                                 FUN = mean)$Projected,
                       Avg_Actual = aggregate(Actual ~ Pos,
                                              data = scoring_no_inj,
                                              FUN = mean)$Actual,
                       Mean = aggregate(Proj_error ~ Pos,
                                        data = scoring_no_inj,
                                        FUN = mean)$Proj_error,
                       Median = aggregate(Proj_error ~ Pos,
                                          data = scoring_no_inj,
                                          FUN = median)$Proj_error,
                       Variance = aggregate(Proj_error ~ Pos,
                                            data = scoring_no_inj,
                                            FUN = var)$Proj_error)

#Averages for each week
weekly_avgs <- data.frame(Week = 1:12,
                          Avg_Projected = aggregate(Projected ~ Week,
                                                    data = scoring_no_inj,
                                                    FUN = mean)$Projected,
                          Avg_Actual = aggregate(Actual ~ Week,
                                                 data = scoring_no_inj,
                                                 FUN = mean)$Actual,
                          Mean = aggregate(Proj_error ~ Week,
                                           data = scoring_no_inj,
                                           FUN = mean)$Proj_error,
                          Median = aggregate(Proj_error ~ Week,
                                             data = scoring_no_inj,
                                             FUN = median)$Proj_error,
                          Variance = aggregate(Proj_error ~ Week,
                                               data = scoring_no_inj,
                                               FUN = var)$Proj_error)

#Averages against each team
team_avgs <- data.frame(Team = aggregate(Projected ~ Opp,
                                         data = scoring_no_inj,
                                         FUN = mean)$Opp,
                        Avg_Projected = aggregate(Projected ~ Opp,
                                                  data = scoring_no_inj,
                                                  FUN = mean)$Projected,
                        Avg_Actual = aggregate(Actual ~ Opp,
                                               data = scoring_no_inj,
                                               FUN = mean)$Actual,
                        Mean = aggregate(Proj_error ~ Opp,
                                         data = scoring_no_inj,
                                         FUN = mean)$Proj_error,
                        Median = aggregate(Proj_error ~ Opp,
                                           data = scoring_no_inj,
                                           FUN = median)$Proj_error,
                        Variance = aggregate(Proj_error ~ Opp,
                                             data = scoring_no_inj,
                                             FUN = var)$Proj_error)

#Which players were the most consistent with their projected output?
scoring_no_inj %>% group_by(Name) %>%
  summarize(Weeks_played = n(), Pos = last(Pos), Team = last(Team),
            Avg_Projected = mean(Projected),
            Avg_Actual = mean(Actual),
            Mean = mean(Proj_error),
            Var = var(Proj_error)) -> player_data

t(data.frame(lapply(split(scoring_no_inj, scoring_no_inj$Name),
                    function(X) cor(X$Projected, X$Actual)))) -> player_pred
as.vector(player_pred) -> player_pred

player_data %>%
  mutate(Cor = player_pred) %>%
  filter(!is.na(Cor)) -> player_data

ggplot(player_data, aes(Weeks_played, Cor)) + geom_point()


## Testing models


#Kruskal-Wallis test for difference between weeks
kruskal.test(scoring_no_inj$Proj_error, scoring_no_inj$Week)
#p-value: 0.008159, very significant

#Kruskal-Wallis test for difference between opponents
kruskal.test(scoring_no_inj$Proj_error, scoring_no_inj$Opp)
#p-value: 0.03426, very significant

#Kruskal-Wallis test for difference between positions
kruskal.test(scoring_no_inj$Proj_error, scoring_no_inj$Pos)
#p-value: 0.5728, not significant at all


#Linear models
m1 <- lm(Actual ~ Projected + Pos + Opp, data = scoring_no_inj)
m2 <- lm(Actual ~ Projected + Name + as.numeric(as.character(Week))*Opp + Pos*Opp,
         data = scoring_no_inj)
m3 <- lm(Actual ~ Projected + Name + Week:Opp,
         data = scoring_no_inj)
m4 <- lm(Actual ~ Projected + Name + as.numeric(as.character(Week)):Opp,
         data = scoring_no_inj)
m5 <- lm(Actual ~ Projected + Name + Pos:Opp,
         data = scoring_no_inj) #Best one I could find
m6 <- lm(Actual ~ Projected + Name + Week:Opp + Pos:Opp,
          data = scoring_no_inj)
m7 <- lm(Actual ~ Projected + Name + as.numeric(as.character(Week)) + Pos:Opp,
          data = scoring_no_inj)

#Mixed models
m8 <- lmer(Actual ~ Projected + (1|Name), data = scoring_no_inj)
m9 <- lmer(Actual ~ Projected + (0 + Projected|Name),
           data = scoring_no_inj)
m10 <- lmer(Actual ~ Projected + (1 + Projected|Name),
            data = scoring_no_inj)
m11 <- lmer(Actual ~ Projected + (1|Name) + (0 + Projected|Name),
            data = scoring_no_inj)
m12 <- lmer(Actual ~ Projected + Opp + (1 + Projected|Name),
            data = scoring_no_inj)

plot(m5)
plot(m10)

#Linear models RMSEs and MAEs
rmse1 <- sqrt(mean((predict(m1) - scoring_no_inj$Actual)^2)) #7.45
mae1 <- mean(abs(predict(m1) - scoring_no_inj$Actual)) #5.90
rmse1_13 <- sqrt(mean((predict(m1, week13) - week13$Actual)^2)) #8.93
mae1_13 <- mean(abs(predict(m1, week13) - week13$Actual)) #7.34

rmse2 <- sqrt(mean((predict(m2) - scoring_no_inj$Actual)^2)) #6.15
mae2 <- mean(abs(predict(m2) - scoring_no_inj$Actual)) #4.73
rmse2_13 <- sqrt(mean((predict(m2, week13) - week13$Actual)^2)) #10.29
mae2_13 <- mean(abs(predict(m2, week13) - week13$Actual)) #8.37

rmse3 <- sqrt(mean((predict(m3) - scoring_no_inj$Actual)^2)) #5.23
mae3 <- mean(abs(predict(m3) - scoring_no_inj$Actual)) #4.04
rmse3_13 <- sqrt(mean((predict(m3, week13) - week13$Actual)^2)) #none
mae3_13 <- mean(abs(predict(m3, week13) - week13$Actual)) #none

rmse4 <- sqrt(mean((predict(m4) - scoring_no_inj$Actual)^2)) #6.58
mae4 <- mean(abs(predict(m4) - scoring_no_inj$Actual)) #5.06
rmse4_13 <- sqrt(mean((predict(m4, week13) - week13$Actual)^2)) #10.00
mae4_13 <- mean(abs(predict(m4, week13) - week13$Actual)) #8.13

rmse5 <- sqrt(mean((predict(m5) - scoring_no_inj$Actual)^2)) #6.32
mae5 <- mean(abs(predict(m5) - scoring_no_inj$Actual)) #4.88
rmse5_13 <- sqrt(mean((predict(m5, week13) - week13$Actual)^2)) #9.38
mae5_13 <- mean(abs(predict(m5, week13) - week13$Actual)) #7.51

rmse6 <- sqrt(mean((predict(m6) - scoring_no_inj$Actual)^2)) #4.92
mae6 <- mean(abs(predict(m6) - scoring_no_inj$Actual)) #3.78
rmse6_13 <- sqrt(mean((predict(m6, week13) - week13$Actual)^2)) #none
mae6_13 <- mean(abs(predict(m6, week13) - week13$Actual)) #none

rmse7 <- sqrt(mean((predict(m7) - scoring_no_inj$Actual)^2)) #6.32
mae7 <- mean(abs(predict(m7) - scoring_no_inj$Actual)) #4.88
rmse7_13 <- sqrt(mean((predict(m7, week13) - week13$Actual)^2)) #9.36
mae7_13 <- mean(abs(predict(m7, week13) - week13$Actual)) #7.51

#Mixed models RMSEs and MAEs
rmse8 <- sqrt(mean((predict(m8) - scoring_no_inj$Actual)^2)) #7.60
mae8 <- mean(abs(predict(m8) - scoring_no_inj$Actual)) #5.99
rmse8_13 <- sqrt(mean((predict(m8, week13) - week13$Actual)^2)) #8.70
mae8_13 <- mean(abs(predict(m8, week13) - week13$Actual)) #7.07

rmse9 <- sqrt(mean((predict(m9) - scoring_no_inj$Actual)^2)) #7.56
mae9 <- mean(abs(predict(m9) - scoring_no_inj$Actual)) #5.96
rmse9_13 <- sqrt(mean((predict(m9, week13) - week13$Actual)^2)) #8.69
mae9_13 <- mean(abs(predict(m9, week13) - week13$Actual)) #7.04

rmse10 <- sqrt(mean((predict(m10) - scoring_no_inj$Actual)^2)) #7.54
mae10 <- mean(abs(predict(m10) - scoring_no_inj$Actual)) #5.94
rmse10_13 <- sqrt(mean((predict(m10, week13) - week13$Actual)^2)) #8.68
mae10_13 <- mean(abs(predict(m10, week13) - week13$Actual)) #7.05

rmse11 <- sqrt(mean((predict(m11) - scoring_no_inj$Actual)^2)) #7.56
mae11 <- mean(abs(predict(m11) - scoring_no_inj$Actual)) #5.96
rmse11_13 <- sqrt(mean((predict(m11, week13) - week13$Actual)^2)) #8.69
mae11_13 <- mean(abs(predict(m11, week13) - week13$Actual)) #7.05

rmse12 <- sqrt(mean((predict(m12) - scoring_no_inj$Actual)^2)) #7.39
mae12 <- mean(abs(predict(m12) - scoring_no_inj$Actual)) #5.85
rmse12_13 <- sqrt(mean((predict(m12, week13) - week13$Actual)^2)) #8.69
mae12_13 <- mean(abs(predict(m12, week13) - week13$Actual)) #7.05

#Predictions
scoring_no_inj %>%
  mutate(lm_Pred1 = predict(m1),
         lm_Pred2 = predict(m2),
         lm_Pred3 = predict(m3),
         lm_Pred4 = predict(m4),
         lm_Pred5 = predict(m5),
         lm_Pred6 = predict(m6),
         lm_Pred7 = predict(m7),
         lm_Pred8 = predict(m8),
         lm_Pred9 = predict(m9),
         lm_Pred10 = predict(m10),
         lm_Pred11 = predict(m11),
         lm_Pred12 = predict(m12)) -> scoring_pred


#Correlation between the model's predictions and actual points
cor(scoring_pred$Actual, scoring_pred$lm_Pred1) #0.3933018
cor(scoring_pred$Actual, scoring_pred$lm_Pred2) #0.6510562
cor(scoring_pred$Actual, scoring_pred$lm_Pred3) #0.7638842
cor(scoring_pred$Actual, scoring_pred$lm_Pred4) #0.5832136
cor(scoring_pred$Actual, scoring_pred$lm_Pred5) #0.6257609
cor(scoring_pred$Actual, scoring_pred$lm_Pred6) #0.7943444
cor(scoring_pred$Actual, scoring_pred$lm_Pred7) #0.625845
cor(scoring_pred$Actual, scoring_pred$lm_Pred8) #0.347976
cor(scoring_pred$Actual, scoring_pred$lm_Pred9) #0.3589557
cor(scoring_pred$Actual, scoring_pred$lm_Pred10) #0.367419
cor(scoring_pred$Actual, scoring_pred$lm_Pred11) #0.3589825
cor(scoring_pred$Actual, scoring_pred$lm_Pred12) #0.411463
#Improved correlation?: 0.7943444

#Testing model in Week 13 data
week13 %>%
  mutate(lm_Pred1 = predict(m1, week13),
         lm_Pred2 = predict(m2, week13),
         lm_Pred4 = predict(m4, week13),
         lm_Pred5 = predict(m5, week13),
         lm_Pred7 = predict(m7, week13),
         lm_Pred8 = predict(m8, week13),
         lm_Pred9 = predict(m9, week13),
         lm_Pred10 = predict(m10, week13),
         lm_Pred11 = predict(m11, week13),
         lm_Pred12 = predict(m12, week13)) -> week13_pred
#m3 and m6 don't work since week13 introduces a new factor to 'Week'

cor(week13_pred$Actual, week13_pred$lm_Pred1) #0.2560979
cor(week13_pred$Actual, week13_pred$lm_Pred2) #0.1734696
cor(week13_pred$Actual, week13_pred$lm_Pred4) #0.1951451
cor(week13_pred$Actual, week13_pred$lm_Pred5) #0.2421344
cor(week13_pred$Actual, week13_pred$lm_Pred7) #0.241236
cor(week13_pred$Actual, week13_pred$lm_Pred8) #0.3161242
cor(week13_pred$Actual, week13_pred$lm_Pred9) #0.3179525
cor(week13_pred$Actual, week13_pred$lm_Pred10) #0.3185671
cor(week13_pred$Actual, week13_pred$lm_Pred11) #0.3179565
cor(week13_pred$Actual, week13_pred$lm_Pred12) #0.253809
#Much worse correlation: 0.1734696, due to overfitting and the week # extrapolation