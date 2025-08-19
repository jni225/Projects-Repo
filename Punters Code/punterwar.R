#This is a modified version of the three scripts that I used during this project, all compiled into this singular R script.
library(tidyverse)
library(nflfastR)
library(GGally)
library(corrplot)

#Loading NFL play-by-play files dating back to 1999
pbp99 <- load_pbp(1999)
pbp00 <- load_pbp(2000)
pbp01 <- load_pbp(2001)
pbp02 <- load_pbp(2002)
pbp03 <- load_pbp(2003)
pbp04 <- load_pbp(2004)
pbp05 <- load_pbp(2005)
pbp06 <- load_pbp(2006)
pbp07 <- load_pbp(2007)
pbp08 <- load_pbp(2008)
pbp09 <- load_pbp(2009)
pbp10 <- load_pbp(2010)
pbp11 <- load_pbp(2011)
pbp12 <- load_pbp(2012)
pbp13 <- load_pbp(2013)
pbp14 <- load_pbp(2014)
pbp15 <- load_pbp(2015)
pbp16 <- load_pbp(2016)
pbp17 <- load_pbp(2017)
pbp18 <- load_pbp(2018)
pbp19 <- load_pbp(2019)
pbp20 <- load_pbp(2020)
pbp21 <- load_pbp(2021)
pbp22 <- load_pbp(2022)
pbp23 <- load_pbp(2023)

pbp <- rbind(pbp99, pbp00, pbp01, pbp02, pbp03,
             pbp04, pbp05, pbp06, pbp07, pbp08,
             pbp09, pbp10, pbp11, pbp12, pbp13,
             pbp14, pbp15, pbp16, pbp17, pbp18,
             pbp19, pbp20, pbp21, pbp22, pbp23)


pbp %>% mutate(play_pts = 0) -> pbp

for (i in 1:nrow(pbp)) {
  if (pbp$field_goal_result[[i]] == "made" & !is.na(pbp$field_goal_result[[i]])) {
    pbp$play_pts[[i]] <- 3
  }
  if (pbp$extra_point_result[[i]] == "good" & !is.na(pbp$extra_point_result[[i]])) {
    pbp$play_pts[[i]] <- 1
  }
  if (pbp$two_point_conv_result[[i]] == "success" & !is.na(pbp$two_point_conv_result[[i]])) {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$safety[[i]] == 1 & !is.na(pbp$safety[[i]])) {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$touchdown[[i]] == 1 & !is.na(pbp$touchdown[[i]])) {
    pbp$play_pts[[i]] <- 6
  }
}

#Removing kickoffs and timeouts from play-by-play data
pbp %>%
  filter(play_type != "kickoff") %>%
  filter(!is.na(ydsnet)) -> pbp_1

#Finding out starting and ending field position for each drive
pbp_1 %>%
  group_by(game_id, drive, posteam) %>%
  dplyr::summarize(starting_field = first(yardline_100),
                   total_yds = first(ydsnet),
                   drive_pts = sum(play_pts)) -> drive_data

#Summarizing drive data for each starting field position
drive_data %>%
  group_by(starting_field) %>%
  dplyr::summarize(avg_yds = mean(total_yds),
                   avg_pts = mean(drive_pts),
                   count = n()) -> field_pos

#Removing the NA drives (not sure why they're NAs but it's only a small sample)
field_pos %>% filter(!is.na(starting_field)) -> field_pos



###  Using aggregated field position data from past 25 NFL seasons (AKA without a seasonal constant)

punter_data_aggregate <- function(pbpxx, year) {
  
  pbpxx %>% filter(play_type == "punt" & punt_blocked == 0 &
                     safety == 0) -> pbp_punts
  
  #Finding out avg yds per punt for each yard line
  pbp_punts %>%
    group_by(yardline_100) %>%
    dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                     count = n()) -> punt_data
  
  pbp_punts %>% mutate(exp_dist = 0,
                       obs_ydline = 100 - (yardline_100 - kick_distance) - 20 * touchback,
                       exp_ydline = 0,
                       obs_pts = 0,
                       exp_pts = 0) -> pbp_punts
  
  for (i in 1:nrow(pbp_punts)) {
    if(pbp_punts$obs_ydline[[i]] >= 100) {
      pbp_punts$obs_ydline[[i]] <- 80 #For punts returned from the end zone (very rare)
    }
    pbp_punts$exp_dist[[i]] <- punt_data$avg_dist[which(punt_data$yardline_100 == pbp_punts$yardline_100[[i]])]
    pbp_punts$exp_ydline[[i]] <- round(100 - (pbp_punts$yardline_100[[i]] - pbp_punts$exp_dist[[i]]))
    pbp_punts$obs_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$obs_ydline[[i]])]
    pbp_punts$exp_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$exp_ydline[[i]])]
  }
  
  pbp_punts %>% mutate(net_pts = exp_pts - obs_pts) -> pbp_punts
  
  #Group by punter
  pbp_punts %>%
    group_by(punter_player_id) %>%
    dplyr::summarize(player = first(punter_player_name),
                     season = year,
                     punts = n(),
                     avg_kick_dist = mean(kick_distance),
                     long_kick = max(kick_distance),
                     touchbacks = sum(touchback),
                     PAA = sum(net_pts),
                     avg_PAA = mean(net_pts)) -> punter_data
}

#Punter data for each season
punter_data_agg99 <- punter_data_aggregate(pbp99, 1999)
punter_data_agg00 <- punter_data_aggregate(pbp00, 2000)
punter_data_agg01 <- punter_data_aggregate(pbp01, 2001)
punter_data_agg02 <- punter_data_aggregate(pbp02, 2002)
punter_data_agg03 <- punter_data_aggregate(pbp03, 2003)
punter_data_agg04 <- punter_data_aggregate(pbp04, 2004)
punter_data_agg05 <- punter_data_aggregate(pbp05, 2005)
punter_data_agg06 <- punter_data_aggregate(pbp06, 2006)
punter_data_agg07 <- punter_data_aggregate(pbp07, 2007)
punter_data_agg08 <- punter_data_aggregate(pbp08, 2008)
punter_data_agg09 <- punter_data_aggregate(pbp09, 2009)
punter_data_agg10 <- punter_data_aggregate(pbp10, 2010)
punter_data_agg11 <- punter_data_aggregate(pbp11, 2011)
punter_data_agg12 <- punter_data_aggregate(pbp12, 2012)
punter_data_agg13 <- punter_data_aggregate(pbp13, 2013)
punter_data_agg14 <- punter_data_aggregate(pbp14, 2014)
punter_data_agg15 <- punter_data_aggregate(pbp15, 2015)
punter_data_agg16 <- punter_data_aggregate(pbp16, 2016)
punter_data_agg17 <- punter_data_aggregate(pbp17, 2017)
punter_data_agg18 <- punter_data_aggregate(pbp18, 2018)
punter_data_agg19 <- punter_data_aggregate(pbp19, 2019)
punter_data_agg20 <- punter_data_aggregate(pbp20, 2020)
punter_data_agg21 <- punter_data_aggregate(pbp21, 2021)
punter_data_agg22 <- punter_data_aggregate(pbp22, 2022)
punter_data_agg23 <- punter_data_aggregate(pbp23, 2023)

#Punter data for every season
punter_data_agg <- rbind(punter_data_agg99, punter_data_agg00,
                         punter_data_agg01, punter_data_agg02,
                         punter_data_agg03, punter_data_agg04,
                         punter_data_agg05, punter_data_agg06,
                         punter_data_agg07, punter_data_agg08,
                         punter_data_agg09, punter_data_agg10,
                         punter_data_agg11, punter_data_agg12,
                         punter_data_agg13, punter_data_agg14,
                         punter_data_agg15, punter_data_agg16,
                         punter_data_agg17, punter_data_agg18,
                         punter_data_agg19, punter_data_agg20,
                         punter_data_agg21, punter_data_agg22,
                         punter_data_agg23)

#write.csv(punter_data_agg, "punter_data_wo_seasonal.csv", row.names = FALSE)

#Creating total punter stats w/o seasonal adjustments
punter_data_agg %>%
  group_by(punter_player_id) %>%
  dplyr::summarize(player = first(player),
                   first_season = min(season),
                   last_season = max(season),
                   total_punts = sum(punts),
                   avg_kick_dist = weighted.mean(avg_kick_dist, punts),
                   long_kick = max(long_kick),
                   touchbacks = sum(touchbacks),
                   total_PAA = sum(PAA),
                   avg_PAA_season = mean(avg_PAA),
                   avg_PAA_punt = weighted.mean(avg_PAA, punts),
                   high_PAA = max(PAA)) -> punter_total_agg

#write.csv(punter_total_agg, "punter_total_wo_seasonal.csv", row.names = FALSE)

#Correlation between kick distance and points gained
cor(punter_data_agg$avg_kick_dist, punter_data_agg$avg_PAA) #0.2525592
cor(punter_data_agg$avg_kick_dist, punter_data_agg$PAA) #0.294802
cor(punter_total_agg$avg_kick_dist, punter_total_agg$avg_PAA_punt) #0.1879161
cor(punter_total_agg$avg_kick_dist, punter_total_agg$total_PAA) #0.1579866



###  Using field position data from from each of the 25 NFL seasons (AKA with a seasonal constant)

field_pos_func <- function(pbp) {
  
  pbp %>% mutate(play_pts = 0) -> pbp
  
  for (i in 1:nrow(pbp)) {
    if (pbp$field_goal_result[[i]] == "made" & !is.na(pbp$field_goal_result[[i]])) {
      pbp$play_pts[[i]] <- 3
    }
    if (pbp$extra_point_result[[i]] == "good" & !is.na(pbp$extra_point_result[[i]])) {
      pbp$play_pts[[i]] <- 1
    }
    if (pbp$two_point_conv_result[[i]] == "success" & !is.na(pbp$two_point_conv_result[[i]])) {
      pbp$play_pts[[i]] <- 2
    }
    if (pbp$safety[[i]] == 1 & !is.na(pbp$safety[[i]])) {
      pbp$play_pts[[i]] <- 2
    }
    if (pbp$touchdown[[i]] == 1 & !is.na(pbp$touchdown[[i]])) {
      pbp$play_pts[[i]] <- 6
    }
  }
  
  #Removing kickoffs and timeouts from play-by-play data
  pbp %>%
    filter(play_type != "kickoff") %>%
    filter(!is.na(ydsnet)) -> pbp
  
  #Finding out starting and ending field position for each drive
  pbp %>%
    group_by(game_id, drive, posteam) %>%
    dplyr::summarize(starting_field = first(yardline_100),
                     total_yds = first(ydsnet),
                     drive_pts = sum(play_pts)) -> drive_data
  
  #Summarizing drive data for each starting field position
  drive_data %>%
    group_by(starting_field) %>%
    dplyr::summarize(avg_yds = mean(total_yds),
                     avg_pts = mean(drive_pts),
                     count = n()) -> field_pos
  
  #Removing the NA drives (not sure why they're NAs but it's only a small sample)
  field_pos %>% filter(!is.na(starting_field)) -> field_pos
}

field_pos99 <- field_pos_func(pbp99)
field_pos00 <- field_pos_func(pbp00)
field_pos01 <- field_pos_func(pbp01)
field_pos02 <- field_pos_func(pbp02)
field_pos03 <- field_pos_func(pbp03)
field_pos04 <- field_pos_func(pbp04)
field_pos05 <- field_pos_func(pbp05)
field_pos06 <- field_pos_func(pbp06)
field_pos07 <- field_pos_func(pbp07)
field_pos08 <- field_pos_func(pbp08)
field_pos09 <- field_pos_func(pbp09)
field_pos10 <- field_pos_func(pbp10)
field_pos11 <- field_pos_func(pbp11)
field_pos12 <- field_pos_func(pbp12)
field_pos13 <- field_pos_func(pbp13)
field_pos14 <- field_pos_func(pbp14)
field_pos15 <- field_pos_func(pbp15)
field_pos16 <- field_pos_func(pbp16)
field_pos17 <- field_pos_func(pbp17)
field_pos18 <- field_pos_func(pbp18)
field_pos19 <- field_pos_func(pbp19)
field_pos20 <- field_pos_func(pbp20)
field_pos21 <- field_pos_func(pbp21)
field_pos22 <- field_pos_func(pbp22)
field_pos23 <- field_pos_func(pbp23)

punter_data_seasonal <- function(pbp, field_pos, year) {
  
  pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                   safety == 0) -> pbp_punts
  
  #Finding out avg yds per punt for each yard line
  pbp_punts %>%
    group_by(yardline_100) %>%
    dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                     count = n()) -> punt_data
  
  pbp_punts %>% mutate(exp_dist = 0,
                       obs_ydline = 100 - (yardline_100 - kick_distance) - 20 * touchback,
                       exp_ydline = 0,
                       obs_pts = 0,
                       exp_pts = 0) -> pbp_punts
  
  for (i in 1:nrow(pbp_punts)) {
    if(pbp_punts$obs_ydline[[i]] >= 100) {
      pbp_punts$obs_ydline[[i]] <- 80 #For punts returned from the end zone (very rare)
    }
    pbp_punts$exp_dist[[i]] <- punt_data$avg_dist[which(punt_data$yardline_100 == pbp_punts$yardline_100[[i]])]
    pbp_punts$exp_ydline[[i]] <- round(100 - (pbp_punts$yardline_100[[i]] - pbp_punts$exp_dist[[i]]))
    pbp_punts$obs_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$obs_ydline[[i]])]
    pbp_punts$exp_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$exp_ydline[[i]])]
  }
  
  pbp_punts %>% mutate(net_pts = exp_pts - obs_pts) -> pbp_punts
  
  #Group by punter
  pbp_punts %>%
    group_by(punter_player_id) %>%
    dplyr::summarize(player = first(punter_player_name),
                     season = year,
                     punts = n(),
                     avg_kick_dist = mean(kick_distance),
                     long_kick = max(kick_distance),
                     touchbacks = sum(touchback),
                     PAA = sum(net_pts),
                     avg_PAA = mean(net_pts)) -> punter_data
}

#Punter data for each season
punter_data_seasonal99 <- punter_data_seasonal(pbp99, field_pos99, 1999)
punter_data_seasonal00 <- punter_data_seasonal(pbp00, field_pos00, 2000)
punter_data_seasonal01 <- punter_data_seasonal(pbp01, field_pos01, 2001)
punter_data_seasonal02 <- punter_data_seasonal(pbp02, field_pos02, 2002)
punter_data_seasonal03 <- punter_data_seasonal(pbp03, field_pos03, 2003)
punter_data_seasonal04 <- punter_data_seasonal(pbp04, field_pos04, 2004)
punter_data_seasonal05 <- punter_data_seasonal(pbp05, field_pos05, 2005)
punter_data_seasonal06 <- punter_data_seasonal(pbp06, field_pos06, 2006)
punter_data_seasonal07 <- punter_data_seasonal(pbp07, field_pos07, 2007)
punter_data_seasonal08 <- punter_data_seasonal(pbp08, field_pos08, 2008)
punter_data_seasonal09 <- punter_data_seasonal(pbp09, field_pos09, 2009)
punter_data_seasonal10 <- punter_data_seasonal(pbp10, field_pos10, 2010)
punter_data_seasonal11 <- punter_data_seasonal(pbp11, field_pos11, 2011)
punter_data_seasonal12 <- punter_data_seasonal(pbp12, field_pos12, 2012)
punter_data_seasonal13 <- punter_data_seasonal(pbp13, field_pos13, 2013)
punter_data_seasonal14 <- punter_data_seasonal(pbp14, field_pos14, 2014)
punter_data_seasonal15 <- punter_data_seasonal(pbp15, field_pos15, 2015)
punter_data_seasonal16 <- punter_data_seasonal(pbp16, field_pos16, 2016)
punter_data_seasonal17 <- punter_data_seasonal(pbp17, field_pos17, 2017)
punter_data_seasonal18 <- punter_data_seasonal(pbp18, field_pos18, 2018)
punter_data_seasonal19 <- punter_data_seasonal(pbp19, field_pos19, 2019)
punter_data_seasonal20 <- punter_data_seasonal(pbp20, field_pos20, 2020)
punter_data_seasonal21 <- punter_data_seasonal(pbp21, field_pos21, 2021)
punter_data_seasonal22 <- punter_data_seasonal(pbp22, field_pos22, 2022)
punter_data_seasonal23 <- punter_data_seasonal(pbp23, field_pos23, 2023)

#Punter data for every season
punter_data_seasonal <- rbind(punter_data_seasonal99, punter_data_seasonal00,
                     punter_data_seasonal01, punter_data_seasonal02,
                     punter_data_seasonal03, punter_data_seasonal04,
                     punter_data_seasonal05, punter_data_seasonal06,
                     punter_data_seasonal07, punter_data_seasonal08,
                     punter_data_seasonal09, punter_data_seasonal10,
                     punter_data_seasonal11, punter_data_seasonal12,
                     punter_data_seasonal13, punter_data_seasonal14,
                     punter_data_seasonal15, punter_data_seasonal16,
                     punter_data_seasonal17, punter_data_seasonal18,
                     punter_data_seasonal19, punter_data_seasonal20,
                     punter_data_seasonal21, punter_data_seasonal22,
                     punter_data_seasonal23)

#write.csv(punter_data_seasonal, "punter_data_seasonal.csv", row.names = FALSE)

#Creating total punter stats w/ seasonal adjustments
punter_data_seasonal %>%
  group_by(punter_player_id) %>%
  dplyr::summarize(player = first(player),
                   first_season = min(season),
                   last_season = max(season),
                   total_punts = sum(punts),
                   avg_kick_dist = weighted.mean(avg_kick_dist, punts),
                   long_kick = max(long_kick),
                   touchbacks = sum(touchbacks),
                   total_PAA = sum(PAA),
                   avg_PAA_season = mean(avg_PAA),
                   avg_PAA_punt = weighted.mean(avg_PAA, punts),
                   high_PAA = max(PAA)) -> punter_total_seasonal

#write.csv(punter_total_seasonal, "punter_agg_seasonal.csv", row.names = FALSE)

#Correlation between kick distance and points gained
cor(punter_data_seasonal$avg_kick_dist, punter_data_seasonal$avg_PAA) #0.2017573
cor(punter_data_seasonal$avg_kick_dist, punter_data_seasonal$PAA) #0.2483635
cor(punter_total_seasonal$avg_kick_dist, punter_total_seasonal$avg_PAA_punt) #0.1845165
cor(punter_total_seasonal$avg_kick_dist, punter_total_seasonal$total_PAA) #0.2581708



### Comparing PAA to PFF's punter grades

#PFF grades (CSV of PFF's punter grades downloaded from their website)
pff <- read.csv("pff_grades.csv")

#Comparing all PFF grades to points above average
merge(pff, punter_data_seasonal, by = c("player", "season")) -> pff_seasonal
pff_seasonal %>% filter(!is.na(grades_punter)) -> pff_seasonal

cor(pff_seasonal$PAA, pff_seasonal$grades_punter) #0.3569815, weak correlation
cor(pff_seasonal$avg_PAA, pff_seasonal$grades_punter) #0.1571558
cor(pff_seasonal$avg_kick_dist, pff_seasonal$grades_punter) #0.3255408

#Checking rank correlation for seasonal adjustments
cor(pff_seasonal$PAA, pff_seasonal$grades_punter,
    method = "spearman") #0.3518032, the same
cor(pff_seasonal$avg_PAA, pff_seasonal$grades_punter,
    method = "spearman") #0.3185076, a lot better

#Without seasonal adjustments
merge(pff, punter_data_agg,
      by = c("player", "season")) -> pff_wo_seasonal
pff_wo_seasonal %>% filter(!is.na(grades_punter)) -> pff_wo_seasonal

cor(pff_wo_seasonal$PAA,
    pff_wo_seasonal$grades_punter) #0.4825416, once again better than seasonal???
cor(pff_wo_seasonal$avg_PAA,
    pff_wo_seasonal$grades_punter) #0.3947054

#Checking rank correlation for no seasonal adjustments
cor(pff_wo_seasonal$PAA, pff_wo_seasonal$grades_punter,
    method = "spearman") #0.4538026, slightly worse
cor(pff_wo_seasonal$avg_PAA, pff_wo_seasonal$grades_punter,
    method = "spearman") #0.4678699, better??

#Figure 4
ggplot(pff_wo_seasonal, aes(PAA, grades_punter)) + geom_point() +
  labs(y = "Punter Grade") +
  ggtitle("PAA without Seasonal Adjustments")

#Figure 3
ggplot(pff_seasonal, aes(PAA, grades_punter)) + geom_point() +
  labs(y = "Punter Grade") +
  ggtitle("PAA with Seasonal Adjustments")


#ggpairs for PFF data (GGally package)
pff %>% filter(attempts >= 10) -> pff10
ggpairs(pff10[, c(6, 8, 9, 11, 17, 19)])

cor(pff10[, c(6, 8, 9, 11, 17, 19)]) -> cor_matrix

#Corrplot package
corrplot(cor(pff10[, c(6, 8, 9, 11, 17, 19)]))
#PFF grades appear to be moderately correlated with hangtime

cor(pff10$grades_punter, pff10$average_hangtime) #0.6190872
cor(pff10$grades_punter, pff10$average_net_yards) #0.4827723

ggplot(pff10, aes(average_hangtime, grades_punter)) +
  geom_point()


#Seeing if any specific season has a good correlation (w/ seasonal adjustments)
pff_seasonal %>% filter(attempts >= 10) -> pff_seasonal_sample
pff_wo_seasonal %>% filter(attempts >= 10) -> pff_wo_sample

pff_seasonal_sample %>%
  group_by(season) %>%
  dplyr::summarize(total_cor = cor(grades_punter, PAA),
                   avg_cor = cor(grades_punter, avg_PAA),
                   adj = "With") -> season_cors
#2013 had the best correlation with 0.5763533 for total and 0.5812799 for average

#Seeing if any specific season has a good correlation (w/o seasonal adjustments)
pff_wo_sample %>%
  group_by(season) %>%
  dplyr::summarize(total_cor = cor(grades_punter, PAA),
                   avg_cor = cor(grades_punter, avg_PAA),
                   adj = "Without") -> wo_season_cors
#2016 had the best total correlation with 0.6454418
#2023 had the best average correlation with 0.6514230

#Line chart showing correlations over time
ggplot(rbind(season_cors, wo_season_cors),
       aes(season, total_cor, group = adj,
           color = adj, fill = adj)) +
  geom_line() +
  labs(x = "Season", y = "Correlation", color = "Seasonal Adjustment") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))


#Didn't even think about this before, but comparing PAA w/ and w/o seasonal adjustments
cor(punter_data_seasonal$PAA, punter_data_agg$PAA) #0.6214692
cor(punter_data_seasonal$avg_PAA,
    punter_data_agg$avg_PAA) #0.5038972


#Average punt distance over time
pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                 safety == 0) -> pbp_punts
pbp_punts %>%
  group_by(season) %>%
  dplyr::summarize(avg_kick_dist = mean(kick_distance)) -> punt_dist

ggplot(punt_dist, aes(season, avg_kick_dist)) + geom_line() +
  labs(x = "Year", y = "Avg Punt Distance (yds)") +
  ggtitle("Average NFL Punt Distance (1999-2023)")

#Avg punt distance
pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                 safety == 0) -> pbp_punts

#Finding out avg yds per punt for each yard line
pbp_punts %>%
  group_by(yardline_100) %>%
  dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                   count = n()) -> punt_data

ggplot(punt_data, aes(yardline_100, avg_dist)) + geom_point() +
  labs(x = "Line of Scrimmage", y = "Avg Distance (yds)") +
  ggtitle("Average Punt Distance by Field Position (1999-2023 seasons)")

cor(punt_data$yardline_100, punt_data$avg_dist) #0.8947715, honestly higher than I thought