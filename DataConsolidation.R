library(nflfastR)
library(tidyverse)
library(neuralnet)
library(lubridate)
games <- readRDS(url("http://www.habitatring.com/games.rds"))
weekly_team_stats <- read_csv("data/weekly_team_stats.csv")
dvoa_all_weighted <- read_csv("data/dvoa_all_weighted.csv")
games <- games %>% 
  filter(season>=2002)

games <- games %>% 
  mutate(temp = case_when(is.na(temp)==FALSE~temp, is.na(temp)==TRUE~67)) %>% 
  mutate(wind = case_when(is.na(wind)==FALSE~wind, is.na(wind)==TRUE~0))

games <- games %>% 
  mutate(home_team = case_when(home_team!="SD"~home_team, home_team=="SD"~"LAC")) %>% 
  mutate(home_team = case_when(home_team!="STL"~home_team, home_team=="STL"~"LA")) %>%
  mutate(home_team = case_when(home_team!="OAK"~home_team, home_team=="OAK"~"LV")) %>% 
  mutate(away_team = case_when(away_team!="SD"~away_team, away_team=="SD"~"LAC")) %>% 
  mutate(away_team = case_when(away_team!="STL"~away_team, away_team=="STL"~"LA")) %>%
  mutate(away_team = case_when(away_team!="OAK"~away_team, away_team=="OAK"~"LV"))
games <- games %>% 
  arrange(season,week,home_team) %>% 
  filter(game_type == "REG")

weekly_team_stats <- weekly_team_stats %>% 
  arrange(season,week,team)
dvoa_all_weighted <- dvoa_all_weighted %>% 
  arrange(season,week,team)

dvoa_all_weighted <- dvoa_all_weighted %>% 
  mutate(week = week +1) %>% 
  filter(week != 18)
weekly_team_stats <- weekly_team_stats %>% 
  mutate(week = week +1) %>% 
  filter(week != 18)
games <- games %>% 
  filter(week != 1)

#add team metrics to training data
count <- 1
for (row1 in 1:nrow(games)){
  #query wts df for proper stats
  temp.h <- weekly_team_stats[(weekly_team_stats$team == games$home_team[row1] &
                                 weekly_team_stats$week == games$week[row1] &
                                 weekly_team_stats$season == games$season[row1]),]
  dvoa.h <- dvoa_all_weighted[(dvoa_all_weighted$team == games$home_team[row1] &
                               dvoa_all_weighted$week == games$week[row1] &
                               dvoa_all_weighted$season == games$season[row1]),]
  #fill into games df
  games[row1, "home_off_pass_yd_att"] = temp.h$off_pass_yd_att
  games[row1, "home_off_pass_att_game"] = temp.h$off_pass_att_game
  games[row1, "home_def_pass_yd_att"] = temp.h$def_pass_yd_att
  games[row1, "home_def_pass_att_game"] = temp.h$def_pass_att_game
  games[row1, "home_off_rush_yd_att"] = temp.h$off_rush_yd_att
  games[row1, "home_off_rush_att_game"] = temp.h$off_rush_att_game
  games[row1, "home_def_rush_yd_att"] = temp.h$def_rush_yd_att
  games[row1, "home_def_rush_att_game"] = temp.h$def_rush_att_game
  games[row1, "home_off_epa"] = temp.h$off_epa_play
  games[row1, "home_def_epa"] = temp.h$def_epa_play
  games[row1, "home_off_drive_success_rate"] = temp.h$off_drive_success_rate
  games[row1, "home_def_drive_success_rate"] = temp.h$def_drive_success_rate
  games[row1, "home_off_rz_td_rate"] = temp.h$off_rz_td_rate
  games[row1, "home_def_rz_td_rate"] = temp.h$def_rz_td_rate
  games[row1, "home_off_int_rate"] = temp.h$off_int_rate
  games[row1, "home_off_fumble_rate"] = temp.h$off_fumble_rate
  games[row1, "home_def_int_rate"] = temp.h$def_int_rate
  games[row1, "home_def_fumble_rate"] = temp.h$def_fumble_rate
  
  
  
  games[row1, "home_tot_dvoa"] = dvoa.h$tot_dvoa
  games[row1, "home_tot_dvoa_weighted"] = dvoa.h$tot_dvoa_weighted
  games[row1, "home_off_dvoa"] = dvoa.h$off_dvoa
  games[row1, "home_off_dvoa_weighted"] = dvoa.h$off_dvoa_weighted
  games[row1, "home_def_dvoa"] = dvoa.h$def_dvoa
  games[row1, "home_def_dvoa_weighted"] = dvoa.h$def_dvoa_weighted
  
  
  #do same for away
  temp.a <- weekly_team_stats[(weekly_team_stats$team == games$away_team[row1] &
                                 weekly_team_stats$week == games$week[row1] &
                                 weekly_team_stats$season == games$season[row1]),]
  dvoa.a <- dvoa_all_weighted[(dvoa_all_weighted$team == games$away_team[row1] &
                                 dvoa_all_weighted$week == games$week[row1] &
                                 dvoa_all_weighted$season == games$season[row1]),]
  
  games[row1, "away_off_pass_yd_att"] = temp.a$off_pass_yd_att
  games[row1, "away_off_pass_att_game"] = temp.a$off_pass_att_game
  games[row1, "away_def_pass_yd_att"] = temp.a$def_pass_yd_att
  games[row1, "away_def_pass_att_game"] = temp.a$def_pass_att_game
  games[row1, "away_off_rush_yd_att"] = temp.a$off_rush_yd_att
  games[row1, "away_off_rush_att_game"] = temp.a$off_rush_att_game
  games[row1, "away_def_rush_yd_att"] = temp.a$def_rush_yd_att
  games[row1, "away_def_rush_att_game"] = temp.a$def_rush_att_game
  games[row1, "away_off_epa"] = temp.a$off_epa_play
  games[row1, "away_def_epa"] = temp.a$def_epa_play
  games[row1, "away_off_drive_success_rate"] = temp.a$off_drive_success_rate
  games[row1, "away_def_drive_success_rate"] = temp.a$def_drive_success_rate
  games[row1, "away_off_rz_td_rate"] = temp.a$off_rz_td_rate
  games[row1, "away_def_rz_td_rate"] = temp.a$def_rz_td_rate
  games[row1, "away_off_int_rate"] = temp.a$off_int_rate
  games[row1, "away_off_fumble_rate"] = temp.a$off_fumble_rate
  games[row1, "away_def_int_rate"] = temp.a$def_int_rate
  games[row1, "away_def_fumble_rate"] = temp.a$def_fumble_rate
  
  games[row1, "away_tot_dvoa"] = dvoa.a$tot_dvoa
  games[row1, "away_tot_dvoa_weighted"] = dvoa.a$tot_dvoa_weighted
  games[row1, "away_off_dvoa"] = dvoa.a$off_dvoa
  games[row1, "away_off_dvoa_weighted"] = dvoa.a$off_dvoa_weighted
  games[row1, "away_def_dvoa"] = dvoa.a$def_dvoa
  games[row1, "away_def_dvoa_weighted"] = dvoa.a$def_dvoa_weighted
  
  count <- count+1
  print(count)
}

write_csv(games, "data/train_and_test.csv")
