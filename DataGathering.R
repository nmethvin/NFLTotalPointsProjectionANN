library(nflfastR)
library(tidyverse)
library(neuralnet)
library(lubridate)
#vector to grab pbp for selected seasons
seasons <- c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,
             2013, 2014,2015,2016,2017,2018,2019,2020)


#nflfastR function to get pbp
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})


#Take the pbp data and get list of unique teams for argument.
teams <- pbp %>% 
  select(posteam)
teams <- unique(teams)
teams <- teams %>% 
  arrange(posteam)
teams <- teams %>% 
  na.omit()
teams <- teams[-1,]
teams <- as.vector(t(teams))

#used to look at 2001 schedule
scheduleCheck <- pbp %>% 
  select(week,season,game_date,season_type) %>% 
  filter(year(game_date)==2001,season_type == "REG")
scheduleCheck <- unique(scheduleCheck$week)
#1999-2001 seasons had to be excluded because the Texans weren't a team until 2002
#and the 2001 season was missing a week



pbp <- pbp %>% 
  filter(season_type == "REG")

#
#
#
#identify what'swrong with specific game (TBvsMIA in week 1 2017 got postponed)
# pbp.test <- pbp %>% 
#   filter(posteam %in% "TB", year(game_date) %in% 2017, week %in% 1) 
# test_opy <- pbp.test %>% 
#   filter(play_type == "pass") %>% 
#   summarize(opy = sum(yards_gained))
# test_opy <- test_opy$opy
# test.att <- pbp.test %>% 
#   filter(play_type == "pass") %>%
#   count(play_type)
# test.att <- test.att$n
# test.opy_att <- test_opy/test.att
# is_empty(test.opy_att)
#
#
#
  
#opy/a for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
opy_att_team <- c()
opy_att_value <- c()
opy_att_week <- c()
pass_att_game <- c()
season <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      pbp.team <- pbp.week %>% 
        filter(posteam %in% team)
      print(team)
      tot_opy <- pbp.team %>% 
        filter(play_type == "pass") %>% 
        summarize(opy = sum(yards_gained))
      tot_opy <- tot_opy$opy
      att <- pbp.team %>% 
        filter(play_type == "pass") %>%
        count(play_type)
      att <- att$n
      pass_att <- att/x
      opy_att <- tot_opy/att
      if(is_empty(opy_att)==TRUE){
        opy_att <- 0 
        pass_att <- 0}
      opy_att_team <- c(opy_att_team, team)
      opy_att_value <- c(opy_att_value, opy_att)
      opy_att_week <- c(opy_att_week, x)
      season <- c(season, sea)
      pass_att_game <- c(pass_att_game, pass_att)
      print(count)
      count <- count+1
    }
  }
}
#dpy/a for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
dpy_att_team <- c()
dpy_att_value <- c()
dpy_att_week <- c()
def_pass_att_game <- c()
season <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      pbp.team <- pbp.week %>% 
        filter(defteam %in% team)
      print(team)
      tot_dpy <- pbp.team %>% 
        filter(play_type == "pass") %>% 
        summarize(dpy = sum(yards_gained))
      tot_dpy <- tot_dpy$dpy
      att <- pbp.team %>% 
        filter(play_type == "pass") %>%
        count(play_type)
      att <- att$n
      def_pass_att <- att/x
      dpy_att <- tot_dpy/att
      if(is_empty(dpy_att)==TRUE){
        dpy_att <- 0
        def_pass_att <- 0}
      dpy_att_team <- c(dpy_att_team, team)
      dpy_att_value <- c(dpy_att_value, dpy_att)
      dpy_att_week <- c(dpy_att_week, x)
      def_pass_att_game <- c(def_pass_att_game,def_pass_att)
      season <- c(season, sea)
      print(count)
      count <- count+1
    }
  }
}
#offensive rushing yards per attempt for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ory_att_team <- c()
ory_att_value <- c()
ory_att_week <- c()
rush_att_game <- c()
season <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      pbp.team <- pbp.week %>% 
        filter(posteam %in% team)
      print(team)
      tot_ory <- pbp.team %>% 
        filter(play_type == "run") %>% 
        summarize(ory = sum(yards_gained))
      tot_ory <- tot_ory$ory
      att <- pbp.team %>% 
        filter(play_type == "run") %>%
        count(play_type)
      att <- att$n
      rush_att <- att/x
      ory_att <- tot_ory/att
      if(is_empty(ory_att)==TRUE){
        ory_att <- 0
        rush_att <- 0}
      ory_att_team <- c(ory_att_team, team)
      ory_att_value <- c(ory_att_value, ory_att)
      ory_att_week <- c(ory_att_week, x)
      rush_att_game <- c(rush_att_game, rush_att)
      season <- c(season, sea)
      print(count)
      count <- count+1
    }
  }
}
opy_att_team<- opy_att_team %>% na.omit()

#defensive rushing yards per attempt for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
dry_att_team <- c()
dry_att_value <- c()
dry_att_week <- c()
def_rush_att_game <- c()
season <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      pbp.team <- pbp.week %>% 
        filter(defteam %in% team)
      print(team)
      tot_dry <- pbp.team %>% 
        filter(play_type == "run") %>% 
        summarize(dry = sum(yards_gained))
      tot_dry <- tot_dry$dry
      att <- pbp.team %>% 
        filter(play_type == "run") %>%
        count(play_type)
      att <- att$n
      def_rush_att <- att/x
      dry_att <- tot_dry/att
      if(is_empty(dry_att)==TRUE){
        dry_att <- 0
        def_rush_att <- 0}
      dry_att_team <- c(dry_att_team, team)
      dry_att_value <- c(dry_att_value, dry_att)
      dry_att_week <- c(dry_att_week, x)
      def_rush_att_game <- c(def_rush_att_game, def_rush_att)
      season <- c(season, sea)
      print(count)
      count <- count+1
    }
  }
}



#offensive epa for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
epa_play_team <- c()
epa_play_value <- c()
epa_play_week <- c()
#count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      off_epa <- pbp.week %>% 
        filter(posteam %in% team, pass == 1| rush ==1, !is.na(epa)) %>%
        summarize(epa = mean(epa))
      epa <- off_epa$epa 
      
      epa_play_team <- c(epa_play_team, team)
      epa_play_value <- c(epa_play_value, epa)
      epa_play_week <- c(epa_play_week, x)
      print(count)
      count <- count+1
    }
  }
}



#defensive epa for each team for each week before the given week
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
def_epa_play_team <- c()
def_epa_play_value <- c()
def_epa_play_week <- c()
#count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      def_epa <- pbp.week %>% 
        filter(defteam %in% team, pass == 1| rush ==1, !is.na(epa)) %>%
        summarize(epa = mean(epa))
      epa <- def_epa$epa 
      
      def_epa_play_team <- c(def_epa_play_team, team)
      def_epa_play_value <- c(def_epa_play_value, epa)
      def_epa_play_week <- c(def_epa_play_week, x)
      print(count)
      count <- count+1
    }
  }
}
#calculate offensive drive success rate
drive_success_rate_value <- c()
team_dsr <- c()
week_dsr <- c()
season_dsr <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.drive <- pbp.week %>% 
        filter(posteam == team, down==1) %>% 
        select(series_result) %>% 
        group_by(series_result) %>% 
        count()
      drive_success_rate <- (pbp.drive[(pbp.drive$series_result=="First down"),]$n+pbp.drive[(pbp.drive$series_result=="Touchdown"),]$n)/sum(pbp.drive$n)
      drive_success_rate <- drive_success_rate[1]
      print(drive_success_rate)
      drive_success_rate_value <- c(drive_success_rate_value, drive_success_rate)
      team_dsr <- c(team_dsr, team)
      week_dsr <- c(week_dsr, x)
      season_dsr <- c(season_dsr, sea)
      print(count)
      count <- count+1
    }
  }
}

#calculate defensive drive success rate
def_drive_success_rate_value <- c()
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.drive <- pbp.week %>% 
        filter(defteam == team, down==1) %>% 
        select(series_result) %>% 
        group_by(series_result) %>% 
        count()
      def_drive_success_rate <- (pbp.drive[(pbp.drive$series_result=="First down"),]$n+pbp.drive[(pbp.drive$series_result=="Touchdown"),]$n)/sum(pbp.drive$n)
      def_drive_success_rate <- def_drive_success_rate[1]
      print(def_drive_success_rate)
      def_drive_success_rate_value <- c(def_drive_success_rate_value, def_drive_success_rate)
      # team_dsr <- c(team_dsr, team)
      # week_dsr <- c(week_dsr, x)
      # season_dsr <- c(season_dsr, sea)
      print(count)
      count <- count+1
    }
  }
}

#calculate offensive redzone efficiency
off_rz_td_rate_value <- c()
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.team <- pbp.week %>% 
        filter(posteam == team)
      pbp.drive <- pbp.team %>% 
        filter(down==1, yardline_100<=20) %>% 
        select(fixed_drive_result) %>% 
        group_by(fixed_drive_result) %>% 
        count()
      off_rz_td_rate <- (pbp.drive[(pbp.drive$fixed_drive_result=="Touchdown"),]$n)/sum(pbp.drive$n)
      off_rz_td_rate <- off_rz_td_rate[1]
      if(is.na(off_rz_td_rate)==TRUE){
        off_rz_td_rate <- 0
      }
      off_rz_td_rate_value <- c(off_rz_td_rate_value, off_rz_td_rate)
    }
  }
}
#calculate defensive redzone efficiency
def_rz_td_rate_value <- c()
team_drz <- c()
week_drz <- c()
season_drz <- c()
count <- 1
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.team <- pbp.week %>% 
        filter(defteam == team)
      pbp.drive <- pbp.team %>% 
        filter(down==1, yardline_100<=20) %>% 
        select(fixed_drive_result) %>% 
        group_by(fixed_drive_result) %>% 
        count()
      def_rz_td_rate <- (pbp.drive[(pbp.drive$fixed_drive_result=="Touchdown"),]$n)/sum(pbp.drive$n)
      def_rz_td_rate <- def_rz_td_rate[1]
      if(is.na(def_rz_td_rate)==TRUE){
        def_rz_td_rate <- 0
      }
      def_rz_td_rate_value <- c(def_rz_td_rate_value, def_rz_td_rate[1])
    }
  }
}
#calculate offensive int and fumble rates
off_fumble_rate_value <- c()
off_int_rate_value <- c()
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.team <- pbp.week %>% 
        filter(posteam == team, pass == 1| rush ==1) %>% 
        select(interception,fumble) %>%
        group_by(interception,fumble) %>% 
        count()
      
      off_fumble_rate <- (pbp.team[(pbp.team$fumble==1),]$n)/sum(pbp.team$n)
      off_fumble_rate <- off_fumble_rate[1]
      if(is.na(off_fumble_rate)==TRUE){
        off_fumble_rate <- 0
      }
      off_int_rate <- (pbp.team[(pbp.team$interception==1),]$n)/sum(pbp.team$n)
      off_int_rate <- off_int_rate[1]
      if(is.na(off_int_rate)==TRUE){
        off_int_rate <- 0
      }
      off_fumble_rate_value <- c(off_fumble_rate_value, off_fumble_rate)
      off_int_rate_value <- c(off_int_rate_value, off_int_rate)
    }
  }
}
#calculate defensive int and fumble rates
def_fumble_rate_value <- c()
def_int_rate_value <- c()
for (sea in seasons){
  pbp.sea <- pbp %>% 
    filter(year(game_date) %in% sea)
  for (x in weeks){
    pbp.week <- pbp.sea %>% 
      filter(week <= x)
    for (team in teams){
      print(team)
      pbp.team <- pbp.week %>% 
        filter(defteam == team, pass == 1| rush ==1) %>% 
        select(interception,fumble) %>%
        group_by(interception,fumble) %>% 
        count()
      
      def_fumble_rate <- (pbp.team[(pbp.team$fumble==1),]$n)/sum(pbp.team$n)
      def_fumble_rate <- def_fumble_rate[1]
      if(is.na(def_fumble_rate)==TRUE){
        def_fumble_rate <- 0
      }
      def_int_rate <- (pbp.team[(pbp.team$interception==1),]$n)/sum(pbp.team$n)
      def_int_rate <- def_int_rate[1]
      if(is.na(def_int_rate)==TRUE){
        def_int_rate <- 0
      }
      def_fumble_rate_value <- c(def_fumble_rate_value, def_fumble_rate)
      def_int_rate_value <- c(def_int_rate_value, def_int_rate)
    }
  }
}

#add all epa to tibble
weekly_team_stats <- tibble(
  team = opy_att_team,
  week = opy_att_week,
  off_pass_yd_att = opy_att_value,
  off_pass_att_game = pass_att_game,
  def_pass_yd_att = dpy_att_value,
  def_pass_att_game = def_pass_att_game,
  off_rush_yd_att = ory_att_value,
  off_rush_att_game = rush_att_game,
  def_rush_yd_att = dry_att_value,
  def_rush_att_game = def_rush_att_game,
  off_epa_play = epa_play_value,
  def_epa_play = def_epa_play_value,
  off_drive_success_rate = drive_success_rate_value,
  def_drive_success_rate = def_drive_success_rate_value,
  off_rz_td_rate = off_rz_td_rate_value,
  def_rz_td_rate = def_rz_td_rate_value,
  off_int_rate = off_int_rate_value,
  off_fumble_rate = off_fumble_rate_value,
  def_int_rate = def_int_rate_value,
  def_fumble_rate = def_fumble_rate_value,
  season = season
)
weekly_team_stats_base <- weekly_team_stats

glimpse(weekly_team_stats)
drz.test <- as.data.frame(def_rz_td_rate_value)

weekly_team_stats %>% 
  filter(is_empty(home_team) == TRUE)

#establish base to go back to
weekly_team_stats <- weekly_team_stats_base

weekly.test <- weekly_team_stats %>%
  filter(is.na(off_drive_success_rate)==TRUE) %>% 
  group_by(off_drive_success_rate) %>% 
  count()


weekly_team_stats <- weekly_team_stats %>% 
  arrange(season,week,team)



temp <- weekly_team_stats[(weekly_team_stats$team == "KC" &
                             weekly_team_stats$week == 5 &
                             weekly_team_stats$season == 2019),]


#create csv file to save values
write_csv(weekly_team_stats, "data/weekly_team_stats.csv")
write_csv(games,"data/games.csv")





# 
# for (row1 in 1:nrow(games)){
#   for (row2 in 1:nrow(weekly_team_stats)){
#     if ((games[row1, "season"] == weekly_team_stats[row2, "season"])&
#         (games[row1, "week"] == weekly_team_stats[row2, "week"]-1)&
#         (games[row1, "home_team"] == weekly_team_stats[row2, "team"])){
#       games[row1, "home_opy_att"] = as.numeric(weekly_team_stats[row2, "opy_per_att"])
#       games[row1, "home_dpy_att"] = as.numeric(weekly_team_stats[row2, "dpy_per_att"])
#       games[row1, "home_off_epa"] = as.numeric(weekly_team_stats[row2, "off_epa_play"])
#       games[row1, "home_def_epa"] = as.numeric(weekly_team_stats[row2, "def_epa_play"])
#       
#       weekly_team_stats <- weekly_team_stats[-c(row2),]
#       print(" stats counted!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
#       break
#     }
#   }
#   print(count)
#   count <- count+1
# }
# 
# #weekly_team_stats <- weekly_team_stats_base
# #add away team metrics to training data
# count <- 1
# for (row1 in 1:nrow(games)){
#   for (row2 in 1:nrow(weekly_team_stats)){
#     if ((games[row1, "away_team"] == weekly_team_stats[row2, "team"])&
#         (games[row1, "week"] == weekly_team_stats[row2, "week"]-1)&
#         (games[row1, "season"] == weekly_team_stats[row2, "season"])){
#       games[row1, "away_opy_att"] = as.numeric(weekly_team_stats[row2, "opy_per_att"])
#       games[row1, "away_dpy_att"] = as.numeric(weekly_team_stats[row2, "dpy_per_att"])
#       games[row1, "away_off_epa"] = as.numeric(weekly_team_stats[row2, "off_epa_play"])
#       games[row1, "away_def_epa"] = as.numeric(weekly_team_stats[row2, "def_epa_play"])
#       
#       weekly_team_stats <- weekly_team_stats[-c(row2),]
#       print(" stats counted!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
#       break
#     }
#   }
#   print(count)
#   count <- count+1
# }
# 
# 














