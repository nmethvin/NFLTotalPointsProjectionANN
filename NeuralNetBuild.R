library(nflfastR)
library(tidyverse)
library(neuralnet)
library(lubridate)
#functions to scale data and unscale to get projections
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
unscale <- function(x,min,max){
  return ((x*(max-min))+min)
}




#read in stats to train and test neural network
stats <- read_csv("data/train_and_test.csv")

stats<- stats %>% 
  select(season, week, gametime, 
         away_team, home_team, total, 
         total_line, temp, wind, 
         div_game,home_off_pass_yd_att,
         home_off_pass_att_game,
         home_def_pass_yd_att,
         home_def_pass_att_game,
         home_off_rush_yd_att,
         home_off_rush_att_game,
         home_def_rush_yd_att,
         home_def_rush_att_game,
         away_off_pass_yd_att,
         away_off_pass_att_game,
         away_def_pass_yd_att,
         away_def_pass_att_game,
         away_off_rush_yd_att,
         away_off_rush_att_game,
         away_def_rush_yd_att,
         away_def_rush_att_game,
         home_off_drive_success_rate,
         home_def_drive_success_rate,
         home_off_rz_td_rate,
         home_def_rz_td_rate,
         home_off_int_rate,
         home_off_fumble_rate,
         home_def_int_rate,
         home_def_fumble_rate,
         away_off_drive_success_rate,
         away_def_drive_success_rate,
         away_off_rz_td_rate,
         away_def_rz_td_rate,
         away_off_int_rate,
         away_off_fumble_rate,
         away_def_int_rate,
         away_def_fumble_rate,
         home_off_epa,home_def_epa,
         away_off_epa,away_def_epa,
         home_tot_dvoa,home_tot_dvoa_weighted,
         home_off_dvoa,home_off_dvoa_weighted,
         home_def_dvoa,home_def_dvoa_weighted,
         away_tot_dvoa,away_tot_dvoa_weighted,
         away_off_dvoa,away_off_dvoa_weighted,
         away_def_dvoa,away_def_dvoa_weighted)
#add temp and wind variables for dome games
stats <- stats %>% 
  mutate(temp = case_when(is.na(temp)==FALSE~temp, is.na(temp)==TRUE~67)) %>% 
  mutate(wind = case_when(is.na(wind)==FALSE~wind, is.na(wind)==TRUE~0))
  

#optional code to convert total into total_result (binary) for 1 if over and 0 if under
  # games <- games %>%
  #   mutate(total_result = total-total_line)
  # games <- games %>%
  #   mutate(total_result = ifelse(total_result>0, 1, 0))
  # games <- games %>%
  #   select(-total_result)


#This block randomly selects 20% of the data for testing and 80% for training
dt = sort(sample(nrow(stats), nrow(stats)*.8))
train.data<-stats[dt,]
test.data<-stats[-dt,]

test.data <- test.data %>% 
  filter(is.na(total)!=TRUE)
train.data <- train.data %>% 
  filter(is.na(total)!=TRUE)

#Gets the minimum and maximum variables from test so that in can be unscaled later
min1 <- min(train.data$total)
max1 <- max(train.data$total)


train.data %>% 
  filter(is.na(total)==TRUE)



#remove identifying variables that NN cannot recognize
train <- train.data %>%
  select(-season, -week, -gametime, -away_team, -home_team) %>%  
  na.omit()

#Same for test data, but also remove total 
test.data <- test.data %>% na.omit()
test <- test.data %>%
  select(-season, -week, -gametime, -away_team, -home_team,-total) %>%
  na.omit()

#Normalize the data within train and test so that NN can handle better
train <- as.data.frame(lapply(train, normalize))
test <- as.data.frame(lapply(test, normalize))


#build neural network
nn=neuralnet(total~total_line+temp+wind+div_game+
               home_off_pass_yd_att+
               home_off_pass_att_game+
               home_def_pass_yd_att+
               home_def_pass_att_game+
               home_off_rush_yd_att+
               home_off_rush_att_game+
               home_def_rush_yd_att+
               home_def_rush_att_game+
               away_off_pass_yd_att+
               away_off_pass_att_game+
               away_def_pass_yd_att+
               away_def_pass_att_game+
               away_off_rush_yd_att+
               away_off_rush_att_game+
               away_def_rush_yd_att+
               away_def_rush_att_game+
               home_off_epa+home_def_epa+
               away_off_epa+away_def_epa+
               home_tot_dvoa+
               home_tot_dvoa_weighted+
               home_off_dvoa+
               home_off_dvoa_weighted+
               home_def_dvoa+
               home_def_dvoa_weighted+
               away_tot_dvoa+
               away_tot_dvoa_weighted+
               away_off_dvoa+
               away_off_dvoa_weighted+
               away_def_dvoa+
               away_def_dvoa_weighted+home_off_drive_success_rate+
               home_def_drive_success_rate+
               home_off_rz_td_rate+
               home_def_rz_td_rate+
               home_off_int_rate+
               home_off_fumble_rate+
               home_def_int_rate+
               home_def_fumble_rate+
               away_off_drive_success_rate+
               away_def_drive_success_rate+
               away_off_rz_td_rate+
               away_def_rz_td_rate+
               away_off_int_rate+
               away_off_fumble_rate+
               away_def_int_rate+
               away_def_fumble_rate,
             data=train, 
             hidden=c(27,27),
             lifesign = "full",
             stepmax=1e6,
             rep=6,
             threshold = 0.035,
             linear.output = FALSE)
plot(nn)
nn$result.matrix
#Compute the results of test data
Predict = compute(nn,test)
#Get the test results as a data frame
test.results <- as.data.frame(Predict$net.result)

#Unscale test results to get projections
test.results <- test.results %>% 
  mutate(predTotal = unscale(V1, min1,max1))


test.comp <- test.data %>% 
  na.omit() %>% 
  select(season,week, home_team, away_team, total_line, total) %>% 
  mutate(total_projection = test.results$predTotal)

results <- data.frame(actual = test.data$total, prediction=test.results$predTotal)
predicted=results$prediction * abs(diff(range(test.data$total))) + min(test.data$total)
actual=results$actual * abs(diff(range(test.data$total))) + min(test.data$total)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

write_rds(nn,"data/test_11_1_D.rds")
write_csv(test.comp,"data/test_11_1_D.csv")



