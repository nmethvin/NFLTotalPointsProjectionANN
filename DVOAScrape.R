library(nflfastR)
library(tidyverse)
library(neuralnet)
library(lubridate)
library(robotstxt)
library(rvest)
library(jsonlite)
library(httr)
paths_allowed("https://www.footballoutsiders.com/foplus/dvoa-database/every-team-one-week-season?year=2002&week=1&offense_defense=offense")


url <- "https://www.footballoutsiders.com/foplus/dvoa-database/dvoa-specific-week?year=2002&week=1&offense_defense=offense"
session <- html_session(url)
unfilled_forms <- html_form(session)
form <- html_form(session)[[1]]

filled_form <- set_values(form,
                          "name" = "nathanmethvin97@gmail.com",
                          "pass" = "NiteRanger69*")

## Save main page url
main_page <- submit_form(session, filled_form)
fo.test <- jump_to(session, url)
#download.file(download_url, "data/file.csv", method = "curl")

page <- read_html(fo.test)


DVOAteam <- page %>% 
  html_nodes('td:nth-child(1)') %>% 
  html_text()
totDVOA <- page %>% 
  html_nodes("td:nth-child(2)") %>% 
  html_text()
offDVOA <- page %>% 
  html_nodes("td:nth-child(3)") %>% 
  html_text()
defDVOA <- page %>% 
  html_nodes("td:nth-child(4)") %>% 
  html_text()




seasons <- c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,
             2013, 2014,2015,2016,2017,2018,2019,2020)
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
teams <- c()
tot_dvoa <- c()
tot_dvoa_w <- c()
off_dvoa <- c()
off_dvoa_w <- c()
def_dvoa <- c()
def_dvoa_w <- c()
week_dvoa <- c()
season_dvoa <- c()
count <- 0
for (season in seasons){
  for (week in weeks){
    url <- paste0("https://www.footballoutsiders.com/foplus/dvoa-database/dvoa-specific-week?year=",season,"&week=",week,"&offense_defense=offense")
    session <- html_session(url)
    unfilled_forms <- html_form(session)
    form <- html_form(session)[[1]]
    filled_form <- set_values(form,
                              "name" = "nathanmethvin97@gmail.com",
                              "pass" = "NiteRanger69*")
    ## Save main page url
    main_page <- submit_form(session, filled_form)
    fo <- jump_to(session, url)
    #download.file(download_url, "data/file.csv", method = "curl")
    
    page <- read_html(fo)
    
    DVOAteam <- page %>% 
      html_nodes("td:nth-child(1)") %>% 
      html_text()
    totDVOA <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(4)") %>% 
      html_text()
    totDVOAWeighted <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(6)") %>% 
      html_text()
    offDVOA <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(8)") %>% 
      html_text()
    offDVOAWeighted <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(10)") %>% 
      html_text()
    defDVOA <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(12)") %>% 
      html_text()
    defDVOAWeighted <- page %>% 
      html_nodes(".new-table-right-joined:nth-child(14)") %>% 
      html_text()
    seasonValues <- rep(c(season), times = length(DVOAteam))
    weekValues <- rep(c(week), length(DVOAteam))
    
    teams <- c(teams, DVOAteam)
    tot_dvoa <- c(tot_dvoa, totDVOA)
    off_dvoa <- c(off_dvoa, offDVOA)
    def_dvoa <- c(def_dvoa, defDVOA)
    tot_dvoa_w <- c(tot_dvoa_w, totDVOAWeighted)
    off_dvoa_w <- c(off_dvoa_w, offDVOAWeighted)
    def_dvoa_w <- c(def_dvoa_w, defDVOAWeighted)
    week_dvoa <- c(week_dvoa,weekValues)
    season_dvoa <- c(season_dvoa, seasonValues)
    if(length(teams)!=length(tot_dvoa)){
      print(season)
      print(week)
      print("problem week!!!!!!!!!!!")
    }
    count <- count +1
    print(count)
  }
}


teams <- teams[1:9984]
season_dvoa <- season_dvoa[1:9984]
week_dvoa <- week_dvoa[1:9984]

dvoa_all_weighted <- tibble(
  team = teams,
  season = season_dvoa,
  week = week_dvoa,
  tot_dvoa = tot_dvoa,
  tot_dvoa_weighted = tot_dvoa_w,
  off_dvoa = off_dvoa,
  off_dvoa_weighted = off_dvoa_w,
  def_dvoa = def_dvoa,
  def_dvoa_weighted = def_dvoa_w
)


dvoa_all_weighted_base <- dvoa_all_weighted
dvoa_all_weighted <- dvoa_all_weighted_base
dvoa_all_weighted <- dvoa_all_weighted %>% 
  mutate(team = case_when(team!="SD"~team, team=="SD"~"LAC")) %>% 
  mutate(team = case_when(team!="STL"~team, team=="STL"~"LA")) %>%
  mutate(team = case_when(team!="OAK"~team, team=="OAK"~"LV")) %>% 
  mutate(team = case_when(team!="LAR"~team, team=="LAR"~"LA"))

dvoa_all_weighted <- dvoa_all_weighted %>% 
  mutate(tot_dvoa = as.numeric(str_remove(tot_dvoa,"[%]")),
         tot_dvoa_weighted = as.numeric(str_remove(tot_dvoa_weighted,"[%]")),
         off_dvoa = as.numeric(str_remove(off_dvoa,"[%]")),
         off_dvoa_weighted = as.numeric(str_remove(off_dvoa_weighted,"[%]")),
         def_dvoa = as.numeric(str_remove(def_dvoa,"[%]")),
         def_dvoa_weighted = as.numeric(str_remove(def_dvoa_weighted,"[%]")))
  

write_csv(dvoa_all_weighted, "data/dvoa_all_weighted.csv")


# 
# 
# seasons <- c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,
#              2013, 2014,2015,2016,2017,2018,2019,2020)
# weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
# teams <- c()
# tot_dvoa <- c()
# off_dvoa <- c()
# def_dvoa <- c()
# week_dvoa <- c()
# season_dvoa <- c()
# count <- 0
# for (season in seasons){
#   for (week in weeks){
#     url <- paste0("https://www.footballoutsiders.com/foplus/dvoa-database/every-team-one-week-season?year=",season,"&week=",week,"&offense_defense=offense")
#     session <- html_session(url)
#     unfilled_forms <- html_form(session)
#     form <- html_form(session)[[1]]
#     filled_form <- set_values(form,
#                               "name" = "nathanmethvin97@gmail.com",
#                               "pass" = "NiteRanger69*")
#     ## Save main page url
#     main_page <- submit_form(session, filled_form)
#     fo <- jump_to(session, url)
#     #download.file(download_url, "data/file.csv", method = "curl")
#     
#     page <- read_html(fo)
#     
#     DVOAteam <- page %>% 
#       html_nodes("#block-fo-front-content b") %>% 
#       html_text()
#     totDVOA <- page %>% 
#       html_nodes("td:nth-child(2)") %>% 
#       html_text()
#     offDVOA <- page %>% 
#       html_nodes("td:nth-child(3)") %>% 
#       html_text()
#     defDVOA <- page %>% 
#       html_nodes("td:nth-child(4)") %>% 
#       html_text()
#     seasonValues <- rep(c(season), times = length(DVOAteam))
#     weekValues <- rep(c(week), length(DVOAteam))
#     
#     teams <- c(teams, DVOAteam)
#     tot_dvoa <- c(tot_dvoa, totDVOA)
#     off_dvoa <- c(off_dvoa, offDVOA)
#     def_dvoa <- c(def_dvoa, defDVOA)
#     week_dvoa <- c(week_dvoa,weekValues)
#     season_dvoa <- c(season_dvoa, seasonValues)
#     count <- count +1
#     print(count)
#   }
# }

