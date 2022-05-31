---
title: "Opal-dummy-data-practice"
author: "Wanjin Li"
date: "25/05/2022"
output: pdf_document
---

#source("./2_scripts/00_helper_functions.R")

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

#Read data from excel
opal.visit <- read_excel("./1_data/Opal_dummy_data.xlsx", sheet="visit_data")
opal.usr.list <- read_excel("./1_data/Opal_dummy_data.xlsx", sheet="opal_usr_list")

#Shift each visit date to get previous visit and next visit
opal.visit <- opal.visit %>%
  group_by(Pat_ID) %>%
  mutate(last_visit_date=lag(Date, 1), next_visit_date=lead(Date, 1))

#Set data.frame to data.table
setDT(opal.visit)
setDT(opal.usr.list)

#Shift each visit date to get previous visit and next visit
#opal.visit[, last_visit_date := shift(Date, 1, type="lag", fill=NA), by = Pat_ID]
#opal.visit[, next_visit_date := shift(Date, 1, type="lead"), by = Pat_ID]

#Calculate the gaps between each visit and its previous and/or next visit
opal.visit$days_last_visit <- as.numeric(as.Date(opal.visit$Date) - as.Date(opal.visit$last_visit_date))
opal.visit$days_next_visit <- as.numeric(as.Date(opal.visit$next_visit_date) - as.Date(opal.visit$Date))


##TEST OF EPISODE CREATION: MAX of MAX GAP = 90

#Condition when an episode starts
opal.visit[, episode_start:=fifelse(days_last_visit > 180 | is.na(last_visit_date), 1, 0, na=NA)]

#Condition when an episode ends
opal.visit[, episode_end:=fifelse(days_next_visit > 90 | is.na(next_visit_date), 1, 0, na=NA)]

#Classify episode start as 1
dt_ep_start <- opal.visit[episode_start == 1,
                          c("Pat_ID", "Date")]
colnames(dt_ep_start)[2] <- "ep_start_date"

#Classify episode end as 1
dt_ep_end <- opal.visit[episode_end == 1,
                        c("Pat_ID", "Date")]
colnames(dt_ep_end)[2] <- "ep_end_date"

#Create unique id for episode start and end dates for each patient
dt_ep_start[, ep_start_id:=frank(ep_start_date), by=Pat_ID]
dt_ep_end[, ep_end_id:=frank(ep_end_date), by=Pat_ID]

#link episode end date to episode start date
dt_ep_start[, RollDate:= ep_start_date]
dt_ep_end[, RollDate:= ep_end_date]

setkey(dt_ep_start, Pat_ID, "RollDate")
setkey(dt_ep_end, Pat_ID, "RollDate")
dt_start_end_ep <- dt_ep_start[dt_ep_end, roll=Inf, nomatch=NULL] %>%
  filter(ep_end_date > ep_start_date) 

#Select the first episode end date that corresponds to the first episode start date
first_start_end_ep <- dt_start_end_ep[, head(.SD, 1), by = list(Pat_ID, ep_start_id)]

#Calculate the duration of each episode
first_start_end_ep[, ep_duration:= as.numeric(ep_end_date - ep_start_date)]

#drop the column RollDate
first_start_end_ep[, RollDate:=NULL]

#Link the date patients became an Opal user at the first time to episodes
opal.usr.list[, join_date := First_login]
login_ep <- opal.usr.list[first_start_end_ep, 
                                 on = .(Pat_ID, 
                                        join_date >= ep_start_date,
                                        join_date <= ep_end_date)]
setnames(login_ep, "join_date", "episode_start_date")
setnames(login_ep, "join_date.1", "episode_end_date")
 

#Link the calendar date of each individual visit to the episodes
opal.individaul.visit <- opal.visit[, .(Pat_ID, Date)]
opal.individaul.visit[, join_visit := Date]
join_visit_ep <- opal.individaul.visit[login_ep,
                                       on = .(Pat_ID,
                                              join_visit >= episode_start_date,
                                              join_visit <= episode_end_date)]
setnames(join_visit_ep, "join_visit", "episode_start_date")
setnames(join_visit_ep, "join_visit.1", "episode_end_date")

#Count the number of visits 
join_visit_ep[, visit_id := fifelse(Date >= episode_start_date & Date <= episode_end_date, 1, 0)] 
join_visit_ep[, cum_visit := cumsum(visit_id), by = .(Pat_ID, ep_start_id)]

#Calculate the number of visits per episode
num_visit_ep <- join_visit_ep %>%
  group_by(Pat_ID, ep_start_id) %>%
  summarise(visits_per_episode=n())

visit_episodes <- merge(login_ep, num_visit_ep, by=c("Pat_ID", "ep_start_id"), all=T)

#Calculate the difference in days between episode start end and the first time patients became the Opal user
visit_episodes$diff_start_login <- visit_episodes$episode_start_date - visit_episodes$First_login
visit_episodes$max_gap = 90
visit_episodes$baseline_gap = 180




#DEFINE INTERVENTION ASSIGNMENT PERIOD

#intervention assignment period ends after 2 visits
join_visit_ep[, int_assign_id := fifelse(cum_visit == 2, 1, 0)] #the calendar date of the 2nd visit prior to intervention assignment period end is flagged as 1
join_visit_ep[, int_end_date := fifelse(int_assign_id == 1, as.Date(Date)+1, as.Date(episode_start_date))] #set the end date of intervention assignment period as the following calendar date after 2 visits
join_visit_ep[, int_dur := as.numeric(as.Date(int_end_date) - as.Date(episode_start_date))] #?


#Define follow-up period
join_visit_ep[, follow_up_start_date := int_end_date + 1] #set the start date of follow-up period as the following calendar date after intervention assignment period ends
join_visit_ep[, follow_up_dur := as.numeric(as.Date(episode_end_date) - follow_up_start_date)] #?

#filter 
matching_int <- join_visit_ep[, .SD[int_assign_id==1]]

##
episode_int <- merge(visit_episodes, matching_int, by=c("Pat_ID", "ep_start_id"), all=T) #need edits





###HELPER FUNCTIONS
##Helper function that creates episodes

create.episode <- function(max_gap, baseline_gap, dt.visit, dt.usr){
  opal.visit[, episode_start:=fifelse(days_last_visit > baseline_gap | is.na(last_visit_date), 1, 0, na=NA)]
  
  #Condition when an episode ends
  opal.visit[, episode_end:=fifelse(days_next_visit > max_gap | is.na(next_visit_date), 1, 0, na=NA)]
  
  #Classify episode start as 1
  dt_ep_start <- opal.visit[episode_start == 1,
                            c("Pat_ID", "Date")]
  colnames(dt_ep_start)[2] <- "ep_start_date"
  
  #Classify episode end as 1
  dt_ep_end <- opal.visit[episode_end == 1,
                          c("Pat_ID", "Date")]
  colnames(dt_ep_end)[2] <- "ep_end_date"
  
  #Create unique id for episode start and end dates for each patient
  dt_ep_start[, ep_start_id:=frank(ep_start_date), by=Pat_ID]
  dt_ep_end[, ep_end_id:=frank(ep_end_date), by=Pat_ID]
  
  #link episode end date to episode start date
  dt_ep_start[, RollDate:= ep_start_date]
  dt_ep_end[, RollDate:= ep_end_date]
  
  setkey(dt_ep_start, Pat_ID, "RollDate")
  setkey(dt_ep_end, Pat_ID, "RollDate")
  dt_start_end_ep <- dt_ep_start[dt_ep_end, roll=Inf, nomatch=NULL] %>%
    filter(ep_end_date > ep_start_date) 
  
  #Select the first episode end date that corresponds to the first episode start date
  first_start_end_ep <- dt_start_end_ep[, head(.SD, 1), by = list(Pat_ID, ep_start_id)]
  
  #Calculate the duration of each episode
  first_start_end_ep[, ep_duration:= as.numeric(ep_end_date - ep_start_date)]
  
  #drop the column RollDate
  first_start_end_ep[, RollDate:=NULL]
  
  #Link the date patients became an Opal user at the first time to episodes
  opal.usr.list[, join_date := First_login]
  login_ep <- opal.usr.list[first_start_end_ep, 
                            on = .(Pat_ID, 
                                   join_date >= ep_start_date,
                                   join_date <= ep_end_date)]
  setnames(login_ep, "join_date", "episode_start_date")
  setnames(login_ep, "join_date.1", "episode_end_date")
  
  
  #Link the calendar date of each individual visit to the episodes
  opal.individaul.visit <- opal.visit[, .(Pat_ID, Date)]
  opal.individaul.visit[, join_visit := Date]
  join_visit_ep <- opal.individaul.visit[login_ep,
                                         on = .(Pat_ID,
                                                join_visit >= episode_start_date,
                                                join_visit <= episode_end_date)]
  setnames(join_visit_ep, "join_visit", "episode_start_date")
  setnames(join_visit_ep, "join_visit.1", "episode_end_date")
  
  #Count the number of visits 
  join_visit_ep[, visit_id := fifelse(Date >= episode_start_date & Date <= episode_end_date, 1, 0)] 
  join_visit_ep[, cum_visit := cumsum(visit_id), by = .(Pat_ID, ep_start_id)]
  
  #Calculate the number of visits per episode
  num_visit_ep <- join_visit_ep %>%
    group_by(Pat_ID, ep_start_id) %>%
    summarise(visits_per_episode=n())
  
  merge_visit_episodes <- merge(login_ep, num_visit_ep, by=c("Pat_ID", "ep_start_id"), all=T)
  
  #Calculate the difference in days between episode start end and the first time patients became the Opal user
  merge_visit_episodes$diff_start_login <- merge_visit_episodes$episode_start_date - merge_visit_episodes$First_login
  
  merge_visit_episodes$max_gap = max_gap
  merge_visit_episodes$baseline_gap = baseline_gap
  
  #return the final data.table 
  return(merge_visit_episodes)
}

##Helper function to compare the trade-off in percentage of episodes remained between different max gaps
compare.max.gap <- function(maxGap1, maxGap2, baselineGap, dt1, dt2){
  visit_ep_gap1 <- create.episode(maxGap1, baselineGap, dt1, dt2)
  visit_ep_gap2 <- create.episode(maxGap2, baselineGap, dt1, dt2)
  comb_gap1_gap2 <- rbind(visit_ep_gap1, visit_ep_gap2)
  
  compare_cdf <- ggplot(comb_gap1_gap2, aes(x=visits_per_episode, color=as.factor(max_gap))) +
    scale_x_continuous(breaks = seq(0, max(comb_gap1_gap2$visits_per_episode),1)) +
    ylab("Percentage of episodes") +
    stat_ecdf()
  
  return(compare_cdf)
}

compare.max.gap(30, 90, 180, opal.visit, opal.usr.list)
compare.max.gap(40, 90, 180, opal.visit, opal.usr.list)
compare.max.gap(50, 90, 180, opal.visit, opal.usr.list)

#merge_visit_ep_90 <- create.episode(90, 180, opal.visit, opal.usr.list)
#merge_visit_ep_30 <- create.episode(30, 180, opal.visit, opal.usr.list)
#comb_gap_30_90 <- rbind(merge_visit_ep_30, merge_visit_ep_90)

#ggplot(comb_gap_30_90, aes(x=visits_per_episode, color=as.factor(max_gap))) +
#  scale_x_continuous(breaks = seq(0, max(comb_gap_30_90$visits_per_episode), 1)) +
#  ylab("Percentage of episodes")+
#  stat_ecdf()





