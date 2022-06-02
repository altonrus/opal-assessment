---
title: "001-Opal-dummy-data-practice"
author: "Wanjin Li"
date: "31/05/2022"
output: pdf_document
---
  
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

#Read data from excel
opal.visit <- read_excel("1_data/Opal_dummy_data.xlsx", sheet="visit_data")
opal.usr.list <- read_excel("1_data/Opal_dummy_data.xlsx", sheet="opal_usr_list")

#Shift each visit date to get previous visit and next visit
opal.visit <- opal.visit %>%
  group_by(Pat_ID) %>%
  mutate(last_visit_date=lag(Date, 1), next_visit_date=lead(Date, 1))

#Set data.frame to data.table
setDT(opal.visit)
setDT(opal.usr.list)

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

#Link Opal user login times to the episodes
#join dates that patients became Opal users before and during an episode -> define Opal users
opal.usr.list[, join_date := Login]
login_ep <- opal.usr.list[first_start_end_ep, nomatch=NULL, 
                          on = .(Pat_ID, 
                                 join_date <= ep_end_date)]
setnames(login_ep, "join_date", "ep_end_date")

login_ep <- select(login_ep, Pat_ID, Login, ep_start_date, ep_end_date, ep_start_id, ep_end_id, ep_duration)


#Link the calendar date of each individual visit to the episodes
opal.individual.visit <- opal.visit[, .(Pat_ID, Date)]
opal.individual.visit[, join_visit := Date]
join_visit_ep <- opal.individaul.visit[first_start_end_ep,
                                       on = .(Pat_ID,
                                              join_visit >= ep_start_date,
                                              join_visit <= ep_end_date)]
setnames(join_visit_ep, "join_visit", "ep_start_date")
setnames(join_visit_ep, "join_visit.1", "ep_end_date")

#DEFINE INTERVENTION ASSIGNMENT PERIOD

#Assign ID to each visit within an episode for each patient
join_visit_ep[, visit_id := fifelse(Date >= ep_start_date & Date <= ep_end_date, 1, 0)] 

#Count the number of visit 
join_visit_ep[, cum_visit := cumsum(visit_id), by = .(Pat_ID, ep_start_id)]

#If intervention assignment period ends after 2 visits

join_visit_ep[, int_assign_id := fifelse(cum_visit == 2, 1, 0)] #the calendar date of the 2nd visit prior to intervention assignment period end is flagged as 1
join_visit_ep[, int_end_date := fifelse(int_assign_id == 1, as.Date(Date)+1, as.Date(ep_start_date))] #set the end date of intervention assignment period as the following calendar date after 2 visits
join_visit_ep[, int_dur := as.numeric(int_end_date - as.Date(ep_start_date))] 

#Define follow-up period
join_visit_ep[, follow_up_start_date := int_end_date + 1] #set the start date of follow-up period as the following calendar date after intervention assignment period ends
join_visit_ep[, follow_up_dur := as.numeric(as.Date(ep_end_date) - follow_up_start_date)] 

#filter & merge
select_int <- join_visit_ep[, .SD[int_dur != 0]]
episode_int_fp <- merge(login_ep, select_int, by=c("Pat_ID", "ep_start_date", "ep_end_date"), nomatch=NULL, all=T) %>%
  select(Pat_ID, Login, ep_start_date, ep_end_date,int_end_date, follow_up_start_date) 

#Define eligible episode - assignment periods before the end of an episode
eligible_ep <- episode_int_fp %>% filter(int_end_date < ep_end_date & !is.na(Login))


#DEFINE ELIGIBLE AND INELIGIBLE OPAL USERS
#Use while loop to identify eligible opal user that is classified as 1
i_patient <- 1
while (i_patient <= dim(eligible_ep)[1]) {
  eligible_ep[, eligible_usr := fifelse(Login <= int_end_date, 1, 0)]
  i_patient = i_patient+1
}

#Define eligible opal user - patients who became opal users or ever logged in opal before the assignment period ends
eligible_usr <- eligible_ep %>% 
  filter(Login <= int_end_date)

#Define ineligible opal user - patients who became opal users or logged into opal after the assignment period ends 
ineligible_usr <- eligible_ep %>%
  filter(Login > int_end_date)

#Count the total number of patients who became Opal users before and duration episodes
num_usr <- length(unique(login_ep$Pat_ID)) #but also include ineligible episodes though

#count the number of eligible opal users
num_elg_usr <- length(unique(eligible_usr$Pat_ID))

#count the number of ineligible opal users
num_inelg_usr <- length(unique(ineligible_usr$Pat_ID))

#count the number of overlaps that patients who are defined as eligible and ineligible in the mean time
n_overlap <- data.frame(id = numeric())

k <- 1
for (i in 1: nrow(ineligible_usr)){
  for (j in 1: nrow(eligible_usr)){
    if (ineligible_usr[, 1][i] == eligible_usr[, 1][j]){
     n_overlap[k,] <- ineligible_usr[, 1][i]
     k <- k+1
    } else{
      next
    }
  }
}

num_overlap <- nrow(unique(n_overlap))
num_overlap

#calculate the percentage of opal user discarded
pt_usr_discard <- (num_inelg_usr - num_overlap)/(num_usr)
pt_usr_discard

