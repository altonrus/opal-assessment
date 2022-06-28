

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

###Set up----
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



###Helper functions----

#Create episodes
gen.episodes <- function(dt.visit, init_min_gap, max_gap){
  #Condition when an episode starts
  dt.visit[, episode_start:=fifelse(days_last_visit > init_min_gap | is.na(last_visit_date), 1, 0, na=NA)]
  
  #Condition when an episode ends
  dt.visit[, episode_end:=fifelse(days_next_visit > max_gap | is.na(next_visit_date), 1, 0, na=NA)]
  
  #Classify episode start as 1
  dt_ep_start <- dt.visit[episode_start == 1,
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
  first_episodes <- dt_start_end_ep[, head(.SD, 1), by = list(Pat_ID, ep_start_id)]
  
  #Calculate the duration of each episode
  first_episodes[, ep_duration:= as.numeric(ep_end_date - ep_start_date)]
  
  #drop the column RollDate
  first_episodes[, RollDate:=NULL]
  
  first_episodes$init_min_gap = init_min_gap
  first_episodes$max_gap = max_gap
 
  #Link the calendar date of each individual visit to the episodes
  opal.individual.visit <- dt.visit[, .(Pat_ID, Date)]
  opal.individual.visit[, join_visit := Date]
  join_visit_ep <- opal.individaul.visit[first_episodes,
                                         on = .(Pat_ID,
                                                join_visit >= ep_start_date,
                                                join_visit <= ep_end_date), nomatch=NULL]
  setnames(join_visit_ep, "join_visit", "ep_start_date")
  setnames(join_visit_ep, "join_visit.1", "ep_end_date")
  
  #calculate the number of visit per episode for each patient
  num_visit_per_ep <- join_visit_ep[, .N, by=.(Pat_ID, ep_start_id)]
  
  #match the number of visit to episodes by episode id
  first_episodes$visits <- num_visit_per_ep$N
  
  return(first_episodes) 
}  

t_ep <- gen.episodes(opal.visit, 100, 30)

#Define intervention assignment and follow up periods
add.assign.period <- function(dt.visit, dt.episodes, n_assign_visits, n_assign_days){
  #Link the calendar date of each individual visit to the episodes
  opal.individual.visit <- dt.visit[, .(Pat_ID, Date)]
  opal.individual.visit[, join_visit := Date]
  join_visit_ep <- opal.individaul.visit[dt.episodes,
                                         on = .(Pat_ID,
                                                join_visit >= ep_start_date,
                                                join_visit <= ep_end_date), nomatch=NULL]
  setnames(join_visit_ep, "join_visit", "ep_start_date")
  setnames(join_visit_ep, "join_visit.1", "ep_end_date")
  
  #Assign ID to each visit within an episode for each patient
  join_visit_ep[, visit_id := fifelse(Date >= ep_start_date & Date <= ep_end_date, 1, 0)] 
  
  #Count the number of visit 
  join_visit_ep[, cum_visit := cumsum(visit_id), by = .(Pat_ID, ep_start_id)]
  
  if (!is.na(n_assign_visits) & !is.na(n_assign_days)){
    #If intervention assignment period ends 30 days after 2 visits
    join_visit_ep[, int_end_date := fifelse(cum_visit == n_assign_visits, as.Date(Date)+n_assign_days, as.Date(ep_start_date))]
    
  }else if (!is.na(n_assign_visits)){
    #If intervention assignment period ends after 2 visits
    
    join_visit_ep[, int_assign_id := fifelse(cum_visit == n_assign_visits, 1, 0)] #the calendar date of the 2nd visit prior to intervention assignment period end is flagged as 1
    join_visit_ep[, int_end_date := fifelse(int_assign_id == 1, as.Date(Date)+1, as.Date(ep_start_date))] #set the end date of intervention assignment period as the following calendar date after 2 visits
    
  }else if (!is.na(n_assign_days)){
    #If intervention assignment period ends 30 days after the first visits
    join_visit_ep[, int_end_date := fifelse(cum_visit == 1, as.Date(Date)+n_assign_days, as.Date(ep_start_date))]

  }
  
  #Define follow-up period
  join_visit_ep[, follow_up_start_date := int_end_date + 1] #set the start date of follow-up period as the following calendar date after intervention assignment period ends
  
  join_visit_ep[, int_dur := as.numeric(int_end_date - as.Date(ep_start_date))]
  
  
  #select observations classified as the end of intervention assignment period
  select_int <- join_visit_ep[, .SD[int_dur != 0]] %>%
    select(Pat_ID, ep_start_id, ep_end_id, int_end_date, follow_up_start_date, int_dur)
  
  #match assignment period end date and follow up start date to episodes
  dt.episodes$int_end_date <- select_int$int_end_date
  dt.episodes$follow_up_start_date <- select_int$follow_up_start_date
  dt.episodes$int_dur <- select_int$int_dur
  dt.episodes[, follow_up_dur := as.numeric(as.Date(ep_end_date) - follow_up_start_date)]
  
  return(dt.episodes)
}

t_ep <- add.assign.period(opal.visit, t_ep, 2, 30)



#Link Opal user log in times to the episodes
link.usr.ep <- function(dt.usr, dt.episodes){
  dt.usr[, join_date := Login]
  login_ep <- dt.usr[dt.episodes, nomatch=NULL, 
                            on = .(Pat_ID, 
                                   join_date <= ep_end_date)]
  setnames(login_ep, "join_date", "ep_end_date")
  
  login_ep <- select(login_ep, Pat_ID, Login, visits, ep_start_date, ep_end_date, int_end_date, follow_up_start_date, ep_start_id, ep_end_id, ep_duration, int_dur, follow_up_dur)
  
  #Define eligible episode - assignment periods before the end of an episode
  eligible_ep <- login_ep %>% filter(int_end_date < ep_end_date)
  return(eligible_ep)
}

t_elg_ep <- link.usr.ep(opal.usr.list, t_ep)
nrow(t_elg_ep)


#Define eligible and ineligible Opal users
gen.eligible.usr <- function(dt.elg.ep){
  
  if (nrow(dt.elg.ep) != 0){
    #Use while loop to identify eligible opal user that is classified as 1
    i_patient <- 1
    while (i_patient <= dim(dt.elg.ep)[1]) {
      dt.elg.ep[, eligible_user := fifelse(Login <= int_end_date, 1, 0)]
      i_patient = i_patient+1
    }
    
  }else{
    print("no eligible episodes")
  }
  return(dt.elg.ep)
}

t_elg_ep2 <- gen.eligible.usr(t_elg_ep)
nrow(t_elg_ep2)

#Calculate summary statistics
summary.stats <- function(dt.episodes, dt.elg.ep){
  
    #calculate the number of eligible episodes with defined intervention assignment and follow up periods
    num_episode <- dt.episodes[int_end_date < ep_end_date][, .N]
    
    #Calculate the number of episodes > 60 days
    num_episode_gt_60d <- dt.episodes[int_end_date < ep_end_date & ep_duration > 60][, .N] 
    
    #calculate the number of episodes having more than 3 visits
    num_episode_gt_3v <- dt.episodes[int_end_date < ep_end_date & visits > 3][, .N]
    
    #calculate the number of episodes having more than 6 visits
    num_episode_gt_6v <- dt.episodes[int_end_date < ep_end_date & visits > 6][, .N]
    
    #calculate summary statistics of duration of assignment and follow up periods
    summary_int_dur <- dt.episodes[int_end_date < ep_end_date][, as.list(summary(int_dur))]
    
    summary_fp_dur <- dt.episodes[int_end_date < ep_end_date][, as.list(summary(follow_up_dur))]
    
    if (nrow(dt.elg.ep) != 0){
      ##Count the total number of patients who became Opal users before and duration episodes
      num_usr <- length(unique(dt.elg.ep$Pat_ID))
      
      #Define eligible opal user - patients who became opal users or ever logged in opal before the assignment period ends
      eligible_usr <- dt.elg.ep %>% 
        filter(eligible_user==1)
      
      #Define ineligible opal user - patients who became opal users or logged into opal after the assignment period ends 
      ineligible_usr <- dt.elg.ep %>%
        filter(eligible_user==0)
      
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
      
      #calculate the percentage of opal user discarded
      pt_usr_discard <- (num_inelg_usr - num_overlap)/(num_usr)
      
      
    }else {
      num_usr <- NA
      num_elg_usr <- NA
      num_inelg_usr <- NA
      num_overlap <- NA
      pt_usr_discard <- NA
      
    }
    
    #create a summary statistic table
    df.summary.stats <- data.frame(pct_usr_discarded = numeric(),
                                   n_episode = numeric(),
                                   n_episode_gt_3v = numeric(),
                                   n_episode_gt_6v = numeric(),
                                   n_episode_gt_60d = numeric(),
                                   int_assign_dur = numeric(),
                                   follow_up_dur = numeric(),
                                   n_elg_usr = numeric(),
                                   n_inelg_usr = numeric(),
                                   n_overlap = numeric(),
                                   n_usr = numeric())
    df.summary.stats[1,1] <- round(pt_usr_discard,2)
    df.summary.stats[1,2] <- num_episode
    df.summary.stats[1,3] <- num_episode_gt_3v
    df.summary.stats[1,4] <- num_episode_gt_6v
    df.summary.stats[1,5] <- num_episode_gt_60d
    df.summary.stats[1,6] <- summary_int_dur$Median
    df.summary.stats[1,7] <- summary_fp_dur$Median
    df.summary.stats[1,8] <- num_elg_usr
    df.summary.stats[1,9] <- num_inelg_usr
    df.summary.stats[1,10] <- num_overlap
    df.summary.stats[1,11] <- num_usr
    
    return(df.summary.stats)
    
}

t_elg_ep2_sumstats <- summary.stats(t_ep, t_elg_ep2)


# Loop----

comb_param <- expand.grid(
  init_min_gap <- c(100, 180),
  max_gap <- c(30, 60, 90),
  n_assign_visits <- c(2),
  n_assign_days <- c(30)
)
colnames(comb_param) <- c("init_min_gap","max_gap", "assign_visits","assign_days")
comb_param

summary_table <- data.frame(pct_usr_discarded = numeric(),
                            n_episode = numeric(),
                            n_episode_gt_3v = numeric(),
                            n_episode_gt_6v = numeric(),
                            n_episode_gt_60d = numeric(),
                            int_assign_dur = numeric(),
                            follow_up_dur = numeric(),
                            n_elg_usr = numeric(),
                            n_inelg_usr = numeric(),
                            n_overlap = numeric(),
                            n_usr = numeric())

for (i in 1: nrow(comb_param)){
  t_episodes <- gen.episodes(opal.visit, comb_param[i,1],comb_param[i,2])
  t_episodes <- add.assign.period(opal.visit, t_episodes, comb_param[i,3], comb_param[i,4])
  t_elg_episodes <- link.usr.ep(opal.usr.list, t_episodes)
  t_elg_episodes <- gen.eligible.usr(t_elg_episodes)
  t_summary_stats <- summary.stats(t_episodes, t_elg_episodes)
  summary_table <- rbind(summary_table, t_summary_stats)
  
}

test_sum <- summary_table

merge_sum_stats <- cbind(comb_param, summary_table)


###test ------
















  
#add_assign_period <- function(t_episodes, n_assign_visits = 2, n_assign_days = NA){
#if (!is.na(n_assign_days) & !is.na(n_assign_days)){
# e.g., 10 days after second visit
#} else if (is.na(n_assign_visits)){
#assign based on n_assign_days
#} else {
#based on n visit 
#}
#returns(t_episodes)
#}

#analyze_policy <- function(opal.visit, init_min_gap=180, max_gap = 30, 
#                           n_assign_visits = 2, n_assign_days = NA){
#  t_episodes <- gen_episodes()
#  t_episodes <- add_assign_period()

#Calculate summary table statistics
#  return()
#}

#expand.grid(
#  init_min_gap <- c(180, 365),
#  max_gap <- c(30, 45, 60),
#  n_assign_visits <- c(NA, 1, 2, 3),
#  n_assign_days < - c()
#)

