rm(list = setdiff(ls(), lsf.str())) # removes all objects except for functions:
#----------------------------------------------------------------------
setwd("/Users/soniabaee/Documents/Projects/MindTrails/R01/") 
#----------------------------------------------------------------------
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)
library(anytime)
#----------------------------------------------------------------------
raw_data_dir = "/Users/soniabaee/Documents/Projects/MindTrails/R01/Raw-Data-18-09-20"
setwd(raw_data_dir)
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)

data = lapply(filenames, read.csv) #if you downloaded the data from the server run this
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)}) #if you downloaded the data from the MindTrails run this

names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split='_', fixed=FALSE))[1]}))
#---------------------------
cat("the list of tables: ")
cat("---------------------------\n")
names(data)
cat("---------------------------\n")
#---------------------------

#---------------------------
# function `standardize_colnames` enforces a standardized column naming scheme across data tables
#  For instance, `participandID` replaces the columns `participant_id` and `id`
standardize_columns_special_tables = function(data){
  
  participant_table = grep("^participant", names(data))
  data[[names(data)[participant_table][1]]] =
    data[[names(data)[participant_table][1]]] %>% select(systemID = study_id, participantID = id, everything()) 
  
  study_table = grep("^study", names(data))
  data[[names(data)[study_table][1]]] =
    data[[names(data)[study_table][1]]] %>% select(systemID = id, everything()) 
  
  taskLog_table = grep("^taskLog", names(data))
  data[[names(data)[taskLog_table][1]]] =
    data[[names(data)[taskLog_table][1]]] %>% select(systemID = study_id, everything()) 
  
  return(data)
}
#---------------------------
#---------------------------
data = standardize_columns_special_tables(data)
#---------------------------


#---------------------------
standardize_columns = function(df){
  df_colnames = colnames(df)
  
  if("participant_id" %in% df_colnames){
    df = df %>% select(participantID = participant_id, everything())
  }
  
  date_columns = grep("^date", df_colnames)
  if(length(date_columns)!=0){
    for(i in date_columns){
      df = mutate(df, dateTime = anytime(as.factor(df[[df_colnames[i]]]))) 
    }
  }
  
  if(("session_name" %in% df_colnames)){
    df = df %>% select(session = session_name, everything()) 
  }
  if(("current_session" %in% df_colnames)){
    df = df %>% select(session = current_session, everything()) 
  }
  
  return(df)
}
#---------------------------
#---------------------------
# Standardize column names across dataset
data = lapply(data, standardize_columns)
#---------------------------


#---------------------------
# function `add_participant_info` creates helper columns that explain
#  launch membership and condition assignment
add_participant_info = function(data, study_name){
  if(study_name == "R01"){
    
    #remove admin and test_account
    data$participant = filter(data$participant, test_account == 0 & admin == 0) 
    
    # Create new variable to differentiate soft and true launch Ps
    data$participant$launch_type = NA
    data$participant = mutate(data$participant, launch_type = ifelse(participantID >= 159,"TRUE","SOFT"))
    
    # Create new variable describing how Ps were assigned to a condition
    data$participant$condition_assignment_method = NA
    manual = c(43,45,57,63,67,71,82,90,94,96,97,104,108,120,130,131,132,140)
    data$participant = mutate(data$participant, condition_assignment_method = ifelse(participantID %in% manual,"manual","algorithm")) 
    
    # # we don't need this part because we have all coaches as a test_account and we already took care of them
    # # Create a label for coaches :
    # data$participant$coach_id = NA
    # coaches = c(8, 10, 41, 42, 49, 50, 54, 55, 56, 68, 74, 400, 906, 1103, 1107, 1111, 1112, 1772)
    # data$participant = mutate(data$participant, coach_id = ifelse(participantID %in% coaches,"coach","normal"))
    
    #special IDs: 
    #2004: Assigned to R01 condition but has a TET label. This was due to a bug at launch. 
    #According to a message by Dan, the studyExtension field was not properly being passed through to the data server, 
    #and this was fixed on 4/7/2020.2004
    
    #2005: Assigned to R01 condition but has a TET label. This was due to a bug at launch. According to Dan, 
    #the studyExtension field was not properly being passed through to the data server, and this was fixed on 4/7/2020.
    
    specialIDs = c(2004, 2005)
    tmp = filter(data$participant, participantID %in% specialIDs)
    specialIDs_systemIDs = tmp$systemID
    data$study[which(data$study$systemID %in% specialIDs_systemIDs),]$study_extension = "NULL"
    
  }
  return(data)
}
#---------------------------
#---------------------------
# Add helper columns
data = add_participant_info(data, "R01")
#---------------------------


#---------------------------
# Extract the participants for the given study
# the second argument of `study_ID_function` is the study name, so it can be `R01`, `TET`, or `GIDI`
get_study_participants = function(data, study_name){
  study = data$study
  participant = data$participant
  systemID_match = select(participant, participantID, systemID)
  merge_study_participant_tables = left_join(study, systemID_match, by="systemID") 
  participant_study_tables = inner_join(participant, merge_study_participant_tables, by=c("participantID","systemID")) 
  tmp = data.frame()
  if(study_name == "TET"){
    tmp = filter(participant_study_tables, study_extension == "TET")
  }
  else if(study_name == "GIDI"){
    tmp = filter(participant_study_tables, study_extension == "GIDI")
  }
  else if(study_name == "R01"){
    tmp = filter(participant_study_tables, (study_extension != "TET")&(study_extension != "GIDI"))
  }
  return(tmp)
}
#---------------------------
#---------------------------
study_name = "R01"
study_participants = get_study_participants(data, "R01")

# extract the participant ids of the selected study (here is `R01`)
participantIDs = study_participants$participantID
# extract the system ids of the selected study (here is `R01`)
systemIDs = study_participants$systemID

cat("number of participant in study", study_name, "is: ", length(participantIDs))
#---------------------------


#---------------------------
#`select_study_participants_data` function extracts the data of each tables for the users of the selected study
select_study_participants_data = function(data, participant_ids, system_ids, study_name = "R01"){
  tmp = list()
  
  cnt = 1
  for(df in data){
    if("systemID" %in% colnames(df)){
      df = subset(df, systemID %in% systemIDs)
    }
    else if("participantID" %in% colnames(df)){
      df = subset(df, participantID %in% participantIDs)
    } 
    tmp[[cnt]] = df
    cnt = cnt + 1
  }
  names(tmp) = names(data)
  return(tmp)
}
#---------------------------
#---------------------------
#restore the data of each tables for the users of the selected study in the participant_data 
participant_data = select_study_participants_data(data, participantIDs, systemIDs, "R01")
#---------------------------


# Update active column of some specific participants
#---------------------------
update_active_column = function(data){
  #Participant 891, 1627, 1663, and 1852 were supposed to be labelled 
  #'active=false' after a period of inactivity, but this switch did not occur in the system. 
  selected_users = c(891, 1627, 1663, 1852)
  data$participant[which(data$participant$participantID %in% selected_users),]$active = 0
  
  return(data)
}
#---------------------------
#---------------------------
updated_participant_data = update_active_column(participant_data)
#---------------------------


# special participants
#---------------------------
update_specific_participants = function(data){
  
  #Participant 1992 only progressed to the early Pre-test phase before the R01 study closed, 
  #but re-engaged with the program at a later point and got assigned to a TET study 
  #condition, so we change their progress to what it was in R01 before the switch happened. 
  #Participants 1992, 2004, and 2005 received 'studyExtension=TET' labels incorrectly as we 
  #were switching over to the TET study.
  tmp_systemID = data$participant[which(data$participant$participantID == 1992),]$systemID
  data$study[which(data$study$systemID == tmp_systemID),]$conditioning = "NONE"
  data$study[which(data$study$systemID == tmp_systemID),]$session = "preTest"
  data$study[which(data$study$systemID == tmp_systemID),]$study_extension = "NULL"
  
  special_participant_IDs = c(2004, 2005)
  tmp_systemID = data$participant[which(data$participant$participantID %in% special_participant_IDs),]$systemID
  data$study[which(data$study$systemID %in% tmp_systemID),]$study_extension = "NULL"

  return(data)
  
} 
#---------------------------
#---------------------------
updated_participant_data = update_specific_participants(updated_participant_data)
#---------------------------



#---------------------------
# 1. calculate the duration (days) of last task/last session (inactivity duration)
# 2. add the calculated value as a column in participant table (days_since_task)
#---------------------------
add_days_since_task_column = function(data){
  daysSinceTask = as.Date(as.character(Sys.Date()), format = "%Y-%m-%d") -
    as.Date(as.character(data$participant$last_login_date), format="%Y-%m-%d")
  
  daysSinceSession = as.Date(as.character(Sys.Date()), format = "%Y-%m-%d") -
    as.Date(as.character(data$study$last_session_date), format="%Y-%m-%d")
  
  
  participant = mutate(data$participant, days_since_task = daysSinceTask)
  participant = mutate(participant, days_since_session = daysSinceSession)
  data$participant = participant
  return(data)
}
#---------------------------
#---------------------------
participant_data_with_inactivity_days = add_days_since_task_column(updated_participant_data)
cat("the columns of participant table:")
names(participant_data_with_inactivity_days$participant)
#---------------------------


#---------------------------
# 1. add a new column for checking the actual active column and the calculated column
# 2. update the calculated_active column of those that have the inactivity threshold ~21
#---------------------------
calculated_active_retention_clm = function(data, inactivity_threshold){
  
  data = participant_data_with_inactivity_days
  participant_study_table = left_join(data$participant, data$study, by = c("systemID"))
  
  waiting_time = c("firstSession" = 5 , "secondSession" = 5, "thirdSession" = 5, 
                   "fourthSession" = 5)
  
  waitingSessionList = names(waiting_time)
  participant_study_table$calculated_active = 1
  for(session in waitingSessionList){
    wait_days = waiting_time[[session]]
    inactivity_duration = inactivity_threshold + wait_days

    participant_study_table[which(participant_study_table$session == session & 
                                    (participant_study_table$days_since_session > inactivity_duration |
                                       participant_study_table$days_since_task > inactivity_duration)),]$calculated_active = 0
  }
  
  
  #after the complete session, participants have followup session which we consider as retention
  waiting_time_retention = c("COMPLETE" = 60)
  waitingSessionList = names(waiting_time_retention)
  for(session in waitingSessionList){
    wait_days = waiting_time_retention[[session]]
    inactivity_duration = inactivity_threshold + wait_days
    
    participant_study_table$retention = NA
    participant_study_table[which(participant_study_table$session == session & 
                                    (participant_study_table$days_since_session > inactivity_duration |
                                       participant_study_table$days_since_task > inactivity_duration)),]$retention = 1
  }
  
  data$study_participant = participant_study_table
  return(data)
}
#---------------------------
#---------------------------
participant_active_retention = calculated_active_retention_clm(participant_data_with_inactivity_days, 21)
cat("the columns of participant table:")
table(participant_active_retention$study_participant$calculated_active)
#---------------------------


#---------------------------
# calculate session based dropout
# the number of participant who drop out in the selected session and add coresponding column
dropout_per_session = function(data, session = ""){
  
  #data = participant_data_with_inactivity_days
  participant_study_table = data$study_participant
  
  #order the sessions of the study
  participant_study_table$session = factor(participant_study_table$session,
                                       levels = c("preTest", "firstSession", "secondSession", 
                                                  "thirdSession", "fourthSession", "fifthSession",
                                                  "COMPLETE", "PostFollowUp"), ordered = TRUE)
  
  #select participants of attrition algorithm 
  attrition_condition = c("TRAINING", "NONE", "LR_TRAINING", "HR_NO_COACH")
  attrition_participants = participant_study_table[which(participant_study_table$conditioning %in% attrition_condition),]
  
  #overall attrition 
  attrition_participants = mutate(attrition_participants, 
                               dropout = 
                                 factor(ifelse(attrition_participants$session == "COMPLETE" & active == 1, 0, 1)))
  
  attrition_participants = mutate(attrition_participants, 
                                  dropout_calculated = 
                                    factor(ifelse(attrition_participants$session == "COMPLETE" & calculated_active == 1, 0, 1)))
  
  
  #select data of previous sessions of the selectes session
  #attrition_session_participants = attrition_participants[which(attrition_participants$session <= session),]
  
  
  #sessionList = unique(attrition_participants$session)
  dropout_participant = mutate(attrition_participants, 
                           dropout_column_active = 
                             factor(ifelse(attrition_participants$session == session & 
                                             attrition_participants$current_task_index == 0 & 
                                             active == 0, 
                                           1, 0)))
  
  dropout_participant = mutate(dropout_participant, 
                               dropout_column_calculated_active = 
                                 factor(ifelse(dropout_participant$session == session & 
                                                 dropout_participant$current_task_index == 0 & 
                                                 calculated_active == 0, 
                                               1, 0)))
  
  #rename dropout column
  names(dropout_participant)[names(dropout_participant) == "dropout_column_active"] = paste("dropout_prior_", session , sep="")
  names(dropout_participant)[names(dropout_participant) == "dropout_column_calculated_active"] = paste("dropout_calculated_prior_", session , sep="")
  return(dropout_participant)
}
#---------------------------
session_dropout = dropout_per_session(participant_active_retention, session = "secondSession")
cat("the columns of participant table:")
names(session_dropout)
table(session_dropout$dropout_prior_secondSession)
table(session_dropout$dropout_calculated_prior_secondSession)

table(session_dropout$dropout)
table(session_dropout$dropout_calculated)

write.csv(session_dropout, file = "dropoutBeforeS2.csv", row.names = FALSE)







