rm(list = setdiff(ls(), lsf.str())) # removes all objects except for functions:
#----------------------------------------------------------------------
raw_data_dir = "/Users/soniabaee/Documents/Projects/Attrition/data/r01/interim/CalmThinking-12_02_2021"
setwd(raw_data_dir)
#----------------------------------------------------------------------
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(waldo)
library(data.table)
require(lubridate)
library(anytime)
#----------------------------------------------------------------------
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)
data = lapply(filenames, read.csv) #if you downloaded the data from the server run this
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)}) #if you downloaded the data from the MindTrails run this
split_char = '-'
names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split=split_char, fixed=FALSE))[1]}))
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
  
  taskLog_table = grep("^task_log", names(data))
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
  # if(("current_session" %in% df_colnames)){
  #   df = df %>% select(session = current_session, everything()) 
  # }
  
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
    if(all(is.na(data$study[which(data$study$systemID %in% specialIDs_systemIDs),]$study_extension))){
      print("No manual changes needed for the special IDs! We alread took care of it in the server.")
    }else{
      data$study[which(data$study$systemID %in% specialIDs_systemIDs),]$study_extension = ""
    }
    
  }
  return(data)
}
#---------------------------
#---------------------------
# Add helper columns
slc_study_name = "R01"
data = add_participant_info(data, study_name = slc_study_name)
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
  
  last_entry_taskLog = data$task_log[!rev(duplicated(rev(data$task_log$systemID))),]
  task_log_Ids = last_entry_taskLog$systemID
  tmp2 = filter(tmp, systemID %in% task_log_Ids)
  return(tmp2)
}
#---------------------------
#---------------------------
slc_study_name = "R01"
study_participants = get_study_participants(data, slc_study_name)

# extract the participant ids of the selected study (here is `R01`)
participantIDs = study_participants$participantID
# extract the system ids of the selected study (here is `R01`)
systemIDs = study_participants$systemID

cat("number of participant in study", slc_study_name, "is: ", length(participantIDs))
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
  data$study[which(data$study$systemID == tmp_systemID),]$current_session = "preTest"
  data$study[which(data$study$systemID == tmp_systemID),]$study_extension = ""
  
  # special_participant_IDs = c(2004, 2005)
  # tmp_systemID = data$participant[which(data$participant$participantID %in% special_participant_IDs),]$systemID
  # data$study[which(data$study$systemID %in% tmp_systemID),]$study_extension = ""
  
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
  
  last_entry_taskLog = data$task_log[!rev(duplicated(rev(data$task_log$systemID))),]
  daysSinceCompleted = as.Date(as.character(Sys.Date()), format = "%Y-%m-%d") -
    as.Date(as.character(last_entry_taskLog$date_completed), format="%Y-%m-%d")
  
  
  participant = mutate(data$participant, days_since_task = daysSinceTask)
  participant = mutate(participant, days_since_session = daysSinceSession)
  participant = mutate(participant, days_since_completed = daysSinceCompleted)
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
# 1. report the number of completed sessions and not completed sessions 
# 2. tasklog, study, participants tables are involved
#---------------------------
completed_session_report = function(data){
  # data = participant_data_with_inactivity_days
  #sessions
  sessionList = c("Eligibility", "preTest", "firstSession", "secondSession", 
    "thirdSession", "fourthSession", "fifthSession",
    "PostFollowUp", "COMPLETE")
  #extract the last entry of the task log for all of the participants
  last_entry_taskLog = data$task_log[!rev(duplicated(rev(data$task_log$systemID))),]
  #mereg tasklog with the participant table 
  taskLog_participant_data = left_join(data$participant, last_entry_taskLog, by = c("systemID"))
  #study table for checking the current session of the 
  study = data$study
  #order the sessions of the study
  taskLog_participant_data$session = factor(taskLog_participant_data$session,
                                           levels = c("Eligibility", "preTest", "firstSession", "secondSession", 
                                                      "thirdSession", "fourthSession", "fifthSession",
                                                      "PostFollowUp", "COMPLETE"), ordered = TRUE)
  for(session in sessionList){
    cat("--------------------------------\n")
    cat("--------------------------------\n")
    cat("---------", session, "----------\n")
    cat("--------------------------------\n")
    current_session_idx = which(levels(taskLog_participant_data$session) == session)
    next_session = levels(taskLog_participant_data$session)[current_session_idx + 1]
    if(session == "Eligibility"){
      tmp_tasklog_completed = taskLog_participant_data[which(taskLog_participant_data$session == session ),
                                                       c('participantID', 'systemID','session', 'task_name', 'date_completed', 'active')]
      cat("--------------------------------\n")
      cat(n_distinct(tmp_tasklog_completed$systemID), " number of participants completed at session:", session, "\n")
      tmp_study = study[which(study$current_session == next_session & study$current_task_index == 0),]
      cat("--------------------------------\n")
      cat(n_distinct(tmp_study$systemID), " number of participants did not start task session: ", next_session, "\n")
      cat("--------------------------------\n")
      diff_ids = c()
      if(dim(tmp_tasklog_completed)[1] > dim(tmp_study)[1]){
        diff_ids = setdiff(tmp_tasklog_completed$systemID, tmp_study$systemID)
      }
      else{
        diff_ids = setdiff(tmp_study$systemID, tmp_tasklog_completed$systemID)
      }
      if(length(diff_ids) > 0){
        tmp_study_ids = filter(data$study, systemID %in% diff_ids)
        tmp_tasklog_ids = filter(last_entry_taskLog, systemID %in% diff_ids)
        cat(unique(tmp_study_ids$systemID), " participants have issues\n")
        cat("--------------------------------\n")
        print(tmp_study_ids)
        cat("Tasklog information for >>>", unique(tmp_tasklog_ids$systemID), "participants\n")
        cat("--------------------------------")
        print(tmp_tasklog_ids)
      }
    }
    else{
      tmp_tasklog_completed = taskLog_participant_data[which(taskLog_participant_data$session == session & 
                                                               taskLog_participant_data$task_name == 'SESSION_COMPLETE'),c('participantID', 'systemID','session', 'task_name', 'date_completed', 'active')]
      cat("--------------------------------\n")
      cat(n_distinct(tmp_tasklog_completed$systemID), " number of participants completed at session:", session, "\n")
      tmp_tasklog_NOT_completed = taskLog_participant_data[which(taskLog_participant_data$session == session & 
                                                                   taskLog_participant_data$task_name != 'SESSION_COMPLETE'),c('participantID', 'systemID','session', 'task_name', 'date_completed', 'active')]
      cat("--------------------------------\n")
      cat(n_distinct(tmp_tasklog_NOT_completed$systemID), " number of participants NOT completed at session:", session, "\n")
      tmp_study = study[which(study$current_session == next_session & study$current_task_index == 0),]
      cat("--------------------------------\n")
      cat(n_distinct(tmp_study$systemID), " number of participants did not start task at session: ", next_session, "\n")
      diff_ids = c()
      if(dim(tmp_tasklog_completed)[1] > dim(tmp_study)[1]){
        diff_ids = setdiff(tmp_tasklog_completed$systemID, tmp_study$systemID)}
      else{
        diff_ids = setdiff(tmp_study$systemID, tmp_tasklog_completed$systemID)}
      if(length(diff_ids) > 0){
        tmp_study_ids = filter(data$study, systemID %in% diff_ids)
        tmp_tasklog_ids = filter(last_entry_taskLog, systemID %in% diff_ids)
        cat(unique(tmp_study_ids$systemID), " participants have issues\n")
        cat("--------------------------------\n")
        print(tmp_study_ids)
        cat("Tasklog information for >>>", unique(tmp_tasklog_ids$systemID), "participants\n")
        cat("--------------------------------")
        print(tmp_tasklog_ids)
      }
    }
  }
  return("done")
}
#---------------------------
#---------------------------
completed_session_report(participant_data_with_inactivity_days)
#---------------------------


#---------------------------
# 1. add a new column for completed a session
# 2. if the task_name of the taskLog is session_complete we automatically move them to the next session with task_index = 0
# 3. attrtion dropout means if they did not finish dropout_session
# 4. select participant whose condition is 
#---------------------------
session_dropout = function(data, dropout_session){
  
  data = participant_data_with_inactivity_days
  #extract the last entry of the task log for all of the participants
  last_entry_taskLog = data$task_log[!rev(duplicated(rev(data$task_log$systemID))),]
  #mereg tasklog with the participant table 
  taskLog_participant_data = left_join(data$participant, last_entry_taskLog, by = c("systemID"))
  taskLog_participant_data$session = factor(taskLog_participant_data$session,
                                            levels = c("Eligibility", "preTest", "firstSession", "secondSession", 
                                                       "thirdSession", "fourthSession", "fifthSession",
                                                       "PostFollowUp", "COMPLETE"), ordered = TRUE)
  
  current_session_idx = which(levels(taskLog_participant_data$session) == dropout_session)
  #dropout before starting dropout_session
  previous_sessions = levels(taskLog_participant_data$session)[1:current_session_idx-1]
  taskLog_participant_data = mutate(taskLog_participant_data,  prior_dropout = ifelse(session %in% previous_sessions ,1,0))
  # taskLog_participant_data = mutate(taskLog_participant_data,  prior_dropout = ifelse(session %in% previous_sessions & task_name == 'SESSION_COMPLETE' ,1,0))
  #dropout during dropout session
  previous_current_sessions = levels(taskLog_participant_data$session)[1:current_session_idx]
  taskLog_participant_data = mutate(taskLog_participant_data,  in_dropout = ifelse(session %in% previous_current_sessions & task_name != 'SESSION_COMPLETE' ,1,0))
  
  #rename dropout column
  names(taskLog_participant_data)[names(taskLog_participant_data) == "prior_dropout"] = paste("dropout_prior_", dropout_session , sep="")
  names(taskLog_participant_data)[names(taskLog_participant_data) == "in_dropout"] = paste("dropout_in_", dropout_session , sep="")
  
  # tasklog, study, participant table
  taskLog_participant_study_data = left_join(taskLog_participant_data, data$study, by = c("systemID"))
  
  #select participants of attrition algorithm 
  attrition_condition = c("TRAINING", "NONE", "LR_TRAINING", "HR_NO_COACH", "HR_COACH")
  attrition_participants_data = taskLog_participant_study_data[which(taskLog_participant_study_data$conditioning %in% attrition_condition),]
  
  #completed session and not completed session
  return(attrition_participants_data)
}
#---------------------------
# calculate session based dropout
# the number of participant who drop out in the selected session and add coresponding column
interested_dropout_session = "secondSession"
labeled_data = session_dropout(participant_data_with_inactivity_days, interested_dropout_session)
col1 = paste("dropout_prior_", interested_dropout_session , sep="")
table(labeled_data[col1])
#---------------------------
#---------------------------

#---------------------------
# 1. saving the labeled data
# 2. select the informative columns
#---------------------------
slc_columns = c("systemID", "participantID", 
                "conditioning", "current_session",
                "dropout_prior_secondSession", "dropout_in_secondSession")
attrition_labeled_data = labeled_data[slc_columns]
raw_data_date = unlist(strsplit(filenames[1],split=split_char, fixed=FALSE))[2]
save_file_name = paste(paste("R01_dropout_based_on_tasklog",raw_data_date, sep = split_char), sep = '')
write.csv(attrition_labeled_data, file = save_file_name , row.names = FALSE)






# #---------------------------
# # 1. add a new column for checking the actual active column and the calculated column
# # 2. update the calculated_active column of those that have the inactivity threshold ~21
# #---------------------------
# calculated_active_retention_clm = function(data, inactivity_threshold){
#   
#   data = participant_data_with_inactivity_days
#   participant_study_table = left_join(data$participant, data$study, by = c("systemID"))
#   
#   #order the sessions of the study
#   participant_study_table$session = factor(participant_study_table$current_session,
#                                            levels = c("preTest", "firstSession", "secondSession", 
#                                                       "thirdSession", "fourthSession", "fifthSession",
#                                                       "PostFollowUp", "COMPLETE"), ordered = TRUE)
#   
#   waiting_time = c( "preTest" = 0, "firstSession" = 0 , "secondSession" = 5, "thirdSession" = 5, 
#                    "fourthSession" = 5, "fifthSession" = 5, "PostFollowUp" = 60)
#   
#   waitingSessionList = names(waiting_time)
#   participant_study_table$calculated_active = 1
#   for(session in waitingSessionList){
#     wait_days = waiting_time[[session]]
#     inactivity_duration = inactivity_threshold + wait_days
#     
#     # we automatically move them to the next session after completing the current session
#     # the current_task_index remain 0 until they start that session
#     current_session_idx = which(levels(participant_study_table$session) == session)
#     next_session = levels(participant_study_table$session)[current_session_idx + 1]
#     
#     # therefore those that didn't start the session for specified duration, we consider them as inactive participants
#     participant_study_table[which(participant_study_table$session == next_session & 
#                                     participant_study_table$current_task_index == 0 & 
#                                     (participant_study_table$days_since_session > inactivity_duration |
#                                        participant_study_table$days_since_completed > inactivity_duration)),]$calculated_active = 0
#   }
#   
#   data$study_participant = participant_study_table
#   return(data)
# }
# #---------------------------
# #---------------------------
# participant_active_retention = calculated_active_retention_clm(participant_data_with_inactivity_days, 21)
# cat("the columns of participant table:")
# names(participant_active_retention)
# table(participant_active_retention$study_participant$active)
# #---------------------------





