setwd("/Users/soniabaee/Documents/Projects/MindTrails/R01/") 
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)
library(anytime)

#----------------------------------------------------------------------
raw_data_dir = "/Users/soniabaee/Documents/Projects/MindTrails/R01/Raw-Data-analysis"
setwd(raw_data_dir)
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)

data = lapply(filenames, read.csv) #if you downloaded the data from the server run this
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)}) #if you downloaded the data from the MindTrails run this

names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split='_', fixed=FALSE))[1]}))
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
  
    # we don't need this part because we have all coaches as a test_account and we already took care of them
    # Create a label for coaches :
    # data$coach_id = NA
    # coaches = c(8, 10, 41, 42, 49, 50, 54, 55, 56, 68, 74, 400, 906, 1103, 1107, 1111, 1112, 1772)
    # data$participant = mutate(data$participant, coach_id = ifelse(participantID %in% coaches,"coach","normal")) 
    
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


#---------------------------
# function `remove_duplicates` shows which ids (systemID or participantID, depending on the table) have 
# duplicated values (across all data tables) and returns the dataset without duplication 
# return the data with the any duplication
remove_duplicates = function(data){
  tmp = list()
  cnt = 1
  data = participant_data
  for(name in names(data)){
    if(name == 'taskLog'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("systemID","session","task_name","tag")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values in the table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("systemID","session","task_name","tag")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication in the table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if(name == 'participant'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("participantID")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if(name == 'study'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("systemID", "session")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("systemID", "session")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if(name == 'affect'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID", "session", "tag")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("participantID", "session", "tag")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if((name == 'attritionPrediction') || (name == 'errorLog') || (name == 'smsLog')){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("participantID")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if(name == 'angularTraning'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID", "session_counter", "session", "button_pressed", "step_title", "stimulus_name")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!(duplicated(data[[name]][, c("participantID", "session_counter", "session", "button_pressed", "step_title", "stimulus_name")])),]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else if(name == 'emailLog'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID", "session", "email_type", "date_sent")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("participantID", "session", "email_type", "date_sent")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
    else{
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("participantID", "session")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("participantID", "session")]), ] 
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]]
        cnt = cnt + 1
      }
    }
  }
  
  names(tmp) = names(data)
  return(tmp)
}
#---------------------------
#---------------------------
# `participant_data_no_duplication` is a collection of tables without any duplication
# based on the id show the duplication in tables
participant_data_no_duplication = remove_duplicates(participant_data)
#---------------------------


# -------------- Handle special case of DASS --------------------------------
# for participant who have duplication in dass21As we keep the last entry
# eligible = filter(participant_data$dass21AS, session == "ELIGIBLE")
# eligible = eligible[!rev(duplicated(rev(eligible[, c("participantID")]))),]
# other  =  filter(participant_data$dass21AS, session != "ELIGIBLE")
# participant_data$dass21AS = rbind(eligible, other)
#---------------------------

#---------------------------
#the range of each item in the table is stored in the data_summary
data_summary = lapply(participant_data, summary)
for (i in 1:length(no_duplicated_data)){
  assign(paste(paste("df", i, sep=""), "summary", sep="."), data_summary[[i]])
}
data_summary
#---------------------------

#---------------------------
# prefer not to answer coding for each table
# pna =  -1 or 555
# this function return the participant/system Ids of the row with prefer not to answer value for each table
get_ids_with_pna = function(df, pna = 555){
  tmp_df = df[ , -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols = apply(tmp_df, 2, function(col) names(which(col == pna)))
  idx_list = list()
  cnt_idx = 1
  par_id_list = list()
  cnt1 = 1
  sys_id_list = list()
  cnt2 = 1
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] = idx
      cnt_idx = cnt_idx + 1
    }
  }
  idx_list = unlist(idx_list, recursive=FALSE)
  idx_list = idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] = df[idx,]$participantID
        cnt1 = cnt1 + 1
      }
      par_id_list = unlist(par_id_list, recursive=FALSE)
      par_id_list = par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] = df[idx,]$systemID
        cnt1 = cnt1 + 1
      }
      sys_id_list = unlist(sys_id_list, recursive=FALSE)
      sys_id_list = sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with prefer not to answer = ", pna, " found!\n"))
  # }
} 
#---------------------------
ids_with_pna = lapply(participant_data, get_ids_with_pna)
ids_with_pna
#---------------------------

#---------------------------
# this function return the participant/system Ids with null values in each table
get_ids_with_missing = function(df){
  tmp_df = df[ , -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols = apply(tmp_df, 2, function(col) names(which(is.na(col))))
  # `idx_list` is a list of row' index that has null value
  idx_list = list()
  cnt_idx = 1
  # `par_id_list` is a list of participant ids with the null value
  par_id_list = list()
  cnt1 = 1
  # `sys_id_list` is a list of system ids with the null value
  sys_id_list = list()
  cnt2 = 1
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] = idx
      cnt_idx = cnt_idx + 1
    }
  }
  idx_list = unlist(idx_list, recursive=FALSE)
  idx_list = idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] = df[idx,]$participantID
        cnt1 = cnt1 + 1
      }
      par_id_list = unlist(par_id_list, recursive=FALSE)
      par_id_list = par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] = df[idx,]$systemID
        cnt1 = cnt1 + 1
      }
      sys_id_list = unlist(sys_id_list, recursive=FALSE)
      sys_id_list = sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with missing values found!\n"))
  # }
} 
#---------------------------
ids_with_missing = lapply(participant_data, get_ids_with_missing )
ids_with_missing
#---------------------------

#---------------------------
#create an object with the number of taks that should be done per session
number_of_tasks = c(2, 14, 8, 5) 
names(number_of_tasks) = c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks #e.g., session eligibility should have 2 different task 
#---------------------------
#`session_task_check` function, return if the participant complete a session or it is in the middle of the session
session_task_check = function(df, session_name){
  tmp = ddply(df,~systemID+session,summarise,number_of_distinct_tasks=length(unique(task_name)))
  tmp2 = filter(tmp, session == session_name) 
  tmp2$stage = NULL
  tmp2 = transform(tmp2, stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], "completed", "middle"))
  return(tmp2)
}
#---------------------------
# the second argument can be any session name of the study
# we can use this `number_of_distinct_task_for_session` variable to make sure, participant didn't skip any tasks 
number_of_distinct_task_for_session = session_task_check(participant_data$taskLog, "preTest")
number_of_distinct_task_for_session
#---------------------------
