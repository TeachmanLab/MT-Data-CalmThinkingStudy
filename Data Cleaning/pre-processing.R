setwd("/Users/soniabaee/Documents/Projects/MindTrails/R01/") 
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)
library(anytime)

#----------------------------------------------------------------------
raw_data_dir = "/Users/soniabaee/Documents/Projects/MindTrails/R01/Raw-Data"
setwd(raw_data_dir)
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)

data = lapply(filenames, read.csv)
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)})
names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split='_', fixed=FALSE))[1]}))
# names(data) = unlist(strsplit(filenames, ".csv"))
#---------------------------

#---------------------------
#1. standard the IDs, study, and session
#2. remove test and admin users
#3. standard the data column
standard_ID_function = function(df){
  df_colnames = colnames(df)
  
  # Create variable with consistent participant and study id name
  if("participant_id" %in% df_colnames){
    df = df %>% select(participantID = participant_id, everything()) 
  }
  if(("study" %in% df_colnames)&("id" %in% df_colnames)){
    df = df %>% select(systemID = study, participantID = id, everything()) 
  }
  if(("study_id" %in% df_colnames)){
    df = df %>% select(systemID = study_id, everything()) 
  }
  if(("study_extension" %in% df_colnames)){
    df = df %>% select(systemID = id, everything()) 
  }
  
  # Remove admin and test account in the participant table
  if(("admin" %in% df_colnames)&("test_account" %in% df_colnames) ){
    df = df %>% select(participantID = id, everything()) 
    df = filter(df, test_account == 0 & admin == 0) 
  }
  
  # Create variable with consistent session column name
  if(("session_name" %in% df_colnames)){
    df = df %>% select(session = session_name, everything()) 
  }
  if(("current_session" %in% df_colnames)){
    df = df %>% select(session = current_session, everything()) 
  }
  
  # Create variable with consistent date format
  if(("date" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$date))) 
  }
  if(("dateSent" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$dateSent))) 
  }
  if(("last_login_date" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$last_login_date))) 
  }
  if(("dateTime" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$last_login_date))) 
  }
  
  return(df)
}
#---------------------------
new_data = lapply(data, standard_ID_function)
#---------------------------

#---------------------------
#1. soft and true launch
#2. algorithm or manual condition assignment
add_info = function(data, study_name){
  if(study_name == "R01"){
    
    # Create new variable to differentiate soft and true launch Ps
    data$participant$launch_type = NA
    data$participant = mutate(data$participant, launch_type = ifelse(participantID >= 159,"TRUE","SOFT"))
    
    # Create new variable describing how Ps were assigned to a condition
    data$participant$condition_assignment_method = NA
    manual = c(43,45,57,63,67,71,82,90,94,96,97,104,108,120,130,131,132,140)
    data$participant = mutate(data$participant, condition_assignment_method = ifelse(participantID %in% manual,"manual","algorithm")) 
  
  }
  return(data)
}
#---------------------------
new_data = add_info(new_data, "R01")
#---------------------------


#---------------------------
#extract the users' IDs for each specific study
study_ID_function = function(data, study_name){
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
study_participants = study_ID_function(new_data, "R01")
participantIDs = study_participants$participantID
systemIDs = study_participants$systemID
#---------------------------

#---------------------------
#extract the data of users in each study
select_study_participants_data = function(data, participant_ids, system_ids, study_name = "R01"){
  tmp = list()
  
  cnt = 1
  for(df in new_data){
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
participant_data = select_study_participants_data(new_data, participantIDs, systemIDs, "R01")
#---------------------------

#---------------------------
#check for the duplication and show which ID have a duplicated values in the corresponding tables
check_duplication = function(data){
  tmp = list()
  cnt = 1
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
no_duplicated_data = check_duplication(participant_data)
#---------------------------


#---------------------------
#the range of each item in the table is stored in the data_summary
data_summary = lapply(no_duplicated_data, summary)
for (i in 1:length(no_duplicated_data)){
  assign(paste(paste("df", i, sep=""), "summary", sep="."), data_summary[[i]])
}
#---------------------------


#---------------------------
# prefer not to answer coding for each table
# pna =  -1 or 555
# this function return the participant/system Ids with prefer not to answer in each table
pna_function = function(df, pna = 555){
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
  else{
    return(cat("\nNo entries with prefer not to answer = ", pna, " found!\n"))
  }
} 
#---------------------------
data_pna = lapply(no_duplicated_data, pna_function)
#---------------------------


#---------------------------
# this function return the participant/system Ids with null values in each table
missing_function = function(df){
  tmp_df = df[ , -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols = apply(tmp_df, 2, function(col) names(which(is.na(col))))
  # is.na(x)
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
  else{
    return(cat("\nNo entries with missing values found!\n"))
  }
} 
#---------------------------
data_missing = lapply(no_duplicated_data, missing_function)
#---------------------------



#---------------------------
#check the number of taks per session for each participant
number_of_tasks = c(2, 14, 8, 5)
names(number_of_tasks) = c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks
#---------------------------
session_task_check = function(df, session_name){
  tmp = ddply(df,~systemID+session,summarise,number_of_distinct_tasks=length(unique(task_name)))
  tmp2 = filter(tmp, session == session_name) 
  tmp2$stage = NULL
  tmp2 = transform(tmp2, stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], "completed", "middle"))
  return(tmp2)
}
#---------------------------
number_of_distinct_task_for_session = session_task_check(no_duplicated_data$taskLog, "preTest")
#---------------------------
