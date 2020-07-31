setwd("/Users/soniabaee/Documents/Projects/MindTrails/R01/") 
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)
library(anytime)

#----------------------------------------------------------------------
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
  
  # Remove admin and test account
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
  
  return(df)
}
#----------------------------------------------------------------------
raw_data_dir = "/Users/soniabaee/Documents/Projects/MindTrails/R01/Raw-Data"
setwd(raw_data_dir)
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)

data = lapply(filenames, read.csv)
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)})
names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split='_', fixed=FALSE))[1]}))
# names(data) = unlist(strsplit(filenames, ".csv"))

#---------------------------
data_summary = lapply(data, summary)
names(data_summary) = unlist(strsplit(filenames, ".csv"))
for (i in 1:length(data_summary)){
  assign(paste(paste("df", i, sep=""), "summary", sep="."), data_summary[[i]])
}
#---------------------------

#---------------------------
#1. standard the IDs
#2. remove test and admin users
new_data = lapply(data, standard_ID_function)
#---------------------------

#---------------------------
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

#---------------------------
#1. soft and true launch
#2. algorithm or manual condition assignment
new_data = add_info(new_data, "R01")
#---------------------------


#---------------------------
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

#---------------------------
study_participants = study_ID_function(new_data, "R01")
participantIDs = study_participants$participantID
systemIDs = study_participants$systemID
#---------------------------

#---------------------------
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

#---------------------------
participant_data = select_study_participants_data(new_data, participantIDs, systemIDs, "R01")
#---------------------------



#---------------------------
check_duplication = function(data){
  tmp = list()
  cnt = 1
  for(name in names(data)){
    if(name == 'taskLog'){
      duplicated_rows = data[[name]][(duplicated(data[[name]][, c("systemID","session","task_name","tag")])),]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c("systemID","session","task_name","tag")]), ] 
        cnt = cnt + 1
      }else{
        cat("no duplication for table:", name)
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
no_duplicated_data = check_duplication(participant_data)
#---------------------------


