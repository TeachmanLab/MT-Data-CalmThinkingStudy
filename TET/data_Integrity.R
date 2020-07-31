setwd("/Users/soniabaee/Documents/Projects/MindTrails/TET/") 
library(dplyr) # Load package
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)

#----------------------------------------------------------------------
participant = read.csv("ParticipantExport_07_09_2020.csv", sep="\t", header=TRUE)
participant_remove_admin_account = filter(participant, testAccount == "false" & admin == "false") 
participant_rename_ID = participant_remove_admin_account %>% select(participantID = id, systemID = study, everything()) 
cleaned_participant = participant_rename_ID
systemID_match = select(cleaned_participant, participantID, systemID)

#----------------------------------------------------------------------
study_table = read.csv("reweeklyreportfiles/StudyImportExport_07_16_2020.csv", sep="\t", header=TRUE) 
study_table_rename_ID = study_table %>% select(systemID = id,everything()) 
merge_study_participant_tables = left_join(study_table_rename_ID, systemID_match, by="systemID") 

#----------------------------------------------------------------------
participant_study_tables = inner_join(cleaned_participant, merge_study_participant_tables, by=c("participantID","systemID")) 
TET_participant_study = filter(participant_study_tables, studyExtension == "TET")
TET_study = filter(TET_participant_study, participantID >= 2010) 
View(TET_study)

participant_study_Ids = TET_study[,c('systemID', 'participantID')]

TET_participant_IDs = TET_study$participantID
TET_study_IDS = TET_study$systemID
#----------------------------------------------------------------------
#check the number of questionnaires per session based on tasklog
tasklog_table = read.csv("taskLog_07_21_2020.csv",  header=TRUE) 
# add session number to the table
tasklog_table$session_number = 100
tasklog_table$session_number[tasklog_table$session_name == 'Eligibility'] = 0
tasklog_table$session_number[tasklog_table$session_name == 'preTest'] = 1
tasklog_table$session_number[tasklog_table$session_name == 'firstSession'] = 2
tasklog_table$session_number[tasklog_table$session_name == 'secondSession'] = 3
tasklog_table$session_number[tasklog_table$session_name == 'thirdSession'] = 4
tasklog_table$session_number[tasklog_table$session_name == 'fourthSession'] = 5
tasklog_table$session_number[tasklog_table$session_name == 'fifthSession'] = 6
tasklog_table$session_number[tasklog_table$session_name == 'PostFollowUp'] = 7
tasklog_table$session_number[tasklog_table$session_name == 'COMPLETE'] = 8


# Change ID variable name to match PARTICIPANT table
tasklog_table_rename_ID <- tasklog_table %>% select(systemID = study_id, everything()) 
TET_tasklog = subset(tasklog_table_rename_ID, systemID %in% TET_study_IDS)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#----------------------------------------------------------------------
# dropout
tmp = filter(tasklog_table, task_name == "SESSION_COMPLETE" & study_id %in% TET_study_IDS)
View(tmp)

lastSessionComp = aggregate(tmp[, c('session_name', 'task_name')], list(tmp$study_id), tail, 1)
names(lastSessionComp) = c('systemID','session_complete', 'task_name')

active = left_join(TET_study, lastSessionComp, by="systemID") 
active_task = active[,c('participantID', 'systemID', 'active', 'currentSession', 'session_complete', 'lastLoginDate', 'emailReminders', 'phoneReminders', 'conditioning')]
View(active_task)

#it shows the number of waiting days per session
number_of_days = c(0,0,0, 5,5,5,5, 60)
sessionList = c("Eligibility", "preTest", "firstSession", 
                "secondSession", "thirdSession", "fourthSession", "fifthSession",
                "PostFollowUp")
names(number_of_days) = sessionList
number_of_days

# the dating time and active column
format(active_task$lastLoginDate,"%Y")
active_task$date = as.Date(active_task$lastLoginDate, format= '%a, %d %b %Y %T')
active_task$dayDiff = difftime(now() ,active_task$date , units = c("days"))
active_task$wait = 10000
active_task$drop = -1
completeSession = filter(active_task, !is.na(session_complete))
for (session in sessionList){
  if (session %in% unique(completeSession$session_complete)){
    completeSession[completeSession$session_complete == session, 'wait'] = 
      completeSession$dayDiff[completeSession$session_complete == session] - number_of_days[[session]]
    completeSession[(completeSession$session_complete == session)&(completeSession$wait > 21), 'drop'] = 1
  }
}

colnames(completeSession)
tmp = completeSession[,c('participantID', 'systemID', 'active', 'drop', 'session_complete', 'lastLoginDate', 'dayDiff', 'wait')]
View(tmp)

View(filter(tmp, (active == 'true')&(drop == 1)))

#----------------------------------------------------------------------


#########################################################################################################
#create a function to return the number of distinct per user in a give session 
#----------------------------------------------------------------------
distinct_questionnaire_function <- function(df, session_name, eligible = FALSE){
  if(eligible == TRUE){
    TET_tasklog_questionnairs = ddply(df,~systemID+date_completed,
                                          summarise,
                                          number_of_distinct_questionnaire = n_distinct(task_name))
    return(TET_tasklog_questionnairs)
  }else{
    TET_tasklog_questionnairs = ddply(df,
                                      ~systemID,
                                      summarise,
                                      number_of_distinct_questionnaire = n_distinct(task_name))
    return(TET_tasklog_questionnairs)
  }
}

#########################################################################################################




#----------------------------------------------------------------------
#it shows the number of tasks/questionnaire for each session (MUST BE COMPLETED)
number_of_tasks = c(2, 15, 9, 5)
names(number_of_tasks) = c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#eligibility (one or two tasks - must check the timing)
#----------------------------------------------------------------------
session = 'Eligibility' #You can change this to any session that you like (preTest, firstSession, secondSession)
nValue = number_of_tasks[[session]]
TET_tasklog_session = subset(TET_tasklog, session_name == session)
TET_tasklog_session_questionnairs =
  distinct_questionnaire_function(TET_tasklog_session, session, eligible = TRUE)
head(TET_tasklog_session_questionnairs)
tail(TET_tasklog_session_questionnairs)

inValid = filter(TET_tasklog_session_questionnairs, number_of_distinct_questionnaire > nValue)
cat("System ID with invalid number of task in session -> ", session , "\n",
    "The number of invalid userstables -> ", dim(inValid)[1], "\n",
    " SystemID ->", inValid$systemID, "\n")

#----------------------------------------------------------------------

#########################################################################################################
questionnaireList = c("BBSIQ", 'OA', 'CC', 'RR')
questionnaire_session = list(c("pretest", "thirdSession", "fifthSession"), 
                             c("preTest", "firstSession", "secondSession", "thirdSession", "fourthSession", "fifthSession"),
                             c("firstSession", "thirdSession", "fifthSession"),
                             c("preTest", "thirdSession", "fifthSession"))
names(questionnaire_session) = questionnaireList
questionnaire_session

#----------------------------------------------------------------------
lastTasks = aggregate(TET_tasklog[, c('session_name', 'task_name')], list(TET_tasklog$systemID), tail, 1)
names(lastTasks) = c('systemID','session_name', 'task_name')

lastTasks$session_name[lastTasks$session_name == 'Eligibility'] = 0
lastTasks$session_name[lastTasks$session_name == 'preTest'] = 1
lastTasks$session_name[lastTasks$session_name == 'firstSession'] = 2
lastTasks$session_name[lastTasks$session_name == 'secondSession'] = 3
lastTasks$session_name[lastTasks$session_name == 'thirdSession'] = 4
lastTasks$session_name[lastTasks$session_name == 'fourthSession'] = 5
lastTasks$session_name[lastTasks$session_name == 'PostFollowUp'] = 6
lastTasks$session_name[lastTasks$session_name == 'COMPLETE'] = 7

#----------------------------------------------------------------------
#check the number of questionnaires per session based on tasklog
bbsiq_table = read.csv("bbsiq_07_21_2020.csv",  header=TRUE) 
# Change ID variable name to match PARTICIPANT table
bbsiq_table_rename_ID = bbsiq_table %>% select(participantID = participant_id, everything()) 
TET_bbsiq = subset(bbsiq_table_rename_ID, participantID %in% TET_participant_IDs)

#to check each participant have bbsiq in the targeted session
questionnaire = 'BBSIQ'
sessionList = questionnaire_session[[questionnaire]]

TET_tasklog_questionnaire = subset(TET_tasklog, task_name == questionnaire)

for (id in TET_tasklog_questionnaire$systemID){
  tmp = filter(lastTasks, systemID == id)
  
}
  

#----------------------------------------------------------------------

