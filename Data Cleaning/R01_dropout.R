# This code is used to create a table indicating which participants dropped out BEFORE starting Session Two and which participants dropped
# out at any point BEFORE completing the study (the end of the Post Follow Up session). The final table includes participants who are not test/admin
# accounts and who are not in the psychoeducation or HR_coach conditions.

library(dplyr)
setwd("/Users/soniabaee/Documents/Projects/Attrition/attrition-master/data/r01/cleaned/")

#R01 dropout table########################################################################################################################

participant <- read.csv("participant.csv", header = TRUE) #Change file name
participant <- filter(participant, test_account == 0 & admin == 0)
study <- read.csv("study.csv", header = TRUE) #Change file name
study$study_id <- study$id
merge3 <- left_join(participant, study, by = "study_id")
merge3 <- filter(merge3, conditioning == "TRAINING"| conditioning == "NONE" | conditioning == "LR_TRAINING"|conditioning == "HR_NO_COACH")
merge3 <- filter(merge3, current_session != "preTest")
merge3 <- filter(merge3, current_session != "firstSession")

#"do_beforeS2" = Did P drop out before starting Session Two? 0 = No, 1 = Yes
merge4 <-mutate(merge3, do_beforeS2 = ifelse(current_session == "secondSession" & current_task_index == 0 & active == 0,1,0))

# Correct "active" value for participants who dropped out before completing PostFollowUp (60 days of inactivity)
post <- filter(merge3, current_session == "PostFollowUp" & active == 1)
tasklog <- read.csv("TaskLog.csv", header = TRUE) 
tl_post <- filter(tasklog, study_id %in% post$study_id)
tl_post$date_completed <- tl_post$date_completed
tl_post <- tl_post[!rev(duplicated(rev(tl_post$study_id))),] #Return last entry of Task Log for each P (where they dropped out)
tl_post$diff <- Sys.Date() - as.Date(tl_post$date_completed) # Current epoch time (CHANGE EVERYTIME YOU RUN THE SCRIPT) - last login date
tl_post <- filter(tl_post, diff > 60) # filter Ps who were inactive more than 60 days

merge4$active[merge4$study_id %in% tl_post$study_id] <- 0 # Replace incorrect "true" values with correct "false" values
post <- filter(merge4, current_session == "PostFollowUp" & active == 1) # Check to see it worked

#"do_all" = Did P drop out at any point before completing the study? 0 = No, 1 = Yes
merge4 <- mutate(merge4, do_all = ifelse(active == 0,1,0))

# Note: Participant 910 was never put in a condition past TRAINING despite completing Session One
# They dropped out before completing Session Two; had they returned, they would have been randomized
# into a condition. Use the line of code below to remove them.
# merge4 <- filter(merge4, id.x !=910)

# Rename ID column and create table
merge4$participantID <- merge4$id.x
final <- merge4[,c("do_beforeS2","do_all","participantID")]
write.csv(final, file = "R01_dropout_29April20.csv", row.names = FALSE)