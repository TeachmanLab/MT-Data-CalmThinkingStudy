# This is the cleaning script for the Calm Thinking (R01) study (written by Claudia)

setwd("~/data/") # Set working directory (Will need to change path to folder where the data is stored)
# install.packages("dplyr") # Install package (You only need to run this line the first time you set up R)
# install.packages("anytime")

library(dplyr) # Load package
library(anytime)

# ALL PARTICIPANTS: 01/21/2019 - 03/17/2019 (ID < 159)
# REAL PARTICIPANTS: After 03/18/2019 (ID >= 159)

#########################################################################################################
# PARTICIPANT EXPORT table                                                                              #
#########################################################################################################

participant <- read.csv("participant_07_21_2020.csv", header=TRUE) # Read PARTICIPANT table 
participant <- filter(participant, test_account == 0 & admin == 0) # Remove test and admin accounts
part1 <- participant %>% select(participantID = id, systemID = study_id, everything()) # Create ID variables consistent with all other tables

IDlist <- part1$participantID # Save IDs of participants (not the final list)
systemID_match <- select(part1, participantID,systemID) # Create table to match correct participant IDs with system IDs

View(part1)
part1[part1==""] <- NA
part1 <- mutate(part1, launchType = ifelse(participantID >= 159,"TRUE","SOFT")) # Create new variable to differentiate soft and true launch Ps
manual <- c(43,45,57,63,67,71,82,90,94,96,97,104,108,120,130,131,132,140)
part1 <- mutate(part1, assignmentMethod = ifelse(participantID %in% manual,"manual","algorithm")) # Create new variable describing how Ps were assigned to a condition
part2 <- mutate(part1, dateTime = anytime(as.factor(part1$last_login_date))) # Create variable with consistent date format

dup <- part1[(duplicated(part1[, c("participantID")])),] # NONE

#########################################################################################################
# STUDY IMPORT EXPORT table                                                                             #
#########################################################################################################

study <- read.csv("study.csv",  header=TRUE) # Read STUDY table 
study1 <- study %>% select(systemID = id,everything()) # Change ID variable name to match PARTICIPANT table
merge <- left_join(study1, systemID_match, by="systemID") # Merge the two tables to match participant and system IDs

study2 <- subset(merge, participantID %in% IDlist)
study2 <- study2 %>% select(participantID,everything())
study3 <- filter(study2, study_extension != "TET")

IDlist <- study3$participantID

View(study3)
study3[study3==""] <- NA
study3 <- mutate(study3, last_session_date = anytime(as.factor(study3$last_session_date))) # Create variable with consistent date format
dup <- study3[(duplicated(study3[, c("participantID")])),] # NONE

# write.csv(study3)

part2 <- filter(part1, participantID %in% IDlist)
# write.csv(part2)

#########################################################################################################
# AFFECT table                                                                                          #
#########################################################################################################
affect <- read.csv("Affect.csv", header=TRUE) # Read AFFECT table (Change file name)
affect1 <- affect %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
affect2 <- subset(affect1, participantID %in% IDlist)

View(affect2)
affect2 <- mutate(affect2, dateTime = anytime(as.factor(affect2$date))) # Create variable with consistent date format
dup <- affect2[(duplicated(affect2[, c("participantID","session","tag")])),] # NONE

#########################################################################################################
# ANGULAR TRAINING table                                                                                #
#########################################################################################################
angular <- read.csv("AngularTraining.csv", header=TRUE) # Read table (Change file name)
angular1 <- angular %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
angular2 <- subset(angular1, participantID %in% IDlist)

View(angular2)
angular2 <- mutate(angular2, dateTime = anytime(as.factor(angular2$date))) # Create variable with consistent date format

#########################################################################################################
# ANXIETY IDENTITY table                                                                                #
#########################################################################################################
identity <- read.csv("AnxietyIdentity.csv", header=TRUE) # Read table (Change file name)
identity1 <- identity %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
identity2 <- subset(identity1, participantID %in% IDlist)

View(identity2)

identity3 <- identity2[,-c(grep("tag", colnames(identity2)))] # Remove empty "tag" column
identity3 <- mutate(identity3, dateTime = anytime(as.factor(identity3$date))) # Create variable with consistent date format
dup <- identity3[(duplicated(identity3[, c("participantID","session")])),] # NONE

#########################################################################################################
# ANXIETY TRIGGERS table                                                                                #
#########################################################################################################
triggers <- read.csv("AnxietyTriggers.csv",  header=TRUE) # Read table (Change file name)
triggers1 <- triggers %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
triggers2 <- subset(triggers1, participantID %in% IDlist)

View(triggers2)
coronavirusClm = grep("coronavirus", colnames(triggers2))
tagClm = grep("tag", colnames(triggers2))
triggers3 <- triggers2[,-c(coronavirusClm, tagClm)] # Remove empty "coronavirus" and "tag" columns
triggers3 <- mutate(triggers3, dateTime = anytime(as.factor(triggers3$date))) # Create variable with consistent date format
dup <- triggers3[(duplicated(triggers3[, c("participantID","session")])),] # NONE

#########################################################################################################
# ASSESSING PROGRAM table                                                                               #
#########################################################################################################
assess <- read.csv("AssessingProgram.csv", sep="\t", header=TRUE) # Read table (Change file name)
assess1 <- assess %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
assess2 <- subset(assess1, participantID %in% IDlist) 

View(assess2)
tagClm = grep("tag", colnames(assess2))
assess3 <- assess2[,-c(tagClm)] # Remove empty "tag" column
assess3[assess3=="N/A"] <- NA
assess3 <- mutate(assess3, dateTime = anytime(as.factor(assess3$date))) # Create variable with consistent date format
dup <- assess3[(duplicated(assess3[, c("participantID","session")])),] # NONE

#########################################################################################################
# ATTRITION table                                                                                       #
#########################################################################################################

attrition <- read.csv("AttritionPrediction.csv",  header=TRUE) # Read ATTRITION table (Change file name)
att1 <- attrition %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
att2 <- subset(att1, participantID %in% IDlist)

View(att2)

#########################################################################################################
# BBSIQ table                                                                                           #
#########################################################################################################
BBSIQ <- read.csv("BBSIQ.csv",  header=TRUE) # Read table (Change file name)
BBSIQ1 <- BBSIQ %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
BBSIQ2 <- subset(BBSIQ1, participantID %in% IDlist)

View(BBSIQ2)
tagClm = grep("tag", colnames(BBSIQ2))
BBSIQ3 <- BBSIQ2[,-c(tagClm)] # Remove empty "tag" column
BBSIQ3 <- mutate(BBSIQ3, dateTime = anytime(as.factor(BBSIQ3$date))) # Create variable with consistent date format

dup <- BBSIQ3[(duplicated(BBSIQ3[, c("participantID","session")])),] # one case
BBSIQ3 <- BBSIQ3[!duplicated(BBSIQ3[, c("participantID","session")]), ]

#########################################################################################################
# COACH PROMPT table                                                                                    #
#########################################################################################################
coach <- read.csv("CoachPrompt.csv", header=TRUE) # Read table (Change file name)
coach1 <- coach %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
coach2 <- subset(coach1, participantID %in% IDlist)

View(coach2)
tagClm = grep("tag", colnames(coach2))
coach3 <- coach2[,-c(tagClm)] # Remove empty "tag" column
coach3 <- mutate(coach3, dateTime = anytime(as.factor(coach3$date))) # Create variable with consistent date format
dup <- coach3[(duplicated(coach3[, c("participantID","session")])),] # NONE

#########################################################################################################
# COMPARE AND CONTRAST table                                                                            #
#########################################################################################################
CC <- read.csv("CC.csv", header=TRUE) # Read table (Change file name)
CC1 <- CC %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
CC2 <- subset(CC1, participantID %in% IDlist)

View(CC2)
tagClm = grep("tag", colnames(CC2))
CC3 <- CC2[,-c(tagClm)] # Remove empty "tag" column
CC3 <- mutate(CC3, dateTime = anytime(as.factor(CC3$date))) # Create variable with consistent date format
dup <- CC3[(duplicated(CC3[, c("participantID","session")])),] # NONE

#########################################################################################################
# CREDIBILITY table                                                                                     #
#########################################################################################################
cred <- read.csv("Credibility.csv",  header=TRUE) # Read table (Change file name)
cred1 <- cred %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
cred2 <- subset(cred1, participantID %in% IDlist)

View(cred2)
tagClm = grep("tag", colnames(cred2))
cred2 <- cred2[,-c(tagClm)] # Remove empty "tag" column
cred2 <- mutate(cred2, dateTime = anytime(as.factor(cred2$date))) # Create variable with consistent date format
dup <- cred2[(duplicated(cred2[, c("participantID","session")])),] # two cases
cred3 <- cred2[!duplicated(cred2[, c("participantID","session")]), ]

#########################################################################################################
# COMORBID table                                                                                        #
#########################################################################################################
comor <- read.csv("Comorbid.csv", header=TRUE) # Read table (Change file name)
comor1 <- comor %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
comor2 <- subset(comor1, participantID %in% IDlist)

View(comor2)
tagClm = grep("tag", colnames(comor2))
comor2 <- comor2[,-c(tagClm)] # Remove empty "tag" column
comor2 <- mutate(comor2, dateTime = anytime(as.factor(comor2$date))) # Create variable with consistent date format
dup <- comor2[(duplicated(comor2[, c("participantID","session")])),] # NONE

#########################################################################################################
# DASS ANXIETY table                                                                                    #
#########################################################################################################
dassa <- read.csv("DASS21_AS.csv", sep="\t", header=TRUE) # Read table (Change file name)
dassa1 <- dassa %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
dassa2 <- subset(dassa1, participantID %in% IDlist)

View(dassa2)
tagClm = grep("tag", colnames(dassa2))
dassa2 <- dassa2[,-c(tagClm)] # Remove empty "tag" column
dassa2$over18[dassa2$over18=="N/A"] <- NA
dup <- dassa2[(duplicated(dassa2[, c("participantID","session")])),] # 124 instances of duplication

eligible <- filter(dassa2, session == "ELIGIBLE")
eligible2 <- eligible[!rev(duplicated(rev(eligible[, c("participantID")]))),]
other <- filter(dassa2, session != "ELIGIBLE")
dassa3 <- rbind(eligible2, other)

dassa32 <- mutate(dassa3, dateTime = anytime(as.factor(dassa3$date))) # Create variable with consistent date format

dup <- dassa3[(duplicated(dassa3[, c("participantID","session")])),] #NONE

#########################################################################################################
# DEMOGRAPHICS table                                                                                    #
#########################################################################################################

dem <- read.csv("Demographics.csv",  header=TRUE) # Read DEMOGRAPHICS table (Change file name)
dem1 <- dem %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
dem2 <- subset(dem1, participantID %in% IDlist)

View(dem2)
tagClm = grep("tag", colnames(dem2))
dem2 <- dem2[,-c(tagClm)] # Remove empty "tag" column
dem2[dem2==""] <- NA
dem2 <- mutate(dem2, dateTime = anytime(as.factor(dem2$date))) # Create variable with consistent date format

dup <- dem2[(duplicated(dem2[, c("participantID")])),] # NONE

#########################################################################################################
# EMAIL LOG table                                                                                       #
#########################################################################################################

email <- read.csv("EmailLog.csv", header = TRUE) # Read EMAIL LOG data (Change file name)
email1 <- email %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
email2 <- subset(email1, participantID %in% IDlist)

View(email2)
email2[email2==""] <- NA
email2 <- mutate(email2, dateTime = anytime(as.factor(email2$dateSent))) # Create variable with consistent date format

#########################################################################################################
# EVALUATION table                                                                                      #
#########################################################################################################
eval <- read.csv("Evaluation.csv", header=TRUE) # Read table (Change file name)
eval1 <- eval %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
eval2 <- subset(eval1, participantID %in% IDlist)

View(eval2)

eval2[eval2==""] <- NA
eval2[eval2=="N/A"] <- NA
eval2[eval2==""] <- NA
eval2 <- mutate(eval2, dateTime = anytime(as.factor(eval2$date))) # Create variable with consistent date format

dup <- eval2[(duplicated(eval2[, c("participantID","session")])),] # NONE

#########################################################################################################
# HELP SEEKING table                                                                                    #
#########################################################################################################

help <- read.csv("HelpSeeking.csv", header=TRUE) # Read table (Change file name)
help1 <- help %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
help2 <- subset(help1, participantID %in% IDlist)

View(help2)
dem2 <- dem2[,-c(15)] # Remove empty "tag" column
help2[help2==""] <- NA
help2[help2=="N/A"] <- NA
help3 <-mutate(help2, otherChange = ifelse(is.na(other),"false","true"))
help3 <- mutate(affect2, dateTime = anytime(as.factor(help3$date))) # Create variable with consistent date format

dup <- help2[(duplicated(help2[, c("participantID","session")])),] # NONE

#########################################################################################################
# JS PSYCH TRIAL table                                                                                  #
#########################################################################################################

js <- read.csv("JsPsychTrial.csv", header=TRUE) # Read table (Change file name)
js1 <- js %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
js2 <- subset(js1, participantID %in% IDlist)

View(js2)
tagClm = grep("tag", colnames(js2))
js2 <- js2[,-c(tagClm)] # Remove empty "tag" column
js2 <- mutate(js2, dateTime = anytime(as.factor(js2$date))) # Create variable with consistent date format

#########################################################################################################
# MECHANISMS table                                                                                      #
#########################################################################################################

mech <- read.csv("Mechanisms.csv", header=TRUE) # Read table (Change file name)
mech1 <- mech %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
mech2 <- subset(mech1, participantID %in% IDlist)

View(mech2)
tagClm = grep("tag", colnames(mech2))
mech2 <- mech2[,-c(tagClm)] # Remove empty "tag" column
mech2 <- mutate(mech2, dateTime = anytime(as.factor(mech2$date))) # Create variable with consistent date format

dup <- mech2[(duplicated(mech2[, c("participantID","session")])),] # NONE

#########################################################################################################
# MENTAL HEALTH HISTORY table                                                                           #
#########################################################################################################

mhh <- read.csv("MentalHealthHistory.csv", header=TRUE) # Read table (Change file name)
mhh1 <- mhh %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
mhh2 <- subset(mhh1, participantID %in% IDlist)

View(mhh2)
tagClm = grep("tag", colnames(mhh2))
mhh2 <- mhh2[,-c(tagClm)] # Remove empty "tag" column
mhh2[mhh2==""] <- NA
mhh2[mhh2=="N/A"] <- NA
mhh2 <- mutate(mhh2, dateTime = anytime(as.factor(mhh2$date))) # Create variable with consistent date format

dup <- mhh2[(duplicated(mhh2[, c("participantID","session")])),] # NONE

#########################################################################################################
# OASIS table                                                                                           #
#########################################################################################################

OA <- read.csv("OA.csv",header=TRUE) # Read table (Change file name)
OA1 <- OA %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
OA2 <- subset(OA1, participantID %in% IDlist)

View(OA2)
tagClm = grep("tag", colnames(OA2))
OA2 <- OA2[,-c(tagClm)] # Remove empty "tag" column
OA2 <- mutate(OA2, dateTime = anytime(as.factor(OA2$date))) # Create variable with consistent date format
dup <- OA2[(duplicated(OA2[, c("participantID","session")])),] # one case of duplication
OA3 <- OA2[!duplicated(OA2[, c("participantID","session")]), ]

#########################################################################################################
# REASONS FOR ENDING table                                                                              #
#########################################################################################################

reas <- read.csv("ReasonsForEnding.csv", header = TRUE) # Read EMAIL LOG data (Change file name)
reas1 <- reas %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
reas2 <- subset(reas1, participantID %in% IDlist)

View(reas2)
tagClm = grep("tag", colnames(reas2))
reas2 <- reas2[,-c(tagClm)] # Remove empty "tag" column
reas2[reas2==""] <- NA
reas2[reas2=="N/A"] <- NA
reas2 <- mutate(reas2, dateTime = anytime(as.factor(reas2$date))) # Create variable with consistent date format

#########################################################################################################
# RECOGNITION RATINGS table                                                                             #
#########################################################################################################

RR <- read.csv("RR.csv",header=TRUE) # Read RR table (Change file name)
RR1 <- RR %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
RR2 <- subset(RR1, participantID %in% IDlist)

View(RR2)
tagClm = grep("tag", colnames(RR2))
RR2 <- RR2[,-c(tagClm)] # Remove empty "tag" column
RR2 <- mutate(RR2, dateTime = anytime(as.factor(RR2$date))) # Create variable with consistent date format
dup <- RR2[(duplicated(RR2[, c("participantID","session")])),] #NONE

#########################################################################################################
# RETURN INTENTION table                                                                                #
#########################################################################################################

return <- read.csv("ReturnIntention.csv", header=TRUE) # Read RETURN INTENTION table (Change file name)
return1 <- return %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
return2 <- subset(return1, participantID %in% IDlist)

View(return2)
tagClm = grep("tag", colnames(return2))
return2 <- return2[,-c(tagClm)] # Remove empty "tag" column
return2$daysTillReturning[return2$daysTillReturning=="-6926"] <- NA 
return2$daysTillReturning[return2$daysTillReturning=="-6930"] <- NA
return2[return2==""] <- NA
return2 <- mutate(return2, dateTime = anytime(as.factor(return2$date))) # Create variable with consistent date format

dup <- return2[(duplicated(return2[, c("participantID","session")])),] # one case of duplication
return3 <- return2[!duplicated(return2[, c("participantID","session")]), ]
dup <- return3[(duplicated(return3[, c("participantID","session")])),] # one case of duplication

#########################################################################################################
# SESSION REVIEW table                                                                                  #
#########################################################################################################

review <- read.csv("SessionReview.csv", header=TRUE) # Read table (Change file name)
review1 <- review %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
review2 <- subset(review1, participantID %in% IDlist)

View(review2)
tagClm = grep("tag", colnames(review2))
review2 <- review2[,-c(tagClm)] # Remove empty "tag" column
review2[review2==""] <- NA
review2[review2=="N/A"] <- NA
review2 <- mutate(review2, dateTime = anytime(as.factor(review2$date))) # Create variable with consistent date format

dup <- review2[(duplicated(review2[, c("participantID","session")])),] #NONE

#########################################################################################################
# TASK LOG table                                                                                        #
#########################################################################################################

tasklog <- read.csv("TaskLog.csv", header=TRUE) # Read table (Change file name)
tasklog1 <- tasklog %>% select(systemID = study_id,everything()) # Change ID variable name to match PARTICIPANT table
merge <- left_join(tasklog1, systemID_match, by="systemID") # Merge the two tables to match participant and system IDs
tasklog2 <- subset(merge, participantID %in% IDlist)
tasklog2 <- tasklog2 %>% select(participantID,everything())

View(tasklog2)
dup <- tasklog2[(duplicated(tasklog2[, c("participantID","sessionName","taskName","tag")])),] # 129 instances of duplication (all accounted for in this script)
tasklog3 <- tasklog2[!duplicated(tasklog2[, c("participantID","sessionName","taskName","tag")]), ] 

#########################################################################################################
# TECHNOLOGY USE table                                                                                  #
#########################################################################################################

tech <- read.csv("TechnologyUse.csv",header=TRUE) # Read table (Change file name)
tech1 <- tech %>% select(participantID = participant,everything()) # Change ID variable name to match PARTICIPANT table
tech2 <- subset(tech1, participantID %in% IDlist)

View(tech2)
tagClm = grep("tag", colnames(tech2))
tech2 <- tech2[,-c(tagClm)] # Remove empty "tag" column
tech2 <- mutate(tech2, dateTime = anytime(as.factor(tech2$date))) # Create variable with consistent date format
dup <- tech2[(duplicated(tech2[, c("participantID","session")])),] # NONE

#########################################################################################################
# WELLNESS table                                                                                        #
#########################################################################################################

well <- read.csv("Wellness.csv", header=TRUE) # Read table (Change file name)
well1 <- well %>% select(participantID = participant_id,everything()) # Change ID variable name to match PARTICIPANT table
well2 <- subset(well1, participantID %in% IDlist)

View(well2)
tagClm = grep("tag", colnames(well2))
well2 <- well2[,-c(tagClm)] # Remove empty "tag" column
well2 <- mutate(well2, dateTime = anytime(as.factor(well2$date))) # Create variable with consistent date format

dup <- well2[(duplicated(well2[, c("participantID","session")])),] #NONE
