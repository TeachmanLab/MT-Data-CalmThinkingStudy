# ---------------------------------------------------------------------------- #
# Outtakes - Clean "angular_training" table
# Author: Jeremy Eberle
# ---------------------------------------------------------------------------- #

# This code, drafted by Jeremy Eberle, was removed from "4_clean_data.R" as it was 
# decided that cleaning "angular_training" table will not be part of centralized 
# cleaning. The code is incomplete and has not been checked for accuracy.

# ---------------------------------------------------------------------------- #
# Clean "angular_training" table ----
# ---------------------------------------------------------------------------- #

# TODO: Check on the following potential issues identified in the process of
# checking that data are deidentified. Also consider adding an indicator for
# each kind of row below and clarifying what session the Recognition Ratings
# and Quick Thinking (also called Flexible Thinking) Exercise were done. Note
# that some of the rows below may not arise in the Calm Thinking Study, but
# still check for whether they are present.

# Create new columns for sorting rows based on the tasks they correspond to

dat$angular_training$task <- NA
dat$angular_training$subtask <- NA
dat$angular_training$subtask_detail <- NA

# TODO: Label rows for Anxious Imagery Prime task

dat$angular_training$task[dat$angular_training$trial_type == "FillInBlank" &
                            (dat$angular_training$step_title %in%
                               c("Use Your Imagination", "Use your Imagination"))] <- 
  "anx_imag_prime"





# 1. Some rows are participant descriptions of an anxious situation for the Use 
# Your Imagination task at "firstSession" before starting training

rows1 <- dat$angular_training[dat$angular_training$trial_type == "FillInBlank" &
                                (dat$angular_training$step_title %in%
                                   c("Use Your Imagination", "Use your Imagination")), ]

table(rows1$conditioning) # TODO: Why are there rows for conditions other than 
# those below? Asked Henry 1/14/2021. He said it looks
# like the table may have been pulling the incorrect
# condition. See if this is still an issue after you
# check the conditioning variable above.

expected_conditions <- c("TRAINING",
                         "TRAINING_ORIG", "TRAINING_30", "TRAINING_CREATE", 
                         "TRAINING_ED")

question1 <- rows1[!(rows1$conditioning %in% expected_conditions), ]
View(question1)
write.csv(question1, "./temp_cleaning/angular_training_question1.csv", 
          row.names = FALSE)





# 2. Some rows are participant responses for training scenarios at "fifthSession"
# that required filling in a blank (vs. completing a word fragment). Prior to
# 2/15/2019, these responses were indexed not with a "step_title" value of 
# "scenario", but with a "step_title" value of the scenario's title, which 
# subsequently was stored in "stimulus_name". Among the scenario titles prior 
# to this change was "pub"; after this change, "pub" was renamed to "bar".

scenario_titles <- 
  c(unique(dat$angular_training[dat$angular_training$step_title == 
                                  "scenario", ]$stimulus_name), "pub")

rows2 <- dat$angular_training[dat$angular_training$trial_type == "FillInBlank" &
                                (dat$angular_training$step_title == "scenario" |
                                   dat$angular_training$step_title %in% 
                                   scenario_titles), ]

table(rows2$conditioning)
table(rows2$session_and_task_info)

# TODO: Label rows for Quick Thinking Exercise

dat$angular_training$task[dat$angular_training$stimulus_name == 
                            "flex_thinking_explanations"] <- "quick_thinking"





# 3. Henry says that this criterion reflects participants' responses to the Quick
# Thinking Exercise (also called Flexible Thinking Exercise). Also see his email
# "MT Flex Thinking data for control pps" on 9/27/21 for a draft cleaning script.

rows3 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "flex_thinking_explanations", ]

# TODO: Not all rows have "step_title" of "Exercise: Quick Thinking" due to a
# programming error.

table(rows3$step_title)





table(rows3$conditioning) # TODO: Some Calm Thinking Participants seem to have 
# gotten this. Check this.

View(rows3[rows3$participant_id %in% dat$study[dat$study$study_extension == 
                                                 "", ]$participant_id, ])





table(rows3$session_and_task_info) # TODO: For "CONTROL" participants, the "session" 
# column is populated by "flexible_thinking", so the session is
# unclear. Consider clarifying the session.

table(rows3[rows3$session_and_task_info == "flexible_thinking", ]$conditioning)
table(rows3[rows3$conditioning != "CONTROL", ]$session_and_task_info)

table(rows3$step_title)
table(rows3$trial_type)

View(dat$angular_training[dat$angular_training$conditioning == "CONTROL", ])
View(dat$angular_training[dat$angular_training$conditioning != "CONTROL", ])





# # 4. Henry Behan said these criteria reflect scenarios created by participants 
# in the Write Your Own Scenario exercise in the "TRAINING_CREATE" condition of 
# the TET study. No participants completed this in Calm Thinking.

rows4_all_conditions <- dat$angular_training[dat$angular_training$trial_type == 
                                               "FillInBlank" &
                                               dat$angular_training$stimulus_name == 
                                               "" &
                                               dat$angular_training$step_title == 
                                               "", ]
nrow(rows4_all_conditions)





# 5. Henry Behan said this criterion reflects participants' explanations as to 
# why the they created occurred in the Write Your Own Scenario exercise in the
# "TRAINING_CREATE" condition of the TET study. No participants completed this
# in Calm Thinking.

rows5 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "training_create_explanations", ]
nrow(rows5)

# Confirm no rows remain unaccounted for

ignored_ids <- c(rows1$id, rows2$id, rows3$id, rows4_all_conditions$id, rows5$id)

remaining <- dat$angular_training[!(dat$angular_training$id %in% ignored_ids) &
                                    dat$angular_training$trial_type == 
                                    "FillInBlank", ]

nrow(remaining) == 0

# 6. For indicating the session at which Recognition Ratings were completed, 
# see Henry's email "MT Flex Thinking data for control pps" on 9/30/21 for a 
# draft cleaning script.

# Create new column for session-only information

dat$angular_training$session_only <- 
  ifelse(dat$angular_training$session_and_task_info %in%
           c("firstSession", "secondSession", "thirdSession", "fourthSession", 
             "fifthSession"),
         dat$angular_training$session_and_task_info,
         NA)

# TODO: Label rows for Recognition Ratings

dat$angular_training$task[dat$angular_training$session_and_task_info == 
                            "Recognition Ratings"] <- "recognition_ratings"





# TODO: Determine session. Revise so that "readinessHeader" isn't just NA

for (i in 1:nrow(dat$angular_training)) {
  if (dat$angular_training$task[i] %in% "recognition_ratings") {
    if (dat$angular_training$stimulus_name[i] == "readinessHeader") {
      dat$angular_training$session_only[i] <- NA
    } else {
      if (dat$angular_training$session_index[i] == 0) {
        dat$angular_training$session_only[i] <- "preTest"
      } else if (dat$angular_training$session_index[i] == 3) {
        dat$angular_training$session_only[i] <- "thirdSession"
      } else if (dat$angular_training$session_index[i] == 5) {
        dat$angular_training$session_only[i] <- "fifthSession"
      } else if (dat$angular_training$session_index[i] == 6) {
        dat$angular_training$session_only[i] <- "PostFollowUp"
      } else if (dat$angular_training$session_index[i] == 7) {
        dat$angular_training$session_only[i] <- "PostFollowUp2"
      } 
    }
  }
}





# TODO: Continue reviewing Henry's code with Line 55

View(dat$angular_training[dat$angular_training$task %in% "recognition_ratings", ])

angular3$date <- format(as.Date(strptime(angular3$date, '%Y-%m-%d %H:%M')), "%x")

ratings2 <- filter(ratings, participant_id > 2272)

ratings2 <- ratings2 %>% 
  group_by(participant_id) %>%
  mutate(time_no = as.numeric(as.factor(date)))

if(any(ratings2$time_no > 4)) {
  "TRUE"
} else {
  "FALSE"
}




