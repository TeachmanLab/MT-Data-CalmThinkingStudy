# ---------------------------------------------------------------------------- #
# Deidentify Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script, set your working directory and put the raw data
# obtained from "1_get_raw_data.ipynb" in a folder called "data/raw". The 
# script will import the raw data from that folder and, for files that require
# redaction, output the redacted files into the same folder with "-redacted"
# appended to the file name. The unredacted file can then be manually deleted.

# Rather than changing the structure of the raw data files, the present script 
# is designed to label instances of redaction with "REDACTED". Because it does
# not change the structure of the raw data files, the present script should run
# on redacted raw data files in addition to unredacted raw data files. In this
# way, the script reflects exactly what was redacted.

# Scope: This script is based on data dumped from "calm" database on the Data
# Server ("teachmanlab") on 12/3/2020. The script may need to be updated when
# applied to data downloaded after this date, as there may have been changes
# to the database or newly collected data not accounted for by this script.

# ---------------------------------------------------------------------------- #
# Store working directory, install correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Run the code below to ensure you are running the same version of R used at the
# time the script was written. If you need to install a previous version, go to
# https://cran.r-project.org/bin/windows/base/old/

script_R_version <- "R version 4.0.3 (2020-10-10)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

# Load packages using "groundhog", which installs and loads the most recent
# versions of packages available on the specified date ("groundhog_day"). This 
# is important for reproducibility so that everyone running the script is using
# the same versions of packages used at the time the script was written.

# Note that packages may take longer to load the first time you load them with
# "groundhog.library". This is because you may not have the correct versions of 
# the packages installed based on the "groundhog_day". After "groundhog.library"
# automatically installs the correct versions alongside other versions you may 
# have installed, it will load the packages more quickly.

# If in the process of loading packages with "groundhog.library" for the first 
# time the console states that you first need to install "Rtools", follow steps 
# here (https://cran.r-project.org/bin/windows/Rtools/) for installing "Rtools" 
# and putting "Rtools" on the PATH. Then try loading the packages again.

library(groundhog)
groundhog_day <- "2021-01-01"

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import raw data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of raw CSV data files

raw_data_dir <- paste0(wd_dir, "/data/raw")
filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# Import data files and store them in a list

dat <- lapply(paste0(raw_data_dir, "/", filenames), read.csv)

# Name each data file in the list

split_char <- "-"
names(dat) <- unlist(lapply(filenames, 
                             function(f) {
                               unlist(strsplit(f, 
                                               split = split_char, 
                                               fixed = FALSE))[1]
                             }
))

# Report the names of the imported tables

cat("Imported tables: ")
names(dat)

# ---------------------------------------------------------------------------- #
# Deidentify Data ----
# ---------------------------------------------------------------------------- #

# Before doing any data cleaning, confirm that the raw data are deidentified so
# that subsequent cleaning scripts can be run on a deidentified dataset that can
# be shared and made public. This way all data cleaning steps can be transparent
# and reproducible without compromising identifiable information.

# ---------------------------------------------------------------------------- #
# Determine which columns contain identifiers ----
# ---------------------------------------------------------------------------- #

# Regarding various dates, Bethany indicated on 1/13/2021 that MindTrails is not
# subject to HIPAA regulations, so restrictions on dislcosing dates of service, 
# dates of test measures, and birth years for people over age 89 do not apply.

# TODO: Manually inspect columns and determine which ones may have identifiers

# Consider "button_pressed" column of "angular_training" for rows in which
# "trial_type" is "FillInBlank". No identifiers are present for rows in which
# "trial_type" is "Countdown", "Intro", "MissingLetter", "page", "Question",
# "RandomNonPreviousStatement", "Slider", or "ThoughtBubble".

# 1. Some rows are participant descriptions of an anxious situation for the Use 
# Your Imagination task at "firstSession" before starting training

rows1 <- dat$angular_training[dat$angular_training$trial_type == 
                                "FillInBlank" &
                                (dat$angular_training$step_title == 
                                   "Use Your Imagination" |
                                   dat$angular_training$step_title == 
                                   "Use your Imagination"), ]

# Note that issues regarding entries (a) for unexpected values of "conditioning" 
# and (b) at sessions other than "firstSession" will be addressed in later data 
# cleaning. For now, we focus on deidentification.

table(rows1$conditioning)
table(rows1$session)

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

# 3. Henry Behan said that this criterion reflects participants' responses to 
# the Quick Thinking Exercise (also called Flexible Thinking Exercise). 

rows3 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "flex_thinking_explanations", ]

table(rows3$trial_type)

# Note that later data cleaning will address these issues: (a) not all rows have 
# "step_title" of "Exercise: Quick Thinking" due to a programming error, (b) some 
# Calm Thinking participants appear to have completed this task, and (c) for 
# "CONTROL" participants, "session" is populated by "flexible_thinking", so the
# session number is unclear.

table(rows3$step_title)

table(rows3$conditioning)

table(rows3$session)
table(rows3[rows3$session == "flexible_thinking", ]$conditioning)

# 4. Henry Behan said these criteria reflect scenarios created by participants 
# in the Write Your Own Scenario exercise in the "TRAINING_CREATE" condition of 
# the TET study.

rows4_training_create <- dat$angular_training[dat$angular_training$trial_type == 
                                                "FillInBlank" &
                                                dat$angular_training$conditioning == 
                                                  "TRAINING_CREATE" &
                                                dat$angular_training$stimulus_name == 
                                                  "" &
                                                dat$angular_training$step_title == 
                                                  "", ]

# However, one admin/test account in "CONTROL" also appears to have completed 
# this task, so we cannot restrict to "TRAINING_CREATE" for deidentification.

rows4_all_conditions <- dat$angular_training[dat$angular_training$trial_type == 
                                               "FillInBlank" &
                                               dat$angular_training$stimulus_name == 
                                                 "" &
                                               dat$angular_training$step_title == 
                                                 "", ]

table(rows4_all_conditions$conditioning)

# 5. Henry Behan said this criterion reflects participants' explanations as to 
# why the they created occurred in the Write Your Own Scenario exercise in the
# "TRAINING_CREATE" condition of the TET study

rows5 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "training_create_explanations", ]

table(rows5$trial_type)

# Confirm no rows remain unaccounted for

ignored_ids <- c(rows1$id, rows2$id, rows3$id, rows4_all_conditions$id, rows5$id)

remaining <- dat$angular_training[!(dat$angular_training$id %in% ignored_ids) &
                                    dat$angular_training$trial_type == 
                                      "FillInBlank", ]

nrow(remaining) == 0





# Consider "compare_to_others" and "worth_per_week" of "assessing_program"

# Consider "difficult_to_understand", "other_feedback", and 
# "technical_difficulties" of "coach_prompt"

# Consider "news" and "social_media" of "covid19"

# Consider "ptp_reason_other" in "demographics"

# Consider "other_place", "other_reason_control", "problems_desc",
# "other_coaching", and "other_help_topic" in "evaluation"

# Consider "other" in "help_seeking"

# Consider "change_help_text", "help_other_text", "other_disorder", and
# "other_why_no_help" in "mental_health_history"

# Consider "end_other_desc", "change_med_desc", "control_desc", and 
# "location_desc" in "reasons_for_ending"

# Consider "not_return_reasons" in "return_intention"

# Consider "other_distraction" and "other_location_desc" in "session_review"





# Regarding "user_agent" in "task_log", Dan Funk said on 1/13/2021 that it is 
# not identifiable. Retain it.

# Regarding "session_id" in "dass21_as" and "oa", Dan Funk said on 1/13/2021
# that it is transient, encrypted by HTTPS, and used to index participants
# before they create an account and get assigned a "participant_id". Retain it.





# TODO: Asked Dan for report of free-response columns on 1/11/20.





# TODO: Redact "order_id" in "gift_log" (and in "error" column of "import_log")
# for security reasons





# TODO: Redact "email", "full_name", "password", and "phone" in "participant" 
# for admin and test accounts that have this data.

View(dat$participant[dat$participant$email != "" |
                       dat$participant$full_name != "" |
                       dat$participant$password != "" |
                       !is.na(dat$participant$phone), ])





# ---------------------------------------------------------------------------- #
# Remove phone numbers from "sms_log" ----
# ---------------------------------------------------------------------------- #

# Remove phone numbers from "exception" column of "sms_log"

ignored_values <- c("A 'To' phone number is required.",
                    "Authenticate",
                    "The message From/To pair violates a blacklist rule.")

temp <- dat$sms_log[dat$sms_log$exception != "" &
                      !(dat$sms_log$exception %in% ignored_values), ]
temp <- temp[order(temp$exception), ]
View(temp)

dat$sms_log[grepl("Permission to send an SMS has not been enabled for the region indicated by the 'To' number:", 
                  dat$sms_log$exception), ]$exception <-
  "Permission to send an SMS has not been enabled for the region indicated by the 'To' number: [REDACTED]"

dat$sms_log[grepl("The 'To' number", 
                  dat$sms_log$exception) &
               grepl("is not a valid phone number.", 
                     dat$sms_log$exception), ]$exception <-
  "The 'To' number [REDACTED] is not a valid phone number."

dat$sms_log[grepl("To number", dat$sms_log$exception) &
               grepl("is not a mobile number", 
                     dat$sms_log$exception), ]$exception <- 
  "To number: [REDACTED], is not a mobile number"

deidentified_values <- 
  c("Permission to send an SMS has not been enabled for the region indicated by the 'To' number: [REDACTED]",
    "The 'To' number [REDACTED] is not a valid phone number.",
    "To number: [REDACTED], is not a mobile number")

temp2 <- dat$sms_log[dat$sms_log$exception != "" &
                       !(dat$sms_log$exception %in% ignored_values) &
                       !(dat$sms_log$exception %in% deidentified_values), ]
temp2 <- temp2[order(temp2$exception), ]
View(temp2)

# ---------------------------------------------------------------------------- #
# Save deidentified data ----
# ---------------------------------------------------------------------------- #

# TODO: Handle this better for the case that "-redacted.csv" is already in the
# file name. Currently it would save the file as "-redacted-redacted.csv".
# Also, note that it currently also saves the column X named by R.





# Save redacted "sms_log". Remember to manually delete the original file.

sms_log_redacted_filename <- paste0(gsub("*.csv", "",
                                         filenames[grep("sms_log", filenames)]),
                                    "-redacted.csv")

write.csv(dat$sms_log, paste0("./data/raw/", sms_log_redacted_filename),
          row.names = FALSE)