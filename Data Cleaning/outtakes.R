# ---------------------------------------------------------------------------- #
# Outtakes
# Author: Sonia Baee
# ---------------------------------------------------------------------------- #

# This code, drafted by Sonia Baee, was removed from "3_clean_data.R" as it was 
# decided that these reports will not be part of centralized cleaning. The code
# has not been checked and may no longer work.

# ---------------------------------------------------------------------------- #
# Report response ranges ----
# ---------------------------------------------------------------------------- #

# The range of each item in the table is stored in the dat_summary

dat_summary <- lapply(participant_dat, summary)
for (i in 1:length(no_duplicated_dat)) {
  assign(paste(paste("df", i, sep = ""), "summary", sep = "."), dat_summary[[i]])
}

dat_summary

# TODO: See "return_intention" past "return_date"

unique(as.vector(as.matrix(dat$return_intention[, "days_till_returning"])))

View(dat$return_intention[dat$return_intention$participant_id == 1265, ])

# ---------------------------------------------------------------------------- #
# Report instances of "prefer not to answer" ----
# ---------------------------------------------------------------------------- #

# Prefer not to answer coding for each table
# pna = -1 or 555
# This function return the participant/system Ids of the row with prefer not to
# answer value for each table

get_ids_with_pna <- function(df, pna = 555) {
  tmp_df <- df[, -which(names(df) %in% c("participantID", 
                                         "systemID", 
                                         "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(col == pna)))
  idx_list <- list()
  cnt_idx <- 1
  par_id_list <- list()
  cnt1 <- 1
  sys_id_list <- list()
  cnt2 <- 1
  for (col in tmp_cols) {
    for (idx in col) {
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if (length(idx_list) != 0) {
    if ("participantID" %in% colnames(df)) {
      for (idx in idx_list) {
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if ("systemID" %in% colnames(df)) {
      for (idx in idx_list) {
        sys_id_list[[cnt1]] <- df[idx, ]$systemID
        cnt1 <- cnt1 + 1
      }
      sys_id_list <- unlist(sys_id_list, recursive = FALSE)
      sys_id_list <- sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
  }
  # else{
  #   return(cat("\nNo entries with prefer not to answer = ", pna, " found!\n"))
  # }
}

ids_with_pna <- lapply(participant_dat, get_ids_with_pna)
ids_with_pna

# ---------------------------------------------------------------------------- #
# Report missing data ----
# ---------------------------------------------------------------------------- #

# This function return the participant/system Ids with null values in each table

get_ids_with_missing <- function(df) {
  tmp_df <- df[, -which(names(df) %in% c("participantID", 
                                         "systemID", 
                                         "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(is.na(col))))
  
  # "idx_list" is a list of row index that has null value
  
  idx_list <- list()
  cnt_idx <- 1
  
  # "par_id_list" is a list of participant ids with the null value
  
  par_id_list <- list()
  cnt1 <- 1
  
  # "sys_id_list" is a list of system ids with the null value
  
  sys_id_list <- list()
  cnt2 <- 1
  for (col in tmp_cols) {
    for (idx in col) {
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if (length(idx_list) != 0) {
    if ("participantID" %in% colnames(df)) {
      for (idx in idx_list) {
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if ("systemID" %in% colnames(df)) {
      for (idx in idx_list) {
        sys_id_list[[cnt1]] <- df[idx, ]$systemID
        cnt1 <- cnt1 + 1
      }
      sys_id_list <- unlist(sys_id_list, recursive = FALSE)
      sys_id_list <- sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
  }
  # else{
  #   return(cat("\nNo entries with missing values found!\n"))
  # }
}

ids_with_missing <- lapply(participant_dat, get_ids_with_missing )
ids_with_missing

# ---------------------------------------------------------------------------- #
# Report task completion ----
# ---------------------------------------------------------------------------- #

#---------------------------
# Create an object with the number of tasks that should be done per session

number_of_tasks <- c(2, 14, 8, 5)
names(number_of_tasks) <- c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks # e.g., session eligibility should have 2 different tasks
#---------------------------
# "session_task_check" function, return if the participant complete a session or 
# it is in the middle of the session

session_task_check <- function(df, session_name) {
  tmp <- ddply(df, 
               ~systemID = session, 
               summarise, 
               number_of_distinct_tasks = length(unique(task_name)))
  tmp2 <- filter(tmp, session == session_name)
  tmp2$stage <- NULL
  tmp2 <- transform(tmp2, 
                    stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], 
                                   "completed", 
                                   "middle"))
  return(tmp2)
}
#---------------------------
# The second argument can be any session name of the study
# We can use this "number_of_distinct_task_for_session" variable to make sure 
# participant didn't skip any tasks

number_of_distinct_task_for_session <- 
  session_task_check(participant_dat$taskLog, "preTest")
number_of_distinct_task_for_session
#---------------------------

# ---------------------------------------------------------------------------- #
# Report dropout ----
# ---------------------------------------------------------------------------- #

#---------------------------
# Dropout
# Claudia was using "current_task_index". I didn't find any documentation for that!

tmp <- filter(dat$taskLog, 
              task_name == "SESSION_COMPLETE" & systemID %in% participantIDs)
View(tmp)
#---------------------------
lastSessionComp <- aggregate(tmp[, c("session", "task_name")], 
                             list(tmp$systemID), 
                             tail, 
                             1)
names(lastSessionComp) <- c("systemID", "session", "task_name")
#---------------------------
participant_lastSession <- left_join(dat$participant, 
                                     lastSessionComp, 
                                     by = "systemID")
participant_lastSession <- participant_lastSession[, c("participantID", 
                                                       "systemID", 
                                                       "active", 
                                                       "session", 
                                                       "last_login_date", 
                                                       "email_reminders", 
                                                       "phone_reminders")]
View(participant_lastSession)
#---------------------------
participant_lastSession$date <- as.Date(participant_lastSession$last_login_date, 
                                        format = "%Y-%m-%d %H:%M:%S")
participant_lastSession$dayDiff <- difftime(now(), 
                                            participant_lastSession$date, 
                                            units = c("days"))
#---------------------------
problematicUsers <- filter(participant_lastSession, 
                           (active == 1) & (session != "PostFollowUp") & (dayDiff > 21))
View(problematicUsers)

#---------------------------
# In the middle of fifth session

View(filter(dat$participant, participantID == 412))
View(filter(dat$taskLog, systemID == 412))

# Completed the fifth session but not follow-up

View(filter(dat$participant, participantID == 577))
View(filter(dat$taskLog, systemID == 577)) # Evaluation and assessing program are not done