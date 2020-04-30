# R01-Data

This repository is the knowledge base for the MindTrails Calm Thinking Study (R01) dataset.

For more information about the Calm Thinking study, see the MindTrails Wiki.

If you are a researcher who wants to contribute to this project, please contact Henry Behan at hb7zz@virginia.edu or Claudia Calicho-Mamani at cpc4tz@virginia.edu. Thanks!

---

## Goal
- The goal of this document is to have a record of what we need in terms of data cleaning steps and modeling techniques.

## Proposed Main steps of the data cleaning
- [X] Name convention of ID column (participantID)
- [X] Actual IDs
- [X] Remove test and admin accounts
- [X] Check the values of ID
    - Take care of jump in the values of IDs
- [X] labeled IDs based on the timeline of the study (e.g., soft-launch, actual launch)
- [X] Remove duplicate values
    - Delete the same values
    - Keep the very last entry
- [X] Check the missing values
    - Blank, n/a, na, NA
- [X] Check the prefer answer value (e.g., 555, -1)
- [X] Check the value of each measurement
    - Visualize the value range of each column if we have outlier (e.g. R34 has a value that shouldn’t be there)
      - As an example, in the return intention table, we have a negative value without any explanation
      - Solution: fill them out with the null value
- [X] Time conversion based on participants’ timezone
- [X] Column shifting (e.g., sometimes the entry’s value shift in some tables)

## Projects
- The name and hypothesis of each project
  - Which algorithms are we planning to use
  - Which data do we need
  - Select the related tables
  - Follow the main steps of the data cleaning process
  - Does this project need special data cleaning?
    - if yes, have a record of them and if it a general issue add it to the data cleaning process
        - For example; have a column in the participant table for the attrition algorithm which indicates if a participant manually assigned or algorithmically assigned 


