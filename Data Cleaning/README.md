# Data Cleaning scripts
You can see the following information in this document:
1. [Main data cleaning steps](#main-data-cleaning)
2. [Tables in each session of the study](#r01-questionnaire-in-each-session)


## Main Data cleaning
Run this script to have a clean dataset which takes care of following steps:

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

### R01 questionnaire in each session

- **Eligible**
  - Recent Anxiety symptoms. _DASS-AS_ Table
