# README

## Overview

Author: Jeremy W. Eberle

This README describes centralized data cleaning for the Calm Thinking Study, an NIMH-funded ([R01MH113752](https://reporter.nih.gov/project-details/9513058)) study of the [MindTrails Project](https://mindtrails.virginia.edu/) targeting interpretation bias in anxious adults (ClinicalTrials.gov [NCT03498651](https://clinicaltrials.gov/ct2/show/NCT03498651?term=NCT03498651&draw=2&rank=1))

### Table of Contents

1. [Data](#data)
2. [Cleaning Scripts](#cleaning-scripts)

## Data

The data are stored in the [MindTrails Calm Thinking Study](https://osf.io/zbd52/) project on the Open Science Framework (OSF). The project has two components, with different permissions: a [Private Component](https://osf.io/jwvnb/) and a [Public Component](https://osf.io/s8v3h/).

### Private Component

The [Private Component](https://osf.io/jwvnb/) contains the full set of raw data tables (with some exceptions) dumped from the SQL database on the "teachmanlab" Data Server on December 3, 2020 (using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb)). The folder structure is below.

The exceptions are that only redacted versions of "gift_log", "import_log", and "sms_log" tables are included (redacted using [3_redact_data.R](code/3_redact_data.R)). 

```
.
├── data
└── └── 1_raw_full               # 67 CSV files (e.g., "dass21_as-03_12_2020.csv", "angular_training-03_12_2020.csv", 
                                 #   "gift_log-03_12_2020-redacted.csv")
```

Researchers can request access to files on this component by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

### Public Component

The [Public Component](https://osf.io/s8v3h/) contains a partial set of raw data tables (i.e., those obtained using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb) that did not need redaction), redacted data tables (from [3_redact_data.R](code/3_redact_data.R)), and intermediately clean data tables (from [4_clean_data.R](code/4_clean_data.R)). The folder structure is below.

Note: Tables in the `1_raw_full` folder of the [Private Component](#private-component) that are not in the `1_raw_partial` folder of this [Public Component](https://osf.io/s8v3h/) contain free-text responses that may or may not have identifiers. In the [Public Component](https://osf.io/s8v3h/), redacted versions of such tables are in `2_redacted`.

```
.
├── data                    
├── ├── 1_raw_partial            # 53 CSV files (did not need redaction; e.g., "dass21_as-03_12_2020.csv")
├── ├── 2_redacted               # 14 CSV files (needed redaction; e.g., "angular_training-03_12_2020-redacted.csv", 
│   │                            #   "gift_log-03_12_2020-redacted.csv")
├── ├── 3_intermediate_clean     # 50 CSV files (note: 17 files were deemed irrelevant and removed during cleaning)
├── materials
├── ├── appendices               # Appendices
└── └── codebooks                # Codebooks
```

## Cleaning Scripts

TODO
