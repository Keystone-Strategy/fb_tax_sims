## ---------------------------
## Script name: _Master_Line_Prof.R
## Purpose of script:
## Author: Michael Gates
## Date Created: 2022-01-08
## ---------------------------
## Notes/Assumptions:
##   
##
## ---------------------------
options(scipen = 6, digits = 4)
memory.limit(30000000)     

## ---------------------------
require(data.table)
library(profvis)

rm(list=ls())
gc()

options(java.parameters = "- Xmx4096m")

# Get the user information
project_fld   <- "C:/_Athey_Production/10_Competitor_Production"
code_fld      <- file.path(project_fld, "01_Code")
fn_file       <- file.path(code_fld, "_FN.R")
master_file   <- file.path(code_fld, "_Master.R")

# Source the master file
source(file.path(code_fld, "_test_line_prof_master.R"))

# Run the master file
profvis({
  line_prof_master()
})


