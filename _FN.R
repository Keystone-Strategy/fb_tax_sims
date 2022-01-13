library(data.table)
library(stringr)
library(openxlsx)
library(tidyr)
library(pbapply)
library(fst)
library(parallel)
library(future)
library(future.apply)
plan(multisession)

# Add the directories  ----------------------------------------------------

data_fld       <- file.path(project_fld , "02_Data"           )
input_fld      <- file.path(data_fld    , "01_Input"          )
out_path       <- file.path(data_fld    , "02_Output"         )
# Remove the output path if it exists
unlink(file.path(data_fld, "02_Output"), recursive = T)
# Recreate the output path
dir.create(out_path)
# Recreate the directory
tables_figures <- file.path(project_fld , "03_Tables_Figures" )

# Simple functions --------------------------------------------------------

median_na <- function(x) {
  median(x, na.rm = T)
}

mean_na <- function(x) {
  mean(x, na.rm = T)
}

max_na <- function(x) {
  max(x, na.rm = T)
}

min_na <- function(x) {
  min(x, na.rm = T)
}

sumna <- function(x) {
  sum(x, na.rm = T)
}

save_fst <- function(dt, name, path){
  write_fst(  dt  , file.path(path, paste0(name, ".fst")))
}

shell_fn <- function(ids, sdate, edate, freq = "day", v1="V1") {
  date_list <- data.table(seq.Date(as.Date(sdate), as.Date(edate), by = freq))
  date_list[, idx := 1, ]
  u_ids <- data.table(unique(ids))
  u_ids[, idx :=1 , ]
  shell <- merge(u_ids, date_list, by = "idx", allow.cartesian = TRUE)
  shell[, idx := NULL, ]
  setnames(shell, old = seq_along(shell), new = c(v1, "date"))
  return(shell)
}

inter.extra.polation <- function(input){
  y = input
  x = which(!is.na(y))
  iend = length(x)
  if (iend <= 1) {return(y)}
  i = 1
  slope = (y[x[i+1]] - y[x[i]]) / (x[i+1] - x[i])
  if (x[i] > 1){
    y[1:(x[i] - 1)] = rep(y[x[i]], x[i] - 1) - c((x[i] - 1):1) * slope
  }
  y[(x[i]+1):x[i+1]] = rep(y[x[i]],x[i+1] - x[i]) + c(1:(x[i+1]-x[i])) * slope
  
  while(i < iend-1){
    i = i+1
    slope = (y[x[i+1]] - y[x[i]]) / (x[i+1] - x[i])
    y[(x[i] + 1):x[i+1]] = rep(y[x[i]],x[i+1] - x[i]) + c(1:(x[i+1] - x[i])) * slope
  }
  if (x[i] < length(y) - 1){
    y[(x[i] + 1):length(y)] = rep(y[x[i]], length(y) - x[i]) + c(1:(length(y) - x[i])) * slope
  }
  return(y)
}

tdt <- function(inpdt){
  transposed <- t(inpdt[,-1,with=F]);
  colnames(transposed) <- inpdt[[1]];
  transposed <- data.table(transposed, keep.rownames=T);
  setnames(transposed, 1, names(inpdt)[1]);
  return(transposed);
}
