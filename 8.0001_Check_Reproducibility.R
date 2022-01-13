check_reproducibility <- function() {
  # Run the master file
  source(file.path(code_fld, "_Master.R"))
  # Read the csv
  twitter_results_1 <- data.table(read.csv(file.path(tables_figures, "Twitter Final Results.csv")))
  saveRDS(twitter_results_1, file.path(tables_figures, "twitter_1.RDS"))
  youtube_results_1 <- data.table(read.csv(file.path(tables_figures, "YouTube Final Results.csv")))
  saveRDS(youtube_results_1, file.path(tables_figures, "youtube_1.RDS"))
  
  # Run the master file
  source(file.path(code_fld, "_Master.R"))
  # Get the new results
  twitter_results_2 <- data.table(read.csv(file.path(tables_figures, "Twitter Final Results.csv")))
  youtube_results_2 <- data.table(read.csv(file.path(tables_figures, "YouTube Final Results.csv")))
  # Load the old results
  twitter_results_1 <- data.table(readRDS(file.path(tables_figures, "twitter_1.RDS")))
  youtube_results_1 <- data.table(readRDS(file.path(tables_figures, "youtube_1.RDS")))
  
  # Check whether or not they're identical
  if (identical(twitter_results_1, twitter_results_2) == TRUE & 
      identical(youtube_results_1, youtube_results_2) == TRUE) {
    print("Process reproduces same results with multiple runs")
  } else {
    print("NOT REPRODUCED. ISSUE!!!")
  }
}

check_reproducibility()
