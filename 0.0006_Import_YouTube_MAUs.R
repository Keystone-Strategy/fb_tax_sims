import_youtube_maus <- function() {
  # Load in the 2011 to 2016 YouTube users
  youtube_2011_2016_users <- data.table(read.xlsx(file.path(input_fld, "YouTube_US_Users_2011_2016.xlsx"), sheet = "Users"))
  # Load in the 2017 to 2019 YouTube users
  youtube_2017_2019_users <- data.table(read.xlsx(file.path(input_fld, "Statista_YouTube_US_MAUs_2017_2019.xlsx"), sheet = "Data"))
  
  # Clean the 2011 to 2016 data first ---------------------------------------
  
  # Keep only the data from Q4 of the Year
  setnames(youtube_2011_2016_users, "Users", "MAUs")
  # Align the dates to January 1
  youtube_2011_2016_users <- youtube_2011_2016_users[, .(Year, MAUs), ]
  # Rename the MAUs variable to "youtube_maus"
  youtube_2011_2016_users[, date := as.Date(paste0(Year, "-01-01")), ]
  
  # Clean the 2017 to 2019 data second --------------------------------------
  
  # Rename the columns
  setnames(youtube_2011_2016_users, "MAUs", "youtube_maus")
  # Keep only specific years
  setnames(youtube_2017_2019_users, seq_along(names(youtube_2017_2019_users)), c("Year", "MAUs"))
  # Set the year variable as a numeric
  youtube_2017_2019 <- youtube_2017_2019_users[!1]
  # Subset the dataset to the time period of interest
  youtube_2017_2019[, Year := as.numeric(Year), ]
  # Set the MAUs as numeric
  youtube_users <- youtube_2017_2019[Year <= 2019,,]
  # Multiply by 1M as that's the appropriate units
  youtube_users[, youtube_maus := as.numeric(MAUs), ]
  # Align the dates to January 1
  youtube_users[, youtube_maus := youtube_maus * 1000000  , ]
  # Remove the MAUs column
  youtube_users[, date := as.Date(paste0(Year, "-01-01")), ]
  
  # Append the two datasets -------------------------------------------------
  
  youtube_users[, MAUs := NULL, ]
  
  youtube <- rbind(youtube_2011_2016_users, youtube_users) 
  # Remove the year variable
  youtube[, Year := NULL, ]
  
  # Save the data
  save_fst(youtube, "YouTube_USA_Users", out_path)
}
