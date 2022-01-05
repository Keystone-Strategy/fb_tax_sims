import_twitter_maus <- function() {
  # Load in the compiled Twitter MAU data for the United States
  twitter_usa_users <- data.table(xlsx::read.xlsx(file.path(input_fld, "Compiled_Twitter_Yearly_MAUs.xlsx"), "MAUs"))
  # Change the name from Users to MAUs
  setnames(twitter_usa_users, "Users", "MAUs")
  # Keep only the data from Q4 of the Year
  twitter_usa_users <- twitter_usa_users[Quarter == 4, .(Year, MAUs), ]
  # Add a year for the alignment of the date
  twitter_usa_users[, Year := Year + 1, ] 
  # Align the dates to January 1, 2021
  twitter_usa_users[, date := as.Date(paste0(Year, "-01-01")), ]
  # Remove the year variable
  twitter_usa_users[, Year := NULL, ]
  # Rename the MAUs to "twitter_maus"
  setnames(twitter_usa_users, "MAUs", "twitter_maus")
  # Save the data
  save_fst(twitter_usa_users, "Twitter_USA_Users", out_path)
}