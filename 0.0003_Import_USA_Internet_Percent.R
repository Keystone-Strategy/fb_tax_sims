import_usa_internet_pct_data <- function() {
  # Unzip the zipped file with the internet user data
  unzip(file.path(input_fld, "76. API_IT.NET.USER.ZS_DS2_en_csv_v2_382946.zip"), exdir=out_path)
  # Load in the internet user data
  ius <- fread(file.path(out_path, "API_IT.NET.USER.ZS_DS2_en_csv_v2_382946.csv"), header = TRUE)
  # Keep only the row with the United States
  usa_ius_wide <- ius[`Country Name` == "United States"]
  # Subset the table
  usa_ius_wide[, c("Indicator Name", "Indicator Code", "V64") := NULL, ]
  # Reshape to long form
  usa_long <- melt(usa_ius_wide                                                     , 
                   id.vars = c("Country Name")                                      ,
                   measure.vars  = grep("Country", names(usa_ius_wide), invert = T) ,
                   variable.name = "Year"                                           ,
                   value.name    = "pct_int_users"                                   )
  # Set the Year variable to numeric
  usa_long[, Year := as.numeric(as.character(Year)), ]
  # Subset the data
  usa_int <- usa_long[Year > 2008 & Year < 2018]
  # Align the date to the beginning of the next year
  usa_int[, Year := Year + 1, ]
  # Align the date to January 1 of the given year
  usa_int[, date := as.Date(paste0(Year, "-01-01")), ]
  # Add the ISO3
  usa_int[, ISO := "USA", ]
  # Subset the dataset
  usa_int <- usa_int[, .(ISO, date, pct_int_users)]
  # Save the data
  save_fst(usa_int, "0.0003", out_path)
  # Remove the csv from the file path
  file.remove(file.path(out_path, "API_IT.NET.USER.ZS_DS2_en_csv_v2_382946.csv"))
  file.remove(file.path(out_path, "Metadata_Country_API_IT.NET.USER.ZS_DS2_en_csv_v2_382946.csv"))
  file.remove(file.path(out_path, "Metadata_Indicator_API_IT.NET.USER.ZS_DS2_en_csv_v2_382946.csv"))
}
