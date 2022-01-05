import_usa_population_data <- function() {
  # Unzip the zipped file with the internet user data
  unzip(file.path(input_fld, "78. API_SP.POP.TOTL_DS2_en_csv_v2_382278.zip"), exdir=out_path)
  # Load in the internet user data
  population <- fread(file.path(out_path, "API_SP.POP.TOTL_DS2_en_csv_v2_382278.csv"), header = TRUE)
  # Keep only the row with the United States
  usa_pop_wide <- population[`Country Name` == "United States"]
  # Subset the table
  usa_pop_wide[, c("Indicator Name", "Indicator Code", "V64") := NULL, ]
  # Reshape to long form
  usa_long <- melt(usa_pop_wide                                                     , 
                   id.vars = c("Country Name")                                      ,
                   measure.vars  = grep("Country", names(usa_pop_wide), invert = T) ,
                   variable.name = "Year"                                           ,
                   value.name    = "population"                                      )
  # Set the Year variable to numeric
  usa_long[, Year := as.numeric(as.character(Year)), ]
  # Subset the data
  usa_pop <- usa_long[Year > 2007 & Year < 2019]
  # Align the dates to July 1
  usa_pop[, date := as.Date(paste0(Year, "-07-01")), ]
  # Add the ISO3
  usa_pop[, ISO := "USA", ]
  # Subset the dataset
  usa_pop <- usa_pop[, .(ISO, date, population), ]
  # Save the data
  save_fst(usa_pop, "USA_Population", out_path)
}
