import_yearly_churn <- function() {
  # Data available from Dr. Athey's Affirmative Report subission
  churn_2009_2011 <- data.table(read.xlsx(file.path(input_fld, "5. FBDIS00060472.xlsx"), sheet = "Data"))
  churn_2012      <- data.table(read.xlsx(file.path(input_fld, "6. FBDIS00060471.xlsx"), sheet = "Data"))
  churn_2013_2018 <- data.table(read.xlsx(file.path(input_fld, "7. FBDIS00060470.xlsx"), sheet = "Data"))
  # Append the data
  churn <- rbind(churn_2009_2011, churn_2012, churn_2013_2018)
  # Keep the data for the US
  us_churn <- churn[country == "United States"]
  # Calculate the sum by date
  us_churn_annual <- 
    us_churn[, .(sum_deleted_deactivated = sum(deleted_or_deactivated),
                 sum_stale               = sum(stale)                 ,
                 sum_overall_base        = sum(overall_base)           ),
             by = .(country, ds)]
  # Calculate the yearly churn rate
  us_churn_annual[, y_churn := (sum_deleted_deactivated + sum_stale) /
                    sum_overall_base, ]
  # Add a date variable
  us_churn_annual[, date := as.Date(ds), ]
  # Add the ISO
  us_churn_annual[, ISO := "USA", ]
  # Subset the data
  us_churn_annual <- us_churn_annual[, .(ISO, date, y_churn), ]
  # Only keep 2011 or greater
  us_churn_annual <- us_churn_annual[date >= "2011-01-01",,]
  # Manually set the churn to 5%
  # us_churn_annual[, y_churn := 0.05, ]
  # Save the table
  save_fst(us_churn_annual, "0.0008", out_path)
}