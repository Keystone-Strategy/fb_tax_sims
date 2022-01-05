import_usa_mau_data <- function() {
  # Load in the MAU data
  mau_pre_2011  <- data.table(xlsx::read.xlsx(file.path(input_fld, "1. FBDIS00060473.xlsx"), sheetName = "Worksheet" ))
  mau_post_2011 <- data.table(xlsx::read.xlsx(file.path(input_fld, "2. FBDIS00060474.xlsx"), sheetName = "Data"      ))
  # Subset the datasets
  mau_pre_2011_sub  <- mau_pre_2011[ , .(ds, country, mau), ]
  mau_post_2011_sub <- mau_post_2011[, .(ds, country, mau), ]
  # Append both datasets
  mau <- rbind(mau_pre_2011_sub, mau_post_2011_sub)
  # Keep the USA MAU data
  mau_usa <- mau[country == "United States"]
  # Add the ISO code and remove the country name
  mau_usa[, ISO := "USA", ][, country := NULL, ]
  # Rename the ds column the date column
  setnames(mau_usa, "ds", "date")
  # Make it a date variable
  mau_usa[, date := as.Date(date), ]
  # Save the data
  save_fst(mau_usa, "mau_usa", out_path)
}