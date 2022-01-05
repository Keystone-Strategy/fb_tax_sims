create_USA_facebook_penetration <- function(){
  # Load in the necessary datasets
  usa_fb_maus <- read_fst(file.path(out_path, "mau_usa.fst"          ))
  usa_int_pct <- read_fst(file.path(out_path, "USA_Internet_Pct.fst" ))
  usa_pop     <- read_fst(file.path(out_path, "USA_Population.fst"   ))
  # Create a shell to hold the data
  month_shell <- shell_fn("USA", "2009-01-01", "2017-12-31", v1 = "ISO", freq = "month")
  
  # Merge the data to the month shell based on the correct dates ------------

  m1 <- merge(month_shell , usa_int_pct , by = c("ISO"  , "date" ) , all = T)
  m2 <- merge(m1          , usa_pop     , by = c("ISO"  , "date" ) , all = T)
  
  # Interpolate the USA population data -------------------------------------
  
  m2[, population_interpolated := inter.extra.polation(population), ]
  # Calculate the internet users based on the population
  m2[, internet_users := population_interpolated * pct_int_users / 100, ]
  
  # Collapse the data down to the annual frequency --------------------------
  
  usa_ius <- m2[month(date) == 1,
                .(ISO, date, internet_users), ]
  
  # Merge the MAUs with the internet users ----------------------------------
  
  usa_maus <- merge(usa_ius, usa_fb_maus, by = c("ISO", "date"))
  
  # Adjust internet users by 5%  --------------------------------------------
  
  usa_maus[, InternetUsersAdjusted := internet_users * 1.05, ]
  
  # Calculate Facebook's US penetration -------------------------------------
  
  usa_maus[, fb_pen := mau / InternetUsersAdjusted, ]
  # Subset the data
  usa_maus <- usa_maus[, .(ISO, date, fb_pen, InternetUsersAdjusted), ]
  
  # Save the dataset
  save_fst(usa_maus, "Facebook US Penetration", out_path)
}