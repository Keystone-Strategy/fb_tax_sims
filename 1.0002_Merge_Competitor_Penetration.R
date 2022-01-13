merge_fb_pen_competitor_maus <- function() {
  # Load in the datasets
  usa_maus          <- data.table(read_fst(file.path(out_path, "1.0001.fst")))
  twitter_usa_users <- data.table(read_fst(file.path(out_path, "0.0005.fst")))
  youtube_usa_users <- data.table(read_fst(file.path(out_path, "0.0006.fst")))

  # Merge the datasets ------------------------------------------------------
  
  fb_twitter <- merge(usa_maus, twitter_usa_users , by = "date")
  # merge with YouTube
  fb_comps <- merge(fb_twitter, youtube_usa_users , by = "date")

  # Calculate the penetration of Twitter and YouTube ------------------------
  
  fb_comps[, twitter_pen := twitter_maus / InternetUsersAdjusted, ]
  fb_comps[, youtube_pen := youtube_maus / InternetUsersAdjusted, ]
  
  # Subset the data to just the penetrations
  fb_comps_s <- fb_comps[, .(date, ISO, fb_pen, twitter_pen, youtube_pen), ]
  
  # Create a year variable
  fb_comps_s[, year := year(date), ]
  
  # Save the dataset
  save_fst(fb_comps_s, "1.0002", out_path)
}