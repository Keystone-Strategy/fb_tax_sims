curve_fit <- function(platform) {
  
  # Import the data
  actual_cf   <- data.table(read_fst(
    file.path(out_path, paste0(platform, "usa_generated_data.fst" ))))
  m_platforms <- data.table(read_fst(
    file.path(out_path, "Multi-Homing Data - Rebuttal Report.fst" )))
  fb_comps_s  <- data.table(read_fst(
    file.path(out_path, "FB_Competitor_Penetration.fst"           )))
  
  # Depending on the platform, load in specific data
  if (platform == "Twitter") {
    comp_usa <- fb_comps_s[, .(comp_pen = twitter_pen, fb_pen, year), ]
  } else if (platform == "YouTube") {
    comp_usa <- fb_comps_s[, .(comp_pen = youtube_pen, fb_pen, year)]
  }

  # Collapse the dataset by the ROW -----------------------------------------
  
  actual_pen <- actual_cf[, .(actual_pen_A  = mean(pen_A_beg         ),
                              actual_pen_B  = mean(pen_B_beg         ),
                              actual_pen_AB = mean(pen_AB_beg        ),
                              utility_A     = mean(u_A               ),
                              utility_B     = mean(u_B               ),
                              beta          = mean(beta              ), 
                              gamma         = mean(gamma             ),
                              start_pen_AB  = mean(start_pen_AB_copy ) ),
                          by = .(iteration, period)]
  # Create an artificial year variable
  actual_pen[, year := period + 2011,]

  actual_pen_copy <- copy(actual_pen)
  
  # Get the max of a variable and a particular year in a new column
  max_dt_year <- function(dt, var, year_m) {
    temp <- get(copy(dt))
    temp[year == year_m, paste0(var, "_", year_m)        := get(var) ,]
    temp[              , paste0("m_", var, "_", year_m)  :=
                         max_na(get(paste0(var,"_", year_m))),
                           by = .(iteration)]
    temp[, paste0(var, "_", year_m) := NULL, ]
    return(temp)
  }
  
  # Merge the competing social networks -------------------------------------
  
  # Merge the new competitor data
  actual_sub <- merge(actual_pen_copy, comp_usa, by = "year")
  # Remove specific years
  actual_sub <- actual_sub[year != 2019]
  
  # Calculate level of simulated multi-homing for 2014 and 2018 -------------
  
  actual_sub <- max_dt_year(dt = "actual_sub" , var = "actual_pen_AB" , year_m = 2014)
  actual_sub <- max_dt_year(dt = "actual_sub" , var = "actual_pen_AB" , year_m = 2018)
  
  # Get the Facebook penetration for 2014 and 2018 in new columns -----------
  
  actual_sub <- max_dt_year(dt = "actual_sub" , var = "fb_pen"        , year_m = 2014)
  actual_sub <- max_dt_year(dt = "actual_sub" , var = "fb_pen"        , year_m = 2018)

  # Create MSE between comparable curves ------------------------------------
  
  # Calculate the MSE for Facebook
  actual_sub[, SE_fb      := (fb_pen - actual_pen_A)^2 ,                   ]
  actual_sub[, tot_SE_fb  := sumna(SE_fb)              , by = .(iteration) ]
  actual_sub[!is.na(fb_pen), n := .N,                  , by = .(iteration) ]
  actual_sub[, MSE_fb     := tot_SE_fb / n             , by = .(iteration) ]
  

  # Calculate the MSE for the competitor ------------------------------------

  actual_sub[, SE_comp      := (comp_pen - actual_pen_B)^2   ,                    ]
  actual_sub[, tot_SE_comp  := sumna(SE_comp)                , by = .(iteration), ]
  actual_sub[!is.na(comp_pen), n_comp := .N                  , by = .(iteration), ]
  actual_sub[, MSE_comp     := tot_SE_comp / n_comp          , by = .(iteration), ]
  
  # Add to get the total MSE ------------------------------------------------
  
  actual_sub[, MSE_tot      := MSE_fb + MSE_comp, ]
  
  # Get a subset of the data.table to view the results
  unique_params_1 <- unique(actual_sub[, .(iteration   ,
                                           utility_A   , utility_B ,
                                           beta        , gamma     ,
                                           start_pen_AB , MSE_tot    ), ])
  # Rank the results
  unique_params_1 <- unique_params_1[order(MSE_tot)]
  
  # Change the name for the input
  platform_input <- platform 
  
  # Get the m_2014 and the m_2018 data points
  m_2014 <- m_platforms[platform == platform_input, m_2014,]
  m_2018 <- m_platforms[platform == platform_input, m_2018,]
  
  # Create the min_multi-homing variable for 2014
  actual_sub[, min_multi_2014 := m_fb_pen_2014 * m_2014, ]
  actual_sub[, min_multi_2018 := m_fb_pen_2018 * m_2018, ]
  
  min_multi_2014 <- actual_sub[1, min_multi_2014, ]
  min_multi_2018 <- actual_sub[1, min_multi_2018, ]
  
  # If the platform is Twitter then adjust the 2014 values if necessary
  if (platform == "Twitter") {
    if (actual_sub[year == 2014 , comp_pen, ][1] < min_multi_2014) {
      actual_sub[, min_multi_2014 := actual_sub[year == 2014, comp_pen, ][1] , ]
    }
  }
  
  # Adjust the 2018 values for both competitors
  if (actual_sub[year == 2018 , comp_pen, ][1] < min_multi_2018) {
    actual_sub[, min_multi_2018 := actual_sub[year == 2018, comp_pen, ][1] , ]    
  }
  
  if (platform == "Twitter") {
    # Get the distribution of gammas for only the 2014 restriction
    actual_sub      <- actual_sub[m_actual_pen_AB_2014 > min_multi_2014, ]
  }
  
  # Get the distribution of gammas for only the 2018 restriction
  actual_2018_r    <- actual_sub[m_actual_pen_AB_2018 > min_multi_2018  , ]

  # Get the unique MSEs
  unique_params_2 <- unique(actual_2018_r[, .(iteration    ,
                                              utility_A    , utility_B ,
                                              beta         , gamma     ,
                                              start_pen_AB , MSE_tot    ), ])
  # Order the data.table
  unique_params_2 <- unique_params_2[order(MSE_tot)]
  # Get the best ranking
  min_iteration <- unique_params_2[1]
  
  # Get the associated data for the minimum iteration
  min_iteration_data <- actual_sub[iteration == min_iteration[, iteration, ], 
                                   .(period, actual_pen_A, actual_pen_B),]
  
  # Save the unique parameters data -----------------------------------------
  save_fst(unique_params_2     , paste0("Minimized Iterations MSE "        , platform), out_path          )
  write.csv(unique_params_2    , file.path(out_path, paste0("Ranking of Parameters  " , platform , ".csv" )))
  write.csv(min_iteration      , file.path(out_path, paste0("Minimum Iteration for  " , platform , ".csv" )))
  save_fst(actual_sub          , paste0("Best Fit Lines for Graphing "     , platform), out_path          )
  save_fst(min_iteration_data  , paste0("Data from Best Fit for "          , platform), out_path          )
  return(min_iteration)
}
