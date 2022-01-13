curve_fit <- function(table_num, 
                      platform  ) {
  
  # TESTING CENTER
  # platform  <- "YouTube"
  # table_num <- 1
  # print("TESTING CENTER ON!!!!!")

  # Import the data
  actual_cf   <- data.table(read_fst(file.path(out_path, paste0("2.0002_", platform, "_", table_num, ".fst" ))))
  m_platforms <- data.table(read_fst(file.path(out_path, "0.0007.fst"  )))
  fb_comps_s  <- data.table(read_fst(file.path(out_path, "1.0002.fst"  )))
  
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
    temp <- copy(get(dt))
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
  # Rank the results by each starting point
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
  # Get the unique starting penetration of multi-homers
  u_start_pen_multi <- unique_params_2[, .N, by = start_pen_AB][, start_pen_AB, ]
  # Minimum by each starting penetration
  min_iterations_start_multi <- rbindlist(lapply(u_start_pen_multi, function(x) {
    unique_params_2[start_pen_AB == x,,][order(MSE_tot)][1]
  }))
  
  if (nrow(min_iterations_start_multi) > 0) {
    # Associated penetrations for minimum iterations
    lapply(min_iterations_start_multi[, iteration, ], function(x) {
      # Get the penetrations based on the iteration
      temp <- actual_sub[iteration == x,
                         .(period, actual_pen_A, actual_pen_B, start_pen_AB),]
      # Save the data.table
      save_fst(temp , paste0("3.0001_" , 
                             platform                 ,
                             "_start_multi_"          ,
                             temp[1, start_pen_AB]    ,
                             "_grid_num_"             , 
                             table_num), out_path)
    })
    
    # Get the associated data for the minimum iteration
    min_iteration <- min_iterations_start_multi[order(MSE_tot)][1]
    
    # Save the datasets -------------------------------------------------------
    
    # Save the minimized by each starting penetration
    save_fst(min_iterations_start_multi,
             paste0("3.0001_"    ,
                    platform     ,
                    "_grid_num_" ,
                    table_num    ,
                    "_A"          ),
             out_path)
    # Return the minimum iteration
    return(min_iterations_start_multi)
  } else if (nrow(min_iterations_start_multi) == 0)  {
    # Print that there are no scenarios for this parameter space
    print(paste0("There are no parameters which meet the constraints laid out for grid number ", table_num, "."))
    # Save a data.table with the table_num set to NA
    empty_dt <- data.table(iteration    = NA, utility_A    = NA, utility_B = NA, beta         = NA,
                           gamma        = NA, start_pen_AB = NA, MSE_tot   = NA, table_number = table_num   )
    # Save the minimum iteration among all starting penetrations of multi-homing
    save_fst(empty_dt            ,
             paste0("3.0001_"    ,
                    platform     ,
                    "_grid_num_" ,
                    table_num    ,
                    "_A"          ),
             out_path)
  }
}
