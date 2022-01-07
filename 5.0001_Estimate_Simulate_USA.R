estimate_simulate_usa <- function(dt         ,
                                  platform    ) {
  
  # Import the data
  same_util          <- data.table(read_fst(file.path(out_path, "Output from Calibration.fst")))
  # Get the minimum iteration in use
  minimum_iterations <- data.table(read_fst(file.path(out_path, "Temporary Iteration.fst")))

  # Starting penetration for this data.table
  start_multi_pen <- minimum_iterations[, start_pen_AB, ]

  # Calculate the pen_A beginning
  same_util[, pen_A_beg := pen_A_only_beg + pen_AB_beg, ]
  
  # Order the data.table
  same_util <- same_util[order(seed, iteration, country, period)]
  
  # Add lags
  same_util[, lag_pen_A           := shift(pen_A      , n=1L, type = "lag"), by = .(seed, iteration, country)]
  same_util[, lag_pen_A_only      := shift(pen_A_only , n=1L, type = "lag"), by = .(seed, iteration, country)]
  same_util[, lag_pen_AB          := shift(pen_AB     , n=1L, type = "lag"), by = .(seed, iteration, country)]
  same_util[, lag_pen_A_beg       := shift(pen_A_beg  , n=1L, type = "lag"), by = .(seed, iteration, country)]
  same_util[, lag_y_churn         := shift(y_churn    , n=1L, type = "lag"), by = .(seed, iteration, country)]
  
  # Calculate the probability of adopting A
  same_util[, prob_A_calc := (pen_A_only_beg + pen_AB_beg - lag_pen_A_only - lag_pen_AB) /
              (1 - (lag_pen_A_only + lag_pen_AB)), ]
  
  # Calculate the LHS variable
  same_util[, lhs   := log(prob_A_calc / (1 - prob_A_calc)), ]
  
  # Remove iterations where the lhs becomes infinite
  same_util[, inf_flag := (is.infinite(lhs) | is.nan(lhs)),]
  same_util[, max_inf_flag := max_na(inf_flag), by = .(seed, iteration)]
  same_util_ninf <- same_util[max_inf_flag != 1,,]
  
  # Set the order of the variables in the regression
  same_util_ninf <- same_util_ninf[order(seed, iteration, country, period),,]
  
  # Create binary competitor variable ---------------------------------------
  
  same_util_ninf[pen_B_beg >= 0.25 * pen_A_beg , comp_var := TRUE  , ]
  # Set the binary competitor variable to zero if missing
  same_util_ninf[is.na(comp_var)       , comp_var := FALSE , ]
  
  # Copy the dataset
  sutil <- copy(same_util_ninf)
  
  # Run the regression
  sutil[, intercept         :=  coef(lm(lhs ~ lag_pen_A_beg + comp_var))[1], by = .(seed, iteration)]
  sutil[, lag_pen_A_coeff   :=  coef(lm(lhs ~ lag_pen_A_beg + comp_var))[2], by = .(seed, iteration)]
  sutil[, comp_var_coeff    :=  coef(lm(lhs ~ lag_pen_A_beg + comp_var))[3], by = .(seed, iteration)]
  sutil[period != 0, resids := resid(lm(lhs ~ lag_pen_A_beg + comp_var))   , by = .(seed, iteration)]
  
  # Reorder the dataset
  s_util_resids <- sutil[order(seed, iteration, country, period),,]
  
  # Get the columns to apply the change to
  zero_cols <- c("pen_None"       , "pen_A"          ,
                 "pen_AB"         , "pen_A_only"     ,
                 "pen_None_beg"   , "pen_A_beg_only" ,
                 "pen_AB_beg"     , "prob_A_calc"    , 
                 "lhs"            , "lag_pen_A"       )
  
  # Clear the data after the 0th period
  s_util_resids[period > 0, (zero_cols) := lapply(zero_cols, function(x) {
    x = 0
  }), ]
  
  # Simulate the trajectory -------------------------------------------------
  
  for (scenario in c("actual", "cf")) {
    
    # Change the penetration of A based on the scenario
    if (scenario == "cf") {
      s_util_resids[period == 0, pen_A_beg := 0, ]
    }
    
    # Get a new number for each iteration
    same_util[, iteration_num := .GRP, by = .(seed, iteration)]
    
    for (i in 1:(nrow(same_util[iteration_num == 1 & country == 1]) - 1)) {
      # Before churn calculation of lag_pen_A_beg
      s_util_resids[, lag_pen_A_beg_next := shift(pen_A_beg, n=1L, type = "lag") , by = .(seed, iteration, country)]
      s_util_resids[period == i , lag_pen_A_beg := lag_pen_A_beg_next , ]
      # After churn calculation of lag_pen_A
      s_util_resids[, lag_pen_A_next := lag_pen_A_beg * (1 - lag_y_churn) , by = .(seed, iteration, country)]
      s_util_resids[period == i , lag_pen_A := lag_pen_A_next , ]
      # Calculation of RHS
      s_util_resids[period == i , rhs := lag_pen_A_coeff * lag_pen_A_beg +
                      comp_var_coeff * comp_var +
                      resids + intercept, ]
      s_util_resids[period == i , prob_A_calc := exp(rhs) / (1 + exp(rhs)), ]
      s_util_resids[period == i , pen_A_beg := lag_pen_A + prob_A_calc * (1 - lag_pen_A), ]
      temp <- copy(s_util_resids)
      assign(paste0(scenario, "_dt"), temp[, .(seed, iteration, country, period, pen_A_beg), ])
    }
  }
  
  # Remove any missings
  set_NA <- actual_dt[is.na(pen_A_beg), .N, by = .(seed, iteration), ][, N := NULL, ]
  actual_dt <- actual_dt[!(seed %in% set_NA[, seed, ] &
                             iteration %in% set_NA[, iteration, ])]
  same_util_ninf <- same_util_ninf[!(seed %in% set_NA[, seed, ] &
                                       iteration %in% set_NA[, iteration, ])]
  
  
  if ((all(actual_dt[, .(pen_A_beg), ] - same_util_ninf[, .(pen_A_beg), ] < 0.1^10) == FALSE)) {
    print("STOP! ACTUALS ARE NOT RECOVERED THROUGH SIMULATION.")
    STOP
  }
  
  # Change the names for the pen_A variable
  setnames(actual_dt , "pen_A_beg" , "actual_pen_A_bin"  )
  setnames(cf_dt     , "pen_A_beg" , "cf_pen_A_bin"      )
  
  # Merge the two datasets
  actual_cf_athey <- merge(actual_dt, cf_dt, by = c("seed", "iteration", "country", "period"), all.x = T)
  # Merge the original table to this 
  actual_cf_athey_m <- merge(sutil, actual_cf_athey, by = c("seed"      ,
                                                            "iteration" ,
                                                            "country"   ,
                                                            "period"     ), all.y = T)
  # Add the competitor threshold to the dataset
  actual_cf_athey_m[, competitor_pct_fb := 0.25, ]
  
  # Save the datasets
  save_fst(actual_cf_athey_m , paste0(platform                                               ,
                                      "_Actual_Counterfactual_Affirmative_Model_Start_Multi" ,
                                      start_multi_pen                                         ) , out_path)
  # save_fst(same_util         , paste0(platform , "_Updated_Estimation_Dataset"             ) , out_path) @MG can likely remove
  return(actual_cf_athey_m)
}
