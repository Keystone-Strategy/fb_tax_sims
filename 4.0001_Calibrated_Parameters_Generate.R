calibrated_parameters <- function(dt_min_its           , 
                                  platform             ,
                                  seed = 1              ) {
  
  min_iterations_start_multi <- data.table(read_fst(file.path(out_path                                            , 
                                                              paste0("Minimized Iterations Starting Penetration " ,
                                                                     platform                                     , 
                                                                     ".fst"                                        ))))
  
  unique_params_2 <- data.table(read_fst(file.path(out_path, paste0("Minimized Iterations MSE "   , platform, ".fst"))))
  unique_params_2 <- unique_params_2[order(MSE_tot)]
  dt_min_its      <- unique_params_2[1]
  
  # Characteristics of the minimum iterations
  utility_A    <- dt_min_its[, utility_A, ]
  utility_B    <- dt_min_its[, utility_B, ]
  beta         <- dt_min_its[, beta     , ]
  gamma_input  <- dt_min_its[, gamma    , ]
  
  # Create a data.table with the inputs
  inputs_dt <- data.table(u_A       = utility_A , u_B        = utility_B    ,
                          beta      = beta      , gamma      = gamma_input   )
  
  # Add a merge variable
  inputs_dt[, merge_var :=  1, ]
  inputs_dt[, iteration := .I, ]
  
  # Load in the vector of facebook penetration as a starting point
  user_data <- data.table(read_fst(file.path(out_path, "user_model_data.fst")))
  # Get the distribution for 2011
  user_year <- user_data[year(TimeUnique) == 2011, .(Code, fb_pen), ]
  
  # Get the mean and standard deviation of the distribution
  m_fb_pen  <- user_year[, mean(fb_pen), ]
  sd_fb_pen <- user_year[, sd(fb_pen  ), ]
  
  set.seed(seed)

  # Country shell
  country_dt <- data.table(country = 1:139 , merge_var = 1)
  
  # Add a dataset with a the probabilities to be calculated
  L <- as.data.table(cbind(         period          = c(seq(0,7))       ,
                                    pen_None        = c(rep(0,8))       ,
                                    pen_AB          = c(rep(0,8))       ,
                                    prob_A          = c(rep(0,8))       ,
                                    prob_B          = c(rep(0,8))       ,
                                    prob_None       = c(rep(0,8))       ,
                                    prob_B_given_A  = c(rep(0,8))       ,
                                    prob_A_given_B  = c(rep(0,8))       ,
                                    prob_stay_A     = c(rep(0,8))       ,
                                    prob_stay_B     = c(rep(0,8))))
  L[, merge_var := 1, ]
  
  # Load in the churn data
  us_churn <- data.table(read_fst(file.path(out_path, "US Annual_Churn.fst")))
  
  # Add a merge_var to the US churn data
  us_churn[, period := .I - 1, ]
  
  # Merge the data
  L <- merge(L, us_churn, by = "period")
  
  # Merge to the country_dt
  period_shell <- merge(L, country_dt, by = "merge_var", allow.cartesian = TRUE)
  M <- copy(period_shell)

  # Parameters for Facebook
  mean <- m_fb_pen
  sd   <- sd_fb_pen
  m    <- log(mean^2 / sqrt(sd^2 + mean^2))
  s    <- sqrt(log(1 + (sd^2 / mean^2)))
  
  # Parameters for competitor
  mean_comp <- m_fb_pen
  sd_comp   <- sd_fb_pen
  m_comp    <- log(mean_comp^2 / sqrt(sd_comp^2 + mean_comp^2))
  s_comp    <- sqrt(log(1 + (sd_comp^2 / mean_comp^2)))
  
  # Starting and ending penetrations
  start_pen <- data.table(pen_A_only = rlnorm(nrow(M)*20, m      , s      ),
                          pen_B_only = rlnorm(nrow(M)*20, m_comp , s_comp ) ) 
  # Create the pen_A_only + pen_B_only variable
  start_pen[, pen_A_B_only := pen_A_only + pen_B_only, ]
  # Keep only observations where pen_A_B_only is less than 1
  start_pen_sub <- start_pen[pen_A_B_only < 1,,]
  start_pen_sub[, merge_var := .I, ]
  
  # If the number of rows generated is fewer than what's necessary, then redraw
  while(nrow(start_pen_sub) < nrow(M)) {
    start_pen <- data.table(pen_A_only = rlnorm(nrow(M)*20, m     , s     ),
                            pen_B_only = rlnorm(nrow(M)*20, m_comp, s_comp) ) 
    # Create the pen_A_only + pen_B_only variable
    start_pen[, pen_A_B_only := pen_A_only + pen_B_only, ]
    # Keep only observations where pen_A_B_only is less than 1
    start_pen_sub <- start_pen[pen_A_B_only < 1,,]
    # Add a merge_variable
    start_pen_sub[, merge_var := .I, ]
  }
  # Order the dataset
  M <- M[order(country, period)]
  M[, merge_var := NULL, ]
  
  # Add a merge variable to only period zero of the M dataset
  M[period == 0, merge_var := .I,  ]
  
  # Merge m to the starting penetration dataset 
  M <- merge(M, start_pen_sub, by = "merge_var", all.x = T)
  M <- M[order(country, period)]
  
  # Set the prior merge vars to NULL
  M[, merge_var := NULL, ]
  # Add a merge variable to only period zero of the M dataset
  M[, merge_var := 1,  ]
  
  # Merge L to the input data to get all scenarios 
  M <- merge(M, inputs_dt, by = "merge_var", allow.cartesian = TRUE)

  # Run both the actual and counterfactual scenarios
  for (scenario in c("actual", "cf")) {
    
    # Data.table to hold the results
    assign(paste0(scenario, "_dt"), data.table())
    
    # Set pen_A_only to 0 if the it's the counterfactual run
    if (scenario == "cf") {
      cf_flag <- 0
    } else {
      cf_flag <- 1
    }

    # Apply the cf_flag to the penetration of platform A only
    M[period == 0 , pen_A_only   := pen_A_only * cf_flag                   , ]
    
    # Create the additional penetrations
    M[period == 0 , pen_AB       := 0                                      , ]
    M[period == 0 , pen_None     := 1 - pen_A_only - pen_B_only - pen_AB   , ]
    
    # Create the platform-time specific shocks
    M[, x_A :=  rnorm(nrow(M), 0, 1) , ]
    M[, x_B :=  rnorm(nrow(M), 0, 1) , ]
    
    for (p in 0:(7)) {
      
      # Keep track of the initial penetrations of A and AB
      M[period == p, pen_None_beg   := pen_None   , ]
      M[period == p, pen_A_only_beg := pen_A_only , ]
      M[period == p, pen_B_only_beg := pen_B_only , ]
      M[period == p, pen_AB_beg     := pen_AB     , ]
      
      # Calculate the update for pen_None with churn
      M[period == p, pen_None := pen_None + pen_A_only * y_churn + 
          pen_B_only * y_churn + pen_AB * y_churn * y_churn, ]
      
      # Calculate the update for pen_A with churn
      M[period == p, pen_A_only := pen_A_only * (1 - y_churn) + 
          pen_AB * (1 - y_churn) * y_churn, ]
      
      # Calculate the update for pen_A with churn
      M[period == p, pen_B_only := pen_B_only * (1 - y_churn) + 
          pen_AB * (1 - y_churn) * y_churn, ]
      
      # Calculate the update for pen_A with churn
      M[period == p, pen_AB := pen_AB * (1 - y_churn) * (1 - y_churn), ]
      
      # Calculate the probability of adoption of A
      M[period == p, prob_A := (exp(u_A + beta*(pen_A_only + pen_AB) + x_A)) / 
          (1 + (exp(u_A + beta*(pen_A_only + pen_AB) + x_A) +
                  exp(u_B + beta*(pen_B_only + pen_AB) + x_B))) , ] 
      
      # Calculate the probability of adoption of B
      M[period == p, prob_B := (exp(u_B + beta*(pen_B_only + pen_AB) + x_B)) / 
          (1 + (exp(u_B + beta*(pen_B_only + pen_AB) + x_B) +
                  exp(u_A + beta*(pen_A_only + pen_AB) + x_A))) , ]
      
      # Calculate the probability of no adoption
      M[period == p, prob_None := 1 - prob_A - prob_B, ]
      
      # Calculate the probability of joining B given the user is a user of platform A
      M[period == p, prob_B_given_A := (exp(u_B + beta*(pen_B_only + pen_AB) - gamma + x_B)) / 
          (1 + exp(u_B + beta*(pen_B_only + pen_AB) - gamma + x_B)), ]
      
      # Calculate the probability of staying with platform A
      M[period == p, prob_stay_A := 1 - prob_B_given_A, ]
      
      # Calculate the probability of joining platform A given the user is a user of platform B
      M[period == p, prob_A_given_B := (exp(u_A + beta*(pen_A_only + pen_AB) - gamma + x_A)) / 
          (1 + exp(u_A + beta*(pen_A_only + pen_AB) - gamma + x_A)), ] 
      
      # Calculate the probability of the user staying on platform B
      M[period == p, prob_stay_B := 1 - prob_A_given_B, ]  
      
      # Calculate the update ----------------------------------------------------
      M[, pen_None_next    :=  shift(pen_None*(1 -  prob_A - prob_B)                                      ,
                                     n=1L, type = "lag") , by = .(iteration, country)]
      M[, pen_A_only_next  :=  shift(pen_A_only  -  pen_A_only*prob_B_given_A + pen_None*prob_A           ,
                                     n=1L, type = "lag") , by = .(iteration, country)]
      M[, pen_B_only_next  :=  shift(pen_B_only  -  pen_B_only*prob_A_given_B + pen_None*prob_B           ,
                                     n=1L, type = "lag") , by = .(iteration, country)]
      M[, pen_AB_next      :=  shift(pen_AB      +  pen_B_only*prob_A_given_B + pen_A_only*prob_B_given_A ,
                                     n=1L, type = "lag") , by = .(iteration, country)]
      
      # Update the real variables
      M[period == p+1, pen_None    := pen_None_next                                 , by = .(iteration, country)]
      M[period == p+1, pen_A_only  := pen_A_only_next                               , by = .(iteration, country)]
      M[period == p+1, pen_B_only  := pen_B_only_next                               , by = .(iteration, country)]
      M[period == p+1, pen_AB      := pen_AB_next                                   , by = .(iteration, country)]
      M[period == p+1, pen_T       := pen_None + pen_A_only + pen_B_only + pen_AB   , by = .(iteration, country)]
      
    }
    
    # Make a copy so data.table doesn't mess it up
    assign(paste0(scenario, "_dt"), copy(M)) 
  }
  
  # Calculate the actual penetration for each country
  actual_dt[, pen_A := pen_A_only + pen_AB, ]
  actual_dt[, pen_B := pen_B_only + pen_AB, ]
  
  # Calculate the penetration prior to churn for each country
  actual_dt[, pen_A_beg := pen_A_only_beg + pen_AB_beg, ]
  actual_dt[, pen_B_beg := pen_B_only_beg + pen_AB_beg, ]
  
  # Calculate the counterfactual penetration for each country
  cf_dt[, pen_A := pen_A_only + pen_AB, ]
  cf_dt[, pen_B := pen_B_only + pen_AB, ]
  
  # Calculate the counterfactual penetration for each country
  cf_dt[, pen_A_beg := pen_A_only_beg + pen_AB_beg, ]
  cf_dt[, pen_B_beg := pen_B_only_beg + pen_AB_beg, ]
  
  # Actual data.table
  actual_dt_n <- actual_dt[, .(iteration      , country        , period         , 
                               pen_None       , pen_A          , pen_B          , 
                               pen_AB         , pen_A_only     , pen_B_only     , 
                               x_A            , x_B            , u_A            ,
                               u_B            , beta           , gamma          ,
                               pen_None_beg   , pen_A_only_beg , pen_B_only_beg ,
                               pen_AB_beg     , pen_A_beg      , pen_B_beg      , 
                               y_churn                                           )]
  # Counterfactual data.table
  cf_dt_n     <-     cf_dt[, .(iteration                           ,
                               country                             , 
                               period                              ,
                               pen_None_cf       = pen_None        ,
                               pen_A_cf          = pen_A           ,
                               pen_B_cf          = pen_B           ,
                               pen_AB_cf         = pen_AB          ,
                               pen_A_only_cf     = pen_A_only      ,
                               pen_B_only_cf     = pen_B_only      ,
                               x_A_cf            = x_A             ,
                               x_B_cf            = x_B             , 
                               pen_None_beg_cf   = pen_None_beg    ,
                               pen_A_only_beg_cf = pen_A_only_beg  ,
                               pen_B_only_beg_cf = pen_B_only_beg  ,
                               pen_AB_beg_cf     = pen_AB_beg      ,
                               pen_A_beg_cf      = pen_A_beg       ,
                               pen_B_beg_cf      = pen_B_beg        )]
  
  # Merge both datasets
  facebook_dt <- merge(actual_dt_n, cf_dt_n, by = c("iteration", "country", "period"), all.x = T)
  facebook_dt <- facebook_dt[order(iteration, country, period)]
  facebook_dt[, seed := seed , ]
  
  # Save the datasets
  save_fst(facebook_dt , paste0(platform, "fb_simulation_data") , out_path)
  return(facebook_dt)
}

