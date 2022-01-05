usa_gen_data <- function(utility_A     =    (-7.00:-1.00      ) , utility_B      =    (-7.00:-1.00       )  ,
                         beta          = seq( 0.50, 4.50, 0.50) , gamma_input    = seq( 0.00, 5.00, 0.50 )  , 
                         platform                                                                        ) {

  # TESTING CENTER
  utility_A    <- seq(-5.00,  1.00, 0.50)
  utility_B    <- seq(-5.00,  1.00, 0.50)
  beta         <- seq( 0.10,  4.10, 0.50)
  gamma_input  <- seq( 0.00,  1.00, 0.10)
  platform     <- "YouTube"
  print("TESTING CENTER ON!!!")
  
  # Create the data.table of inputs based on the inputs to the function
  inputs_dt <- data.table(u_A       = utility_A , u_B        = utility_B    ,
                          beta      = beta      , gamma      = gamma_input   )
  # Expand the inputs data.table
  inputs_dt <- data.table(expand(inputs_dt, u_A, u_B, beta, gamma))
  # Add an iteration counter
  inputs_dt[, iteration := .I, ]
  # Add a merge variable
  inputs_dt[, merge_var :=  1, ]
  
  # Load in the vector of Facebook Penetration as a starting point
  fb_comps_s <-   data.table(read_fst(file.path(out_path, "FB_Competitor_Penetration.fst")))
  # Get the starting penetration of each platform 
  m_fb_pen      <- fb_comps_s[year == 2011, fb_pen      ,]
  m_twitter_pen <- fb_comps_s[year == 2011, twitter_pen ,]
  m_youtube_pen <- fb_comps_s[year == 2011, youtube_pen ,]
  
  # If statement to get the corresponding platform
  if(platform == "Twitter") {
    # Set the competitor penetration to the Twitter penetration
    m_comp_pen <- m_twitter_pen
    # Additionally, set the iterations for the level of multi-homing at 0.01
    comp_iter <- 0.01
  } else if (platform == "YouTube"){
    # Set the competitor penetration to the YouTube penetration
    m_comp_pen <- m_youtube_pen
    # Additionally, set the iterations for the level of multi-homing at 0.01
    comp_iter <- 0.05
  }
  
  # Calculations of probabilities -------------------------------------------
  
  # Create an initial data.table for a given country with 10 periods
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
  # Add a merge variable for creation of a shell
  L[, merge_var := 1, ]
  
  # Load in the churn data
  us_churn <- data.table(read_fst(file.path(out_path, "US Annual_Churn.fst")))
  
  # Add a merge_var to the US churn data
  us_churn[, period := .I - 1, ]
  
  # Merge the data
  L <- merge(L, us_churn, by = "period")

  # Merge L to the input data to get all scenarios 
  L_inputs <- merge(L, inputs_dt, by = "merge_var", allow.cartesian = TRUE)
  
  # Add a data.table with different levels of the initial penetration of multi-homing between the two platforms
  ab <- data.table(start_pen_AB   = seq(0, m_comp_pen, comp_iter) , 
                   merge_var      = 1)
  
  # Expand data.table to include different starting penetrations of multi-homing
  L_inputs <- merge(L_inputs, ab, by = "merge_var", allow.cartesian = TRUE)
  
  # Redefine the iteration
  iteration_redefine <- unique(L_inputs[, .(temp = iteration), by = .(iteration, start_pen_AB),])
  
  # Get a new index for each iteration in the dataset
  iteration_redefine[, new_iteration := .I, ]
  
  # Merge the inputs dataset and the re-defined iterations
  L_inputs <- merge(L_inputs, iteration_redefine, by = c("iteration", "start_pen_AB"))
  # Clean the table
  L_inputs[, iteration := NULL, ][, iteration := new_iteration, ][, new_iteration := NULL, ][, temp := NULL, ]
  
  # Create a copy of the starting level of multi-homing to keep track of later
  L_inputs[, start_pen_AB_copy     := start_pen_AB, ]
  # Keep only the starting penetration of multi-homing in the first period, to allow it to evolve
  L_inputs[period!=0, start_pen_AB := NA, ]
  
  # Order the data
  L_inputs <- L_inputs[order(iteration, period)]
  
  # Set the the penetration of Facebook and the competitor to the starting penetrations observed in reality
  L_inputs[period == 0, pen_A_only := m_fb_pen   - start_pen_AB  , ]
  L_inputs[period == 0, pen_B_only := m_comp_pen - start_pen_AB  , ]
  # Get the initial penetration for both platforms in a new row
  L_inputs[, pen_A_only_first := max_na(pen_A_only), by = iteration]
  L_inputs[, pen_B_only_first := max_na(pen_B_only), by = iteration]
  # Drop any scenarios where pen_A_only + pen_B_only > 1
  L_inputs <- L_inputs[pen_A_only_first + pen_B_only_first + start_pen_AB_copy <= 1]
  # Create the only country for the set
  L_inputs[, country := 1 , ]
  # Recreate the merge_variable
  L_inputs[, merge_var := .I, ]
  # Order the dataset
  L_inputs <- L_inputs[order(country, period),,]
  
  # Copy the dataset
  M <- copy(L_inputs)
  
  # Create the additional penetrations
  M[period == 0 , pen_AB       := start_pen_AB                             , ]
  M[period == 0 , pen_None     := 1 - pen_A_only - pen_B_only - pen_AB     , ]
  
  # Order M
  M <- M[order(iteration, country)]
  
  # Loop through the time periods
  for (p in 0:(7)) {

    # Save a copy of the metrics prior to churn
    M[period == p, pen_None_beg     := pen_None    , ]
    M[period == p, pen_A_only_beg   := pen_A_only  , ]
    M[period == p, pen_B_only_beg   := pen_B_only  , ]
    M[period == p, pen_AB_beg       := pen_AB      , ]
    
    # Calculate the update for pen_None with churn
    M[period == p, pen_None := pen_None + pen_A_only * y_churn + 
        pen_B_only * y_churn + pen_AB * y_churn * y_churn, ]
    
    # Calculate the update for pen_A with churn
    M[period == p, pen_A_only := pen_A_only * (1 - y_churn) + 
        pen_AB * (1 - y_churn) * y_churn, ]
    
    # Calculate the update for pen_B with churn
    M[period == p, pen_B_only := pen_B_only * (1 - y_churn) + 
        pen_AB * (1 - y_churn) * y_churn, ]
    
    # Calculate the update for pen_AB with churn
    M[period == p, pen_AB := pen_AB * (1 - y_churn) * (1 - y_churn), ]
    
    # Calculation of probability of adoption of A
    M[period == p, prob_A := (exp(u_A + beta*(pen_A_only + pen_AB))) / 
        (1 + (exp(u_A + beta*(pen_A_only + pen_AB)) +
                exp(u_B + beta*(pen_B_only + pen_AB)))) , ] 
    
    # Calculation of the probability of adoption of B  
    M[period == p, prob_B := (exp(u_B + beta*(pen_B_only + pen_AB))) / 
        (1 + (exp(u_B + beta*(pen_B_only + pen_AB)) +
                exp(u_A + beta*(pen_A_only + pen_AB)))) , ]
    
    # Calculation of no adoption
    M[period == p, prob_None := 1 - prob_A - prob_B, ]
    
    # Calculate the probability of choosing B given A
    M[period == p, prob_B_given_A := (exp(u_B + beta*(pen_B_only + pen_AB) - gamma)) / 
        (1 + exp(u_B + beta*(pen_B_only + pen_AB) - gamma)), ]
    
    # Calculate the probability of staying A
    M[period == p, prob_stay_A := 1 - prob_B_given_A, ]
    
    # Calculate the probability of choosing A given B
    M[period == p, prob_A_given_B := (exp(u_A + beta*(pen_A_only + pen_AB) - gamma)) / 
        (1 + exp(u_A + beta*(pen_A_only + pen_AB) - gamma)), ]
    
    # Calculate the probability of staying in B
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
  # Copy the output data.table
  actual_dt <- copy(M)  
  
  # Order the data.table
  actual_dt <- actual_dt[order(iteration, country, period)]
  
  # Calculate the actual penetration for each country
  actual_dt[, pen_A := pen_A_only + pen_AB, ]
  actual_dt[, pen_B := pen_B_only + pen_AB, ]
  
  # Calculate the actual penetration prior to the churn in each country  
  actual_dt[, pen_A_beg := pen_A_only_beg + pen_AB_beg, ]
  actual_dt[, pen_B_beg := pen_B_only_beg + pen_AB_beg, ]

  # Save the data.table with the iterations
  save_fst(actual_dt , paste0(platform , "usa_generated_data" ) , out_path              )
  write.csv(actual_dt, file.path(out_path, paste0(platform , "usa_generated_data.csv" )))
  save_fst(inputs_dt , paste0(platform , "Input_Data"         ) , out_path              )
  return(actual_dt)
}

