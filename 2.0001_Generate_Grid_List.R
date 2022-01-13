create_grid_sample <- function(utility_A , utility_B   ,
                               beta      , gamma_input , 
                               platform  , divisor      ) {
  
  # TESTING CENTER
  # utility_A   = seq(-5.00,  1.00, 1.00)
  # utility_B   = seq(-5.00,  1.00, 1.00)
  # beta        = seq( 0.10,  5.00, 1.00)
  # gamma_input = seq( 0.00,  5.00, 1.00)
  # divisor <- 5
  # print("TESTING CENTER ON!!!!!!!!!!!!")
  
  # Expand the inputs data.table
  inputs_dt <- data.table(expand_grid(utility_A, utility_B, beta, gamma_input))
  # Order the data.table
  inputs_dt <- inputs_dt[order(utility_A, utility_B, beta, gamma_input)]
  # Rename the variables
  setnames(inputs_dt, seq_along(names(inputs_dt)), c("u_A", "u_B", "beta", "gamma"))
  # Set the index for each set of iterations
  inputs_dt[, index := .I]
  # Add a row index for tracking
  inputs_dt[, filter := (.I%%divisor+1)]
  # Total List
  total_list <- list()
  # List of the smaller datasets
  for (multiplier in 1:divisor) {
    # Assign to a temp table
    assign(paste0("temp", multiplier),
           inputs_dt[filter == multiplier,,])
    # Save the list
    save_fst(get(paste0("temp", multiplier)), paste0("2.0001_", multiplier), out_path)
    # Add the list
    total_list[[multiplier]] <- get(paste0("temp", multiplier))
  }
  # Return the list of the split up grid
  return(total_list)
}

