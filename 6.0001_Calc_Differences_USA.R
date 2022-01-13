calc_differences <- function(platform) {
  
  # TESTING CENTER
  # platform <- "Twitter"
  # print("TESTING CENTER ON!!!!")
  
  # Minimum iterations
  minimum_iterations <- data.table(read_fst(file.path(out_path, paste0("_Master_A_" ,
                                                                       platform     ,
                                                                       ".fst"        ))))
  # Starting penetration for this data.table
  start_multi_pen <- minimum_iterations[, start_pen_AB, ]
  
  # Import the data
  actual_cf  <- data.table(read_fst(
    file.path(out_path,
              paste0("5.0001_"       ,
                     platform        , 
                     "_start_multi_" ,
                     start_multi_pen ,
                     ".fst"           ))))
  # Add the starting penetration of multi-homing
  actual_cf[, start_pen_AB := start_multi_pen, ]
  # Calculate the mean penetration line for each counterfactual by iteration
  mean_tbl <- actual_cf[, .(m_pen_A         = mean(pen_A_beg    ) ,
                            m_pen_A_cf      = mean(pen_A_beg_cf ) ,
                            m_cf_pen_A_bin  = mean(cf_pen_A_bin)) ,
                        by = .(seed, iteration, period, 
                               u_A   , u_B   ,      
                               beta  , gamma , 
                               start_pen_AB  ,
                               MSE            )]
  mean_tbl[, ratio_cf_actual        := m_pen_A_cf     / m_pen_A, ]
  mean_tbl[, ratio_cf_bin_actual    := m_cf_pen_A_bin / m_pen_A, ]
  
  # Compare the mean of the counterfactuals
  counterfactual_curves <- mean_tbl[, .(mean_cf_actual     = mean(m_pen_A_cf     )  ,
                                        mean_cf_bin_actual = mean(m_cf_pen_A_bin )) ,
                                    by = .(u_A, u_B, beta, gamma, start_pen_AB, MSE, period)]
  # Resulting differences
  final_results <- mean_tbl[, .(mean_ratio_cf_actual     = mean(ratio_cf_actual)     ,
                                mean_ratio_cf_bin_actual = mean(ratio_cf_bin_actual)  ), 
                            by = .(u_A, u_B, beta, gamma, start_pen_AB, MSE, period)]
  # Add a year variable
  final_results[, Year := period + 2011, ] 
  # Rename the columns
  f <- final_results[, .(Year                                                              ,
                         "Mean Athey Estimated Counterfactual"  = mean_ratio_cf_bin_actual ,
                         "Mean Corrected Parker Counterfactual" = mean_ratio_cf_actual     ,
                         u_A, u_B, beta, gamma, start_pen_AB, MSE                           )]
  # Make a copy
  final_results <- copy(f)
  
  # Transpose the data
  transposed_f <- tdt(f)
  return(final_results)
}
