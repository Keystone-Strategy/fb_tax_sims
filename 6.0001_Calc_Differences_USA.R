calc_differences <- function(platform) {
  
  # Import the data
  unique_params_2    <- data.table(read_fst(file.path(out_path, paste0("Minimized Iterations MSE "   , platform, ".fst"))))
  minimum_iterations <- unique_params_2[1]
  actual_cf          <- data.table(read_fst(file.path(out_path, paste0(platform, "_Actual_Counterfactual_Affirmative_Model.fst"))))

  # Calculate the mean penetration line for each counterfactual by iteration
  mean_tbl <- actual_cf[, .(m_pen_A         = mean(pen_A_beg    ) ,
                            m_pen_A_cf      = mean(pen_A_beg_cf ) ,
                            m_cf_pen_A_bin  = mean(cf_pen_A_bin)) ,
                        by = .(seed, iteration, period)]
  mean_tbl[, ratio_cf_actual        := m_pen_A_cf     / m_pen_A, ]
  mean_tbl[, ratio_cf_bin_actual    := m_cf_pen_A_bin / m_pen_A, ]
  
  # Compare the mean of the counterfactuals
  counterfactual_curves <- mean_tbl[, .(mean_cf_actual     = mean(m_pen_A_cf     )  ,
                                        mean_cf_bin_actual = mean(m_cf_pen_A_bin )) , by = period]
  # Resulting differences
  final_results <- mean_tbl[, .(mean_ratio_cf_actual     = mean(ratio_cf_actual)     ,
                                mean_ratio_cf_bin_actual = mean(ratio_cf_bin_actual)  ), by = period]
  # Add a year variable
  final_results[, Year := period + 2011, ]
  # Keep if the year <= 2020
  final_results <- final_results[Year < 2021,,]
  # Add a column which is the difference between the two
  final_results[, diff := mean_ratio_cf_bin_actual - mean_ratio_cf_actual, ]
  # Add a row variable
  final_results[, Model := "Athey Estimated Ratio of No-Transfer MAUs to Actual MAUs" , ]
  # Cast the data to wide
  first_row <- dcast(final_results, Model ~ Year, value.var = "mean_ratio_cf_bin_actual" )
  # Add a different name for the model
  final_results[, Model := "Corrected Parker Model Resulting Ratio"                   , ]
  # Cast the data to wide
  second_row  <- dcast(final_results, Model ~ Year, value.var = "mean_ratio_cf_actual"     )
  # Add a different name for the difference
  final_results[, Model := "Difference (Athey Estimated - Corrected Parker)", ]
  # Cast the data to wide
  third_row  <- dcast(final_results, Model ~ Year, value.var = "diff"     )
  # Rbind the tables together
  ratio_maus <- rbind(first_row, second_row, third_row)
  # Save the final_tbl
  save_fst(ratio_maus            , paste0(platform, " Ratio of No-Transfer to Actual MAUs - Corrected Parker & Athey") , out_path)
  save_fst(counterfactual_curves , paste0(platform, " Counterfactual Curves of Corrected Parker and Athey Models"    ) , out_path)
  return(ratio_maus)
}
