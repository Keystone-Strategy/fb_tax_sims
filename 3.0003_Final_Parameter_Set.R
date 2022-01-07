final_param_set <- function(platform) {
  
  # TESTING CENTER
  # platform <- "Twitter"
  
  # Import the dataset
  print(platform)
  print("3.0003")
  agg_min_its <- data.table(readRDS(file.path(out_path, paste0(platform, " Aggregate Set of Parameters.RDS"))))
  # Minimum by each starting level of multi-homing
  agg_min_its[!is.na(start_pen_AB), min_MSE_multi := min(MSE_tot) ,
              by = .(start_pen_AB)]
  # Keep only if it's equal to the minimum
  its <- agg_min_its[min_MSE_multi == MSE_tot,,]
  # Save the dataset
  save_fst(its, "Final - Minimized Iterations", out_path)
  write.csv(its, file.path(out_path, "Final - Minimized Iterations.csv"))
}