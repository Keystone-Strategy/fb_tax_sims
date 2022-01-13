final_param_set <- function(platform) {
  
  # TESTING CENTER
  # platform <- "YouTube"
  # print("TESTING CENTER ON!!!!!")
  
  agg_min_its <- data.table(read_fst(file.path(out_path,
                                               paste0("3.0002_" ,
                                                      platform  ,
                                                      ".fst"     ))))
  # Minimum by each starting level of multi-homing
  agg_min_its[!is.na(start_pen_AB), min_MSE_multi := min(MSE_tot) ,
              by = .(start_pen_AB)]
  # Keep only if it's equal to the minimum
  its <- agg_min_its[min_MSE_multi == MSE_tot,,]
  # Save the dataset
  save_fst(its, paste0("3.0003_", platform), out_path)
  write.csv(its, file.path(out_path, paste0("3.0003_", platform, ".csv")))
}