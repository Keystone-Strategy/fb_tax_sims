min_params <- function(table_num ,
                       platform   ) {
  
  # TESTING CENTER
  # table_num <- 33
  # platform   <- "YouTube"
  Sys.sleep(1)
  print(table_num)
  # withCallingHandlers({
  print(platform)
  print("3.0002")
  print("Before agg_min_its import")
  # tryCatch({
    agg_min_its <- data.table(readRDS(file.path(out_path, paste0(platform, " Aggregate Set of Parameters.RDS"))))
  # },
  # warning = function(table = table_num) {
  #   message(paste0("Table ", table, " caused an issue where it wouldn't read in."))
  # },
  # finally={
  #   message(paste0("Attempting to open the same file again"))
  #   agg_min_its <- data.table(readRDS(file.path(out_path, paste0(platform, " Aggregate Set of Parameters.RDS"))))
  # })
  # },
  # warning = function(w) {print(paste0('table_num=',table_num,',platform=',platform)); print(w)}
  # )
  # Load in the data
  min_iterations_start_multi <- data.table(read_fst(
    file.path(out_path, paste0("Minimized Iterations Starting Penetration ",
                               platform, "_grid_num_", table_num, ".fst"))))
  # Add the table number it came from
  min_iterations_start_multi[, table_number := table_num, ]
  # Rbind all the data together
  agg_min_its <- rbind(agg_min_its, min_iterations_start_multi)
  # Save the data
  saveRDS(agg_min_its, file.path(out_path, paste0(platform, " Aggregate Set of Parameters.RDS")))
}