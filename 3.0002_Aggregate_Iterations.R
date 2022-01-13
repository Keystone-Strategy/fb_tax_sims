min_params <- function(platform ,  
                       divisor   ) {
  
  # TESTING CENTER
  # divisor    <- 1
  # platform   <- "YouTube"
  # print("TESTING CENTER ON!!!!")
  
  # Load in all the data
  agg_min_its <- rbindlist(lapply(1:divisor, function(x) {
    temp_dt <- data.table(read_fst(file.path(
      out_path, paste0("3.0001_"    ,
                       platform     ,
                       "_grid_num_" ,
                       x            ,
                       "_A"         ,
                       ".fst"        ))))
    temp_dt[, table_number := x, ]
  }))
  # Save the data
  save_fst(agg_min_its, paste0("3.0002_" ,
                               platform   ), 
           out_path)
}