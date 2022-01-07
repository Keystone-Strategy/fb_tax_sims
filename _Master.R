rm(list=ls())
gc()

options(java.parameters = "- Xmx4096m")

# Get the user information
project_fld   <- "C:/_Athey_Production/10_Competitor_Production"
code_fld      <- file.path(project_fld, "01_Code")
fn_file       <- file.path(code_fld, "_FN.R")

# source and run the script
source(fn_file)
source(file.path(code_fld, "0.0001_Import_User_Data.R"                 ))
source(file.path(code_fld, "0.0002_Import_USA_MAU_Data.R"              ))
source(file.path(code_fld, "0.0003_Import_USA_Internet_Percent.R"      ))
source(file.path(code_fld, "0.0004_Import_USA_Population_Data.R"       ))
source(file.path(code_fld, "0.0005_Import_Twitter_MAUs_Data.R"         ))
source(file.path(code_fld, "0.0006_Import_YouTube_MAUs.R"              ))
source(file.path(code_fld, "0.0007_Import_Multi_Homing_Data.R"         ))
source(file.path(code_fld, "0.0008_Import_Yearly_Churn.R"              ))
source(file.path(code_fld, "1.0001_Create_USA_Facebook_Penetration.R"  ))
source(file.path(code_fld, "1.0002_Merge_Competitor_Penetration.R"     ))
source(file.path(code_fld, "2.0001_Generate_Grid_List.R"               ))
source(file.path(code_fld, "2.0002_USA_Generate_Same_Data.R"           ))
source(file.path(code_fld, "3.0001_Curve_Fit.R"                        ))
source(file.path(code_fld, "3.0002_Aggregate_Iterations.R"             ))
source(file.path(code_fld, "3.0003_Final_Parameter_Set.R"              ))
source(file.path(code_fld, "4.0001_Calibrated_Parameters_Generate.R"   ))
source(file.path(code_fld, "5.0001_Estimate_Simulate_USA.R"            ))
source(file.path(code_fld, "6.0001_Calc_Differences_USA.R"             ))
source(file.path(code_fld, "7.0001_Output_Figures.R"                   ))


# Run the scripts to import the data
import_user_data()                  # 0.0001_Import_User_Data.R
import_usa_mau_data()               # 0.0002_Import_USA_MAU_Data.R
import_usa_internet_pct_data()      # 0.0003_Import_Twitter_Penetration_Data.R
import_usa_population_data()        # 0.0004_Import_USA_Population_Data.R
import_twitter_maus()               # 0.0005_Import_Twitter_MAUs_Data.R
import_youtube_maus()               # 0.0006_Import_YouTube_MAUs.R
import_multi_homing_data()          # 0.0007_Import_Multi_Homing_Data.R
import_yearly_churn()               # 0.0008_Import_Yearly_Churn.R
create_USA_facebook_penetration()   # 1.0001_Create_USA_Facebook_Penetration.R
merge_fb_pen_competitor_maus()      # 1.0002_Merge_Competitor_Penetration.R

for (platform_name in c("Twitter", "YouTube")) {
  
  # TESTING CENTER!!!!!!!!
  # platform_name <- "YouTube"
  # print(platform_name)
  # Set the divisor for the data
  
  # Initialize a data.table for storing later results
  agg_min_its <- data.table(iteration    = NA, utility_A    = NA, utility_B = NA, beta         = NA,
                            gamma        = NA, start_pen_AB = NA, MSE_tot   = NA, table_number = NA )
  saveRDS(agg_min_its, file.path(out_path, paste0(platform_name, " Aggregate Set of Parameters.RDS")))

  # Print the platform name
  print(platform_name)
  
  divisor_input <- 37
  # Create a smaller grid
  list_small_grids <- create_grid_sample(utility_A   = seq(-5.00,  1.00, 1.00) , 
                                         utility_B   = seq(-5.00,  1.00, 1.00) ,
                                         beta        = seq( 0.10,  5.00, 1.00) ,
                                         gamma_input = seq( 0.00,  5.00, 1.00) ,
                                         platform    = platform_name           ,
                                         divisor     = divisor_input            )
  list_small_grids[[1]]
  # Print check
  print("Starting process of finding parameters")
  # Loop through different sets of initial parameters
  lapply(1:divisor_input, function(x) {
    # Generate the data for the smaller dataset
    results  <- usa_gen_data(table_num = x             ,
                             platform  = platform_name  )
    # Find the best-fit curves
    min_its <- curve_fit(table_num = x            , 
                         platform = platform_name  )
    # Additional file to get the minimum from each of the files
    min_params(table_num = x            ,
               platform = platform_name  )
    
  })
  
  # Return the final set of parameters
  final_param_set(platform = platform_name)
  # Load in the minimized iterations
  min_its <- data.table(read_fst(file.path(out_path, "Final - Minimized Iterations.fst")))
  # Order the data.table
  min_its <- min_its[order(MSE_tot)]
  # Get a count by each grouping
  min_its[, ranking := seq_len(.N), by = .(utility_A,
                                           utility_B,
                                           beta     ,
                                           gamma     )]
  # Keep only the top ranking by each parameter space
  min_its <- min_its[ranking == 1,,]

  # Print check
  print("Starting simulation process to get results.")
  # Set the minimum number of iterations
  num_sims <- 5
  # For the set of best-fit curves at different levels of initial multi-homing
  assign(paste0(platform_name, "_diff_results_panel"), 
         rbindlist(pblapply(1:nrow(min_its),
                                 function(x) {
    
    # Temporary data.table name for each row
    temp_min_its <- min_its[x]
    # Print Check
    save_fst(temp_min_its, "Temporary Iteration", out_path)

    print("BEFORE CALIBRATION")
    # Run the simulations
    calibrated_results <- rbindlist(future_lapply(c(1:num_sims), future.seed = TRUE, 
                                                  function(x) 
      calibrated_parameters(seed      = x               ,
                            platform  = platform_name    )))
    # Save the calibrated results
    save_fst(calibrated_results, "Output from Calibration", out_path)
    
    # Remove the calibrated results
    rm(list=("calibrated_results"))
    gc()
    
    print("AFTER CALIBRATION, BEFORE ESTIMATION")
    # Estimate based on the calbirated results
    estimation_results <- estimate_simulate_usa(dt         = "calibrated_results" ,
                                                platform   = platform_name         )
    
    rm(list=("estimation_results"))
    gc()
    
    print("AFTER ESTIMATION, BEFORE RESULTS")
    # Differences in results
    diff_results <- calc_differences(platform = platform_name)
    print("AFTER RESULTS")
    # Add the ranking
    diff_results[, start_pen_AB := temp_min_its[, ranking, ], ]
    # Return the diff_results
    return(diff_results)
  
  })))
}
write.csv(Twitter_diff_results_panel , file.path(tables_figures, "Twitter Final Results.csv"))
write.csv(YouTube_diff_results_panel , file.path(tables_figures, "YouTube Final Results.csv"))

