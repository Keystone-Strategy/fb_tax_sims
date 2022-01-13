rm(list=ls())
gc()

options(java.parameters = "- Xmx4096m")


# Options -----------------------------------------------------------------

# Set the minimum number of iterations
num_sims <-  10
grid_num <-  10

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
source(file.path(code_fld, "3.0004_File_Cleanup_1.R"                   ))
source(file.path(code_fld, "4.0001_Calibrated_Parameters_Generate.R"   ))
source(file.path(code_fld, "5.0001_Estimate_Simulate_USA.R"            ))
source(file.path(code_fld, "6.0001_Calc_Differences_USA.R"             ))
source(file.path(code_fld, "7.0001_File_Cleanup_2.R"                   ))
source(file.path(code_fld, "7.0002_File_Cleanup_3.R"                   ))


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
  # x = 1

  # Print the platform name
  print(platform_name)
  
  # Set the number of splits for the grid
  divisor_input <- grid_num
  # Create a smaller grid
  list_small_grids <- create_grid_sample(utility_A   = seq(-5.00,  1.00, 0.50) , 
                                         utility_B   = seq(-5.00,  1.00, 0.50) ,
                                         beta        = seq( 0.10,  5.00, 0.50) ,
                                         gamma_input = seq( 0.00,  5.00, 0.50) ,
                                         platform    = platform_name           ,
                                         divisor     = divisor_input            )
  # Print check
  print("Starting process of finding parameters")
  # Loop through different sets of initial parameters
  future_lapply(1:divisor_input       , future.seed = TRUE ,
                function(x) {
    # Generate the data for the smaller dataset
    results <- usa_gen_data(table_num = x             ,
                            platform  = platform_name  )
    # Find the best-fit curves
    min_its <- curve_fit(table_num = x            , 
                         platform = platform_name  )
  })
  
  # Additional file to get the minimum from each of the files
  min_params(platform  = platform_name , 
             divisor   = divisor_input  )

  # Return the final set of parameters
  final_param_set(platform = platform_name)
  # Load in the minimized iterations
  min_its <- data.table(read_fst(file.path(out_path,
                                           paste0("3.0003_"     ,
                                                  platform_name ,
                                                  ".fst"         ))))
  # File cleanup
  file_cleanup_1()
  
  # Order the data.table
  min_its <- min_its[order(MSE_tot)]
  # Get a count by each grouping
  min_its[, ranking := seq_len(.N), by = .(utility_A ,
                                           utility_B ,
                                           beta      ,
                                           gamma      )]
  # Limit to the top ranking if they're all the same parameters
  min_its <- min_its[ranking == 1]

  # Print check
  print("Starting simulation process to get results.")
  # For the set of best-fit curves at different levels of initial multi-homing
  assign(paste0(platform_name, "_diff_results_panel"), 
         rbindlist(lapply(1:nrow(min_its),
                                 function(x) {
    
    # Temporary data.table name for each row
    temp_min_its <- min_its[x]
    # Print Check
    save_fst(temp_min_its, paste0("_Master_A_"  , 
                                  platform_name  ),
             out_path)

    print("BEFORE CALIBRATION")
    # Run the simulations
    calibrated_results <- rbindlist(future_lapply(c(1:num_sims)         , 
                                                  future.seed = TRUE    ,
                                                  function(x) 
      calibrated_parameters(seed      = x               ,
                            platform  = platform_name    )))
    # Save the calibrated results
    save_fst(calibrated_results, paste0("_Master_B_"  ,
                                        platform_name  ),
             out_path)
    
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
    diff_results[, start_pen_AB := temp_min_its[, start_pen_AB, ], ]
    # Return the diff_results
    return(diff_results)
  
  })))
  # Second file cleanup
  file_cleanup_2(platform = platform_name)
}
# Third file cleanup
file_cleanup_3()
# Write the output
write.csv(Twitter_diff_results_panel , file.path(tables_figures, "Twitter Final Results.csv"))
write.csv(YouTube_diff_results_panel , file.path(tables_figures, "YouTube Final Results.csv"))
# check_reproducibility()


