rm(list=ls())
gc()

# important java restrictions
options(java.parameters = "- Xmx4096m")

# Get the user information
project_fld   <- "C:/_Athey_Production/08_Competitor_Production_Churn"
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
source(file.path(code_fld, "2.0001_USA_Generate_Same_Data.R"           ))
source(file.path(code_fld, "3.0001_Curve_Fit.R"                        ))
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

# @MG - ADD BACK TWITTER
print("TESTING CENTER ON!!!!!!!!!!!!!!!!!!!")
for (platform_name in c("YouTube")) {
  
  print(platform_name)
  # Generate the data for the US
  results_dt <- usa_gen_data(utility_A  = seq(-4.00, -2.00, 1.00) , utility_B      = seq(-4.00, -2.00, 1.00 )  ,
                             beta       = seq( 3.00,  3.00, 0.10) , gamma_input    = seq( 0.00,  4.00, 1.00 )  ,
                             platform   = platform_name                                                         )
  # Remove the large data.table
  rm(list=("results_dt"))
  gc()
  
  # Print that we've moved onto the curve fitting process
  print("Curve fit")
  # Find the best-fit curves
  min_its <- curve_fit(platform = platform_name)
  
  # For the set of best-fit curves at different levels of initial multi-homing
  for (i in 1:nrow(min_its)) {
    
    # Print the iteration
    print(paste0(i, " / ", nrow(min_its)))
    
    # Temporary data.table name for each row
    temp_min_its <- min_its[i]
    
    # Print the results calibration
    print("Calibrating results")
    # Run 250 simulations
    calibrated_results <- rbindlist(pblapply(c(1:25) , function(x) 
      calibrated_parameters(dt_min_its = "temp_min_its" ,
                            seed      = x               ,
                            platform  = platform_name    )))
    
    # Print the start of the estimation process
    print("Estimation")
    estimation_results <- estimate_simulate_usa(dt         = "calibrated_results" ,
                                                dt_min_its = "temp_min_its"       ,
                                                platform   = platform_name         )
    # Remove the calibrated results
    rm(list=("calibrated_results"))
    gc()
    rm(list=("estimation_results"))
    gc()
    
    print("Results")
    # Differences in results
    diff_results <- calc_differences(dt_min_its = "temp_min_its" ,
                                     platform = platform_name     )
    
    print("Output")
    # Output the data to an Excel sheet
    output_figure_data(dt_min_its = "temp_min_its" ,
                       platform   = platform_name   )
  }
}

