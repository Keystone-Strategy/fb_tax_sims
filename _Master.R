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
  results_dt <- usa_gen_data(utility_A  = seq(-2.60, -2.20, 0.20) , utility_B      = seq(-2.60, -2.20, 0.20 )  ,
                             beta       = seq( 3.10,  3.50, 0.10) , gamma_input    = seq( 0.00,  0.50, 0.10 )  ,
                             platform   = platform_name                                                         )
  rm(list=("results_dt"))
  gc()
  
  print("Curve fit")
  min_its <- curve_fit(platform = platform_name)
  
  print("Calibrating results")
  calibrated_results <- rbindlist(pblapply(c(1:250) , function(x) 
    calibrated_parameters(seed=x                   ,
                          platform = platform_name  )))
  
  
  print("Estimation")
  estimation_results <- estimate_simulate_usa(dt       = "calibrated_results" , 
                                              platform = platform_name         )
  rm(list=("calibrated_results"))
  gc()
  rm(list=("estimation_results"))
  gc()

  print("Results")
  diff_results <- calc_differences(platform = platform_name)
  
  print("Output")
  output_figure_data(platform = platform_name)
}

