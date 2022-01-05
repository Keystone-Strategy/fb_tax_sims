output_figure_data <- function(platform) {
  
  # Delete the Excel file if it exists
  comp_excel <- file.path(tables_figures, "Competitor Production Tables and Figures.xlsx")
  if (file.exists(comp_excel) & platform == "Twitter") {
    file.remove(comp_excel)
  }

  # Load in the dataset
  input_data <- data.table(read_fst(file.path(out_path, paste0(platform, "usa_generated_data.fst"))))
  gen_data   <- copy(input_data)
  
  # Output for Figure 1 -----------------------------------------------------
  
  # Write a custom function to get the output from each of these
  get_figure_data <- function(dt, utility_A, utility_B, gamma_input, beta_input) {
    # Get the max level of multi-homing
    max_multi <- get(dt)[u_A     == utility_A   &
                           u_B   == utility_B   &
                           gamma == gamma_input &
                           beta  == beta_input   ,
                         .N,
                         by = start_pen_AB_copy][,
                                                 max(start_pen_AB_copy, na.rm = T)]
    # Get the data for the figure
    figure_data <- get(dt)[u_A                 == utility_A    &
                             u_B               == utility_B    &
                             gamma             == gamma_input  &
                             beta              == beta_input   &
                             start_pen_AB_copy == max_multi      ,
                           .(period, pen_A_beg, pen_B_beg),         ]
    # Return the data
    return(figure_data)
  }
  
  # If it's YouTube then output the data
  if (platform == "YouTube") {
    figure_1a <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -2.0       ,
                                 gamma_input =  0.0       ,
                                 beta_input  =  0.5        )
    figure_1b <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -2.0       ,
                                 gamma_input =  4.0       ,
                                 beta_input  =  0.5        )
    figure_1c <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -4.0       ,
                                 gamma_input =  0.0       ,
                                 beta_input  =  0.5        )
    figure_1d <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -4.0       ,
                                 gamma_input =  4.0       ,
                                 beta_input  =  0.5        )
    write.csv(figure_1a , file=file.path(tables_figures, "Figure 1A.csv"))
    write.xlsx(figure_1a, file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 1A", append=TRUE, row.names=FALSE)
    write.csv(figure_1b , file=file.path(tables_figures, "Figure 1B.csv"))
    write.xlsx(figure_1b, file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 1B", append=TRUE, row.names=FALSE)
    write.csv(figure_1c , file=file.path(tables_figures, "Figure 1C.csv"))
    write.xlsx(figure_1c, file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 1C", append=TRUE, row.names=FALSE)
    write.csv(figure_1d , file=file.path(tables_figures, "Figure 1D.csv"))
    write.xlsx(figure_1d, file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 1D", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 2 -----------------------------------------------------
  
  fb_comps <- data.table(read_fst(file.path(out_path, "FB_Competitor_Penetration.fst")))
  if (platform == "Twitter") {
    figure_2 <- fb_comps[, .(year, fb_pen, twitter_pen), ]
    write.csv(figure_2  , file=file.path(tables_figures, "Figure 2.csv"))
    write.xlsx(figure_2 , file=file.path(
      tables_figures,
      "Competitor Production Tables and Figures.xlsx"),
      sheetName="Figure 2", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 3 -----------------------------------------------------
  
  if (platform == "YouTube"){
    figure_3 <- fb_comps[, .(year(date), fb_pen, youtube_pen), ]
    write.csv(figure_3  , file=file.path(tables_figures, "Figure 3.csv"))
    write.xlsx(figure_3 , file=file.path(
      tables_figures,
      "Competitor Production Tables and Figures.xlsx"),
      sheetName="Figure 3", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 4 -----------------------------------------------------
  
  if (platform == "Twitter") {
    figure_4 <- data.table(read_fst(
      file.path(out_path,
                paste0("Data from Best Fit for " ,
                       platform                  ,
                       ".fst"                     ))))
    write.csv(figure_4  , file=file.path(tables_figures, "Figure 4.csv"))
    write.xlsx(figure_4 , file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 4", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 5 -----------------------------------------------------
  
  if (platform == "YouTube") {
    figure_5 <- data.table(read_fst(
      file.path(out_path,
                paste0("Data from Best Fit for " ,
                       platform                  ,
                       ".fst"                     )))) 
    write.csv(figure_5  , file=file.path(tables_figures, "Figure 5.csv"))
    write.xlsx(figure_5 , file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 5", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 6 -----------------------------------------------------
  
  if (platform == "Twitter") {
    figure_6 <- data.table(read_fst(
      file.path(out_path, paste0(platform, 
                                 " Counterfactual Curves of Corrected Parker and Athey Models.fst"))))
    write.csv(figure_6  , file=file.path(tables_figures, "Figure 6.csv"))
    write.xlsx(figure_6 , file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 6", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 7 -----------------------------------------------------
  
  if (platform == "YouTube") {
    figure_7 <- data.table(read_fst(
      file.path(out_path, paste0(platform, 
                                 " Counterfactual Curves of Corrected Parker and Athey Models.fst"))))
    write.csv(figure_7  , file=file.path(tables_figures, "Figure 7.csv"))
    write.xlsx(figure_7 , file=file.path(tables_figures,
                                         "Competitor Production Tables and Figures.xlsx"),
               sheetName="Figure 7", append=TRUE, row.names=FALSE)
  }

  # Table 1 -----------------------------------------------------------------
  
  if (platform == "Twitter"){
    table_1 <- data.table(read_fst(
      file.path(out_path, paste0(platform, 
                " Ratio of No-Transfer to Actual MAUs - Corrected Parker & Athey.fst"))))
    write.csv(table_1  , file=file.path(tables_figures, "Table 1.csv"))
    write.xlsx(table_1 , file=file.path(tables_figures,
                                        "Competitor Production Tables and Figures.xlsx"),
               sheetName=" Table 1", append=TRUE, row.names=FALSE)
  }
  
  # Table 2 -----------------------------------------------------------------
  
  if (platform == "YouTube") {
    table_2 <- data.table(read_fst(
      file.path(out_path, paste0(platform, 
                "Ratio of No-Transfer to Actual MAUs - Corrected Parker & Athey.fst"))))
    write.csv(table_2  , file=file.path(tables_figures, "Table 2.csv"))
    write.xlsx(table_2 , file=file.path(tables_figures,
                                        "Competitor Production Tables and Figures.xlsx"),
               sheetName="Table 2", append=TRUE, row.names=FALSE)
  }
}



