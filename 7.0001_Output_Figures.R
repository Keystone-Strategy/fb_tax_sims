output_figure_data <- function(dt_min_its , 
                               platform    ) {
  
  # Remove the Excel file if it exists 
  if (platform == "Twitter") {
    if (file.exists(file.path(tables_figures, "Competitor Production Tables and Figures.xlsx"))) {
      file.remove(file.path(tables_figures, "Competitor Production Tables and Figures.xlsx"))
    }
  }
  
  # Load in the dataset
  input_data         <- data.table(read_fst(file.path(out_path, paste0(platform, "usa_generated_data.fst"))))
  gen_data           <- copy(input_data)
  minimum_iterations <- get(dt_min_its )
  minimum_iterations <- temp_min_its
  start_multi_pen    <- minimum_iterations[, start_pen_AB, ]

  # Create the tabs for the Excel sheet -------------------------------------

  # Create the Excel file if it isn't already created
  if (!file.exists(file.path(tables_figures, "Competitor Production Tables and Figures.xlsx"))) {
    wb <- createWorkbook()
  } else {
    wb <- loadWorkbook(file.path(tables_figures, "Competitor Production Tables and Figures.xlsx"))
  }
  
  # Pre-set a list of the tab names for the Excel
  tab_names <- c("Figure 1A", "Figure 1B", 
                 "Figure 1C", "Figure 1D",
                 "Figure 2" , "Figure 3"  )
  
  # Add a worksheet for each of the Figures
  lapply(tab_names, function(x) {
    if (!(x %in% sheets(wb))) {
      addWorksheet(wb        = wb        ,
                   sheetName = x         , 
                   gridLines = FALSE     ,
                   tabColour = "#000000"  )
    }
  })

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
                                 beta_input  =  3.0        )
    figure_1b <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -2.0       ,
                                 gamma_input =  4.0       ,
                                 beta_input  =  3.0        )
    figure_1c <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -4.0       ,
                                 gamma_input =  0.0       ,
                                 beta_input  =  3.0        )
    figure_1d <- get_figure_data(dt          = "gen_data" ,
                                 utility_A   = -3.0       ,
                                 utility_B   = -4.0       ,
                                 gamma_input =  4.0       ,
                                 beta_input  =  3.0        )
    # Write the data to the workbook
    writeData(wb, sheet = "Figure 1A", x = figure_1a)
    
    # write.csv(figure_1a , file=file.path(tables_figures, "Figure 1A.csv"))
    # write.xlsx(figure_1a, file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 1A", append=TRUE, row.names=FALSE)
    
    # Write the data to the workbook
    writeData(wb, sheet = "Figure 1B", x = figure_1b)

    # Write the data to the workbook
    writeData(wb, sheet = "Figure 1C", x = figure_1c)
    
    # Write the data to the workbook
    writeData(wb, sheet = "Figure 1D", x = figure_1d)
    
    # write.csv(figure_1b , file=file.path(tables_figures, "Figure 1B.csv"))
    # write.xlsx(figure_1b, file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 1B", append=TRUE, row.names=FALSE)
    # write.csv(figure_1c , file=file.path(tables_figures, "Figure 1C.csv"))
    # write.xlsx(figure_1c, file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 1C", append=TRUE, row.names=FALSE)
    # write.csv(figure_1d , file=file.path(tables_figures, "Figure 1D.csv"))
    # write.xlsx(figure_1d, file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 1D", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 2 -----------------------------------------------------
  
  fb_comps <- data.table(read_fst(file.path(out_path, "FB_Competitor_Penetration.fst")))
  if (platform == "Twitter") {
    figure_2 <- fb_comps[, .(year, fb_pen, twitter_pen), ]
    # Write the data to the workbook
    writeData(wb, sheet = "Figure 2", x = figure_2)
    # write.csv(figure_2  , file=file.path(tables_figures, "Figure 2.csv"))
    # write.xlsx(figure_2 , file=file.path(
    #   tables_figures,
    #   "Competitor Production Tables and Figures.xlsx"),
    #   sheetName="Figure 2", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 3 -----------------------------------------------------
  
  if (platform == "YouTube"){
    figure_3 <- fb_comps[, .(year(date), fb_pen, youtube_pen), ]
    # Write the data to the workbook
    writeData(wb, sheet = "Figure 3", x = figure_3)
    # write.csv(figure_3  , file=file.path(tables_figures, "Figure 3.csv"))
    # write.xlsx(figure_3 , file=file.path(
    #   tables_figures,
    #   "Competitor Production Tables and Figures.xlsx"),
    #   sheetName="Figure 3", append=TRUE, row.names=FALSE)
  }
  
  # Output for Figure 4 -----------------------------------------------------
  
  if (platform == "Twitter") {
    figure_4 <- data.table(read_fst(
      file.path(out_path, 
                paste0("Minimum Iteration for " ,
                       platform                 ,
                       " StartMulti-"           ,
                       start_multi_pen          ,
                       ".fst"                    ))))
    # Sheet name
    sheet_name <- paste0("Figure 4-", "StartMulti", start_multi_pen)
    # Add a worksheet
    addWorksheet(wb        = wb         ,
                 sheetName = sheet_name , 
                 gridLines = FALSE      ,
                 tabColour = "#000000"   )
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = figure_4)
    # write.csv(figure_4  , file=file.path(tables_figures, "Figure 4.csv"))
    # write.xlsx(figure_4 , file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 4", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 5 -----------------------------------------------------
  
  if (platform == "YouTube") {
    figure_5 <- data.table(read_fst(
      file.path(out_path, 
                paste0("Minimum Iteration for " ,
                       platform                 ,
                       " StartMulti-"           ,
                       start_multi_pen          ,
                       ".fst"                    ))))
    # Sheet name
    sheet_name <- paste0("Figure 5-", "StartMulti", start_multi_pen)
    # Add a worksheet
    if (!(sheet_name %in% sheets(wb))) {
      addWorksheet(wb        = wb         ,
                   sheetName = sheet_name , 
                   gridLines = FALSE      ,
                   tabColour = "#000000"   )
    }
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = figure_5)
    # write.csv(figure_5  , file=file.path(tables_figures, "Figure 5.csv"))
    # write.xlsx(figure_5 , file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 5", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 6 -----------------------------------------------------
  
  if (platform == "Twitter") {
    figure_6 <- data.table(read_fst(
      file.path(out_path, paste0(platform                                                          ,
                                 " Counterfactual Curves of Corrected Parker and Athey Models"     ,
                                 "Start_Multi"                                                     ,
                                 start_multi_pen                                                   ,
                                 ".fst"                                                             ))))
    # Sheet name
    sheet_name <- paste0("Figure 6-", "StartMulti", start_multi_pen)
    # Add a worksheet
    if (!(sheet_name %in% sheets(wb))) {
      addWorksheet(wb        = wb         ,
                   sheetName = sheet_name , 
                   gridLines = FALSE      ,
                   tabColour = "#000000"   )
    }
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = figure_6)
    # write.csv(figure_6  , file=file.path(tables_figures, "Figure 6.csv"))
    # write.xlsx(figure_6 , file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 6", append=TRUE, row.names=FALSE)
  }

  # Output for Figure 7 -----------------------------------------------------
  
  if (platform == "YouTube") {
    figure_7 <- data.table(read_fst(
      file.path(out_path, paste0(platform                                                          ,
                                 " Counterfactual Curves of Corrected Parker and Athey Models"     ,
                                 "Start_Multi"                                                     ,
                                 start_multi_pen                                                   ,
                                 ".fst"                                                             ))))
    # Sheet name
    sheet_name <- paste0("Figure 7-", "StartMulti", start_multi_pen)
    # Add a worksheet
    if (!(sheet_name %in% sheets(wb))) {
      addWorksheet(wb        = wb         ,
                   sheetName = sheet_name , 
                   gridLines = FALSE      ,
                   tabColour = "#000000"   )
    }
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = figure_7)
    # write.csv(figure_7  , file=file.path(tables_figures, "Figure 7.csv"))
    # write.xlsx(figure_7 , file=file.path(tables_figures,
    #                                      "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Figure 7", append=TRUE, row.names=FALSE)
  }

  # Table 1 -----------------------------------------------------------------
  
  if (platform == "Twitter"){
    table_1 <- data.table(read_fst(
      file.path(out_path, paste0(platform                                                           ,
                                 " Ratio of No-Transfer to Actual MAUs - Corrected Parker & Athey " ,
                                 "Start_Multi"                                                      ,
                                 start_multi_pen                                                    ,
                                 ".fst"                                                              ))))
    # Sheet name
    sheet_name <- paste0("Table 1-", "StartMulti", start_multi_pen)
    # Add a worksheet
    if (!(sheet_name %in% sheets(wb))) {
      addWorksheet(wb        = wb         ,
                   sheetName = sheet_name , 
                   gridLines = FALSE      ,
                   tabColour = "#000000"   )
    }
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = table_1)
    # write.csv(table_1  , file=file.path(tables_figures, "Table 1.csv"))
    # write.xlsx(table_1 , file=file.path(tables_figures,
    #                                     "Competitor Production Tables and Figures.xlsx"),
    #            sheetName=" Table 1", append=TRUE, row.names=FALSE)
  }
  
  # Table 2 -----------------------------------------------------------------
  
  if (platform == "YouTube") {
    table_2 <- data.table(read_fst(
      file.path(out_path, paste0(platform                                                           ,
                                 " Ratio of No-Transfer to Actual MAUs - Corrected Parker & Athey " ,
                                 "Start_Multi"                                                      ,
                                 start_multi_pen                                                    ,
                                 ".fst"                                                              ))))
    # Sheet name
    sheet_name <- paste0("Table 2-", "StartMulti", start_multi_pen)
    # Add a worksheet
    if (!(sheet_name %in% sheets(wb))) {
      addWorksheet(wb        = wb         ,
                   sheetName = sheet_name , 
                   gridLines = FALSE      ,
                   tabColour = "#000000"   )
    }
    # Write the data to the workbook
    writeData(wb, sheet = sheet_name, x = table_2)
    # write.csv(table_2  , file=file.path(tables_figures, "Table 2.csv"))
    # write.xlsx(table_2 , file=file.path(tables_figures,
    #                                     "Competitor Production Tables and Figures.xlsx"),
    #            sheetName="Table 2", append=TRUE, row.names=FALSE)
  }
  # Save the end workbook
  saveWorkbook(wb, file.path(tables_figures                                  , 
                             "Competitor Production Tables and Figures.xlsx"  ),
                             overwrite = TRUE                                 )
}
