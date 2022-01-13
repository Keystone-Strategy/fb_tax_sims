import_multi_homing_data <- function() {
  # Data available from Dr. Athey's Rebuttal Report (paragraph 21, pg. 11)
  mult_dt <- data.table(platform = c("Twitter"  , "YouTube"    ) ,
                        m_2014   = c(0.22       ,  NA          ) ,
                        m_2018   = c(0.32       ,  0.87        )  )
  # Save the table
  save_fst(mult_dt, "0.0007", out_path)
}