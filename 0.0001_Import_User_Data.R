import_user_data <- function() {
  # Import user data from Affirmative report
  user_data <- data.table(fread(file.path(input_fld, "8. UserModelData_Main.csv")))
  # Calculate the Facebook penetration
  user_data[, fb_pen := FB.MAU.t / Int.t, ]
  # Save the user model data
  save_fst(user_data, "0.0001", out_path)
}