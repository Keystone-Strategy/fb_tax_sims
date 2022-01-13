file_cleanup_2 <- function(platform) {
  # Open a set of files starting with the following
  delete_files     <- grep("5.0001_", list.files(out_path)  , value=T)
  additional_files <- c(paste0("3.0002_", platform, ".fst") ,
                        paste0("3.0003_", platform, ".fst"   ))
  # Append to delete the files
  delete_files <- append(additional_files, delete_files)
  # Remove these files
  file.remove(file.path(out_path, delete_files))
}