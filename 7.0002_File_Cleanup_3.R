file_cleanup_3 <- function() {
  # Open a set of files starting with the following
  delete_files     <- grep("*.fst", list.files(out_path), value=T)
  # Remove these files
  file.remove(file.path(out_path, delete_files))
}