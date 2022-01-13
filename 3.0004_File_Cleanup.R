file_cleanup <- function() {
  # Open a set of files starting with the following
  delete_files <- grep("2.0001|2.0002_|3.0001_", list.files(out_path), value=T)
  # Remove these files
  file.remove(file.path(out_path, delete_files))
}