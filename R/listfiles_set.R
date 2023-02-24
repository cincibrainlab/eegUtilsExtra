#' List SET files within subdirectories
#'
#' Find all SET files in a directory tree containing a certain string, and return the results as a # nolint
#' data frame.
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import tidyr
#' @importFrom rlang .data
#' @param dir_path A character string specifying the root directory to search.
#' @param subdir_string A character string specifying the subdirectory pattern to search for.
#' @param custom_keyword A character string specifying the keyword to search for in the SET file names.
#' @return A data frame with the results, including the subdirectory, file name (without extension), and extension.
#' @name listfiles_set
#' @keywords listfiles_set, files, directory
#' @examples
#' # List SET files in a directory tree containing "data" and "subject1"
#' # listfiles_set("/path/to/directory", "data|subject1", "SET")


listfiles_set <- function(dir_path = ".", subdir_string = "", custom_keyword = "") {
  # Find all files in a directory tree with a given extension and containing a keyword
  # in their name, in subdirectories containing a certain string
#  requireNamespace("dplyr", quietly = TRUE)

  # Find subdirectories containing a certain string
  subdir_paths <- list.files(dir_path, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  subdir_paths <- subdir_paths[grep(subdir_string, subdir_paths)]

  if (length(subdir_paths) == 0) {
    warning(paste0("No subdirectories found containing '", subdir_string, "'"))
    return(data.frame())
  }

  # Find SET files in subdirectories with a certain keyword
  file_paths <- list.files(subdir_paths, recursive = TRUE, full.names = TRUE, pattern = paste0(".*", custom_keyword, ".*\\.(set)"))

  if (length(file_paths) == 0) {
    warning(paste0("No SET files found containing '", custom_keyword, "'"))
    return(data.frame())
  }

  # Generate a data frame with the results
  file_df <- tibble(file_paths) %>%
    # Extract the subdirectory and filename (without extension) from the full file path
    mutate(subdir = dirname(file_paths),
           full_filename = basename(file_paths)) %>%
    separate(full_filename, c("name", "extension"), sep = "\\.(?=[^\\.]+$)") %>%
    # Remove duplicates based on subdirectory, filename, and extension
    distinct(subdir, name, extension)

  return(file_df)
}
