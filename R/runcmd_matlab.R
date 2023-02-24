#' Run MATLAB command with system command in R
#'
#' This function runs a MATLAB command with the system command in R
#' and saves the output to a log file.
#'
#' @param cmd A character string specifying the MATLAB command to run
#' @return No return value
#' @examples
#' # Run a MATLAB command and save output to a log file
#' runcmd_matlab("disp('Hello, MATLAB!')")
#' @export runcmd_matlab
runcmd_matlab <- function(cmd) {
  system(sprintf('matlab -batch "%s"', cmd))
}
