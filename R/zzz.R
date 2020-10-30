# Pkg startup message
.onAttach <- function(libname, pkgname) {
  message <- c("legalPsych package for calibration statistics")
  packageStartupMessage(paste(message, collapse = " "))
}
