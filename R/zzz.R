# Pkg startup message
.onAttach <- function(libname, pkgname) {
  message <- c("This is beta software. Bugs are possible, both in terms of programming",
               "errors and computation errors.")
  packageStartupMessage(paste(message, collapse = " "))
}
