#' Example dataset for confidence accuracy relationship
#'
#' Dataset including identification accuracy, identification confidence estimates and
#' metamemory scores
#'
#' @docType data
#'
#' @usage data(metamemoryCA)
#'
#' @format  data frame with 356 rows and 9 variables.
#'
#' @keywords datasets
#'
#' @examples
#' data(metamemoryCA)
#' Ch <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                       correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser",
#'                       confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
#'                       jack = TRUE)
#'
"metamemoryCA"
