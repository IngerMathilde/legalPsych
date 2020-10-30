## These are the Confidence-Accuracy relationship functions

#' Create confidence-accuracy relationship object
#'
#' \code{CA.rel()} returns an object which can be placed into the different summary
#'     functions (e.g.,\code{CA.curves()}, \code{CA.print()}) to showcase the different aspect of the
#'     confidence-accuracy relationship.
#' @param data Dataset used for analysis.
#' @param confidence A string which identifies the variable that holds the confidence estimations.
#' @param correct A string which identifies the variable that states whether a person made a
#'    correct identification rated as 0 incorrect and 1 correct.
#' @param test A character string specifying the confidence-accuracy relationship test. The
#'    following is permitted: "CAL" (calibration) or "CAC" (confidence-accuracy characteristics)
#' @param confidenceLevels A list or vector identifying the levels for which confidence is tested.
#'    Either all levels can be tested separately of confidence levels can also be collapsed.
#' @param var   A vector containing variable names, which allows comparison of calibration or
#'    CAC performance across different groups of those variables (e.g., choosers vs. nonchoosers).
#' @param var.names A vector containing variable names that will be displayed in the graph.
#' @param var.levels A vector to be declared if one wants to compare only specific levels of a
#'    certain variable with each other.
#' @param jack A logical variable. If true, a jackknife SEs will be calculated to attain 95 percent
#'    confidence intervals for the C, OU, NRI and ANRI statistics.
#' @param confMin An integer, indicating the minimum value of the confidence scale used. It is only
#'    necessary to define this variable, if the confidenceLevel variable disregards the lower
#'    spectrum of the attainable confidence levels
#' @param confMax An integer, indicating the maximum value of the confidence scale used. It is only
#'    necessary to define this variable, if the confidenceLevel variable disregards the higher
#'    spectrum of the attainable confidence levels
#' @details For a more detailed explanation see the github page for a manual.
#'    \url{https://github.com/IngerMathilde/CArelationship}
#' @author Inger van Boeijen <\email{inger.vb.r@gmail.com}>
#' @return A CA.rel object that can be put into several summary functions
#' @examples
#' library(jtools)
#' data(metamemoryCA)
#'
#' # Compare choosers vs. nonchoosers with collapsed confidence groups and Jackknife SE
#' Ch <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                       correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser",
#'                       confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
#'                       jack = TRUE)
#' CA.curves(Ch)
#' CA.table(Ch)
#'
#' # Create CAC curves for high vs. low raters with adjusted variable names
#' data.CAC <- subset(metamemoryCA, ChoiceValue == "Target") #create a subset with only suspect ID
#' CAC <- CA.rel(data = data.CAC, confidence = "Confidence",
#'                        correct = "ChoiceCorrect", test = "CAC", var =
#'                        c("Rater.EMS.Relative.Face.Recognition", "Rater.EMS.Eyewitness.Ability"),
#'                        var.names = c("EMS Relative Face Recognition", "EMS Eyewitness Ability"),
#'                        var.levels = c('Low', 'High'),
#'                        confidenceLevels = list(c(0,60), c(70,80), c(90,100)))
#' CA.curves(CAC)
#' CA.table(CAC)
#'
#'
#' @export CA.rel

CA.rel <- function(data, confidence, correct, test, confidenceLevels, method="none", var = NULL,
                   var.names = NULL, var.levels = NULL, jack = FALSE, confMin = NULL,
                   confMax = NULL){
  if(!test %in% c("CAL", "CAC")){stop('The test needs to be either "CAL" or "CAC"')}

  # Get confidence variable
  if(confidence %in% names(data)){confidence <- data[[confidence]]
  }else{stop("The declared confidence variable is not present in dataset")}

  #Get correct variable
  if(correct %in% names(data)){correct <- data[[correct]]
  }else{stop("The declared correct variable is not present in dataset")}

  if(length(confidence)!=length(correct)){
    return(stop("The confidence and correct variables are not of the same length"))}

  # Check whether one wants to show a calibration plot of the whole dataset (single = T) or wants a
  # calibration analysis per level of a variable (single = F; e.g., chooser vs chooser). Also if a
  # variable is specified, check whether all the levels of the variable need to be analyzed
  # (e.g., low, med, high raters) or just specific levels of a variable (e.g., only low vs. high
  # and disregard medium raters).
  if(is.null(var)){single <- T}else{single <- FALSE; if(is.null(var.levels)){
    var.levels <- sapply(var, function(x){c(unique(as.character(data[[x]])))}, simplify = FALSE)
  }else{var.levels <- sapply(var, function(x){var.levels}, simplify = FALSE)}}

  # Check whether confMin was defined if not base it upon the confidenceLevel variable
  if(is.null(confMin)){confMin <- min(unlist(confidenceLevels))}

  ## Check whether confMax was defined if not base it upon the confidenceLevel variable
  if(is.null(confMax)){confMax <- max(unlist(confidenceLevels))}

  # If confidence levels are collapsed create names that reflect that (e.g., level 1 is 10-30%, etc)
  names(confidenceLevels)<- unlist(lapply(confidenceLevels, function(x) paste(x, collapse="-")))

  # Create CA.rel variables
  ## Create a properties variable which is used later on to select the appropriate functions

  prop <- list(single = single, test = test, jack = jack, confidenceLevels = confidenceLevels,method=method)

  # Create a variable all, which basically includes all the necessary information.
  all <- if(single){calibration(confidence, correct, confidenceLevels = confidenceLevels,
                                method=method,
                                jack = jack, confMin = confMin, confMax = confMax)
  }else{sapply(var, function(x){sapply(var.levels[[x]], function(y){
    index <- which(data[,x]==y);
    calibration(confidence[index], correct[index], confidenceLevels = confidenceLevels,
                jack = jack, confMin = confMin, confMax = confMax)}, simplify = TRUE)},
    simplify = FALSE)}

  # if one sets specific names for the variable, then change them in the all variable accordingly
  # after all the necessary information has been obtained.
  if(!is.null(var.names)){var <- var.names; names(all)<-var; names(var.levels) <- var}

  # Create a variable with only the calibration/CAC curves to make it easier later on.
  table.curves <- if(single){all[["caltable"]]}else{sapply(var, function(x){
    tab <-do.call(rbind,all[[x]]["caltable",]);
    tab <- do.call(cbind, list(var.levels = rep(var.levels[[x]], each = length(confidenceLevels)),
                               tab)); rownames(tab) <- NULL; tab}, simplify = FALSE)}

  # Create a variable with table of all the C, OU, NRI, and ANRI stats to make it easier later on.
  table.stats <- if(single){as.data.frame(t(all[["calstats"]]))
  }else{do.call(rbind, sapply(var, function(x){
    tab <- as.data.frame(do.call(rbind, all[[x]]["calstats",]));
    tab <- do.call(cbind, list(var = x, var.levels = var.levels[[x]], tab));
    row.names(tab) <- NULL; tab}, simplify = FALSE, USE.NAMES = FALSE))}

  return(list(prop = prop, all = all, table.curves = table.curves, table.stats = table.stats))}

# CA.REL HELPER FUNCTIONS
## The "simple" CALIBRATION function.
## With this function you can attain two separate things necessary for a calibration analysis.
## A variable of the calibration statistics and a table indicating prop correct and other variables
## per level of confidence.
calibration <- function(confidence, correct, confidenceLevels,method=method, confMin, confMax, data = NULL,
                        jack = TRUE){
  if(!is.null(data)){confidence <- data[[confidence]]; correct <- data[[correct]]}
  if(is.null(names(confidenceLevels))){
    names(confidenceLevels)<- unlist(lapply(confidenceLevels, function(x) paste(x, collapse="-")))}

  caltable <- lapply(seq_along(confidenceLevels), function(x){
    unlist(cal.table(confidenceLevels[[x]], correct=correct, confidence=confidence,method=method))})
  caltable <- as.data.frame(do.call(rbind, caltable))
  caltable <- cbind(Levels = names(confidenceLevels), caltable)

  confidence.max <-max(unlist(confidenceLevels))
  confidence.min <- min(unlist(confidenceLevels))

  #Variables --> see Brewer 2006
  a <- mean(caltable$`Proportion correct`, na.rm = TRUE)
  a.j <- caltable$`Proportion correct`
  c.j <- (caltable$`Mean confidence`-confMin)/confMax
  n.j <- caltable$Total
  n.c <- length(c.j)
  n <- sum(caltable$Total, na.rm = TRUE)

  # C, O/U, NRI, and ANRI statistics
  C <- (1/n)*(sum(n.j*(c.j-a.j)^2, na.rm = TRUE))
  OU <- (1/n)*(sum(n.j*(c.j-a.j),  na.rm = TRUE))
  NRI <- ((1/n)*(sum(n.j*(a.j-a)^2, na.rm = TRUE)))/(a*(1-a))
  ANRI <- ((n*NRI)-(n.c+1))/(n-n.c+1)
  calstats = c(C=C, OU = OU, NRI =NRI, ANRI=ANRI)


  if(!jack){
    results <- list(caltable = caltable,  calstats = calstats)
    return(results)}

  jack.SE <- jackknife.cal(1:length(confidence), function(x){
    calibration(confidence = confidence[x], correct = correct[x],
                confidenceLevels = confidenceLevels, jack = FALSE, confMin = confMin,
                confMax = confMax)$calstats})["jack.se",]

  calstats <- unlist(sapply(c("C", "OU", "NRI", "ANRI"), function(x){
    c(stat = calstats[[x]], SE = jack.SE[[x]], `95.L` = calstats[[x]]-(1.96*jack.SE[[x]]),
      `95.H` = calstats[[x]]+(1.96*jack.SE[[x]]))}, simplify = FALSE))

  names(calstats) <- gsub(".stat", "", names(calstats))
  return(list(caltable = caltable,  calstats = calstats))
}

### Calibration helper functions.
#### Function to create a calibration table
cal.table <- function(x, confidence, correct,method) {
  method <- method
  index <- which(confidence >= min(x) & confidence <= max(x))
  Conf <- confidence[index]
  Cor <- correct[index]
  conf2 <- mean(x, na.rm = TRUE)
  conf <- mean(Conf, na.rm = TRUE)
  n <- length(index)
  correct <- sum(Cor==1, na.rm = TRUE)

  #The incorrect field can assume different values depeding on whether there was a designated
  #suspect in the study or not. If there is no designated suspect the number of incorrect
  #identifications is calculated by ignoring filler IDs on target-present trials and dividing filler
  #IDs by the number of lineup members on target-absent trials.

  incorrect <- if(method =="none") {sum(Cor==0, na.rm = TRUE)}
               if(method =="designated.suspect") {sum(Cor==0, na.rm = TRUE)}
               if(method =="non.designated.suspect") {function(lineup.size){

                        incorrect.present <- data %>%
                                            filter(TargetPresence=="Present") %>%
                                            filter(ChoiceValue=="NotPresent") %>%
                                            filter(ChoiceCorrect==0) %>%
                                            nrow()

                        incorrect.absent <- data %>%
                                            filter(TargetPresence=="Absent") %>%
                                            filter(grepl('Filler', ChoiceValue)) %>%
                                            filter(ChoiceCorrect==0) %>%
                                            nrow()/lineup.size

                       incorrect.NDS(sum(incorrect.present,incorrect.absent))

                        return(incorrect.NDS)
                   }
              }

  p <- correct/n
  se <- sqrt(p*(1 - p) / n)
  results <- list(`Mean confidence` = conf, Incorrect = incorrect, Correct = correct, Total = n,
                  `Proportion correct` = p, SE = se, D = (correct/incorrect))
  return(results)
}

#### Function to jackknife the calibration functions
jackknife.cal <- function (x, theta, ...) {
  call <- match.call()
  n <- length(x)
  u <- list(C=rep(0,n), OU = rep(0,n), NRI = rep(0,n),ANRI = rep(0,n))
  for (i in 1:n) {
    temp <- theta(x[-i], ...)
    u$C[i] <- temp["C"]
    u$OU[i] <- temp["OU"]
    u$NRI[i] <- temp["NRI"]
    u$ANRI[i] <- temp["ANRI"]
  }
  thetahat <- theta(x, ...)
  results <- sapply(names(u), function(x){
    list(jack.bias = (n - 1) * (mean(u[[x]], na.rm = TRUE) - thetahat[[x]]),
         jack.se = sqrt(((n - 1)/n) * sum((u[[x]] - mean(u[[x]], na.rm = T))^2, na.rm = TRUE)),
         jack.values = u[[x]])})
  return(results)
}

# SUMMARY FUNCTIONS
## CA.print

#' Print overview table per confidence level
#'
#' \code{CA.print()} takes the argument of \code{CA.rel()} and prints the seperate
#'    calibration tables (mean, prop correct, etc.) for each confidence interval per variable
#' @param CA.rel Object created by the CA.rel function
#' @details For a more detailed explanation see the github page for a manual.
#'    \url{https://github.com/IngerMathilde/CArelationship}
#' @author Inger van Boeijen <\email{inger.vb.r@gmail.com}>
#' @examples
#' library(jtools)
#' data(metamemoryCA)
#'
#' # Create object for the whole dataset and print proportion correct per level of confidence
#'
#' All <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                        correct = "ChoiceCorrect", test = "CAL",
#'                        confidenceLevels = c(0,10,20,30,40,50,60,70,80,90,100))
#'
#' CA.print(All)
#'
#'
#' @export CA.print

CA.print <- function(CA.rel){
  prop <- CA.rel$prop
  if(prop$single){print.calibration(CA.rel$all$calstats, CA.rel$all$caltable, test = prop$test,
                                    CI95 = prop$jack)
  }else{invisible(lapply(names(CA.rel$all), function(var){
    x <- CA.rel$all[[var]];
    lapply(colnames(x), function(y){print.calibration(x[,y]$calstats, x[,y]$caltable, var = var,
                                                      var.levels = y, test = prop$test,
                                                      CI95 = prop$jack)})}))}}

# CA PRINT helper function
print.calibration <- function(calstats, caltable, var = NULL, var.levels = NULL,
                              test = NULL, CI95 = FALSE){
  cat("\n", paste(var, var.levels), "\n")
  caltable <- as.matrix(caltable)
  rownames(caltable) <- rep("", nrow(caltable))
  print(caltable, na.print = "", quote = FALSE)
  calstats <- format(round(calstats, 3), nsmall = 3)
  if(test == "CAL"){
    invisible(lapply(c("C", "OU", "NRI","ANRI"), function(x){
      cat("\n", "The", x, "Statistics is:", calstats[[x]])
      if(CI95){cat(" ; 95 CI is:", paste0(calstats[[paste0(x, '.95.L')]], " - " ,
                                          calstats[[paste0(x, '.95.H')]]))}}))
    cat("\n\n")
  }
}

##CA.curves() functions

#' Plots confidence-accuracy relationship curves
#'
#' \code{CA.curves()} takes the argument of \code{CA.rel()} and prints the seperate
#' calibration tables (mean, prop correct, etc.) for each confidence interval per variable
#' @param CA.rel Object created by the CA.rel function
#' @param legend.position A vector indicating the position of the legend where \code{c(0,0)}
#'    is bottom left and \code{c(1,1)} is top right. To hide the legend, define the argument
#'    as \code{"none"}
#' @param legend.text.size Integer indicating text size of the legend, the standard is 12
#' @param labelVarType A logical variable indicating whether the legend names should include the
#'    variable names.
#' @param ylim A vector indicating the limitation of the y axis. Standard is \code{c(0,100)} for
#'    calibration analyses and \code{c(50,100)} for CAC analyses
#' @param xlim A vector indication the limitation of the x axis.
#' @param ybreaks A vector indicating the breaks of the y axis.
#' @details For a more detailed explanation see the github page for a manual.
#'    \url{https://github.com/IngerMathilde/CArelationship}
#' @author Inger van Boeijen <\email{inger.vb.r@gmail.com}>
#' @examples
#' library(jtools)
#' library(ggplot2)
#' data(metamemoryCA)
#'
#' Ch <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                       correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser",
#'                       confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
#'                       jack = TRUE)
#'
#' CA.curves(Ch)
#' # Include variable names in label
#' CA.curves(Ch, labelVarType = TRUE)
#' # Change legend position to bottom right corner and change legend text size to 23
#' CA.curves(Ch, legend.position = c(1,0), legend.text.size = 10)
#'
#' @export CA.curves


CA.curves <- function(CA.rel, legend.position = c(0,1), legend.text.size = 12, labelVarType = FALSE,
                    ylim = NULL, ybreaks = c(0, 20, 40, 60, 80, 100), xlim = NULL){
  prop <- CA.rel$prop
  confidenceLevels <- prop$confidenceLevels
  if(is.null(ylim)){if(prop$test == "CAL"){ylim = c(0,100)}else{ylim = c(50,100)}}
  if(prop$test == "CAL"){
    if(prop$single){var <- CA.rel$table.curves;
    plots <- plot.CAL(x = var$`Mean confidence`, y = var$`Proportion correct`, se = var$SE,
                      x.confidenceLevels = confidenceLevels, legend.position = legend.position,
                      legend.text.size = legend.text.size, ylim = ylim, ybreaks = ybreaks,
                      xlim = xlim)
    }else{plots <- lapply(names(CA.rel$table.curves), function(z){
      var <- CA.rel$table.curves[[z]];
      if(labelVarType){type <- paste(var$var.levels, z)}else{type <- var$var.levels};
      plot.CAL(x = var$`Mean confidence`, y = var$`Proportion correct`, se = var$SE, type = type,
               x.confidenceLevels = confidenceLevels, legend.position = legend.position,
               legend.text.size = legend.text.size, ylim = ylim, ybreaks = ybreaks, xlim = xlim)})}
  }
  if(prop$test == "CAC"){
    if(prop$single){var <- CA.rel$table.curves;
    plots <- plot.CAC(x = var$Levels, y = var$`Proportion correct`, se = var$SE, CI = T,
                      ylim = ylim, ybreaks = ybreaks, legend.position = legend.position,
                      legend.text.size = legend.text.size)
    }else{plots <- lapply(names(CA.rel$table.curves), function(z){
      var <- CA.rel$table.curves[[z]];
      if(labelVarType){type <- paste(var$var.levels, z)}else{type <- var$var.levels};
      plot.CAC(x = var$Levels, y = var$`Proportion correct`, se = var$SE, type = type,
               CI = T, ylim = ylim, ybreaks = ybreaks, legend.position = legend.position,
               legend.text.size = legend.text.size)})}
  }
  return(plots)

}

# CA PLOT helper function to create one seperate CAL curve
## Function to plot calibration curves
plot.CAL <- function(x, y, se, type = NULL, x.confidenceLevels, CI = F,  ylim = c(0,100),
                     ybreaks = ggplot2::waiver(), xlim = NULL, legend.position = c(0,1),
                     legend.text.size = 12){
  if(length(type) == 1){type <- rep(type, length(x))
  }else if(is.null(type)){type <- rep('Calibration', length(x))}

  df.plot <- do.call(cbind.data.frame, list(type = type, x = x, y = y, se = se))
  df.plot$se[df.plot$se == 0] <- NA # SE of 0 do not receive error bars
  df.plot$y <- df.plot$y *100
  df.plot$se <- df.plot$se *100
  if(CI){df.plot$se <- df.plot$se*1.96} # error bars is confidence intervals instead of SE

  # Label vars
  x.tick.color <- rep("black", length(x.confidenceLevels))
  x.min <- min(unlist(x.confidenceLevels))
  x.max <- max(unlist(x.confidenceLevels))
  ## If confidence levels are classed together, create ticks based upon max value's
  ## but place the labels in the middle
  if(class(x.confidenceLevels) == "list"){
    ticks <- c(x.min, unname(sapply(x.confidenceLevels, max)))
    lab.pos <- sapply(1:(length(ticks)-1), function(x) mean(c(ticks[x], ticks[x+1])))
    names(lab.pos) <- names(x.confidenceLevels)
    x.confidenceLevels <- c(lab.pos, ticks)
    x.tick.color <- ifelse(names(x.confidenceLevels)=="", "black", NA)
  }
  if(is.null(xlim)){xlim <- c(x.min, x.max)}

  # x is conf and y is propCor
  plot.gg <- ggplot2::ggplot(data = df.plot, ggplot2::aes(x=x, y=y, group = type)) +
    ggplot2::geom_abline(intercept = 0, slope = 100/x.max, color = "grey", linetype = 2) +
    ggplot2::geom_point(data = df.plot, ggplot2::aes(shape = type), size = 3, na.rm = T) +
    ggplot2::geom_line(data = df.plot, ggplot2::aes(linetype= type), size = 1, na.rm = T) +
    ggplot2::geom_errorbar(data = df.plot, ggplot2::aes(ymin=y-se, ymax=y+se, width = x.max/100),
                           na.rm = T) +
    ggplot2::labs(x = "Confidence", y =  "Percentage Correct")+
    ggplot2::scale_x_continuous(breaks = x.confidenceLevels, labels = names(x.confidenceLevels)) +
    ggplot2::scale_y_continuous(breaks = ybreaks) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    jtools::theme_apa() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line(color = x.tick.color),
                   legend.position = legend.position,
                   legend.justification = legend.position,
                   legend.text = ggplot2::element_text(size=legend.text.size, lineheight = 1),
                   legend.key.height = ggplot2::unit(1, "line"),
                   legend.key.width = ggplot2::unit(2, "line"))
  return(plot.gg)
}

## Function to plot CAC curves create one seperate CAC curve
plot.CAC <- function(x, y, se, type = NULL, CI = T, ylim = c(50,100), ybreaks = ggplot2::waiver(),
                     legend.position = c(0,1), legend.text.size = 12){
  if(length(type) == 1){type <- rep(type, length(x))
  }else if(is.null(type)){type <- rep('a', length(x))}
  df.plot <- do.call(cbind.data.frame, list(type = type, x = x, y = y, se = se))
  df.plot$se[df.plot$se == 0] <- NA
  df.plot$y <- df.plot$y *100
  df.plot$se <- df.plot$se *100
  if(CI){df.plot$se <- df.plot$se*1.96}
  pd <- ggplot2::position_dodge(width=0.1)
  # x is conf and y is propCor
  plot.gg <- ggplot2::ggplot(data = df.plot, ggplot2::aes(x=x, y=y, group = type)) +
    ggplot2::geom_point(data = df.plot, ggplot2::aes(shape = type), size = 3, position=pd,
                        na.rm = T) +
    ggplot2::geom_line(data = df.plot, ggplot2::aes(linetype= type), size = 1, position = pd,
                       na.rm = T) +
    ggplot2::geom_errorbar(data = df.plot, ggplot2::aes(ymin=y-se, ymax=y+se, width =0.2),
                           position=pd, na.rm = T) +
    ggplot2::labs(x = "Confidence", y =  "Percentage Correct")+
    ggplot2::scale_y_continuous(breaks = ybreaks) +
    ggplot2::coord_cartesian(ylim = ylim) +
    jtools::theme_apa() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.position = legend.position,
                   legend.justification = legend.position,
                   legend.text = ggplot2::element_text(size=legend.text.size, lineheight = 1),
                   legend.key.height = ggplot2::unit(1, "line"),
                   legend.key.width = ggplot2::unit(2, "line"))
  return(plot.gg)
}

## CA.table functions

#' Prints table for calibration statistics
#'
#' \code{CA.table()} takes the argument of \code{CA.rel()} and prints a table with the
#'    C, O/U, NRI and ANRI stats
#'
#' @param CA.rel Object created by the CA.rel function
#' @details For a more detailed explanation see the github page for a manual.
#'    \url{https://github.com/IngerMathilde/CArelationship}
#' @author Inger van Boeijen <\email{inger.vb.r@gmail.com}>
#' @examples
#' data(metamemoryCA)
#'
#' # Compare choosers vs. nonchoosers with collapsed confidence groups and Jackknife SE
#' Ch <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                       correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser",
#'                       confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
#'                       jack = TRUE)
#'
#' CA.table(Ch)
#'
#' @export CA.table


CA.table <- function(CA.rel){
  # Check whether one want an overview of the whole dataset or per variable.
  single <- CA.rel$prop$single

  # Attain a table
  table <- CA.rel$table.stats

  #Make the table APA by changing numbers from 0.10 to .10
  table <- do.call(cbind.data.frame,lapply(table, function(x){
    if(is.numeric(x)){gsub("0\\.", ".", format(round(x, 3), nsmall = 3))}else{x}}))

  # Create table accordingly to whether a JackKnife function was used
  if(CA.rel$prop$jack){
    table.temp <- as.data.frame(sapply(c("C", "OU", "NRI","ANRI"), function(x){
      paste0(table[[x]]," [", table[[paste0(x, '.95.L')]], ", " ,
             table[[paste0(x, '.95.H')]], "]")}))
    if(single){table <- as.data.frame(t(unname(table.temp)))
    }else{table <- cbind(table[,c(1,2)], table.temp)}}
  table <- as.matrix(table)
  rownames(table) <- rep("", nrow(table))
  table <- noquote(table)
  return(table)
}

##CA.plotCI function

#' Plot 95 percent confidence interval plots for calibration statistics
#'
#' \code{CA.plotCI()} takes the argument of \code{CA.rel()} and and plots the confidence
#'    interval plots for the C, O/U, NRI and ANRI calibration statistics.
#' @param CA.rel Object created by the CA.rel function
#' @param ylim A list with the limitation of the y axis for each calibration statistic.
#' @details For a more detailed explanation see the github page for a manual.
#'    \url{https://github.com/IngerMathilde/CArelationship}
#' @author Inger van Boeijen <\email{inger.vb.r@gmail.com}>
#' @examples
#' library(jtools)
#' data(metamemoryCA)
#'
#' # Create a CI plot for the C, OU, NRI and ANRI statistics for high and low metamemory raters of two
#' # different components.
#'
#' ChMM <- CA.rel(data = metamemoryCA, confidence = "Confidence",
#'                         correct = "ChoiceCorrect", test = "CAL",
#'                         var = c("Rater.EMS.Relative.Face.Recognition",
#'                                 "Rater.EMS.Eyewitness.Ability"),
#'                         var.levels = c('Low', 'High'),
#'                         confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
#'                         jack = TRUE)
#' CA.plotCI(ChMM)
#'
#' @export CA.plotCI
CA.plotCI <- function(CA.rel, ylim = list(C = NULL, OU = c(0), NRI = NULL, ANRI = NULL)){
  if(CA.rel$prop$single){stop("Can't plot CI for only one curve")}
  if(!CA.rel$prop$jack){stop("Needs to have jack SE to plot  a CI plot")}
  table <- CA.rel$table.stats
  CA.plotCIs <- lapply(c("C", "OU", "NRI","ANRI"), function(z){
    plot.CI(data = table, x=z, xmin=paste0(z,".95.L"),
            xmax = paste0(z,".95.H"), y="var", ylim = ylim[[z]])})
  return(CA.plotCIs)
}

# CI PLOT helper function creates one seperate CI plot
plot.CI <- function(data,x,xmin,xmax,y, ylim){
  stat.name <- x
  if(stat.name == "OU"){stat.name <- "O/U"}
  x <- data[[x]]
  xmin <- data[[xmin]]
  xmax <- data[[xmax]]
  y <- data[[y]]
  pd <- ggplot2::position_dodge(width=0.5)

  plots <- ggplot2::ggplot(data, ggplot2::aes(y,x, color = data$var.levels)) +
    ggplot2::geom_hline(yintercept=0, color = "grey", linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=xmin,ymax=xmax, width = 0.4), size=0.3, position=pd) +
    ggplot2::geom_point(ggplot2::aes(shape = data$var.levels),fill="white", size=3, position=pd) +
    ggplot2::scale_color_manual(name="type", values = c("black", "black")) +
    ggplot2::scale_shape_manual(name="type",values=c(21,16)) +
    ggplot2::scale_y_continuous() +
    ggplot2::ylab(stat.name) +
    ggplot2::scale_x_discrete("") +
    ggplot2::coord_flip(ylim = ylim) +
    jtools::theme_apa() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.border = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(face="italic"))

  return(plots)
}

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

