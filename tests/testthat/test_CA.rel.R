library(legalPsych)

data(metamemoryCA)

test_that("Wrong test argument is given", {
  expect_that(CA.rel(data = metamemoryCA, confidence = "Confidence",
                     correct = "ChoiceCorrect", test = "CAP", var = "ChoiceChooser",
                     confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
                     jack = TRUE), throws_error())})

test_that("Wrong confidence argument is given", {
  expect_that(CA.rel(data = metamemoryCA, confidence = "confidence",
                     correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser",
                     confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
                     jack = TRUE), throws_error())})

test_that("Wrong confidence argument is given", {
  expect_that(CA.rel(data = metamemoryCA, confidence = "Confidence",
                     correct = "Choice", test = "CAL", var = "ChoiceChooser",
                     confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)),
                     jack = TRUE), throws_error())})
