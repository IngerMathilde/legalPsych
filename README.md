
<!-- README.md is generated from README.Rmd. Please edit that file -->
legalPsych
==========

This package consists of tools commonly used in the legal psychology area. Currently, the package include functions that facilitate the calculation of statistics commonly used to test eyewitness confidence-accuracy relationship. However, future updates might include additional tools, such as lineup fairness testing.

The current functions allow for the computation of C, O/U, NRI and calibration curves (see Brewer & Wells, 2006), as well as confidence-accuracy characteristic curves (see Mickes, 2015).

**Note:** This is beta software. Bugs are possible, both in terms of programming errors and computation errors.

Suggestions for new features to add to the function should be emailed to <inger.vb.r@gmail.com> or <renan.saraiva@port.ac.uk>

Suggested Citation: Van Boeijen, I.M., Saraiva, R.(2018). legalPsych: A tool for calculating calibration statistics in eyewitness research. GitHub: <https://github.com/IngerMathilde/legalPsych>

Getting Started
---------------

### Installation

The legalPsych package is currently not available through CRAN and it needs to be installed through Github. To do this you have to install `devtools`:

``` r
install.packages("devtools")
```

To install or update legalPsych package use the following line:

``` r
devtools::install_github("IngerMathilde/legalPsych")
```

**Note**: Make sure to detach the legalPsych package or restart R prior to updating.

### Dependencies

For the legalPsych package to work, you will need to load the legalPsych, ggplot2 and jtools package.

``` r
library(legalPsych)
library(ggplot2)
library(jtools)
```

### Dataset prerequisites

To use legalPsych functions, your dataset needs to adhere to the following prerequisites.

-   Your data has to include at least one variable indicating identification confidence. Accurcay can be declared on any given scale (e.g., 0-100%, 1-5)
-   Your dataset needs to include one variable indicating identification decision accuracy. Identification accuracy needs to be a binary variable where 0 indicates an incorrect decision and 1 indicates a correct decision.
-   In case of multiple identifications per person, you need to modify your dataset to a long format (see example below).

You can check the `metamemoryCA` dataset for an example of a dataset that meets those requirements.

``` r
data(metamemoryCA)
```

Usage
-----

Here are some examples regarding the functionality of the functions using `data(metamemoryCA)` dataset.

### EXAMPLE 1: Calibration for the whole dataset

Create calibration curves and calculate C, OU, and NRI for the whole dataset.

``` r
All <- CA.rel(data = metamemoryCA, confidence = "Confidence", correct = "ChoiceCorrect", test = "CAL", 
              confidenceLevels = c(0,10,20,30,40,50,60,70,80,90,100))
```

Print a table for each level of confidence that includes proportion correct, diagnosticity, C, OU, and NRI statistic.

``` r
CA.print(All)
#> 
#>   
#>  Levels Mean confidence Incorrect Correct Total Proportion correct SE         D        
#>  0        0              3         0       3    0.0000000          0.00000000 0.0000000
#>  10      10              6         1       7    0.1428571          0.13226001 0.1666667
#>  20      20             10         7      17    0.4117647          0.11936462 0.7000000
#>  30      30              8        10      18    0.5555556          0.11712139 1.2500000
#>  40      40             13        15      28    0.5357143          0.09424976 1.1538462
#>  50      50             13        18      31    0.5806452          0.08862687 1.3846154
#>  60      60             25        24      49    0.4897959          0.07141370 0.9600000
#>  70      70             19        37      56    0.6607143          0.06326968 1.9473684
#>  80      80             12        38      50    0.7600000          0.06039868 3.1666667
#>  90      90             12        43      55    0.7818182          0.05569046 3.5833333
#>  100    100              6        36      42    0.8571429          0.05399492 6.0000000
#> 
#>  The C Statistics is: 0.014
#>  The OU Statistics is: 0.021
#>  The NRI Statistics is: 0.161
```

Print the calibration curve, but don't show the legend. Also change the y axis label to have breaks for every 10% of confidence

``` r
CA.curves(All, legend.position = "none",  ybreaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
```

<img src="man/figures/README-EX1 CA.curves-1.png" width="70%" />

### EXAMPLE 2: Compare choosers vs. nonchoosers with collapsed confidence groups and Jackknife SE.

To compare calibration scores for choosers vs. nonchoosers, the `var` argument in the `CA.rel()` function needs to be defined as "ChoiceChooser". "ChoiceChooser" is the binary numberic variable in the data set that defines whether or not someone is a chooser (0 = incorrect, 1= correct).

In order to collapse certain confidence levels (e.g., group together the 0, 10, 20 confidence levels), the `confidenceLevels` argument needs to be defined as a list (e.g., `confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)`).

To obtain jackknife SE for the C, OU, NRI statistic,`jack = T` is added to the `CA.rel()` function.

``` r
Choosers <- CA.rel(data = metamemoryCA, confidence = "Confidence", correct = "ChoiceCorrect", 
                   test = "CAL", var = "ChoiceChooser", 
                   confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)), jack = T)
```

Create calibration curves to compare choosers and nonchoosers for all five different confidence levels.

``` r
CA.curves(Choosers)
```

![](man/figures/README-EX2%20CA.curves-1.png)

Create a table with the C, OU and NRI statistic, along with 95% CI between brackets (calculated via jackknife).

``` r
CA.table(Choosers) 
#>  var           var.levels C                 OU                   NRI               
#>  ChoiceChooser NonChooser .036 [.008, .065] -.079 [-.149, -.009] .026 [-.024, .075]
#>  ChoiceChooser Chooser    .017 [.000, .033]  .101 [ .036,  .166] .180 [ .073, .286]
```

### EXAMPLE 3: Calibration plot when disregarding lower confidence groups

When lower levels of confidence are not included in the analysis, it is important to define the minimum attainable confidence level in the `ConfMin` variable to ensure that the calibration calculations are still accurate. See the difference between `Choosers.no.low.correct` and `Choosers.no.low.incorrect` calibration tables

``` r
Choosers.no.low.correct <- CA.rel(data = metamemoryCA, confidence = "Confidence", 
                                correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser", 
                                confidenceLevels = list(c(50,60), c(70,80), c(90,100)), 
                                jack = T, confMin = 0)

Choosers.no.low.incorrect <- CA.rel(data = metamemoryCA, confidence = "Confidence", 
                                correct = "ChoiceCorrect", test = "CAL", var = "ChoiceChooser", 
                                confidenceLevels = list(c(50,60), c(70,80), c(90,100)), jack = T)
```

Correctly calculated calibration table

``` r
CA.table(Choosers.no.low.correct)
#>  var           var.levels C                  OU                  NRI               
#>  ChoiceChooser NonChooser .009 [-.005, .024] -.012 [-.084, .059] .021 [-.028, .070]
#>  ChoiceChooser Chooser    .021 [ .000, .042]  .140 [ .066, .215] .098 [ .000, .195]
```

Incorrectly calculated calibration table

``` r
CA.table(Choosers.no.low.incorrect)
#>  var           var.levels C                 OU                   NRI               
#>  ChoiceChooser NonChooser .272 [.194, .349] -.512 [-.584, -.441] .021 [-.028, .070]
#>  ChoiceChooser Chooser    .131 [.076, .185] -.360 [-.434, -.285] .098 [ .000, .195]
```

### EXAMPLE 4: Compare high vs. low metamemory raters for choosers with adjusted variable names for output

To compare metamemory performance for choosers only it is important to first create a subset of the dataset that only includes choosers

``` r
data.ch <- subset(metamemoryCA, ChoiceChooser == "Chooser")
```

It is often that case calibration is compared between different groups (e.g., presence of weapon vs absence of weapon, or intoxicated witnesses vs sober witnesses). To compare different groups the names of the variables need to be defined as a vector in the `var` variable. In this example we are comparing calibration for individuals with high or low scores in self-rated face recognition ability and eyewitness memory ability.The `var.level` argument makes it possible to compare high with low metamemory raters, while disregarding medium raters. To change how the variable names appear in the plots you can use the var.name argument. In this example the variable name in our dataset is `Rater.EMS.Relative.Face.Recognition`, but we want to plot it as EMS Relative Face Recognition.

``` r
ch.raters <- CA.rel(data = data.ch, confidence = "Confidence", correct = "ChoiceCorrect", test = "CAL", 
                    var = c("Rater.EMS.Relative.Face.Recognition", "Rater.EMS.Eyewitness.Ability"), 
                    var.names = c("EMS Relative Face Recognition", "EMS Eyewitness Ability"), 
                    var.levels = c('Low', 'High'), 
                    confidenceLevels = list(c(0,20),c(30,40), c(50,60), c(70,80), c(90,100)), jack = T)
```

Create calibration plots, including the variable names in the legend

``` r
CA.curves(ch.raters, labelVarType = T)
```

![](man/figures/README-EX4%20CA.curves-1.png)![](man/figures/README-EX4%20CA.curves-2.png)

Create a table with the calibrations statistics for each group, including 95% CI:

``` r
CA.table(ch.raters)
#>  var                           var.levels C                  OU                  NRI               
#>  EMS Relative Face Recognition Low        .005 [-.011, .020]  .036 [-.064, .137] .244 [ .059, .429]
#>  EMS Relative Face Recognition High       .056 [ .007, .105]  .192 [ .086, .299] .247 [-.004, .499]
#>  EMS Eyewitness Ability        Low        .002 [-.009, .013] -.018 [-.115, .078] .318 [ .100, .535]
#>  EMS Eyewitness Ability        High       .078 [ .026, .131]  .247 [ .149, .345] .328 [ .040, .615]
```

Create a 95% CI plot for the calibration statistics to make it easier to inspect overlapping CI:

``` r
CA.plotCI(ch.raters)
```

![](man/figures/README-EX4%20CA.plotCI-1.png)![](man/figures/README-EX4%20CA.plotCI-2.png)![](man/figures/README-EX4%20CA.plotCI-3.png)

### EXAMPLE 5: CAC curves

To create CAC curves, the data-frame needs to be subsetted to only include suspect identifications. In this example we are using the subset function to create a separate dataframe including only suspect identifications.

``` r
data.CAC <- subset(metamemoryCA, ChoiceValue == "Target")
```

Since we are computing CAC analysis, our `CA.rel()` function, we need to include the argument `test = "CAC"`.

``` r
CAC.raters <- CA.rel(data = data.CAC, confidence = "Confidence", correct = "ChoiceCorrect", test = "CAC", 
                     var = c("Rater.EMS.Relative.Face.Recognition", "Rater.EMS.Eyewitness.Ability"), 
                     var.names = c("EMS Relative Face Recognition", "EMS Eyewitness Ability"), 
                     var.levels = c('Low', 'High'), 
                     confidenceLevels = list(c(0,60), c(70,80), c(90,100)))
```

Now we can plot CAC curves that include the variable name in the legend, positioning the legend in the right bottom corner (see `legend.position` argument). The error bars in the plot are 95% confidence intervals.

``` r
CA.curves(CAC.raters, labelVarType = T, legend.position = c(1,0), ybreaks = seq(50, 100, 10)) 
```

![](man/figures/README-EX5%20CA.curves-1.png)![](man/figures/README-EX5%20CA.curves-2.png)

Authors
-------

Inger van Boeijen           
Renan Saraiva

Licence
-------

This package is licensed under the The GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details

