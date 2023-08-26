Lab 3: Multilevel Modeling. Additional Complexities (Chapter 13)
================
Constanza F. Schibber
February 4, 2021

- [1 Setting Up Data and Libraries](#1-setting-up-data-and-libraries)
- [2 CHAPTER 13.1 Varying intercept & slopes w/ no group level
  predictors](#2-chapter-131-varying-intercept--slopes-w-no-group-level-predictors)
  - [2.1 Understanding Correlation between Intercepts and
    Slopes](#21-understanding-correlation-between-intercepts-and-slopes)
- [3 CHAPTER 13.1 Varying intercept & slopes w/ group level
  predictors](#3-chapter-131-varying-intercept--slopes-w-group-level-predictors)
  - [3.1 Prediction for a particular
    county](#31-prediction-for-a-particular-county)
  - [3.2 Change in Correlation between Intercepts and
    Slopes?](#32-change-in-correlation-between-intercepts-and-slopes)
- [4 CHAPTER 13.3 Modeling multiple varing coefficients using the scale
  inverse-Wishart
  distribution](#4-chapter-133-modeling-multiple-varing-coefficients-using-the-scale-inverse-wishart-distribution)
- [5 13.4 Understanding Correlations Between group-level Intercepts and
  Slopes](#5-134-understanding-correlations-between-group-level-intercepts-and-slopes)
  - [5.1 Regression: Centering the
    predictors](#51-regression-centering-the-predictors)
- [6 CHAPTER 13.5 Non-nested Model](#6-chapter-135-non-nested-model)
  - [6.1 Pilots](#61-pilots)

# 1 Setting Up Data and Libraries

``` r
library(arm)
```

    ## Loading required package: MASS

    ## Loading required package: Matrix

    ## Loading required package: lme4

    ## 
    ## arm (Version 1.13-1, built: 2022-8-25)

    ## Working directory is /Users/connie/Multilevel Modeling/Lab 3

``` r
library(car)  # function recode allows you to recode a variable
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:arm':
    ## 
    ##     logit

``` r
library(foreign)

# SETUP IN R
srrs2 <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/radon/srrs2.dat",
    header = TRUE, sep = ",")
mn <- srrs2$state == "MN"  # SETS UP A LONG LIST OF TRUE/FALSE
radon <- srrs2$activity[mn]
log.radon <- log(ifelse(radon == 0, 0.1, radon))
floor <- srrs2$floor[mn]  # 0 FOR BASEMENT, 1 FOR FIRST FLOOR
n <- length(radon)
y <- log.radon
x <- floor

# GET COUNTY INDEX VARIABLE (85 COUNTIES, 919 HOUSES)
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep(NA, J)  # WILL MAP 919 -> 85
for (i in 1:J) county[county.name == uniq[i]] <- i

# Group Level Predictors
cty <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/radon/cty.dat",
    header = T, sep = ",")
srrs2.fips <- srrs2$stfips * 1000 + srrs2$cntyfips
usa.fips <- 1000 * cty[, "stfips"] + cty[, "ctfips"]
usa.rows <- match(unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows, "Uppm"]
u <- log(uranium)
u.full <- u[county]
```

# 2 CHAPTER 13.1 Varying intercept & slopes w/ no group level predictors

``` r
M3 <- lmer(y ~ x + (1 + x | county))
display(M3)
```

    ## lmer(formula = y ~ x + (1 + x | county))
    ##             coef.est coef.se
    ## (Intercept)  1.46     0.05  
    ## x           -0.68     0.09  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev. Corr  
    ##  county   (Intercept) 0.35           
    ##           x           0.34     -0.34 
    ##  Residual             0.75           
    ## ---
    ## number of obs: 919, groups: county, 85
    ## AIC = 2180.3, DIC = 2153.9
    ## deviance = 2161.1

``` r
coef(M3)
```

    ## $county
    ##    (Intercept)          x
    ## 1    1.1445374 -0.5406207
    ## 2    0.9333795 -0.7708213
    ## 3    1.4716909 -0.6688831
    ## 4    1.5354352 -0.7525548
    ## 5    1.4270378 -0.6206707
    ## 6    1.4826560 -0.6877094
    ## 7    1.8193569 -0.4747510
    ## 8    1.6907731 -0.6975067
    ## 9    1.1009452 -0.3737238
    ## 10   1.5486981 -0.7798789
    ## 11   1.4305980 -0.6704024
    ## 12   1.5874792 -0.7225586
    ## 13   1.2222709 -0.6011427
    ## 14   1.8899918 -0.8523356
    ## 15   1.3899110 -0.6572603
    ## 16   1.2202158 -0.6004595
    ## 17   1.4454876 -0.8303451
    ## 18   1.1520967 -0.4461587
    ## 19   1.3528648 -0.7689663
    ## 20   1.5963770 -0.7255167
    ## 21   1.6303462 -0.6711922
    ## 22   0.9838000 -0.5534327
    ## 23   1.4251552 -0.6433250
    ## 24   1.8866532 -0.7698507
    ## 25   1.7755309 -0.4800817
    ## 26   1.3728482 -0.7795581
    ## 27   1.6119010 -0.6494601
    ## 28   1.3158404 -0.6245548
    ## 29   1.3017630 -0.6275704
    ## 30   1.0836029 -0.5550416
    ## 31   1.7539207 -0.7778932
    ## 32   1.3571755 -0.6459926
    ## 33   1.7420982 -0.7739627
    ## 34   1.5252202 -0.7357339
    ## 35   1.0025697 -0.5187405
    ## 36   1.8378184 -0.5854593
    ## 37   0.7403795 -0.4468006
    ## 38   1.6076009 -0.6245387
    ## 39   1.5857055 -0.6315858
    ## 40   1.8608339 -0.7809497
    ## 41   1.7548745 -0.6271587
    ## 42   1.4445249 -0.6750325
    ## 43   1.7927831 -1.2154962
    ## 44   1.2486205 -0.8205943
    ## 45   1.3719931 -0.7996969
    ## 46   1.3332400 -0.6380351
    ## 47   1.3702210 -0.8738622
    ## 48   1.2304052 -0.5291715
    ## 49   1.6879021 -0.9616760
    ## 50   1.6474618 -0.7425002
    ## 51   1.7901980 -0.7899538
    ## 52   1.6467949 -0.7422785
    ## 53   1.3852914 -0.6964618
    ## 54   1.3672813 -0.8943441
    ## 55   1.5140918 -0.5872235
    ## 56   1.3964786 -0.8462473
    ## 57   1.0629037 -0.6007468
    ## 58   1.6207514 -0.6432155
    ## 59   1.6173371 -0.8030919
    ## 60   1.4070242 -0.6625652
    ## 61   1.1523640 -0.3268897
    ## 62   1.6388650 -0.4025473
    ## 63   1.4853052 -0.5365443
    ## 64   1.7406288 -0.7879782
    ## 65   1.4130217 -0.6645591
    ## 66   1.5666045 -0.6262481
    ## 67   1.6076623 -0.3977852
    ## 68   1.2257138 -0.6022873
    ## 69   1.3600489 -0.6469479
    ## 70   0.8704024 -0.5624332
    ## 71   1.5069070 -0.8132549
    ## 72   1.5431111 -0.7078081
    ## 73   1.5627787 -0.7143468
    ## 74   1.2409644 -0.6073575
    ## 75   1.5060425 -0.5402654
    ## 76   1.7166251 -0.7475265
    ## 77   1.7159670 -0.9005904
    ## 78   1.3929212 -0.7485168
    ## 79   1.0802843 -0.6538698
    ## 80   1.3599359 -0.8384260
    ## 81   1.8441158 -0.5360212
    ## 82   1.6002883 -0.7268171
    ## 83   1.6942333 -1.1510829
    ## 84   1.5991188 -0.7327214
    ## 85   1.3787939 -0.6531798
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

``` r
fixef(M3)
```

    ## (Intercept)           x 
    ##   1.4627700  -0.6810982

``` r
ranef(M3)
```

    ## $county
    ##     (Intercept)            x
    ## 1  -0.318232574  0.140477472
    ## 2  -0.529390508 -0.089723142
    ## 3   0.008920884  0.012215066
    ## 4   0.072665214 -0.071456546
    ## 5  -0.035732220  0.060427482
    ## 6   0.019886022 -0.006611238
    ## 7   0.356586900  0.206347166
    ## 8   0.228003136 -0.016408455
    ## 9  -0.361824810  0.307374369
    ## 10  0.085928148 -0.098780654
    ## 11 -0.032171954  0.010695776
    ## 12  0.124709250 -0.041460405
    ## 13 -0.240499081  0.079955490
    ## 14  0.427221811 -0.171237421
    ## 15 -0.072858995  0.023837950
    ## 16 -0.242554178  0.080638721
    ## 17 -0.017282337 -0.149246867
    ## 18 -0.310673285  0.234939499
    ## 19 -0.109905152 -0.087868051
    ## 20  0.133607011 -0.044418524
    ## 21  0.167576187  0.009906052
    ## 22 -0.478970028  0.127665509
    ## 23 -0.037614777  0.037773171
    ## 24  0.423883205 -0.088752460
    ## 25  0.312760891  0.201016553
    ## 26 -0.089921767 -0.098459919
    ## 27  0.149130988  0.031638080
    ## 28 -0.146929604  0.056543411
    ## 29 -0.161006984  0.053527823
    ## 30 -0.379167083  0.126056573
    ## 31  0.291150750 -0.096794968
    ## 32 -0.105594517  0.035105587
    ## 33  0.279328200 -0.092864485
    ## 34  0.062450180 -0.054635699
    ## 35 -0.460200251  0.162357665
    ## 36  0.375048463  0.095638863
    ## 37 -0.722390471  0.234297565
    ## 38  0.144830961  0.056559526
    ## 39  0.122935501  0.049512431
    ## 40  0.398063912 -0.099851487
    ## 41  0.292104503  0.053939495
    ## 42 -0.018245119  0.006065709
    ## 43  0.330013065 -0.534397979
    ## 44 -0.214149453 -0.139496073
    ## 45 -0.090776853 -0.118598649
    ## 46 -0.129529940  0.043063075
    ## 47 -0.092549003 -0.192764015
    ## 48 -0.232364758  0.151926746
    ## 49  0.225132064 -0.280577780
    ## 50  0.184691854 -0.061402013
    ## 51  0.327428052 -0.108855594
    ## 52  0.184024898 -0.061180279
    ## 53 -0.077478569 -0.015363624
    ## 54 -0.095488733 -0.213245855
    ## 55  0.051321775  0.093874661
    ## 56 -0.066291386 -0.165149083
    ## 57 -0.399866317  0.080351384
    ## 58  0.157981371  0.037882694
    ## 59  0.154567111 -0.121993718
    ## 60 -0.055745785  0.018533050
    ## 61 -0.310405984  0.354208548
    ## 62  0.176094967  0.278550868
    ## 63  0.022535250  0.144553906
    ## 64  0.277858824 -0.106880029
    ## 65 -0.049748285  0.016539142
    ## 66  0.103834511  0.054850150
    ## 67  0.144892268  0.283313032
    ## 68 -0.237056219  0.078810888
    ## 69 -0.102721135  0.034150312
    ## 70 -0.592367589  0.118665015
    ## 71  0.044137041 -0.132156733
    ## 72  0.080341090 -0.026709920
    ## 73  0.100008742 -0.033248560
    ## 74 -0.221805609  0.073740723
    ## 75  0.043272512  0.140832783
    ## 76  0.253855106 -0.066428305
    ## 77  0.253196998 -0.219492220
    ## 78 -0.069848799 -0.067418559
    ## 79 -0.382485680  0.027228436
    ## 80 -0.102834072 -0.157327767
    ## 81  0.381345824  0.145077015
    ## 82  0.137518358 -0.045718876
    ## 83  0.231463335 -0.469984698
    ## 84  0.136348773 -0.051623170
    ## 85 -0.083976046  0.027918385
    ## 
    ## with conditional variances for "county"

``` r
# Regain estimated varying intercept and varying slope
a.hat.M3 <- fixef(M3)[1] + ranef(M3)$county[, 1]
b.hat.M3 <- fixef(M3)[2] + ranef(M3)$county[, 2]

# So the following are equal
a.hat.M3[1]
```

    ## [1] 1.144537

``` r
coef(M3)$county[1, 1]
```

    ## [1] 1.144537

``` r
b.hat.M3[1]
```

    ## [1] -0.5406207

``` r
coef(M3)$county[1, 2]
```

    ## [1] -0.5406207

``` r
# predict county, x= first floor
coef(M3)$county[1, 1] + coef(M3)$county[1, 2] * 1
```

    ## [1] 0.6039167

``` r
# predict county, x= basement
coef(M3)$county[1, 1] + coef(M3)$county[1, 2] * 0
```

    ## [1] 1.144537

The following reproduces Figure 13.1.

``` r
# Completely Unpooled Model
b.hat.unpooled.varying <- array(NA, c(J, 2))
for (j in 1:J) {
    lm.unpooled.varying <- lm(y ~ x, subset = (county == j))
    b.hat.unpooled.varying[j, ] <- coef(lm.unpooled.varying)
}

# Completely Pooled Model
lm.pooled <- lm(y ~ x)

# Figure
x.jitter <- x + runif(n, -0.05, 0.05)
display8 <- c(2, 84, 24, 26, 19, 55, 45, 71)
y.range <- range(y[!is.na(match(county, display8))])

par(mfrow = c(2, 4), mar = c(4, 4, 3, 1), oma = c(1, 1, 2, 1))
for (j in display8) {
    plot(x.jitter[county == j], y[county == j], xlim = c(-0.05,
        1.05), ylim = y.range, xlab = "floor", ylab = "log radon level",
        cex.lab = 1.2, cex.axis = 1.1, pch = 20, mgp = c(2, 0.7,
            0), xaxt = "n", yaxt = "n", cex.main = 1.1, main = uniq[j])
    axis(1, c(0, 1), mgp = c(2, 0.7, 0), cex.axis = 1.1)
    axis(2, seq(-1, 3, 2), mgp = c(2, 0.7, 0), cex.axis = 1.1)
    curve(coef(lm.pooled)[1] + coef(lm.pooled)[2] * x, lwd = 0.5,
        lty = 2, col = "gray10", add = TRUE)
    curve(b.hat.unpooled.varying[j, 1] + b.hat.unpooled.varying[j,
        2] * x, lwd = 0.5, col = "gray10", add = TRUE)
    curve(a.hat.M3[j] + b.hat.M3[j] * x, lwd = 1, col = "black",
        add = TRUE)
}
```

![](MLMLab3_files/figure-gfm/plots%20on%20Figure%2013.1-1.png)<!-- -->

## 2.1 Understanding Correlation between Intercepts and Slopes

Let’s graph all county lines.

``` r
x.jitter <- x + runif(n, -0.05, 0.05)
col85 <- palette(rainbow(85))

plot(x.jitter, y, xlim = c(-0.05, 1.05), ylim = c(0, 2), xlab = "floor",
    ylab = "log radon level", cex.lab = 1.2, cex.axis = 1.1,
    pch = 20, mgp = c(2, 0.7, 0), xaxt = "n", yaxt = "n", cex.main = 1.1,
    main = "Multilevel Regression Lines for Each County", col = "white")
axis(1, c(0, 1), mgp = c(2, 0.7, 0), cex.axis = 1.1)
axis(2, seq(-1, 3, 2), mgp = c(2, 0.7, 0), cex.axis = 1.1)
for (j in 1:85) {
    curve(a.hat.M3[j] + b.hat.M3[j] * x, lwd = 1, add = TRUE,
        col = col85[j])
}
```

![](MLMLab3_files/figure-gfm/Graphing%20all%20county%20lines-1.png)<!-- -->

# 3 CHAPTER 13.1 Varying intercept & slopes w/ group level predictors

``` r
M4 <- lmer(y ~ x + u.full + x:u.full + (1 + x | county))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
display(M4)
```

    ## lmer(formula = y ~ x + u.full + x:u.full + (1 + x | county))
    ##             coef.est coef.se
    ## (Intercept)  1.44     0.03  
    ## x           -0.64     0.09  
    ## u.full       0.85     0.07  
    ## x:u.full    -0.42     0.24  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev. Corr 
    ##  county   (Intercept) 0.00          
    ##           x           0.37      NaN 
    ##  Residual             0.76          
    ## ---
    ## number of obs: 919, groups: county, 85
    ## AIC = 2148.2, DIC = 2106.2
    ## deviance = 2119.2

``` r
# coef(M4)
fixef(M4)
```

    ## (Intercept)           x      u.full    x:u.full 
    ##   1.4440747  -0.6413080   0.8462757  -0.4158211

``` r
# ranef(M4)

coef(M4)$county[1, 1]
```

    ## [1] 1.444075

``` r
fixef(M4)[1] + ranef(M4)$county[1, 1]
```

    ## (Intercept) 
    ##    1.444075

## 3.1 Prediction for a particular county

We can use this equation,

$$y_{ij}= \alpha_j + \beta_j x_i$$

or this equation,

$$y_i = \gamma^{\alpha}_0 + \gamma^{\alpha}_1 u_j + \gamma^{\beta}_0 x_i + \gamma^{\beta}_1 u_j \times x_i$$

``` r
# EQUATION 1: Prediction for county 85, x = 1

y85.hat.1 <- coef(M4)$county[85, 1] + coef(M4)$county[85, 2] *
    1

# or using fixef+ranef to calculate coef

y.85.hat.2 <- (fixef(M4)[1] + ranef(M4)$county[85, 1]) + (fixef(M4)[2] +
    ranef(M4)$county[85, 2]) * 1

# EQUATION 2: Prediction for county 85, x = 1

y.85.hat.3 <- coef(M4)$county[85, 1] + coef(M4)$county[85, 3] *
    u[85] + coef(M4)$county[85, 2] + coef(M4)$county[85, 4] *
    u[85]

y85.hat4 <- (fixef(M4)[1] + fixef(M4)[3] * u[85] + ranef(M4)$county[85,
    1]) + (fixef(M4)[2] * 1 + fixef(M4)[4] * 1 * u[85] + ranef(M4)$county[85,
    2])


a.hat.M4 <- coef(M4)$county[, 1] + coef(M4)$county[, 3] * u
b.hat.M4 <- coef(M4)$county[, 2] + coef(M4)$county[, 4] * u
a.se.M4 <- se.ranef(M4)$county[, 1]
b.se.M4 <- se.ranef(M4)$county[, 2]
```

The following reproduces Figure 13.2.

``` r
lower <- a.hat.M4 - a.se.M4
upper <- a.hat.M4 + a.se.M4
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2) + 0.1)
plot(u, a.hat.M4, cex.lab = 1.2, cex.axis = 1.1, ylim = range(lower,
    upper), xlab = "county-level uranium \n measure", ylab = "regression intercept",
    pch = 20, yaxt = "n")
axis(2, c(0, 1, 1.5, 2))
curve(fixef(M4)[1] + fixef(M4)[3] * x, lwd = 1, col = "black",
    add = TRUE)
segments(u, lower, u, upper, lwd = 0.5, col = "gray10")
mtext("Intercepts", line = 1)

# plot on Figure 13.2(b)
lower <- b.hat.M4 - b.se.M4
upper <- b.hat.M4 + b.se.M4
par(mar = c(5, 5, 4, 2) + 0.1)
plot(u, b.hat.M4, cex.lab = 1.2, cex.axis = 1.1, ylim = range(lower,
    upper), xlab = "county-level uranium \n measure", ylab = "regression slope",
    pch = 20)
curve(fixef(M4)[2] + fixef(M4)[4] * x, lwd = 1, col = "black",
    add = TRUE)
segments(u, lower, u, upper, lwd = 0.5, col = "gray10")
mtext("Slopes", line = 1)
```

![](MLMLab3_files/figure-gfm/plot%20on%20Figure%2013.2-1.png)<!-- -->

## 3.2 Change in Correlation between Intercepts and Slopes?

Graphing all county lines again.

``` r
x.jitter <- x + runif(n, -0.05, 0.05)
col85 <- palette(rainbow(85))

plot(x.jitter, y, xlim = c(-0.05, 1.05), ylim = c(0, 2), xlab = "floor",
    ylab = "log radon level", cex.lab = 1.2, cex.axis = 1.1,
    pch = 20, mgp = c(2, 0.7, 0), xaxt = "n", yaxt = "n", cex.main = 1.1,
    main = "Multilevel Regression Lines for Each County", col = "white")
axis(1, c(0, 1), mgp = c(2, 0.7, 0), cex.axis = 1.1)
axis(2, seq(-1, 3, 2), mgp = c(2, 0.7, 0), cex.axis = 1.1)
for (j in 1:85) {
    curve(a.hat.M4[j] + b.hat.M4[j] * x, lwd = 1, add = TRUE,
        col = col85[j])
}
```

![](MLMLab3_files/figure-gfm/Graphing%20all%20county%20lines%202-1.png)<!-- -->

# 4 CHAPTER 13.3 Modeling multiple varing coefficients using the scale inverse-Wishart distribution

This cannot be done in R with `lmer`. It has to be a Bayesian model.

# 5 13.4 Understanding Correlations Between group-level Intercepts and Slopes

The following uses the Heights data.

``` r
# Read data
heights <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/earnings/heights.dta")
head(heights)
```

    ##    earn height1 height2 sex race hisp ed yearbn height
    ## 1    NA       5       6   2    1    2 12     53     66
    ## 2    NA       5       4   1    2    2 12     50     64
    ## 3 50000       6       2   1    1    2 16     45     74
    ## 4 60000       5       6   2    1    2 16     32     66
    ## 5 30000       5       4   2    1    2 16     61     64
    ## 6    NA       5       5   2    1    2 17     33     65

``` r
# Define Variables
heights$age <- 90 - heights$yearb  # survey was conducted in 1990
heights$age[heights$age < 18] <- NA

heights$age.category <- ifelse(heights$age < 35, 1, ifelse(heights$age <
    50, 2, 3))

heights$eth <- ifelse(heights$race == 2, 1, ifelse(heights$hisp ==
    1, 2, ifelse(heights$race == 1, 3, 4)))

heights$male <- 2 - heights$sex

# (for simplicity) remove cases with missing data
ok <- !is.na(heights$earn + heights$height + heights$sex) & heights$earn >
    0 & heights$yearbn > 25

heights.clean <- as.data.frame(cbind(heights$earn, heights$height,
    heights$sex, heights$race, heights$hisp, heights$ed, heights$age,
    heights$age.category, heights$eth, heights$male)[ok, ])
n <- nrow(heights.clean)

colnames(heights.clean) <- c("earn", "height", "sex", "race",
    "hisp", "ed", "age", "age.category", "eth", "male")
height.jitter.add <- runif(n, -0.2, 0.2)

# rename variables
y <- log(heights.clean$earn)
x <- heights.clean$height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- heights.clean$age.category
eth <- heights.clean$eth
```

The following fits a multilevel model for `log(earnings)` on height,
age, ethnicity.

``` r
M1 <- lmer(y ~ x + (1 + x | eth))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
display(M1)
```

    ## lmer(formula = y ~ x + (1 + x | eth))
    ##             coef.est coef.se
    ## (Intercept) 7.27     1.10   
    ## x           0.04     0.02   
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev. Corr  
    ##  eth      (Intercept) 1.55           
    ##           x           0.02     -1.00 
    ##  Residual             0.90           
    ## ---
    ## number of obs: 1062, groups: eth, 4
    ## AIC = 2823.3, DIC = 2788.6
    ## deviance = 2800.0

The following reproduces Figure 13.3.

``` r
x.jitter <- x + runif(n, -0.2, 0.2)
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

# mean
ab.hat.M1 <- coef(M1)$eth
a.hat.M1 <- ab.hat.M1[, 1]
b.hat.M1 <- ab.hat.M1[, 2]

### simulation
ab.sim.M1 <- coef(sim(M1))$ranef$eth
a.sim.M1 <- ab.sim.M1[, , 1]
b.sim.M1 <- ab.sim.M1[, , 2]

## Figure 13.3
par(mfrow = c(1, 4), mar = c(4, 4, 3, 1), oma = c(1, 1, 2, 1))
for (j in 1:4) {
    plot(x.jitter[eth == j], y[eth == j], xlab = "height (inches)",
        ylab = "log earnings", mgp = c(2, 0.7, 0), xlim = range(x),
        ylim = range(y), yaxt = "n", cex.lab = 1.4, cex.axis = 1.3,
        pch = 20, cex = 0.6, cex.main = 1.5, main = eth.label[j])

    axis(2, seq(6, 12, 2), cex.axis = 1.3, mgp = c(2, 0.5, 0))
    for (i in 1:20) {
        curve(a.sim.M1[i, j] + b.sim.M1[i, j] * x, lwd = 1, col = "purple",
            add = TRUE)
    }
    curve(a.hat.M1[j] + b.hat.M1[j] * x, lwd = 3, add = TRUE)
}
```

![](MLMLab3_files/figure-gfm/plots%20on%20figure%2013.3-1.png)<!-- -->

## 5.1 Regression: Centering the predictors

The following reproduces the model and Figure 13.6.

``` r
x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)

M2 <- lmer(y ~ x.centered + (1 + x.centered | eth))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
display(M2)
```

    ## lmer(formula = y ~ x.centered + (1 + x.centered | eth))
    ##             coef.est coef.se
    ## (Intercept) 9.68     0.05   
    ## x.centered  0.03     0.02   
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev. Corr 
    ##  eth      (Intercept) 0.07          
    ##           x.centered  0.03     1.00 
    ##  Residual             0.90          
    ## ---
    ## number of obs: 1062, groups: eth, 4
    ## AIC = 2823.3, DIC = 2788.6
    ## deviance = 2800.0

``` r
ab.hat.M2 <- coef(M2)$eth
a.hat.M2 <- ab.hat.M2[, 1]
b.hat.M2 <- ab.hat.M2[, 2]

## Figure 13.6
x.jitter <- x + runif(n, -0.2, 0.2)
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

## Simulation
ab.sim.M2 <- coef(sim(M2))$ranef$eth
a.sim.M2 <- ab.sim.M2[, , 1]
b.sim.M2 <- ab.sim.M2[, , 2]

# Figure 13.6
par(mfrow = c(1, 4), mar = c(4, 4, 3, 1), oma = c(1, 1, 2, 1))
for (j in 1:4) {
    plot(x.centered.jitter[eth == j], y[eth == j], xlab = "height (inches from mean)",
        ylab = "log earnings", mgp = c(2, 0.7, 0), xlim = range(x.centered),
        ylim = range(y), yaxt = "n", cex.lab = 1.1, cex.axis = 1.3,
        pch = 20, cex = 0.6, cex.main = 1.2, main = eth.label[j])
    axis(2, seq(6, 12, 2), cex.axis = 1.1, mgp = c(2, 0.5, 0))
    for (i in 1:20) {
        curve(a.sim.M2[i, j] + b.sim.M2[i, j] * x, lwd = 0.5,
            col = "gray", add = TRUE)
    }
    curve(a.hat.M2[j] + b.hat.M2[j] * x, lwd = 1, add = TRUE)
}
```

![](MLMLab3_files/figure-gfm/Regression%20centering%20the%20predictors-1.png)<!-- -->

# 6 CHAPTER 13.5 Non-nested Model

## 6.1 Pilots

``` r
pilots <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/pilots/pilots.dat",
    header = TRUE)
attach(pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group) {
    for (k in 1:n.scenario) {
        ok <- group == group.names[j] & scenario == scenario.names[k]
        successes <- c(successes, sum(recovered[ok] == 1, na.rm = T))
        failures <- c(failures, sum(recovered[ok] == 0, na.rm = T))
        group.id <- c(group.id, j)
        scenario.id <- c(scenario.id, k)
    }
}

y <- successes/(successes + failures)
y.mat <- matrix(y, n.scenario, n.group)
sort.group <- order(apply(y.mat, 2, mean))
sort.scenario <- order(apply(y.mat, 1, mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario, sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln",
    "Chrlt", "Shemya", "Toledo")
```

Fitting a non-nested model:

``` r
M1 <- lmer(y ~ 1 + (1 | group.id) + (1 | scenario.id))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
display(M1)
```

    ## lmer(formula = y ~ 1 + (1 | group.id) + (1 | scenario.id))
    ## coef.est  coef.se 
    ##     0.44     0.12 
    ## 
    ## Error terms:
    ##  Groups      Name        Std.Dev.
    ##  scenario.id (Intercept) 0.32    
    ##  group.id    (Intercept) 0.00    
    ##  Residual                0.22    
    ## ---
    ## number of obs: 40, groups: scenario.id, 8; group.id, 5
    ## AIC = 20.3, DIC = 7.3
    ## deviance = 9.8

Reproducing the Figure 13.8.

``` r
image(y.mat.new, col = gray((1:11)/12), xaxt = "n", yaxt = "n")
axis(2, seq(0, 1, length = n.group), group.names[sort.group],
    tck = 0, cex.axis = 1.2)
axis(3, seq(0, 1, length = n.scenario), scenario.abbr[sort.scenario],
    tck = 0, cex.axis = 1.2)
for (x in seq(0.5, n.group - 0.5, 1)/(n.group - 1)) lines(c(-10,
    10), rep(x, 2), col = "white", lwd = 0.5)
for (x in seq(0.5, n.scenario - 0.5, 1)/(n.scenario - 1)) lines(rep(x,
    2), c(-10, 10), col = "white", lwd = 0.5)
```

![](MLMLab3_files/figure-gfm/Plot%20figure%2013.8-1.png)<!-- -->
