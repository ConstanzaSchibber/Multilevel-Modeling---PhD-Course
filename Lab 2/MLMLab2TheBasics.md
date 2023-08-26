Lab 2: Multilevel Linear Models. The Basics. (Chapter 12)
================
Constanza F. Schibber
January 28, 2021

- [1 Radon Data](#1-radon-data)
- [2 CHAPTER 12.2 Partial pooling with no
  predictors](#2-chapter-122-partial-pooling-with-no-predictors)
  - [2.1 Complete-pooling and no-pooling estimates of county radon
    levels](#21-complete-pooling-and-no-pooling-estimates-of-county-radon-levels)
  - [2.2 Partial-pooling estimates from a multilevel
    model](#22-partial-pooling-estimates-from-a-multilevel-model)
- [3 CHAPTER 12.3 Partial pooling with
  predictors](#3-chapter-123-partial-pooling-with-predictors)
  - [3.1 Varying-intercept with an individual Level
    Predictor](#31-varying-intercept-with-an-individual-level-predictor)
  - [3.2 Group Level Predictors!](#32-group-level-predictors)

The code from the book has been fixed by Jeff Gill and then myself over
the years. You can look up the code on Gelman’s website, but some of it
has a few errors. However, the code on the book is correct so reference
the book when in doubt.

Additional note: Packages and even base R change, so some things change
over time.

# 1 Radon Data

Read the description of the dataset carefully. If you do not understand
something, like what radon is, Google it! The book uses the data all the
time, so it is important to familiarize yourself with it.

# 2 CHAPTER 12.2 Partial pooling with no predictors

## 2.1 Complete-pooling and no-pooling estimates of county radon levels

``` r
# Pooled and Unpooled Models
lm.pooled <- lm(y ~ x)
lm.unpooled <- lm(y ~ x + factor(county) - 1)  # by taking out the intercept we can estimate a coefficient for each county, though here x has an actual 0

summary(lm.pooled)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6293 -0.5383  0.0342  0.5603  2.5486 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.32674    0.02972  44.640   <2e-16 ***
    ## x           -0.61339    0.07284  -8.421   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8226 on 917 degrees of freedom
    ## Multiple R-squared:  0.07178,    Adjusted R-squared:  0.07077 
    ## F-statistic: 70.91 on 1 and 917 DF,  p-value: < 2.2e-16

``` r
summary(lm.unpooled)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x + factor(county) - 1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.14595 -0.45405  0.00065  0.45376  2.65987 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## x                -0.72054    0.07352  -9.800  < 2e-16 ***
    ## factor(county)1   0.84054    0.37866   2.220 0.026701 *  
    ## factor(county)2   0.87482    0.10498   8.333 3.23e-16 ***
    ## factor(county)3   1.52870    0.43946   3.479 0.000530 ***
    ## factor(county)4   1.55272    0.28897   5.373 1.00e-07 ***
    ## factor(county)5   1.43257    0.37866   3.783 0.000166 ***
    ## factor(county)6   1.51301    0.43672   3.464 0.000558 ***
    ## factor(county)7   2.01216    0.20243   9.940  < 2e-16 ***
    ## factor(county)8   1.98958    0.37999   5.236 2.08e-07 ***
    ## factor(county)9   1.00304    0.23931   4.191 3.07e-05 ***
    ## factor(county)10  1.56391    0.31099   5.029 6.04e-07 ***
    ## factor(county)11  1.40113    0.33828   4.142 3.80e-05 ***
    ## factor(county)12  1.73025    0.37821   4.575 5.49e-06 ***
    ## factor(county)13  1.03872    0.30881   3.364 0.000804 ***
    ## factor(county)14  1.98838    0.20325   9.783  < 2e-16 ***
    ## factor(county)15  1.33797    0.37999   3.521 0.000453 ***
    ## factor(county)16  0.66486    0.53487   1.243 0.214204    
    ## factor(county)17  1.27480    0.38221   3.335 0.000890 ***
    ## factor(county)18  1.12155    0.21913   5.118 3.83e-07 ***
    ## factor(county)19  1.33831    0.09541  14.026  < 2e-16 ***
    ## factor(county)20  1.80032    0.43672   4.122 4.13e-05 ***
    ## factor(county)21  1.73399    0.25227   6.873 1.23e-11 ***
    ## factor(county)22  0.63679    0.30905   2.060 0.039663 *  
    ## factor(county)23  1.39999    0.53613   2.611 0.009183 ** 
    ## factor(county)24  2.10162    0.25267   8.318 3.64e-16 ***
    ## factor(county)25  1.95072    0.20243   9.636  < 2e-16 ***
    ## factor(county)26  1.36058    0.07422  18.332  < 2e-16 ***
    ## factor(county)27  1.77336    0.30978   5.725 1.45e-08 ***
    ## factor(county)28  1.24159    0.34115   3.639 0.000290 ***
    ## factor(county)29  1.05600    0.43672   2.418 0.015818 *  
    ## factor(county)30  0.92576    0.22807   4.059 5.39e-05 ***
    ## factor(county)31  2.02057    0.33828   5.973 3.45e-09 ***
    ## factor(county)32  1.23629    0.37821   3.269 0.001124 ** 
    ## factor(county)33  2.06187    0.37821   5.452 6.58e-08 ***
    ## factor(county)34  1.59044    0.43946   3.619 0.000314 ***
    ## factor(county)35  0.81920    0.28897   2.835 0.004695 ** 
    ## factor(county)36  2.95897    0.53613   5.519 4.55e-08 ***
    ## factor(county)37  0.40209    0.25227   1.594 0.111345    
    ## factor(county)38  1.86772    0.37999   4.915 1.07e-06 ***
    ## factor(county)39  1.74807    0.33860   5.163 3.05e-07 ***
    ## factor(county)40  2.31580    0.37866   6.116 1.48e-09 ***
    ## factor(county)41  1.96715    0.26759   7.351 4.69e-13 ***
    ## factor(county)42  1.36098    0.75642   1.799 0.072343 .  
    ## factor(county)43  1.60224    0.25543   6.273 5.69e-10 ***
    ## factor(county)44  1.04099    0.28609   3.639 0.000291 ***
    ## factor(county)45  1.29541    0.21101   6.139 1.28e-09 ***
    ## factor(county)46  1.21461    0.33828   3.591 0.000349 ***
    ## factor(county)47  0.88393    0.53613   1.649 0.099583 .  
    ## factor(county)48  1.14812    0.25227   4.551 6.13e-06 ***
    ## factor(county)49  1.70211    0.21010   8.102 1.93e-15 ***
    ## factor(county)50  2.49321    0.75642   3.296 0.001022 ** 
    ## factor(county)51  2.16504    0.37821   5.724 1.45e-08 ***
    ## factor(county)52  1.92769    0.43672   4.414 1.15e-05 ***
    ## factor(county)53  1.25080    0.43741   2.860 0.004348 ** 
    ## factor(county)54  1.30676    0.15802   8.270 5.28e-16 ***
    ## factor(county)55  1.61799    0.26885   6.018 2.64e-09 ***
    ## factor(county)56  1.10110    0.43946   2.506 0.012415 *  
    ## factor(county)57  0.76218    0.30905   2.466 0.013855 *  
    ## factor(county)58  1.86092    0.37866   4.915 1.07e-06 ***
    ## factor(county)59  1.72178    0.37999   4.531 6.73e-06 ***
    ## factor(county)60  1.27939    0.53487   2.392 0.016979 *  
    ## factor(county)61  1.15873    0.13389   8.654  < 2e-16 ***
    ## factor(county)62  1.98301    0.33860   5.856 6.80e-09 ***
    ## factor(county)63  1.67070    0.43741   3.820 0.000144 ***
    ## factor(county)64  1.84784    0.22817   8.099 1.97e-15 ***
    ## factor(county)65  1.29912    0.53487   2.429 0.015357 *  
    ## factor(county)66  1.66574    0.20648   8.067 2.50e-15 ***
    ## factor(county)67  1.80312    0.21101   8.545  < 2e-16 ***
    ## factor(county)68  1.09002    0.26743   4.076 5.02e-05 ***
    ## factor(county)69  1.24245    0.37821   3.285 0.001062 ** 
    ## factor(county)70  0.86763    0.07096  12.227  < 2e-16 ***
    ## factor(county)71  1.49184    0.15174   9.832  < 2e-16 ***
    ## factor(county)72  1.57990    0.23920   6.605 7.08e-11 ***
    ## factor(county)73  1.79176    0.53487   3.350 0.000845 ***
    ## factor(county)74  0.98704    0.37821   2.610 0.009223 ** 
    ## factor(county)75  1.72372    0.43741   3.941 8.80e-05 ***
    ## factor(county)76  2.00844    0.37866   5.304 1.45e-07 ***
    ## factor(county)77  1.82168    0.28609   6.367 3.17e-10 ***
    ## factor(county)78  1.28569    0.33956   3.786 0.000164 ***
    ## factor(county)79  0.61488    0.37866   1.624 0.104785    
    ## factor(county)80  1.32952    0.11181  11.890  < 2e-16 ***
    ## factor(county)81  2.70953    0.43946   6.166 1.09e-09 ***
    ## factor(county)82  2.23001    0.75642   2.948 0.003286 ** 
    ## factor(county)83  1.62292    0.21048   7.711 3.57e-14 ***
    ## factor(county)84  1.64535    0.20987   7.840 1.38e-14 ***
    ## factor(county)85  1.18652    0.53487   2.218 0.026801 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7564 on 833 degrees of freedom
    ## Multiple R-squared:  0.7671, Adjusted R-squared:  0.7431 
    ## F-statistic: 31.91 on 86 and 833 DF,  p-value: < 2.2e-16

``` r
# REPLICATE THE PLOT ON PAGE 256 (PANEL ON THE LEFT)
par(mfrow = c(1, 2), mar = c(5, 5, 5, 3), bg = "lightgray")  # this adds both panels on the same page
plot(sample.size.jittered, coef(lm.unpooled)[-1], cex.lab = 0.9,
    cex.axis = 1, xlab = "", ylab = "", pch = 20, log = "x",
    cex = 0.5, ylim = c(0, 3.7), yaxt = "n", xaxt = "n")
mtext(side = 1, line = 2.5, "Jittered Sample Size in County J")
mtext(side = 2, line = 2.5, "Estimated Intercept")
mtext(side = 3, line = 1.5, "No-Pooling", cex = 1.5)
axis(1, c(1, 3, 10, 30, 100), cex.axis = 0.9)
axis(2, seq(0, 3), cex.axis = 0.9)
for (j in 1:J) {
    lines(rep(sample.size.jittered[j], 2), coef(lm.unpooled)[-1][j] +
        c(-1, 1) * se.coef(lm.unpooled)[-1][j], lwd = 0.5)
}
abline(h = ybarbar, lty = 2)
```

![](MLMLab2TheBasics_files/figure-gfm/Pooled%20and%20Unpooled%20Models-1.png)<!-- -->

## 2.2 Partial-pooling estimates from a multilevel model

``` r
# Partial pooling
M0 <- lmer(y ~ -1 + (1 | county))
summary(M0)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: y ~ -1 + (1 | county)
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2449.0   2458.7  -1222.5   2445.0      917 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3090 -0.5259  0.1002  0.6742  3.4027 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  county   (Intercept) 1.8624   1.3647  
    ##  Residual             0.6378   0.7986  
    ## Number of obs: 919, groups:  county, 85

``` r
coef(M0)
```

    ## $county
    ##    (Intercept)
    ## 1    0.6083254
    ## 2    0.8277980
    ## 3    0.9409294
    ## 4    1.0877697
    ## 5    1.1536661
    ## 6    1.3579930
    ## 7    1.8636397
    ## 8    1.5008166
    ## 9    0.9001567
    ## 10   1.1386473
    ## 11   1.3113200
    ## 12   1.5937961
    ## 13   0.9826367
    ## 14   1.7399543
    ## 15   0.9005980
    ## 16   0.5676626
    ## 17   0.6764776
    ## 18   0.9152927
    ## 19   1.2855714
    ## 20   1.6158621
    ## 21   1.5933047
    ## 22   0.4888024
    ## 23   0.8877190
    ## 24   1.8703356
    ## 25   1.8036694
    ## 26   1.2809183
    ## 27   1.4503963
    ## 28   0.7573915
    ## 29   0.9478074
    ## 30   0.8978078
    ## 31   1.8910493
    ## 32   1.1387941
    ## 33   1.8992705
    ## 34   0.9963450
    ## 35   0.3884575
    ## 36   2.2187798
    ## 37   0.3102243
    ## 38   1.3885657
    ## 39   1.5011432
    ## 40   1.9672469
    ## 41   1.8000309
    ## 42   1.0137969
    ## 43   1.1578805
    ## 44   0.8943020
    ## 45   1.0461514
    ## 46   1.1367536
    ## 47   0.4471032
    ## 48   1.0289093
    ## 49   1.5504146
    ## 50   1.8571988
    ## 51   1.9942995
    ## 52   1.7301877
    ## 53   0.9070792
    ## 54   1.1949793
    ## 55   1.2924660
    ## 56   0.5571408
    ## 57   0.6074255
    ## 58   1.5482391
    ## 59   1.2541403
    ## 60   1.0923483
    ## 61   1.0796224
    ## 62   1.7210291
    ## 63   1.2839530
    ## 64   1.7285195
    ## 65   1.1091934
    ## 66   1.2240630
    ## 67   1.5408259
    ## 68   1.0452773
    ## 69   1.1444709
    ## 70   0.7659860
    ## 71   1.3579555
    ## 72   1.5275880
    ## 73   1.5298135
    ## 74   0.9092005
    ## 75   1.3315421
    ## 76   1.6841206
    ## 77   1.6385843
    ## 78   0.9335343
    ## 79   0.4004628
    ## 80   1.2419542
    ## 81   2.0007817
    ## 82   1.6611467
    ## 83   1.4192573
    ## 84   1.5491197
    ## 85   1.0130584
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

``` r
# ranef(M0)

# REPLICATE THE PLOT ON PAGE 256 (PANEL ON THE RIGHT)
plot(sample.size.jittered, unlist(ranef(M0)), cex.lab = 0.9,
    cex.axis = 1, xlab = "Jittered Sample Size in County J",
    ylab = "Estimated Intercept", pch = 20, log = "x", cex = 0.3,
    mgp = c(1.5, 0.5, 0), ylim = c(0, 3.7), yaxt = "n", xaxt = "n")
mtext(side = 3, line = 1.5, "Multilevel Model", cex = 1.5)
axis(1, c(1, 3, 10, 30, 100), cex.axis = 0.9, mgp = c(1.5, 0.5,
    0))
axis(2, seq(0, 3), cex.axis = 0.9, mgp = c(1.5, 0.5, 0))
for (j in 1:J) {
    lines(rep(sample.size.jittered[j], 2), unlist(ranef(M0))[j] +
        c(-1, 1) * unlist(se.ranef(M0))[j], lwd = 0.5)
}
abline(h = ybarbar, lty = 2)
```

![](MLMLab2TheBasics_files/figure-gfm/Varying%20Intercept%20with%20No%20Individual%20or%20Group%20Predictors%20and%20no%20constant-1.png)<!-- -->

# 3 CHAPTER 12.3 Partial pooling with predictors

## 3.1 Varying-intercept with an individual Level Predictor

$$y_i \sim N(\alpha_{j[i]}+ \beta x_i, \sigma^2_y)$$

$$\alpha_j \sim N(\mu_{\alpha}, \sigma_{\alpha}^2)$$

for $i= 1, \dots, 919$ and $j= 1, \dots, 85$.

``` r
M1 <- lmer(y ~ x + (1 | county))
display(M1)
```

    ## lmer(formula = y ~ x + (1 | county))
    ##             coef.est coef.se
    ## (Intercept)  1.46     0.05  
    ## x           -0.69     0.07  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  county   (Intercept) 0.33    
    ##  Residual             0.76    
    ## ---
    ## number of obs: 919, groups: county, 85
    ## AIC = 2179.3, DIC = 2156
    ## deviance = 2163.7

``` r
# summary(M1) coef(M1) ranef(M1) fixef(M2)
```

- What is the value for $\sigma_y$?
- What is the value for $\sigma_{\alpha}$?
- What is the coefficient of $x$?

``` r
# Slope and 95% CI
fixef(M1)[2]
```

    ##          x 
    ## -0.6929937

``` r
fixef(M1)["x"] + c(-1.96, 1.96) * se.fixef(M1)["x"]
```

    ## [1] -0.8310381 -0.5549493

``` r
# Random effect for county 55 and 95% CI (how much the
# intercept is shifted up or down this county)

unlist(ranef(M1))[55]
```

    ## county.(Intercept)55 
    ##           0.08786324

``` r
ranef(M1)$county[55, 1]
```

    ## [1] 0.08786324

``` r
ranef(M1)$county[55, 1] + c(-1.96, 1.96) * se.ranef(M1)$county[55]
```

    ## [1] -0.3182284  0.4939549

``` r
# Estimated Intercept for country 1 with CI (page 260)

fixef(M1)[1] + ranef(M1)$county[1, 1]
```

    ## (Intercept) 
    ##      1.1915

``` r
unlist(coef(M1)[[1]]["(Intercept)"][1])[1]
```

    ## (Intercept)1 
    ##       1.1915

``` r
# Estimates for every county
a.hat.M1 <- fixef(M1)[1] + ranef(M1)$county[1, 1]  #coef(M1)[[1]]['(Intercept)']
b.hat.M1 <- fixef(M1)[2]
```

- What is the regression line for county 1?
- Select another county and calculate the estimated intercept and the
  regression line.

## 3.2 Group Level Predictors!

$$y_i = \alpha_{j[i]}+ \beta X_i + \epsilon_i $$

$$\alpha_j \sim N(\gamma_0 + \gamma_1 u_j, \sigma_{\alpha}^2)$$

for $i= 1, \dots, 919$ and $j= 1, \dots, 85$.

``` r
# Model
M2 <- lmer(y ~ x + u.full + (1 | county))
display(M2)
```

    ## lmer(formula = y ~ x + u.full + (1 | county))
    ##             coef.est coef.se
    ## (Intercept)  1.47     0.04  
    ## x           -0.67     0.07  
    ## u.full       0.72     0.09  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  county   (Intercept) 0.16    
    ##  Residual             0.76    
    ## ---
    ## number of obs: 919, groups: county, 85
    ## AIC = 2144.2, DIC = 2111.4
    ## deviance = 2122.8

``` r
fixef(M2)
```

    ## (Intercept)           x      u.full 
    ##   1.4657628  -0.6682448   0.7202676

``` r
# COMBINE THE AGGREGATE COUNTY LEVEL EFFECTS
a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3] * u + ranef(M2)$county

# note: On the book and until recently, we had to do
# as.vector(ranef(M2)$county) but this has changed and it
# doesn't work anymore

# COLLECT THE VARIABLE FLOOR EFFECT
b.hat.M2 <- fixef(M2)[2]
```

- What is the value for $\sigma_y$?
- What is the value for $\sigma_{\alpha}$?
- What is the coefficient of $x$?
- Compare the values of $\sigma_y$ and $\sigma_{\alpha}$ to those of the
  model without group level predictors.

``` r
x.jitter <- x + runif(n, -0.05, 0.05)
display8 <- c(36, 1, 35, 21, 14, 71, 61, 70)
y.range <- range(y[!is.na(match(county, display8))])

# FIGURE
par(mfrow = c(2, 4), mar = c(3, 2, 2, 1), oma = c(6, 6, 2, 2),
    bg = "lightgray")
for (j in display8) {
    plot(x.jitter[county == j], y[county == j], xlim = c(-0.05,
        1.05), ylim = y.range, xlab = "", ylab = "", cex.lab = 1.6,
        cex.axis = 1.5, pch = 20, mgp = c(2, 0.7, 0), xaxt = "n",
        yaxt = "n", cex.main = 1.3)
    axis(1, c(0, 1), mgp = c(2, 0.7, 0), cex.axis = 1.5)
    axis(2, seq(-1, 3, 2), mgp = c(2, 0.7, 0), cex.axis = 1.5)
    curve(unlist(coef(lm.unpooled)[-1])[j] + coef(lm.unpooled)[1] *
        x, lty = 3, lwd = 2, col = "forestgreen", add = TRUE)
    curve(a.hat.M2[j, ] + b.hat.M2 * x, lty = 1, lwd = 2, col = "firebrick",
        add = TRUE)
    curve(coef(lm.pooled)[1] + coef(lm.pooled)[2] * x, lty = 4,
        lwd = 2, col = "blue", add = TRUE)
    mtext(outer = FALSE, side = 3, line = 0.8, paste(uniq[j]),
        cex = 1.1)
}
mtext(outer = TRUE, side = 1, line = 2.5, "Jittered Floor (x) Variable",
    cex = 1.4)
mtext(outer = TRUE, side = 2, line = 2.5, "Log Radon Level",
    cex = 1.4)
```

![](MLMLab2TheBasics_files/figure-gfm/Figure%2012.5-1.png)<!-- -->

- Write the regression lines for the multilevel models for each county
  on the figure.
- Add a label to the figure indicating what each line color is.

``` r
par(mfrow = c(1, 1), mar = c(5, 5, 2, 1), bg = "lightgray")
plot(u, t(a.hat.M2), cex.lab = 1.1, cex.axis = 1.1, pch = 20,
    ylab = "", xlab = "", yaxt = "n", xaxt = "n", ylim = c(0.5,
        2))
axis(1, seq(-1, 1, 0.5), cex.axis = 1)
axis(2, seq(0, 2, 0.5), cex.axis = 1)
mtext(side = 1, cex = 1.3, "County-Level Uranium", line = 2.5)
mtext(side = 2, cex = 1.3, "Estimated Regression Intercept",
    line = 2.5)
curve(fixef(M2)["(Intercept)"] + fixef(M2)["u.full"] * x, lwd = 1,
    col = "black", add = TRUE)
for (j in 1:J) lines(rep(u[j], 2), a.hat.M2[j, ] + c(-1, 1) *
    se.coef(M2)$county[j, ], lwd = 0.5, col = "gray10")
```

![](MLMLab2TheBasics_files/figure-gfm/Figure%2012.6-1.png)<!-- -->

- The figure above plots coefficients $\alpha_j$ versus county level
  uranium, along with the regression line
  $\alpha= \gamma_0+\gamma_1 u_j$. The deviations from the line are
  captured by $\sigma_{\alpha}$.

``` r
# county 85

## log uranium in county 85
u[85]
```

    ## [1] 0.355287

``` r
## intercept for county 85
a.hat.85 <- fixef(M2)[1] + ranef(M2)$county[85, 1]
# unlist(coef(M2)[[1]]['(Intercept)'])[85]

## slope for x
fixef(M2)[2]
```

    ##          x 
    ## -0.6682448

``` r
## slope for u
fixef(M2)[3]
```

    ##    u.full 
    ## 0.7202676

- Calculate the fitted value for county 85 when $x$ is ‘floor’ and when
  it is ‘basement’.
