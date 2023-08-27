Lab 5: Multilevel Generalized Linear Models (Chapter 14) and Multilevel
Regression Post-Stratification
================
Constanza F. Schibber
February 18, 2021

- [1 G&H Chapter 14: Multilevel Logistic Regression. A simple model with
  some demographic and geographic
  variation](#1-gh-chapter-14-multilevel-logistic-regression-a-simple-model-with-some-demographic-and-geographic-variation)
- [2 G&H Chapter 14: Multilevel Logistic Regression. A fuller model
  including non-nested
  factors](#2-gh-chapter-14-multilevel-logistic-regression-a-fuller-model-including-non-nested-factors)
  - [2.1 Predictions](#21-predictions)
  - [2.2 Graphing the estimated model](#22-graphing-the-estimated-model)
- [3 Estimating State Public Opinion With Multi-Level Regression and
  Poststratification using R, by Kastellec, Lax, and
  Phillips](#3-estimating-state-public-opinion-with-multi-level-regression-and-poststratification-using-r-by-kastellec-lax-and-phillips)
- [4 Multilevel Poisson Model](#4-multilevel-poisson-model)

# 1 G&H Chapter 14: Multilevel Logistic Regression. A simple model with some demographic and geographic variation

$$p(y_i = 1)     = \text{logit}^{-1}(\alpha_{j[i]} + \beta^{\text{female}}\cdot\text{female} + \beta^{\text{black}}\cdot\text{black}), \qquad i=1,\ldots,n $$

$$\alpha_j  \sim N(\mu_\alpha,\sigma^2_\text{state}), \qquad j=1,\ldots,51.$$

``` r
M1 <- glmer(y ~ black + female + (1 | state), family = binomial(link = "logit"),
    data = polls.subset.delete)
display(M1)
```

    ## glmer(formula = y ~ black + female + (1 | state), data = polls.subset.delete, 
    ##     family = binomial(link = "logit"))
    ##             coef.est coef.se
    ## (Intercept)  0.45     0.10  
    ## black       -1.74     0.21  
    ## female      -0.10     0.10  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  state    (Intercept) 0.41    
    ##  Residual             1.00    
    ## ---
    ## number of obs: 2015, groups: state, 49
    ## AIC = 2666.7, DIC = 2531.5
    ## deviance = 2595.1

The 51 varying intercepts by state $\alpha_{state}$ along with their
standard error.

``` r
# Using coef
cbind(coef(M1)$state["(Intercept)"], se.coef(M1)$state)
```

    ##     (Intercept) (Intercept)
    ## 1   0.990577923   0.2856998
    ## 3   0.686196890   0.2734280
    ## 4   0.314917174   0.3359712
    ## 5   0.306467238   0.1388447
    ## 6   0.405039710   0.2900267
    ## 7   0.525405088   0.2884233
    ## 8   0.207973650   0.3642537
    ## 9   0.351646972   0.3964806
    ## 10  0.555017574   0.1690039
    ## 11  0.680322530   0.2445791
    ## 13  0.246698094   0.3806282
    ## 14  0.127338735   0.1953729
    ## 15  0.603560910   0.2695756
    ## 16 -0.002673333   0.3028687
    ## 17  0.772626878   0.2900570
    ## 18  0.587264402   0.2765396
    ## 19  0.591025448   0.2792267
    ## 20  0.251498003   0.3310161
    ## 21 -0.112103816   0.2840585
    ## 22 -0.042783973   0.2332650
    ## 23  0.274932466   0.1976813
    ## 24 -0.027563474   0.2480448
    ## 25  0.846679814   0.2902721
    ## 26  0.343388600   0.2617529
    ## 27  0.308091684   0.3742315
    ## 28  0.434744638   0.3159959
    ## 29  0.533919910   0.3819914
    ## 30  0.474629258   0.3885296
    ## 31  0.478559978   0.2149299
    ## 32  0.222474595   0.2932386
    ## 33 -0.015520921   0.1507383
    ## 34  0.808876650   0.2547938
    ## 35  0.410080601   0.3630067
    ## 36  0.730005641   0.1878325
    ## 37  0.549073328   0.3158876
    ## 38 -0.009792474   0.3100504
    ## 39  0.264074729   0.1847933
    ## 40  0.199560980   0.3353601
    ## 41  0.802824058   0.2846825
    ## 42  0.368705271   0.3682180
    ## 43  1.093493853   0.2602336
    ## 44  0.562909238   0.1684155
    ## 45  0.972570461   0.3504374
    ## 46  0.567653763   0.3961490
    ## 47  0.912512478   0.2357706
    ## 48  0.400200323   0.2299416
    ## 49  0.421551630   0.2816671
    ## 50  0.154784156   0.2286410
    ## 51  0.567653763   0.3961490

If we want shift by state from the their mean.

``` r
# mean
fixef(M1)["(Intercept)"]
```

    ## (Intercept) 
    ##   0.4452311

``` r
# ranef
cbind(ranef(M1), se.ranef(M1))
```

    ##       [,1]         [,2]      
    ## state data.frame,1 numeric,49

In order to compare Model 1 to a null model, we have to fit the null
model and extract information. We can use a $\chi^2_{df}$ test because
the null model and the alternative model are nested. We can also use the
AIC. Moreover, we can compare $\sigma^2_{\alpha}$ for each model.

``` r
M0 <- glmer(y ~ (1 | state), family = binomial(link = "logit"),
    data = polls.subset.delete)
display(M0)
```

    ## glmer(formula = y ~ (1 | state), data = polls.subset.delete, 
    ##     family = binomial(link = "logit"))
    ## coef.est  coef.se 
    ##     0.26     0.08 
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  state    (Intercept) 0.37    
    ##  Residual             1.00    
    ## ---
    ## number of obs: 2015, groups: state, 49
    ## AIC = 2749.2, DIC = 2629.4
    ## deviance = 2687.3

``` r
# chi-square test comparing deviance
pchisq(2745 - 2659, df = 2, lower.tail = FALSE)
```

    ## [1] 2.115131e-19

``` r
# AIC comparison
AIC(M0)
```

    ## [1] 2749.188

``` r
AIC(M1)
```

    ## [1] 2666.66

# 2 G&H Chapter 14: Multilevel Logistic Regression. A fuller model including non-nested factors

$$p(y_i = 1)   = \text{logit}^{-1}\left(\beta^{0} +  \beta^{\text{female}}\cdot\text{female}_i + \beta^{\text{black}}\cdot\text{black}_i  + \beta^{\text{female.black}}\cdot\text{female}_i\cdot\text{black}_i
                      + \alpha_{k[i]}^{\text{age}}
                      + \alpha_{\ell[i]}^{\text{edu}}
                      + \alpha_{k[i],\ell[i]}^{\text{age.edu}}
                      + \alpha_{j[i]}^{\text{state}}\right) $$

$$\alpha_j^{\text{state}} \sim N(\alpha_{m[j]}^{\text{region}} 
                        + \beta^{\text{v.prev}}\cdot \text{v.prev}_j,
                        \sigma^2_\text{state})$$

Notice that these are 4 hierarchies.The remaining coefficients are
modeled as: \$\$\`\`{=tex}\$\$

Notice that when fitting the model using `glmer` there is a convergence
problem. The error says
`In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,:Model failed to converge with max|grad| = 0.00779091 (tol = 0.001, component 1)`.
It happens often when fitting generalized linear models. You need to do
some troubleshooting. In this case, I just increase the number of
iterations for the algorithm and it does not give me the warning
anymore.

``` r
M2.0 <- glmer(y ~ black + female + black*female + v.prev.full
            + (1 | age) 
            + (1 | edu)
    #        + (1 | age.edu) 
            + (1 | state) 
            + (1 | region.full),
            data=polls.subset.delete,
            family=binomial(link="logit"))
start.M2 <- getME(M2.0, c("theta","fixef"))
M2 <- update(M2.0,
             start=start.M2,
             control=glmerControl(optCtrl=list(maxfun=2e4)))
#summary(M2)
display(M2)
```

    ## glmer(formula = y ~ black + female + black * female + v.prev.full + 
    ##     (1 | age) + (1 | edu) + (1 | state) + (1 | region.full), 
    ##     data = polls.subset.delete, family = binomial(link = "logit"), 
    ##     control = glmerControl(optCtrl = list(maxfun = 20000)), start = start.M2)
    ##              coef.est coef.se
    ## (Intercept)  -3.40     1.04  
    ## black        -1.64     0.33  
    ## female       -0.09     0.10  
    ## v.prev.full   6.85     1.86  
    ## black:female -0.18     0.42  
    ## 
    ## Error terms:
    ##  Groups      Name        Std.Dev.
    ##  state       (Intercept) 0.20    
    ##  region.full (Intercept) 0.19    
    ##  edu         (Intercept) 0.14    
    ##  age         (Intercept) 0.10    
    ##  Residual                1.00    
    ## ---
    ## number of obs: 2015, groups: state, 49; region.full, 5; edu, 4; age, 4
    ## AIC = 2648.3, DIC = 2547.4
    ## deviance = 2588.9

Quickly reading this regression output (page 304):

1.  The intercept is not easily interpretable.
2.  The coefficient for **black** is −1.6. Dividing by 4 (see page 82)
    yields a rough estimate that African-American men were 40% less
    likely than other men to support Bush, after controlling for age,
    education, and state.
3.  The coefficient for **female** is −0.1. Dividing by 4 yields a rough
    estimate that non-African-American women were slightly less likely
    than non-African- American men to support Bush, after controlling
    for age, education, and state. However, the standard error on this
    coefficient is as large as the estimate itself, indicating that our
    sample size is too small for us to be certain of this pattern in the
    population.
4.  The coefficient for **v.prev.full** is 7.0, which, when divided by
    4, is 1.7, suggesting that a 1% difference in a state’s support for
    Republican candidates in previous elections mapped to a predicted
    1.7% difference in support for Bush in 1988.
5.  The large standard error on the coefficient for **black:female**
    indicates that the sample size is too small to estimate this
    interaction precisely.
6.  The **state-level errors** have estimated standard deviation 0.2 on
    the logit scale. Dividing by 4 tells us that the states differed by
    approximately $\pm 5\%$ on the probability scale (over and above the
    differences explained by demographic factors).
7.  The differences among **age-education** groups and regions are also
    approximately $\pm 5%$ on the probability scale.
8.  Very little variation is found among **age** groups or **education**
    groups after con- trolling for the other predictors in the model.

The estimates for $\alpha^{\text{age}}$, $\alpha^{\text{edu}}$,
$\alpha^{\text{edu.age}}$, $\alpha^{\text{state}}$,
$\alpha^{\text{region}}$:

``` r
coef(M2)
```

    ## $state
    ##    (Intercept)     black      female v.prev.full black:female
    ## 1    -3.249307 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 3    -3.366674 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 4    -3.452386 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 5    -3.360338 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 6    -3.438172 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 7    -3.368489 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 8    -3.450978 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 9    -3.401803 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 10   -3.644565 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 11   -3.284986 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 13   -3.470253 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 14   -3.478433 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 15   -3.343180 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 16   -3.503840 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 17   -3.308225 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 18   -3.348593 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 19   -3.427110 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 20   -3.442600 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 21   -3.518500 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 22   -3.447128 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 23   -3.438772 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 24   -3.495238 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 25   -3.319628 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 26   -3.369433 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 27   -3.435613 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 28   -3.491081 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 29   -3.387870 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 30   -3.401439 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 31   -3.348673 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 32   -3.454866 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 33   -3.500740 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 34   -3.318783 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 35   -3.424000 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 36   -3.164267 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 37   -3.439268 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 38   -3.513996 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 39   -3.367035 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 40   -3.410513 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 41   -3.326286 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 42   -3.428950 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 43   -3.164731 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 44   -3.566514 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 45   -3.309386 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 46   -3.366205 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 47   -3.370919 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 48   -3.383858 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 49   -3.283003 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 50   -3.451345 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 51   -3.377764 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 
    ## $region.full
    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.490519 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 2   -3.483010 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 3   -3.145489 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 4   -3.484981 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 5   -3.401612 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 
    ## $edu
    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.533139 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 2   -3.427924 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 3   -3.248292 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 4   -3.394010 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 
    ## $age
    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.315134 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 2   -3.480506 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 3   -3.356298 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 4   -3.450316 -1.637079 -0.08792266     6.85051   -0.1752508
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

``` r
se.coef(M2)
```

    ## $fixef
    ## [1] 1.04195517 0.32609125 0.09805142 1.86449512 0.41979117
    ## 
    ## $state
    ##    (Intercept)
    ## 1    0.1784206
    ## 3    0.1772359
    ## 4    0.1887592
    ## 5    0.1403145
    ## 6    0.1806381
    ## 7    0.1794610
    ## 8    0.1929261
    ## 9    0.1986766
    ## 10   0.1465426
    ## 11   0.1685012
    ## 13   0.1956119
    ## 14   0.1545136
    ## 15   0.1750898
    ## 16   0.1824727
    ## 17   0.1795599
    ## 18   0.1766045
    ## 19   0.1777739
    ## 20   0.1876688
    ## 21   0.1788560
    ## 22   0.1666571
    ## 23   0.1554188
    ## 24   0.1700997
    ## 25   0.1802451
    ## 26   0.1733199
    ## 27   0.1943121
    ## 28   0.1871017
    ## 29   0.1955071
    ## 30   0.1963316
    ## 31   0.1608844
    ## 32   0.1809319
    ## 33   0.1401486
    ## 34   0.1716267
    ## 35   0.1930236
    ## 36   0.1509137
    ## 37   0.1862658
    ## 38   0.1841135
    ## 39   0.1516653
    ## 40   0.1885403
    ## 41   0.1786977
    ## 42   0.1935951
    ## 43   0.1712255
    ## 44   0.1455930
    ## 45   0.1922347
    ## 46   0.1969455
    ## 47   0.1679746
    ## 48   0.1663333
    ## 49   0.1778035
    ## 50   0.1647873
    ## 51   0.1972654
    ## 
    ## $region.full
    ##   (Intercept)
    ## 1   0.1124333
    ## 2   0.1094866
    ## 3   0.1062689
    ## 4   0.1217336
    ## 5   0.1863061
    ## 
    ## $edu
    ##   (Intercept)
    ## 1  0.10495003
    ## 2  0.08459034
    ## 3  0.09214942
    ## 4  0.08947498
    ## 
    ## $age
    ##   (Intercept)
    ## 1  0.07765813
    ## 2  0.07321753
    ## 3  0.07759651
    ## 4  0.08348315

## 2.1 Predictions

``` r
# create different profiles
polls.X <- expand.grid(state = 1:51, edu = 1:4, age = 1:4, female = 0:1,
    black = 0:1, region = 1:5)
polls.X[1, ]
```

    ##   state edu age female black region
    ## 1     1   1   1      0     0      1

``` r
# Profile, state 1, edu 1, age 1, female=0, black=0
# Estimates for state 1
coef(M2)$state[1, ]
```

    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.249307 -1.637079 -0.08792266     6.85051   -0.1752508

``` r
coef(M2)$edu[1, ]
```

    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.533139 -1.637079 -0.08792266     6.85051   -0.1752508

``` r
coef(M2)$age[1, ]
```

    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.315134 -1.637079 -0.08792266     6.85051   -0.1752508

``` r
coef(M2)$region[1, ]
```

    ##   (Intercept)     black      female v.prev.full black:female
    ## 1   -3.490519 -1.637079 -0.08792266     6.85051   -0.1752508

``` r
# ranef(M1)$age.edu[1,]

# states 2 and 12 were dropped (?)
v.prev.pred <- v.prev[-2]
v.prev.pred <- v.prev.pred[-12]

# estimating alpha hat for state 1
a.hat.state.1 <- v.prev.pred[1] * coef(M2)$state$v.prev.full[1]
+unlist(ranef(M2)$state)[1]
```

    ## (Intercept)1 
    ##     0.150909

``` r
+unlist(ranef(M2)$region)[3]  #state 1 is in region 3; attention to detail is important and knowing your data!
```

    ## (Intercept)3 
    ##    0.2547268

``` r
a.hat.state.1
```

    ## [1] 3.904791

``` r
# line predictor for demographics

linpred <- fixef(M2)["(Intercept)"]  #b0
+polls.X$female[1] * coef(M2)$state[1, ]$female  #female * b.female
```

    ## [1] 0

``` r
+polls.X$black[1] * coef(M2)$state[1, ]$black  # black * b.black
```

    ## [1] 0

``` r
+polls.X$female[1] * polls.X$black[1] * coef(M2)$state[1, 5]  #inter
```

    ## [1] 0

``` r
+ranef(M2)$age[1, 1]  #alpha.age[1]
```

    ## [1] 0.0850825

``` r
+ranef(M2)$edu[1, 1]  # alpha.edu[1]
```

    ## [1] -0.1329231

``` r
# + ranef(M2)$age.edu[1,1] # alpha.age.edu[1]

# predicted probability of voting for Bush for someone of
# that particular profile in state1
y1.pred <- plogis(linpred + a.hat.state.1)
y1.pred
```

    ## (Intercept) 
    ##   0.6235338

``` r
round(y1.pred, 2)
```

    ## (Intercept) 
    ##        0.62

The probability that an individual that is male, white, young, with low
lever of education in state 1 (which is in region 3) votes for Bush is
0.63.

## 2.2 Graphing the estimated model

Gelman & Hill are doing a Bayesian Model, page 305. We cannot use their
code yet.

# 3 Estimating State Public Opinion With Multi-Level Regression and Poststratification using R, by Kastellec, Lax, and Phillips

Multilevel regression with poststratification (MRP) was first proposed
by Gelman and Little (1997). The aim fo this methodology is to provide
reliable estimates on strata based on census counts.

To use MRP you need to have a reasonable model for predicting public
opinion, preferrably cemented in literature. It is also recommended to
use group level variables (e.g. state level variable like previous vote
share). A lot of benchmarking and diagnostics are necessary.

Source:
<https://scholar.princeton.edu/jkastellec/publications/mrp_primer>

Here, they model individual level opinion on gay marriage rights,

$$\begin{align*}
p(y_i = 1)  & = \text{logit}^{-1}\left(\beta^{0}  
                      + \alpha_{j[i]}^{\text{race, gender}}
                      + \alpha_{k[i]}^{\text{age}}
                      + \alpha_{\ell[i]}^{\text{edu}}
                      + \alpha_{k[i],\ell[i]}^{\text{age.edu}}
                      + \alpha_{s[i]}^{\text{state}}
                      + \alpha_{p[i]}^{\text{year}}\right) \\
\end{align*}$$ The varying intercepts are,

\$\$\``{=tex}$$ The state effects are in turn modeled as a function of the region into which the state falls and the state's conservative religious percentage and Democratic 2004 presidential vote share. $$`\`{=tex}
\$\$

``` r
# clean
rm(list = ls(all = TRUE))
library("arm")
library("foreign")

# read in megapoll and attach
marriage.data <- read.dta("https://www.dropbox.com/s/x50wgfz50kfqtb9/gay_marriage_megapoll.dta?dl=1",
    convert.underscore = TRUE)

# read in state-level dataset
Statelevel <- read.dta("https://www.dropbox.com/s/8kgonlhprh4cp3c/state_level_update.dta?dl=1",
    convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum), ]

# read in Census data
Census <- read.dta("https://www.dropbox.com/s/4lig6cqjbtb0v0w/poststratification%202000.dta?dl=1",
    convert.underscore = TRUE)
Census <- Census[order(Census$cstate), ]
Census$cstate.initnum <- match(Census$cstate, Statelevel$sstate)

# Create index variables At level of megapoll

marriage.data$race.female <- (marriage.data$female * 3) + marriage.data$race.wbh  # from 1 for white males to 6 for hispanic females
marriage.data$age.edu.cat <- 4 * (marriage.data$age.cat - 1) +
    marriage.data$edu.cat  # from 1 for 18-29 with low edu to 16 for 65+ with high edu
marriage.data$p.evang.full <- Statelevel$p.evang[marriage.data$state.initnum]  # proportion of evangelicals in respondent's state
marriage.data$p.mormon.full <- Statelevel$p.mormon[marriage.data$state.initnum]  # proportion of mormon's in respondent's state
marriage.data$p.relig.full <- marriage.data$p.evang.full + marriage.data$p.mormon.full  # combined evangelical + mormom proportions
marriage.data$p.kerry.full <- Statelevel$kerry.04[marriage.data$state.initnum]  # kerry's % of 2-party vote in respondent's state in 2004

## At census level (same coding as above for all variables)
Census$crace.female <- (Census$cfemale * 3) + Census$crace.WBH
Census$cage.edu.cat <- 4 * (Census$cage.cat - 1) + Census$cedu.cat
Census$cp.evang.full <- Statelevel$p.evang[Census$cstate.initnum]
Census$cp.mormon.full <- Statelevel$p.mormon[Census$cstate.initnum]
Census$cp.relig.full <- Census$cp.evang.full + Census$cp.mormon.full
Census$cp.kerry.full <- Statelevel$kerry.04[Census$cstate.initnum]

# run individual-level opinion model
individual.model <- glmer(formula = yes.of.all ~ (1 | race.female) +
    (1 | age.cat) + (1 | edu.cat) + (1 | age.edu.cat) + (1 |
    state) + (1 | region) + (1 | poll) + p.relig.full + p.kerry.full,
    data = marriage.data, family = binomial(link = "logit"))
display(individual.model)
```

    ## glmer(formula = yes.of.all ~ (1 | race.female) + (1 | age.cat) + 
    ##     (1 | edu.cat) + (1 | age.edu.cat) + (1 | state) + (1 | region) + 
    ##     (1 | poll) + p.relig.full + p.kerry.full, data = marriage.data, 
    ##     family = binomial(link = "logit"))
    ##              coef.est coef.se
    ## (Intercept)  -1.41     0.54  
    ## p.relig.full -0.02     0.00  
    ## p.kerry.full  0.02     0.01  
    ## 
    ## Error terms:
    ##  Groups      Name        Std.Dev.
    ##  state       (Intercept) 0.04    
    ##  age.edu.cat (Intercept) 0.09    
    ##  race.female (Intercept) 0.23    
    ##  poll        (Intercept) 0.21    
    ##  region      (Intercept) 0.20    
    ##  edu.cat     (Intercept) 0.36    
    ##  age.cat     (Intercept) 0.55    
    ##  Residual                1.00    
    ## ---
    ## number of obs: 6341, groups: state, 49; age.edu.cat, 16; race.female, 6; poll, 5; region, 5; edu.cat, 4; age.cat, 4
    ## AIC = 7459.4, DIC = 7276
    ## deviance = 7357.7

``` r
# examine 'random effects' and standard errors for
# race-female
ranef(individual.model)$race.female
```

    ##   (Intercept)
    ## 1 -0.21042144
    ## 2 -0.08701671
    ## 3  0.04905051
    ## 4  0.23019579
    ## 5 -0.22602201
    ## 6  0.24576737

``` r
se.ranef(individual.model)$race.female
```

    ##   (Intercept)
    ## 1   0.1083302
    ## 2   0.1455014
    ## 3   0.1473010
    ## 4   0.1080335
    ## 5   0.1362854
    ## 6   0.1485593

``` r
# create vector of state ranefs and then fill in missing
# ones
state.ranefs <- array(NA, c(51, 1))
dimnames(state.ranefs) <- list(c(Statelevel$sstate), "effect")
for (i in Statelevel$sstate) {
    state.ranefs[i, 1] <- ranef(individual.model)$state[i, 1]
}
state.ranefs[, 1][is.na(state.ranefs[, 1])] <- 0  #set states with missing REs (b/c not in data) to zero (Hawaii and Alaska)

# create a prediction for each cell in Census data
cellpred <- invlogit(fixef(individual.model)["(Intercept)"] +
    ranef(individual.model)$race.female[Census$crace.female,
        1] + ranef(individual.model)$age.cat[Census$cage.cat,
    1] + ranef(individual.model)$edu.cat[Census$cedu.cat, 1] +
    ranef(individual.model)$age.edu.cat[Census$cage.edu.cat,
        1] + state.ranefs[Census$cstate, 1] + ranef(individual.model)$region[Census$cregion,
    1] + (fixef(individual.model)["p.relig.full"] * Census$cp.relig.full) +
    (fixef(individual.model)["p.kerry.full"] * Census$cp.kerry.full))

# weights the prediction by the freq of cell
cellpredweighted <- cellpred * Census$cpercent.state

# calculates the percent within each state (weighted
# average of responses)
statepred <- 100 * as.vector(tapply(cellpredweighted, Census$cstate,
    sum))
```

Let’s create a map to illustrate the results.

``` r
# library(ggplot2) library(fiftystater)

# data('fifty_states') # this line is optional due to lazy
# data loading

# states tolower(names(state.ranefs[,1])) another data to
# get names of states crimes <- data.frame(state =
# tolower(rownames(USArrests)), USArrests)

# create data vote.prediction<-cbind(statepred,
# tolower(names(state.ranefs[,1])))
# colnames(vote.prediction)<-c('vote', 'initial') save as
# dataframe for ggplot2 & erase DC because it is not a
# state vote.prediction<-as.data.frame(vote.prediction[-8,
# ]) vote.prediction$state<- crimes$state
# dim(vote.prediction) head(vote.prediction)
# colnames(vote.prediction)[3]<-'state'

# map p <- ggplot(vote.prediction, aes(map_id = state)) +
# map points to the fifty_states shape data
# geom_map(aes(fill = vote), map = fifty_states) +
# expand_limits(x = fifty_states$long, y =
# fifty_states$lat) + coord_map() +
# scale_x_continuous(breaks = NULL) +
# scale_y_continuous(breaks = NULL) + labs(x = '', y = '')
# + theme(legend.position = 'bottom', panel.background =
# element_blank())

# p add border boxes to AK/HI p +
# fifty_states_inset_boxes()
```

In a few weeks, we have a day dedicated to survey data. We’ll read
papers using MRP in their research.

# 4 Multilevel Poisson Model

``` r
# non-nested groups
m <- glmer(stops ~ arrests.offset + (1 | eth) + (1 | precint),
    family = poisson)
display(m)
```

    ## glmer(formula = stops ~ arrests.offset + (1 | eth) + (1 | precint), 
    ##     family = poisson)
    ##                coef.est coef.se
    ## (Intercept)    -1.52     0.14  
    ## arrests.offset  1.10     0.01  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  precint  (Intercept) 0.66    
    ##  eth      (Intercept) 0.19    
    ##  Residual             1.00    
    ## ---
    ## number of obs: 225, groups: precint, 75; eth, 3
    ## AIC = 5396.1, DIC = 820
    ## deviance = 3104.1

``` r
coef(m)
```

    ## $precint
    ##    (Intercept) arrests.offset
    ## 1   -2.3963634       1.100935
    ## 2   -2.5411430       1.100935
    ## 3   -1.9301011       1.100935
    ## 4   -1.2031117       1.100935
    ## 5   -2.2295754       1.100935
    ## 6   -1.2576350       1.100935
    ## 7   -2.2063730       1.100935
    ## 8   -2.9680790       1.100935
    ## 9   -1.8293584       1.100935
    ## 10  -2.0455280       1.100935
    ## 11  -1.7565189       1.100935
    ## 12  -1.2154889       1.100935
    ## 13  -1.4191273       1.100935
    ## 14  -1.8331650       1.100935
    ## 15  -1.4236278       1.100935
    ## 16  -1.6254843       1.100935
    ## 17  -2.5779788       1.100935
    ## 18  -2.4407706       1.100935
    ## 19  -2.2139384       1.100935
    ## 20  -2.5862961       1.100935
    ## 21  -2.1081992       1.100935
    ## 22  -1.3124034       1.100935
    ## 23  -1.9266855       1.100935
    ## 24  -1.1466357       1.100935
    ## 25  -1.6253534       1.100935
    ## 26  -2.7069876       1.100935
    ## 27  -0.4779433       1.100935
    ## 28  -3.3302028       1.100935
    ## 29  -1.3823226       1.100935
    ## 30  -2.0012572       1.100935
    ## 31  -0.7385802       1.100935
    ## 32  -0.9732900       1.100935
    ## 33  -1.4161040       1.100935
    ## 34  -0.9242962       1.100935
    ## 35  -1.5400524       1.100935
    ## 36  -0.7897186       1.100935
    ## 37  -0.9596842       1.100935
    ## 38  -0.6268401       1.100935
    ## 39  -2.0854070       1.100935
    ## 40  -0.8447489       1.100935
    ## 41  -0.4858302       1.100935
    ## 42  -1.4141067       1.100935
    ## 43  -1.9885216       1.100935
    ## 44  -1.6167357       1.100935
    ## 45  -1.7191471       1.100935
    ## 46  -1.9005031       1.100935
    ## 47  -1.2515586       1.100935
    ## 48  -2.1381622       1.100935
    ## 49  -1.2876018       1.100935
    ## 50  -1.6744811       1.100935
    ## 51  -2.1624744       1.100935
    ## 52  -2.0349065       1.100935
    ## 53  -1.8656300       1.100935
    ## 54  -1.9824286       1.100935
    ## 55  -1.9768432       1.100935
    ## 56  -1.4740728       1.100935
    ## 57  -0.7981561       1.100935
    ## 58  -0.8764428       1.100935
    ## 59  -1.3133549       1.100935
    ## 60  -1.7099966       1.100935
    ## 61  -1.1562334       1.100935
    ## 62  -1.2470316       1.100935
    ## 63  -1.0744301       1.100935
    ## 64  -0.6424664       1.100935
    ## 65  -0.5021693       1.100935
    ## 66  -0.2918372       1.100935
    ## 67  -1.2858511       1.100935
    ## 68  -0.1172217       1.100935
    ## 69  -0.6311748       1.100935
    ## 70  -1.4894859       1.100935
    ## 71  -0.9744480       1.100935
    ## 72  -1.0285483       1.100935
    ## 73  -1.5387342       1.100935
    ## 74  -1.3043648       1.100935
    ## 75  -0.7552087       1.100935
    ## 
    ## $eth
    ##   (Intercept) arrests.offset
    ## 1   -1.421337       1.100935
    ## 2   -1.379490       1.100935
    ## 3   -1.771814       1.100935
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

``` r
# one hierarchy
m.1 <- glmer(stops ~ arrests.offset + as.factor(eth) + (1 | precint),
    family = poisson)
display(m.1)
```

    ## glmer(formula = stops ~ arrests.offset + as.factor(eth) + (1 | 
    ##     precint), family = poisson)
    ##                 coef.est coef.se
    ## (Intercept)     -1.42     0.09  
    ## arrests.offset   1.10     0.01  
    ## as.factor(eth)2  0.04     0.01  
    ## as.factor(eth)3 -0.35     0.01  
    ## 
    ## Error terms:
    ##  Groups   Name        Std.Dev.
    ##  precint  (Intercept) 0.65    
    ##  Residual             1.00    
    ## ---
    ## number of obs: 225, groups: precint, 75
    ## AIC = 5380.5, DIC = 837.6
    ## deviance = 3104.1

``` r
coef(m.1)
```

    ## $precint
    ##    (Intercept) arrests.offset as.factor(eth)2 as.factor(eth)3
    ## 1  -2.29211172       1.100795      0.04184402      -0.3511196
    ## 2  -2.43688036       1.100795      0.04184402      -0.3511196
    ## 3  -1.82572968       1.100795      0.04184402      -0.3511196
    ## 4  -1.09890370       1.100795      0.04184402      -0.3511196
    ## 5  -2.12517010       1.100795      0.04184402      -0.3511196
    ## 6  -1.15344693       1.100795      0.04184402      -0.3511196
    ## 7  -2.10208236       1.100795      0.04184402      -0.3511196
    ## 8  -2.86364468       1.100795      0.04184402      -0.3511196
    ## 9  -1.72521188       1.100795      0.04184402      -0.3511196
    ## 10 -1.94121527       1.100795      0.04184402      -0.3511196
    ## 11 -1.65222855       1.100795      0.04184402      -0.3511196
    ## 12 -1.11133689       1.100795      0.04184402      -0.3511196
    ## 13 -1.31490221       1.100795      0.04184402      -0.3511196
    ## 14 -1.72895278       1.100795      0.04184402      -0.3511196
    ## 15 -1.31938698       1.100795      0.04184402      -0.3511196
    ## 16 -1.52131716       1.100795      0.04184402      -0.3511196
    ## 17 -2.47364534       1.100795      0.04184402      -0.3511196
    ## 18 -2.33639650       1.100795      0.04184402      -0.3511196
    ## 19 -2.10961546       1.100795      0.04184402      -0.3511196
    ## 20 -2.48187914       1.100795      0.04184402      -0.3511196
    ## 21 -2.00385720       1.100795      0.04184402      -0.3511196
    ## 22 -1.20811799       1.100795      0.04184402      -0.3511196
    ## 23 -1.82235307       1.100795      0.04184402      -0.3511196
    ## 24 -1.04243209       1.100795      0.04184402      -0.3511196
    ## 25 -1.52103726       1.100795      0.04184402      -0.3511196
    ## 26 -2.60263661       1.100795      0.04184402      -0.3511196
    ## 27 -0.37374984       1.100795      0.04184402      -0.3511196
    ## 28 -3.22581215       1.100795      0.04184402      -0.3511196
    ## 29 -1.27802434       1.100795      0.04184402      -0.3511196
    ## 30 -1.89695229       1.100795      0.04184402      -0.3511196
    ## 31 -0.63440014       1.100795      0.04184402      -0.3511196
    ## 32 -0.86918085       1.100795      0.04184402      -0.3511196
    ## 33 -1.31183082       1.100795      0.04184402      -0.3511196
    ## 34 -0.82001223       1.100795      0.04184402      -0.3511196
    ## 35 -1.43556355       1.100795      0.04184402      -0.3511196
    ## 36 -0.68525893       1.100795      0.04184402      -0.3511196
    ## 37 -0.85549958       1.100795      0.04184402      -0.3511196
    ## 38 -0.52262097       1.100795      0.04184402      -0.3511196
    ## 39 -1.98101394       1.100795      0.04184402      -0.3511196
    ## 40 -0.74036711       1.100795      0.04184402      -0.3511196
    ## 41 -0.38167676       1.100795      0.04184402      -0.3511196
    ## 42 -1.30981279       1.100795      0.04184402      -0.3511196
    ## 43 -1.88420433       1.100795      0.04184402      -0.3511196
    ## 44 -1.51243198       1.100795      0.04184402      -0.3511196
    ## 45 -1.61480274       1.100795      0.04184402      -0.3511196
    ## 46 -1.79614815       1.100795      0.04184402      -0.3511196
    ## 47 -1.14744097       1.100795      0.04184402      -0.3511196
    ## 48 -2.03382009       1.100795      0.04184402      -0.3511196
    ## 49 -1.18346303       1.100795      0.04184402      -0.3511196
    ## 50 -1.57014415       1.100795      0.04184402      -0.3511196
    ## 51 -2.05817929       1.100795      0.04184402      -0.3511196
    ## 52 -1.93058862       1.100795      0.04184402      -0.3511196
    ## 53 -1.76138106       1.100795      0.04184402      -0.3511196
    ## 54 -1.87819088       1.100795      0.04184402      -0.3511196
    ## 55 -1.87259488       1.100795      0.04184402      -0.3511196
    ## 56 -1.36985677       1.100795      0.04184402      -0.3511196
    ## 57 -0.69404294       1.100795      0.04184402      -0.3511196
    ## 58 -0.77223258       1.100795      0.04184402      -0.3511196
    ## 59 -1.20914393       1.100795      0.04184402      -0.3511196
    ## 60 -1.60566149       1.100795      0.04184402      -0.3511196
    ## 61 -1.05192856       1.100795      0.04184402      -0.3511196
    ## 62 -1.14280608       1.100795      0.04184402      -0.3511196
    ## 63 -0.97025741       1.100795      0.04184402      -0.3511196
    ## 64 -0.53834721       1.100795      0.04184402      -0.3511196
    ## 65 -0.39797694       1.100795      0.04184402      -0.3511196
    ## 66 -0.18760023       1.100795      0.04184402      -0.3511196
    ## 67 -1.18159474       1.100795      0.04184402      -0.3511196
    ## 68 -0.01294579       1.100795      0.04184402      -0.3511196
    ## 69 -0.52696040       1.100795      0.04184402      -0.3511196
    ## 70 -1.38518355       1.100795      0.04184402      -0.3511196
    ## 71 -0.87011819       1.100795      0.04184402      -0.3511196
    ## 72 -0.92428282       1.100795      0.04184402      -0.3511196
    ## 73 -1.43434375       1.100795      0.04184402      -0.3511196
    ## 74 -1.19982214       1.100795      0.04184402      -0.3511196
    ## 75 -0.65080303       1.100795      0.04184402      -0.3511196
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"
