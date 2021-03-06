
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href={https://github.com/Ehyaei/FOCI}><img src="man/figures/FOCI.svg" alt="FOCI LOGO" align="right" width="160" style="padding: 0 15px; float: right;"/>

# FOCI

[![R build
status](https://github.com/Ehyaei/FOCI/workflows/R-CMD-check/badge.svg)](https://github.com/Ehyaei/FOCI/actions)
[![](https://img.shields.io/badge/devel%20version-0.1.4-green.svg)](https://github.com/Ehyaei/FOCI)
[![](https://www.r-pkg.org/badges/version/FOCI?color=blue)](https://cran.r-project.org/package=FOCI)

Package `FOCI`, implements the variable selection algorithm **Feature
Ordering by Conditional Independence** algorithm introduced in the paper
[*A Simple Measure Of Conditional
Dependence*](https://arxiv.org/pdf/1910.12327.pdf). It produces an
ordering of the predictors according to their predictive power. This
ordering is used for variable selection without putting any assumption
on the distribution of the data or assuming any particular underlying
model. The simplicity of the estimation of the conditional dependence
coefficient makes it an efficient method for variable ordering and
variable selection that can be used for high dimensional settings.

You can install the released version of FOCI from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("FOCI")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ehyaei/FOCI")
```

## Example 1

In the following example, *Y* is a function of the first four columns of
*X* in a complex way.

``` r
library(FOCI)
```

``` r
n = 2000
p = 100
X = matrix(rnorm(n * p), ncol = p)
colnames(X) = paste0(rep("X", p), seq(1, p))
Y = X[, 1] * X[, 2] + sin(X[, 1] * X[, 3]) + X[, 4]^2
result1 = foci(Y, X, numCores = 1)
result1
```

## Example 2

In the previous example, using the default values of input variables
*stop* and *num_feature* we let the function stop according to the foci
algorithm’s stopping rule. The user can decide to stop after picking how
many variables they want to. In this example, the user decides to stop
the process after seeing exactly 5 selected variables.

``` r
result2 = foci(Y, X, num_features = 5, stop = FALSE, numCores = 1)
```
