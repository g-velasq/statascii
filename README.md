
<!-- README.md is generated from README.Rmd. Please edit that file -->
statascii
=========

Create Stata-like tables in the R console

[![CRAN status](http://www.r-pkg.org/badges/version/statascii)](https://cran.r-project.org/package=statascii) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Installation
------------

You can install statascii from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("gvelasq2/statascii")
```

Usage
-----

``` r
# setup
library(dplyr)
library(stringr)
library(statascii)

# a. demonstrate 'oneway' flavor for one-way tables of frequencies
a <- mtcars %>% count(gear) %>% rename(Freq. = n)
a <- a %>% add_row(gear = "Total", Freq. = sum(a[, 2]))
statascii(a, flavor = "oneway")
#>        gear │     Freq. 
#> ────────────┼───────────
#>           3 │        15 
#>           4 │        12 
#>           5 │         5 
#> ────────────┼───────────
#>       Total │        32

# b. demonstrate 'oneway' flavor with no padding
b <- mtcars %>% count(gear) %>% rename(Freq. = n)
b <- b %>% add_row(gear = "Total", Freq. = sum(b[, 2]))
statascii(b, flavor = "oneway", padding = "none")
#>    gear │ Freq. 
#> ────────┼───────
#>       3 │    15 
#>       4 │    12 
#>       5 │     5 
#> ────────┼───────
#>   Total │    32

# c. demonstrate 'twoway' flavor for n-way tables of frequencies
c <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
c <- c %>% ungroup() %>% add_row(gear = "Total", carb = "", am = "", Freq. = sum(c[, 4]))
statascii(c, flavor = "twoway")
#>       gear │      carb         am │     Freq. 
#> ───────────┼──────────────────────┼──────────
#>          3 │         1          0 │         3 
#>          3 │         2          0 │         4 
#>          3 │         3          0 │         3 
#>          3 │         4          0 │         5 
#>          4 │         1          1 │         4 
#>          4 │         2          0 │         2 
#>          4 │         2          1 │         2 
#>          4 │         4          0 │         2 
#>          4 │         4          1 │         2 
#>          5 │         2          1 │         2 
#>          5 │         4          1 │         1 
#>          5 │         6          1 │         1 
#>          5 │         8          1 │         1 
#> ───────────┼──────────────────────┼──────────
#>      Total │                      │        32

# d. demonstrate 'twoway' flavor with dashed group separators
d <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
d <- d %>% ungroup() %>% add_row(gear = "Total", carb = "", am = "", Freq. = sum(d[, 4]))
statascii(d, flavor = "twoway", separators = TRUE)
#>       gear │      carb         am │     Freq. 
#> ───────────┼──────────────────────┼──────────
#>          3 │         1          0 │         3 
#>          3 │         2          0 │         4 
#>          3 │         3          0 │         3 
#>          3 │         4          0 │         5 
#> -----------┼----------------------┼----------
#>          4 │         1          1 │         4 
#>          4 │         2          0 │         2 
#>          4 │         2          1 │         2 
#>          4 │         4          0 │         2 
#>          4 │         4          1 │         2 
#> -----------┼----------------------┼----------
#>          5 │         2          1 │         2 
#>          5 │         4          1 │         1 
#>          5 │         6          1 │         1 
#>          5 │         8          1 │         1 
#> ───────────┼──────────────────────┼──────────
#>      Total │                      │        32

# e. demonstrate 'summary' flavor for summary statistics
e <- mtcars %>% group_by(gear) %>% summarize(
  Obs = n(),
  Mean = mean(gear),
  "Std. Dev." = sd(gear),
  Min = min(gear),
  Max = max(gear)
)
statascii(e, flavor = "summary")
#>        gear │       Obs        Mean   Std. Dev.         Min         Max 
#> ────────────┼───────────────────────────────────────────────────────────
#>           3 │        15           3           0           3           3 
#>           4 │        12           4           0           4           4 
#>           5 │         5           5           0           5           5

# f. demonstrate wrapping feature for wide tables
f <- mtcars %>%
  mutate(cyl2 = cyl, vs2 = vs, am2 = am, carb2 = carb) %>%
  filter(gear != 5) %>%
  count(gear, carb, am, vs, cyl, carb2, am2, vs2, cyl2) %>%
  rename(Freq. = n) %>%
  ungroup()
f <- f %>% add_row(gear = "Total", Freq. = sum(f[, 10]))
f[is.na(f)] <- ""
statascii(f, flavor = "oneway", separators = TRUE)
#>        gear │      carb          am          vs         cyl       carb2 
#> ────────────┼───────────────────────────────────────────────────────────
#>           3 │         1           0           1           4           1 
#>           3 │         1           0           1           6           1 
#>           3 │         2           0           0           8           2 
#>           3 │         3           0           0           8           3 
#>           3 │         4           0           0           8           4 
#> ------------┼-----------------------------------------------------------
#>           4 │         1           1           1           4           1 
#>           4 │         2           0           1           4           2 
#>           4 │         2           1           1           4           2 
#>           4 │         4           0           1           6           4 
#>           4 │         4           1           0           6           4 
#> ────────────┼───────────────────────────────────────────────────────────
#>       Total │                                                           
#> 
#>        gear │        am2         vs2        cyl2       Freq. 
#> ────────────┼────────────────────────────────────────────────
#>           3 │          0           1           4           1 
#>           3 │          0           1           6           2 
#>           3 │          0           0           8           4 
#>           3 │          0           0           8           3 
#>           3 │          0           0           8           5 
#> ------------┼------------------------------------------------
#>           4 │          1           1           4           4 
#>           4 │          0           1           4           2 
#>           4 │          1           1           4           2 
#>           4 │          0           1           6           2 
#>           4 │          1           0           6           2 
#> ────────────┼────────────────────────────────────────────────
#>       Total │                                             27
```

Reference
---------

`statascii()` borrows heavily from `asciify()`.

`asciify()` was written by @gavinsimpson in [StackOverflow](https://stackoverflow.com/questions/13011383) and [GitHub Gist](https://gist.github.com/gavinsimpson/2b49f3026b50eeba29314398e27a6770).

`statascii()` was written by @gvelasq2 in [Github Gist](https://gist.github.com/gvelasq2/a39348f59f4353a9478704a28f86ed69) and now has its own package [here](https://github.com/gvelasq2/statascii).

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
