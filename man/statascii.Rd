\name{statascii}
\alias{statascii}
\title{Create Stata-like ASCII tables in R}
\usage{
statascii()
}
\description{
Create Stata-like ASCII tables in the R console.
}
\examples{
statascii()
}

## Examples
# setup
library(dplyr)
# 'oneway' flavor for one-way tables of frequencies
a <- mtcars %>% count(gear) %>% rename(Freq. = n)
statascii(a, flavor = "oneway")
# 'oneway' flavor with no Stata-like padding
a <- mtcars %>% count(gear) %>% rename(Freq. = n)
statascii(a, flavor = "oneway", padding = "none")
# 'twoway' flavor for n-way tables of frequencies
b <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
statascii(b, flavor = "twoway")
# 'summary' flavor for summary statistics
c <- mtcars %>% group_by(gear) %>% summarize(
    Obs = n(),
    Mean = mean(gear),
    "Std. Dev." = sd(gear),
    Min = min(gear),
    Max = max(gear)
)
statascii(c, flavor = "summary")

## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()' function was initially written by @gvelasq2 in Github Gist (https://gist.github.com/gvelasq2).
