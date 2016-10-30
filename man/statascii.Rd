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
# Setup
library(dplyr)
library(stringr)

# Demonstrate 'oneway' flavor for one-way tables of frequencies
a <- mtcars %>% count(gear) %>% rename(Freq. = n)
a <- a %>% add_row(gear = "Total", Freq. = sum(a[,2]))
statascii(a, flavor = "oneway")

# Demonstrate 'oneway' flavor with no padding
a <- mtcars %>% count(gear) %>% rename(Freq. = n)
a <- a %>% add_row(gear = "Total", Freq. = sum(a[,2]))
statascii(a, flavor = "oneway", padding = "none")

# Demonstrate 'twoway' flavor for n-way tables of frequencies
b <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
b <- b %>% ungroup() %>% add_row(gear = "Total", carb = "", am = "", Freq. = sum(b[,4]))
statascii(b, flavor = "twoway")

# Demonstrate 'summary' flavor for summary statistics
c <- mtcars %>% group_by(gear) %>% summarize(
    Obs = n(),
    Mean = mean(gear),
    "Std. Dev." = sd(gear),
    Min = min(gear),
    Max = max(gear)
)
statascii(c, flavor = "summary")

# Demonstrate wrapping feature for wide tables
d <- mtcars %>% mutate(mpg2 = mpg, cyl2 = cyl, disp2 = disp, hp2 = hp, drat2 = drat, wt2 = wt, qsec2 = qsec, vs2 = vs, am2 = am, gear2 = gear, carb2 = carb) %>% slice(1:5)
statascii(d, flavor = "oneway")

## Reference
# `statascii()` borrows heavily from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()' function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).