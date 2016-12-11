\name{statascii}
\alias{statascii}
\title{Create Stata-like ASCII tables in R}
\usage{
statascii(df, ..., flavor = "oneway", padding = "stata", separators = FALSE)
}
\description{
Create Stata-like ASCII tables in the R console.
}
\examples{
# Setup
library(dplyr)
library(stringr)

# Demonstrate 'oneway' flavor for one-way tables of frequencies
a <- mtcars \%>\% count(gear) \%>\% rename(Freq. = n)
a <- a \%>\% add_row(gear = "Total", Freq. = sum(a[,2]))
statascii(a, flavor = "oneway")

# Demonstrate 'oneway' flavor with no padding
a <- mtcars \%>\% count(gear) \%>\% rename(Freq. = n)
a <- a \%>\% add_row(gear = "Total", Freq. = sum(a[,2]))
statascii(a, flavor = "oneway", padding = "none")

# Demonstrate 'twoway' flavor for n-way tables of frequencies
b <- mtcars \%>\% count(gear, carb, am) \%>\% rename(Freq. = n)
b <- b \%>\% ungroup() \%>\% add_row(gear = "Total", carb = "", am = "", Freq. = sum(b[,4]))
statascii(b, flavor = "twoway")

# Demonstrate 'twoway' flavor with dashed group separators
b <- mtcars \%>\% count(gear, carb, am) \%>\% rename(Freq. = n)
b <- b \%>\% ungroup() \%>\% add_row(gear = "Total", carb = "", am = "", Freq. = sum(b[,4]))
statascii(b, flavor = "twoway", separators = TRUE)

# Demonstrate 'summary' flavor for summary statistics
c <- mtcars \%>\% group_by(gear) \%>\% summarize(
    Obs = n(),
    Mean = mean(gear),
    "Std. Dev." = sd(gear),
    Min = min(gear),
    Max = max(gear)
)
statascii(c, flavor = "summary")

# Demonstrate wrapping feature for wide tables
d <- mtcars \%>\% mutate(cyl2 = cyl, vs2 = vs, am2 = am, carb2 = carb) \%>\%
filter(gear != 5) \%>\% count(gear, carb, am, vs, cyl, carb2, am2, vs2, cyl2) \%>\% rename(Freq. = n) \%>\% ungroup()
d <- d \%>\% add_row(gear = "Total", Freq. = sum(d[,10]))
d[is.na(d)] <- ""
statascii(d, flavor = "oneway", separators = TRUE)

## Reference
# `statascii()` borrows heavily from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()' function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).
}