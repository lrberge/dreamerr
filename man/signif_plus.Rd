% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_funs.R
\name{signif_plus}
\alias{signif_plus}
\title{Formatting for numbers to appear in-text}
\usage{
signif_plus(x, s = 2, r = 0, commas = TRUE)
}
\arguments{
\item{x}{A numeric vector.}

\item{s}{The number of significant digits to be displayed. Defaults to 2. All digits not in the decimal are always shown.}

\item{r}{For large values, the number of digits after the decimals to be displayed (beyond the number of significant digits). Defaults to 0. It is useful to suggest that a number is not an integer.}

\item{commas}{Whether or not to add commas to separate thousands. Defaults to \code{TRUE}.}
}
\value{
It returns a character vector of the same length as the input.
}
\description{
Formatting of numbers, when they are to appear in messages. Displays only significant digits in a "nice way" and adds commas to separate thousands.
}
\examples{

x = rnorm(1e5)
x[sample(1e5, 1e4, TRUE)] = NA

# Dumb function telling the number of NA values
tell_na = function(x) message("x contains ", signif_plus(sum(is.na(x))), " NA values.")

tell_na(x)

# Some differences with signif:
show_diff = function(x, d = 2) cat("signif(x, ", d, ") -> ", signif(x, d),
                                   " vs signif_plus(x, ", d, ") -> ",
                                   signif_plus(x, d), "\n", sep = "")

# Main difference is for large numbers
show_diff(95123.125)
show_diff(95123.125, 7)

# Identical for small numbers
show_diff(pi / 500)


}
