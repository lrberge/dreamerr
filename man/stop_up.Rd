% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stop.R
\name{stop_up}
\alias{stop_up}
\alias{stopi}
\alias{warni}
\alias{warn_up}
\title{Stops (or warns in) sub-function execution}
\usage{
stop_up(..., up = 1, msg = NULL, envir = parent.frame(), verbatim = FALSE)

stopi(..., envir = parent.frame())

warni(..., envir = parent.frame(), immediate. = FALSE)

warn_up(
  ...,
  up = 1,
  immediate. = FALSE,
  envir = parent.frame(),
  verbatim = FALSE
)
}
\arguments{
\item{...}{Objects that will be coerced to character and will compose the error message.}

\item{up}{The number of frames up, default is 1. The call in the error message will be based on the function \code{up} frames up the stack. See examples. If you have many calls to \code{stop_up}/\code{warn_up} with a value of \code{up} different than one, you can use \code{\link[dreamerr]{set_up}} to change the default value of \code{up} within the function.}

\item{msg}{A character vector, default is \code{NULL}. If provided, this message will be displayed right under the error message. This is mostly useful when the text contains formatting because the function \code{\link{stop}} used to send the error message erases any formatting.}

\item{envir}{An environment, default is \code{parent.frame()}. Only relevant if the error/warning message contains
interpolation (interpolation is performed with \href{https://github.com/lrberge/stringmagic}{stringmagic}). It tells
where the variables to be interpolated should be found. In general you should not worry about this argument.}

\item{verbatim}{Logical scalar, default is \code{FALSE}. By default the error/warning message allows variable
interpolation with \href{https://github.com/lrberge/stringmagic}{stringmagic}. To disable interpolation,
use \code{verbatim = TRUE}.}

\item{immediate.}{Whether the warning message should be prompted directly. Defaults to \code{FALSE}.}
}
\description{
Useful if you employ non-user level sub-functions within user-level functions or
if you want string interpolation in error messages. When an error is thrown in the sub
function, the error message will integrate the call of the user-level function, which
is more informative and appropriate for the user. It offers a similar functionality for \code{warning}.
}
\details{
These functions are really made for package developers to facilitate the good practice of providing informative user-level error/warning messages.

The error/warning messages allow variable interpolation by making use of \href{https://github.com/lrberge/stringmagic}{stringmagic}'s interpolation.
}
\section{Functions}{
\itemize{
\item \code{stopi()}: Error messages with string interpolation

\item \code{warni()}: Warnings with string interpolation

\item \code{warn_up()}: Warnings at the level of user-level functions

}}
\examples{

# We create a main user-level function
# The computation is done by an internal function
# Here we compare stop_up with a regular stop

main_function = function(x = 1, y = 2){
  my_internal_function(x, y)
}

my_internal_function = function(x, y){
  if(!is.numeric(x)){
    stop_up("Argument 'x' must be numeric but currently isn't.")
  }

  # Now regular stop
  if(!is.numeric(y)){
    stop("Argument 'y' must be numeric but currently isn't.")
  }

  nx = length(x)
  ny = length(y)
  if(nx != ny){
    # Note that we use string interpolation with {}
    warn_up("The lengths of x and y don't match: {nx} vs {ny}.")
  }

  x + y
}

# Let's compare the two error messages
# stop_up:
try(main_function(x = "a"))
# => the user understands that the problem is with x

# Now compare with the regular stop:
try(main_function(y = "a"))
# Since the user has no clue of what my_internal_function is,
#  s/he will be puzzled of what to do to sort this out

# Same with the warning => much clearer with warn_up
main_function(1, 1:2)


}
\seealso{
For general argument checking, see \code{\link[=check_arg]{check_arg()}} and \code{\link[=check_set_arg]{check_set_arg()}}.
}
\author{
Laurent Berge
}
