#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-07-26
# ~: stop & co
#------------------------------------------------------------------------------#

####
#### set functions ####
####

#' Sets dreamerr argument checking functions on or off
#'
#' This function allows to disable, or re-enable, all calls to \code{\link[dreamerr]{check_arg}} within any function. Useful only when running (very) large loops (>100K iter.) over small functions that use dreamerr's \code{\link[dreamerr]{check_arg}}.
#'
#' @param check Strict logical: either \code{TRUE} of \code{FALSE}. Default is \code{TRUE}.
#'
#' @author
#' Laurent Berge
#'
#'
#' @examples
#'
#' # Let's create a small function that returns the argument
#' #  if it is a single character string, and throws an error
#' #  otherwise:
#'
#' test = function(x){
#'   check_arg(x, "scalar character")
#'   x
#' }
#'
#' # works:
#' test("hey")
#' # error:
#' try(test(55))
#'
#' # Now we disable argument checking
#' setDreamerr_check(FALSE)
#' # works (although it shouldn't!):
#' test(55)
#'
#' # re-setting argument checking on:
#' setDreamerr_check(TRUE)
#'
#'
setDreamerr_check = function(check = TRUE){
  options("dreamerr_check" = TRUE)
  check_arg(check, "scalar Logical")
  options("dreamerr_check" = check)
}


#' Sets the developer mode to help form check_arg calls
#'
#' Turns on/off a full fledged checking of calls to \code{\link[dreamerr]{check_arg}}. If on, it enables the developer mode which checks extensively calls to check_arg, allowing to find any problem. If a problem is found, it is pinpointed and the associated help is referred to.
#'
#' @param dev.mode A logical, default is \code{FALSE}.
#'
#' @details
#' Since this mode ensures a detailed cheking of all \code{\link[dreamerr]{check_arg}} calls, it is thus a strain on performance and should be always turned off otherwise needed.
#'
#' @author
#' Laurent Berge
#'
#' @seealso
#' \code{\link[dreamerr]{check_arg}}
#'
#' @examples
#'
#' # If you're new to check_arg, given the many types available,
#' # it's very common to make mistakes when creating check_arg calls.
#' # The developer mode ensures that any problematic call is spotted
#' # and the problem is clearly stated
#' #
#' # Note that since this mode ensures a detailed cheking of the call
#' # it is thus a strain on performance and should be always turned off
#' # otherwise needed.
#' #
#'
#' # Setting the developer mode on:
#' setDreamerr_dev.mode(TRUE)
#'
#' # Creating some 'wrong' calls => the problem is pinpointed
#'
#' test = function(x) check_arg(x, "integer scalar", "numeric vector")
#' try(test())
#'
#' test = function(...) check_arg("numeric vector", ...)
#' try(test())
#'
#' test = function(x) check_arg(x$a, "numeric vector")
#' try(test())
#'
#' test = function(x) check_arg(x, "numeric vector integer")
#' try(test())
#'
#' test = function(x) check_arg(x, "vector len(,)")
#' try(test())
#'
#' # etc...
#'
#' # Setting the developer mode off:
#' setDreamerr_dev.mode(FALSE)
#'
#'
setDreamerr_dev.mode = function(dev.mode = FALSE){
  if(!is.logical(dev.mode) || !length(dev.mode) == 1 || is.na(dev.mode)){
    stop("Argument 'dev.mode' must be a logical scalar.")
  }

  options("dreamerr_dev.mode" = dev.mode)
}



#' Sets argument checking on/off "semi-globally"
#'
#' You can allow your users to turn off argument checking within your function by using \code{set_check}. Only the functions \code{\link[dreamerr]{check_arg}} nd \code{\link[dreamerr:check_arg]{check_value}} can be turned off that way.
#'
#' @param x A logical scalar, no default.
#'
#' @details
#' This function can be useful if you develop a function that may be used in large range loops (>100K). In such situations, it may be good to still check all arguments, but to offer the user to turn this checking off with an extra argument (named \code{arg.check} for instance). Doing so you would achieve the feat of i) having a user-friendly function thanks to argument checking and, ii) still achieve high performance in large loops (although the computational footprint of argument checking is quite low [around 30 micro seconds for missing arguments to 80 micro seconds for non-missing arguments of simple type]).
#'
#' @examples
#'
#' # Let's give an example
#' test_check = function(x, y, arg.check = TRUE){
#'   set_check(arg.check)
#'   check_arg(x, y, "numeric scalar")
#'   x + y
#' }
#'
#' # Works: argument checking on
#' test_check(1, 2)
#'
#' # If mistake, nice error msg
#' try(test_check(1, "a"))
#'
#' # Now argument checking turned off
#' test_check(1, 2, FALSE)
#' # But if mistake: "not nice" error message
#' try(test_check(1, "a", FALSE))
#'
#'
#'
set_check = function(x){
  if(isFALSE(x)){
    assign("DREAMERR_CHECK", FALSE, parent.frame())
  }

}


#' Sets "semi-globally" the 'up' argument of dreamerr's functions
#'
#' When \code{\link[dreamerr]{check_arg}} (or \code{\link[dreamerr]{stop_up}}) is used in non user-level functions, the argument \code{.up} is used to provide an appropriate error message referencing the right function.
#'
#' To avoid repeating the argument \code{.up} in each \code{check_arg} call, you can set it (kind of) "globally" with \code{set_up}.
#'
#' @param .up An integer greater or equal to 0.
#'
#' @details
#' The function \code{set_up} does not set the argument \code{up} globally, but only for all calls to \code{check_arg} and \code{check_value} within the same function.
#'
#'
#' @examples
#'
#' # Example with computation being made within a non user-level function
#'
#' sum_fun = function(x, y){
#'   my_internal(x, y, sum = TRUE)
#' }
#'
#' diff_fun = function(x, y){
#'   my_internal(x, y, sum = FALSE)
#' }
#'
#' my_internal = function(x, y, sum){
#'   set_up(1) # => errors will be at the user-level function
#'   check_arg(x, y, "numeric scalar mbt")
#'
#'   # Identical to calling
#'   # check_arg(x, y, "numeric scalar mbt", .up = 1)
#'
#'   if(sum) return(x + y)
#'   return(x - y)
#' }
#'
#' # we check it works
#' sum_fun(5, 6)
#' diff_fun(5, 6)
#'
#' # Let's throw some errors
#' try(sum_fun(5))
#' try(sum_fun(5, 1:5))
#'
set_up = function(.up = 1){
  assign("DREAMERR_UP", .up, parent.frame())
}

#' Settings telling whether or not to display the full call stack on errors
#' 
#' Errors generated with dreamerr functions only shows the call to which
#' the error should point to. If `setDreamerr_show_stack` is set to TRUE,
#' error will display the full call stack instead.
#' 
#' @param show_full_stack Logical scalar, default is `FALSE`. If `TRUE`, then
#' errors generated by dreamerr functions (like [stop_up()]/[stopi()]) will display
#' the full call stack.
#' 
#' @author 
#' Laurent Berge
#' 
#' @examples 
#' 
#' # Let's create a toy example of a function relying on an internal function
#' # for the heavy lifting (although here it's pretty light!)
#' make_sum = function(a, b){
#'   make_sum_internal(a, b)
#' }
#' 
#' make_sum_internal = function(a, b){
#'   if(!is.numeric(a)) stop_up("arg. 'a' must be numeric!")
#'   a + b
#' }
#' 
#' # By default if you feed stg non numeric, the call shown is 
#' # make_sum, and not make_sum_internal, since the user could not
#' # care less of the internal structure of your functions
#' 
#' try(make_sum("five", 55))
#' 
#' # Now with setDreamerr_show_stack(TRUE), you would get the full call stack
#' setDreamerr_show_stack(TRUE)
#' try(make_sum("five", 55))
#' 
#' 
#' 
setDreamerr_show_stack = function(show_full_stack = FALSE){
  check_arg(show_full_stack, "logical scalar mbt")
  options("dreamerr_show_full_stack" = show_full_stack)
}

####
#### hook functions ####
####


#' @describeIn stop_hook Generate a package specific set_hook function
generate_set_hook = function(namespace){
  check_arg(namespace, "character scalar mbt")
  hook_name = paste0("dreamerr_hook_", namespace)
  res = eval(parse(text = sma("function() assign({q ? hook_name}, 1, parent.frame())")))
  res
}

#' @describeIn stop_hook Generate a package specific stop_hook function
generate_stop_hook = function(namespace){
  check_arg(namespace, "character scalar mbt")
  hook_name = paste0("dreamerr_hook_", namespace)
  
  res = function(..., msg = NULL, envir = parent.frame(), verbatim = FALSE){
    up = get_up_hook(hook_name)

    stop_up(..., up = up, msg = msg, envir = envir, verbatim = verbatim)
  }
  
  res
}

#' @describeIn stop_hook Generate a package specific warn_hook function
generate_warn_hook = function(namespace){
  check_arg(namespace, "character scalar mbt")
  hook_name = paste0("dreamerr_hook_", namespace)
  
  res = function(..., envir = parent.frame(), immediate. = FALSE, verbatim = FALSE){
    up = get_up_hook(hook_name)

    warn_up(..., up = up, envir = envir, verbatim = verbatim, immediate. = immediate.)
  }
  
  res
}

#' @describeIn stop_hook Mark the function as the hook
set_hook = function(){
  assign("dreamerr_HOOK", 1, parent.frame())
}

get_up_hook = function(namespace_hook){
  # up with set_up
  f = parent.frame()
  up = 1
  while(!identical(f, .GlobalEnv)){
    if(exists(namespace_hook, f)){
      break
    }
    up = up + 1
    f = parent.frame(up + 1)
  }
  
  # we accept nestedness
  f_up = parent.frame(up + 2)
  while(!identical(f_up, .GlobalEnv) && exists(namespace_hook, f_up)){
    up = up + 1
    f = f_up
    f_up = parent.frame(up + 2)
  }

  if(identical(f, .GlobalEnv)){
    up = 1
  }

  up
}

#' Error displaying a call located at a hook location
#' 
#' When devising complex functions, errors or warnings can be deeply nested in internal 
#' function calls while the user-relevant call is way up the stack. In such cases, these "hook"
#' functions facilitate the creation of error/warnings informative for the user.
#' 
#' @inheritParams stop_up
#' 
#' @param namespace Character scalar giving the namespace for which the hooks are valid. Only useful 
#' when hook functions are used in a package.
#' 
#' @details 
#' These functions are useful when developing complex functions relying on nested internal functions.
#' It is important for the user to know where the errors/warnings come from for quick debugging.
#' This "_hook" family of functions write the call of the user-level function even if the errors
#' happen at the level of the internal functions.
#' 
#' If you need these functions within a package, you need to generate the `set_hook`, `stop_hook` and
#' `warn_hook` functions so that they set, and look up for, hooks speficic to your function. This ensures that
#' if other functions outside your package also use hooks, there will be no conflict. The only thing to do
#' is to write this somewhere in the package files:
#' ```R
#' set_hook = generate_set_hook("pkg_name")
#' stop_hook = generate_stop_hook("pkg_name")
#' warn_hook = generate_warn_hook("pkg_name")
#' ```
#' 
#' @author 
#' Laurent Berge
#' 
#' @seealso 
#' Regular stop functions with interpolation: [stop_up()]. Regular argument checking 
#' with [check_arg()] and [check_set_arg()].
#' 
#' @examples 
#' 
#' # The example needs to be complex since it's about nested functions, sorry
#' # Let's say you have an internal function that is dispatched into several 
#' # user-level functions
#' 
#' my_mean = function(x, drop_na = FALSE){
#'   set_hook()
#'   my_mean_internal(x = x, drop_na = drop_na)
#' }
#' 
#' my_mean_skip_na = function(x){
#'   set_hook()
#'   my_mean_internal(x = x, drop_na = TRUE)
#' }
#' 
#' my_mean_internal = function(x, drop_na){
#'   # simple check
#'   if(!is.numeric(x)){
#'     # note that we use string interpolation with stringmagic.
#'     stop_hook("The argument `x` must be numeric. PROBLEM: it is of class {enum.bq ? class(x)}.")
#'   }
#' 
#'   if(drop_na){
#'     return(mean(x, na.rm = TRUE))
#'   } else {
#'     return(mean(x, na.rm = FALSE))
#'   }
#' }
#' 
#' # Let's run the function with a wrong argument
#' x = "five"
#' try(my_mean(x))
#' 
#' # => the error message reports that the error comes from my_mean 
#' #    and *not* my_mean_internal
#'  
#' 
stop_hook = function(..., msg = NULL, envir = parent.frame(), verbatim = FALSE){
  up = get_up_hook()

  stop_up(..., up = up, msg = msg, envir = envir, verbatim = verbatim)
}

#' @describeIn stop_hook Warning with a call located at a hook location
warn_hook = function(..., envir = parent.frame(), immediate. = FALSE, verbatim = FALSE){
  up = get_up_hook()

  warn_up(..., up = up, envir = envir, verbatim = verbatim, immediate. = immediate.)
}

####
#### regular warn/stop ####
####


#' Stops (or warns in) sub-function execution
#'
#' Useful if you employ non-user level sub-functions within user-level functions or 
#' if you want string interpolation in error messages. When an error is thrown in the sub 
#' function, the error message will integrate the call of the user-level function, which 
#' is more informative and appropriate for the user. It offers a similar functionality for \code{warning}.
#'
#' @param ... Objects that will be coerced to character and will compose the error message.
#' @param up The number of frames up, default is 1. The call in the error message will be based on the function \code{up} frames up the stack. See examples. If you have many calls to \code{stop_up}/\code{warn_up} with a value of \code{up} different than one, you can use \code{\link[dreamerr]{set_up}} to change the default value of \code{up} within the function.
#' @param immediate. Whether the warning message should be prompted directly. Defaults to \code{FALSE}.
#' @param msg A character vector, default is \code{NULL}. If provided, this message will be displayed right under the error message. This is mostly useful when the text contains formatting because the function \code{\link{stop}} used to send the error message erases any formatting.
#' @param envir An environment, default is `parent.frame()`. Only relevant if the error/warning message contains
#' interpolation (interpolation is performed with [stringmagic](https://github.com/lrberge/stringmagic)). It tells
#' where the variables to be interpolated should be found. In general you should not worry about this argument.
#' @param verbatim Logical scalar, default is `FALSE`. By default the error/warning message allows variable
#' interpolation with [stringmagic](https://github.com/lrberge/stringmagic). To disable interpolation,
#' use `verbatim = TRUE`.
#'
#' @details
#' These functions are really made for package developers to facilitate the good practice of providing informative user-level error/warning messages.
#'
#' The error/warning messages allow variable interpolation by making use of [stringmagic](https://github.com/lrberge/stringmagic)'s interpolation.
#' 
#' @author
#' Laurent Berge
#' 
#' @seealso 
#' For general argument checking, see [check_arg()] and [check_set_arg()]. 
#'
#' @examples
#'
#' # We create a main user-level function
#' # The computation is done by an internal function
#' # Here we compare stop_up with a regular stop
#'
#' main_function = function(x = 1, y = 2){
#'   my_internal_function(x, y)
#' }
#'
#' my_internal_function = function(x, y){
#'   if(!is.numeric(x)){
#'     stop_up("Argument 'x' must be numeric but currently isn't.")
#'   }
#'
#'   # Now regular stop
#'   if(!is.numeric(y)){
#'     stop("Argument 'y' must be numeric but currently isn't.")
#'   }
#'
#'   nx = length(x)
#'   ny = length(y)
#'   if(nx != ny){
#'     # Note that we use string interpolation with {}
#'     warn_up("The lengths of x and y don't match: {nx} vs {ny}.")
#'   }
#'
#'   x + y
#' }
#'
#' # Let's compare the two error messages
#' # stop_up:
#' try(main_function(x = "a"))
#' # => the user understands that the problem is with x
#'
#' # Now compare with the regular stop:
#' try(main_function(y = "a"))
#' # Since the user has no clue of what my_internal_function is,
#' #  s/he will be puzzled of what to do to sort this out
#'
#' # Same with the warning => much clearer with warn_up
#' main_function(1, 1:2)
#'
#'
stop_up = function(..., up = 1, msg = NULL, envir = parent.frame(), verbatim = FALSE){

  if(verbatim){
    main_msg = paste0(...)
  } else {
    main_msg = sma(..., .envir = envir)
  }

  # up with set_up
  mc = match.call()
  if(!"up" %in% names(mc)){
    up_value = mget("DREAMERR_UP", parent.frame(), ifnotfound = 1)
    up = up_value[[1]]
  }
  
  up = up + 1

  show_full_stack = isTRUE(getOption("dreamerr_show_full_stack"))

  sc = sys.calls()
  
  if(show_full_stack){
    # The user requests the full stack
    my_call = sapply(sc, function(x) deparse(x, width.cutoff = 200L, nlines = 1))
    my_call = sma("{'\n'c ! [{format.0 ? 1:length(my_call)}] {'100|...'k ? my_call}}")

    intro = paste0("the full stack is shown (set this off with setDreamerr_show_stack(FALSE))\n", my_call)

  } else {
    # only the original call
    my_call = sys.call(sys.parent(up))
    my_call = deparse(my_call)[1]
    nmax = 50
    if(nchar(my_call) > nmax) my_call = paste0(substr(my_call, 1, nmax - 1), "...")

    intro = paste0("in ", my_call)
  }  

  main_msg = fit_screen(main_msg)

  if(!is.null(msg)){
    if(length(msg) > 1){
      msg = paste(msg, collapse = "")
    }
    msg = fit_screen(msg)
    on.exit(message(msg))
  }

  stop(intro, ": \n", main_msg, call. = FALSE)

}

#' @describeIn stop_up Error messages with string interpolation
stopi = function(..., envir = parent.frame()){
  stop_up(..., up = 1, envir = envir, verbatim = FALSE)
}

#' @describeIn stop_up Warnings with string interpolation
warni = function(..., envir = parent.frame(), immediate. = FALSE){
  warn_up(..., up = 1, envir = envir, verbatim = FALSE, immediate. = immediate.)
}

#' @describeIn stop_up Warnings at the level of user-level functions
warn_up = function (..., up = 1, immediate. = FALSE, envir = parent.frame(), verbatim = FALSE){

  if(verbatim){
    message = paste0(...)
  } else {
    message = sma(..., .envir = envir)
  }
  
  mc = match.call()

  if (!"up" %in% names(mc)) {
      up_value = mget("DREAMERR_UP", parent.frame(), ifnotfound = 1)
      up = up_value[[1]]
  }
  
  up = up + 1
  
  my_call = sys.call(sys.parent(up))
  my_call = deparse(my_call)[1]
  
  nmax = 50
  if (nchar(my_call) > nmax){
    my_call = paste0(substr(my_call, 1, nmax - 1), "...")
  }
      
  warning("In ", my_call, ":\n ", fit_screen(message),
          call. = FALSE, immediate. = immediate.)
}

####
#### warnings ####
####

warn_no_named_dots = function(dots, extra_args = NULL, extra_funName = ""){
  if(!is.null(names(dots))){
    is_extra = !is.null(extra_args)
    args_fun = names(formals(sys.function(sys.parent())))
    args_fun = unique(c(args_fun, extra_args))
    args_fun = setdiff(args_fun, "...")

    dot_names = names(dots)
    dot_names = dot_names[nchar(dot_names) > 0]
    sugg_txt = suggest_item(dot_names[1], args_fun, info = "argument")

    warn_up("The arguments in `...` shall not have names{&is_extra; or may refer to the ",
                      "arguments of the function in {bq?extra_funName}}. ",
            "The name{$s, enum.bq ? dot_names}",
            " may refer to {$(an;some)} argument{$s}?", sugg_txt)

  }
}

#' Checks the arguments in dots from methods
#'
#' This function informs the user of arguments passed to a method but which are not used by the method.
#'
#' @param valid_args A character vector, default is missing. Arguments that are not in the definition of the function but which are considered as valid. Typically internal arguments that should not be directly accessed by the user.
#' @param suggest_args A character vector, default is missing. If the user provides invalid arguments, he might not be aware of the main arguments of the function. Use this argument to inform the user of these main arguments.
#' @param stop Logical, default is \code{FALSE}. If \code{TRUE}, when the user provides invalid arguments, the function will call \code{\link[base]{stop}} instead of prompting a warning (default).
#' @param warn Logical, default is \code{TRUE}. If \code{TRUE}, when the user provides invalid arguments, the function will call \code{\link[base]{warning}} (default). If \code{FALSE} (and so are the other arguments \code{stop} and \code{message}), then no message is prompted to the user, rather it is the only output of the function.
#' @param message Logical, default is \code{FALSE}. If \code{TRUE}, a standard message is prompted to the user (instead of a warning).
#' @param call. Logical, default is \code{FALSE}. If \code{TRUE}, when the user provides invalid arguments, then the message will also contain the call to the initial function (by default, only the function name is shown).
#' @param immediate. Logical, default is \code{FALSE}. Can be only used with the argument \code{warn = TRUE}: whether the warning is immediately displayed or not.
#'
#' @return
#' This function returns the message to be displayed. If no message is to be displayed because all the arguments are valid, then \code{NULL} is returned.
#'
#' @examples
#'
#' # The typical use of this function is within methods
#'
#' # Let's create a 'my_class' object and a summary method
#' my_obj = list()
#' class(my_obj) = "my_class"
#'
#' # In the summary method, we add validate_dots
#' # to inform the user of invalid arguments
#'
#' summary.my_class = function(object, arg_one, arg_two, ...){
#'
#'   validate_dots()
#'   # CODE of summary.my_class
#'   invisible(NULL)
#' }
#'
#' # Now let's test it, we add invalid arguments
#' summary(my_obj, wrong = 3)
#' summary(my_obj, wrong = 3, info = 5)
#'
#' # Now let's :
#' #   i) inform the user that argument arg_one is the main argument
#' #  ii) consider 'info' as a valid argument (but not shown to the user)
#' # iii) show a message instead of a warning
#'
#' summary.my_class = function(object, arg_one, arg_two, ...){
#'
#'   validate_dots(valid_args = "info", suggest_args = "arg_one", message = TRUE)
#'   # CODE of summary.my_class
#'   invisible(NULL)
#' }
#'
#' # Let's retest it
#' summary(my_obj, wrong = 3) # not OK => suggestions
#' summary(my_obj, info = 5)  # OK
#'
#'
#'
validate_dots = function(valid_args = c(), suggest_args = c(), message, warn, stop, 
                         call. = FALSE, immediate. = TRUE){
  # Function to catch the arguments passing in ...
  # we suggest some principal arguments

  mc = sys.calls()[[sys.nframe() - 1]]
  args_up = names(mc)
  args_fun = names(formals(sys.function(sys.parent())))

  args = setdiff(args_up, c(args_fun, ""))
  fun_name = as.character(mc[[1]])

  args_invalid = setdiff(args, valid_args)
  res = NULL
  if(length(args_invalid) > 0){

    # Default values
    if(missing(message)) message = FALSE
    if(missing(stop)) stop = FALSE
    if(missing(warn)) warn = !isTRUE(message) & !isTRUE(stop)

    my_call = ""
    if(stop == FALSE && warn == FALSE){
      if(call.){
        my_call = deparse(sys.calls()[[max(1, sys.nframe() - 1)]])[1] # call can have svl lines
        nmax = 70
        if(nchar(my_call) > nmax) my_call = paste0(substr(my_call, 1, nmax-1), "...")
        my_call = paste0(my_call, "\n")
      }
    }


    suggest_info = setdiff(suggest_args, names(mc))
    suggest = ""
    if(length(suggest_info) == 1){
      if(length(suggest_args) == 1){
        suggest = paste0(" (fyi, its main argument is '", suggest_info, "').")
      } else {
        suggest = paste0(" (fyi, another of its main arguments is '", suggest_info, "').")
      }
    } else if(length(suggest_info) >= 2){
      suggest = paste0(" (fyi, some of its main arguments are ", enumerate_items(suggest_info, quote = TRUE), ").")
    } else {
      suggest = "."
    }

    res = paste0(enumerate_items(args_invalid, "is.quote"), " not", ifsingle(args_invalid, " a", ""), " valid argument", plural_len(args_invalid), " of function ", fun_name, suggest)

    my_call = fit_screen(my_call)

    if(stop){
      stop_up(my_call, res)
    } else if(warn){
      warn_up(my_call, res, immediate. = immediate.)
    } else if(message){
      base::message(my_call, res)
    }

  }

  res
}


