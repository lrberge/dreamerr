#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Mar 18 18:12:46 2020
# ~: small utilities
#----------------------------------------------#


####
#### utilities ####
####

string_x = function(x, n, from){
  # string extract

  if(is.character(n)){
    n = nchar(n)
  }

  if(n < 0){
    if(!missing(from)) stop("Internal error: you should not provide arg. `from` when `n` is negative.")
    from = nchar(x) + n + 1
    to = nchar(x)
  } else if(!missing(from)){
    to = from + n - 1
  } else {
    from = 1
    to = n
  }

  substr(x, from, to)
}

####
#### functions ####
####


#' Enumerates the elements of a vector
#'
#' Transforms a vector into a single character string enumerating the values of the vector. Many options exist to customize the result. The main purpose of this function is to ease the creation of user-level messages.
#'
#' @param x A vector.
#' @param type A single character string, optional. If this argument is used, it supersedes all other arguments. It compactly provides the arguments of the function: it must be like \code{"arg1.arg2.arg3"}, i.e. a list of arguments separated by a point. The arguments are: "s" (to add a starting s if \code{length(x)>1}), "or" (to have "or" instead of "and"), "start" (to place the verb at the start instead of in the end), "quote" (to quote the elements of the vector), "enum" (to make an enumeration), "past" (to put the verb in past tense), a verb (i.e. anything different from the previous codes is a verb). Use \code{other(XX)} to set the argument \code{other} to \code{XX}. See details and examples.
#' @param verb Default is \code{FALSE}. If provided, a verb is added at the end of the string, at the appropriate form. You add the verb at the start of the string using the argument \code{start_verb}. Valid verbs are: "be", "is", "has", "have", and any other verb with a regular form.
#' @param s Logical, default is \code{FALSE}. If \code{TRUE} a \code{s} is added at the beginning of the string if the length of \code{x} is greater than one.
#' @param past Logical, default is \code{FALSE}. If \code{TRUE} the verb is put at the past tense.
#' @param or Logical, default is \code{FALSE}. If \code{TRUE} the two last items of the vector are separated by "or" instead of "and".
#' @param start_verb Logical, default is \code{FALSE}. If \code{TRUE} the verb is placed at the beginning of the string instead of the end.
#' @param quote Logical, default is \code{FALSE}. If \code{TRUE} all items are put in between single quotes.
#' @param enum Logical, default is \code{FALSE}. If provided, an enumeration of the items of \code{x} is created. The possible values are "i", "I", "1", "a" and "A". Example: \code{x = c(5, 3, 12)}, \code{enum = "i"} will lead to "i) 5, ii) 3, and iii) 12".
#' @param other Character scalar, defaults to the empty string: \code{""}. If there are more than \code{nmax} elements, then the character string will end with \code{"and XX others"} with \code{XX} the number of remaining items. Use this argument to change what is between the \code{and} and the \code{XX}. E.g. if \code{other = "any of"}, then you would get \code{"... and any of 15 others"} instead of \code{"... and 15 others"}.
#' @param nmax Integer, default is 7. If \code{x} contains more than \code{nmax} items, then these items are grouped into an "other" group.
#'
#' @section The argument \code{type}:
#' The argument \code{type} is a "super argument". When provided, it supersedes all other arguments. It offers a compact way to give the arguments to the function.
#'
#' Its sytax is as follows: \code{"arg1.arg2.arg2"}, where \code{argX} is an argument code. The codes are "s", "past", "or", "start", "quote", "enum" -- they refer to the function arguments. If you want to add a verb, since it can have a free-form, it is deduced as the argument not equal to the previous codes. For example, if you have \code{type = "s.contain"}, this is identical to calling the function with \code{s = TRUE} and \code{verb = "contain"}.
#'
#' A note on \code{enum}. The argument \code{enum} can be equal to "i", "I", "a", "A" or "1". When you include it in \code{type}, by default "i" is used. If you want another one, add it in the code. For example \code{type = "is.enum a.past"} is identical to calling the function with \code{verb = "is"}, \code{past = TRUE} and \code{enum = "a"}.
#'
#' @author
#' Laurent Berge
#'
#' @return
#' It returns a character string of lentgh one.
#'
#' @examples
#'
#' # Let's say you write an error/information message to the user
#' # I just use the "type" argument but you can obtain the
#' #  same results by using regular arguments
#'
#' x = c("x1", "height", "width")
#' message("The variable", enumerate_items(x, "s.is"), " not in the data set.")
#' # Now just the first item
#' message("The variable", enumerate_items(x[1], "s.is"), " not in the data set.")
#'
#' # Past
#' message("The variable", enumerate_items(x, "s.is.past"), " not found.")
#' message("The variable", enumerate_items(x[1], "s.is.past"), " not found.")
#'
#' # Verb first
#' message("The problematic variable", enumerate_items(x, "s.is.start.quote"), ".")
#' message("The problematic variable", enumerate_items(x[1], "s.is.start.quote"), ".")
#'
#' # covid times
#' todo = c("wash your hands", "stay home", "code")
#' message("You should: ", enumerate_items(todo[c(1, 1, 2, 3)], "enum 1"), "!")
#' message("You should: ", enumerate_items(todo, "enum.or"), "?")
#'
#'
enumerate_items = function (x, type, verb = FALSE, s = FALSE, past = FALSE, or = FALSE, start_verb = FALSE, quote = FALSE, enum = FALSE, other = "", nmax = 7){
  # function that enumerates items and add verbs
  # in argument type, you can have a mix of the different arguments, all separated with a "."

  if(length(x) == 0) return("");

  if(!missing(type)){
    args = strsplit(type, "\\.")[[1]]
    s = "s" %in% args
    past = "past" %in% args
    or = "or" %in% args
    start_verb = "start" %in% args
    quote = "quote" %in% args
    enum = any(grepl("^enum", args))
    if(enum){
      arg_enum = args[grepl("^enum", args)][1]
      if(grepl("i", arg_enum)){
        enum = "i"
      } else if(grepl("I", arg_enum)){
        enum = "I"
      } else if(grepl("a", arg_enum)){
        enum = "a"
      } else if(grepl("A", arg_enum)){
        enum = "A"
      } else if(grepl("1", arg_enum)){
        enum = "1"
      } else {
        enum = "i"
      }

      args = args[!grepl("^enum", args)]
    }

    is_other = any(grepl("^other\\(", args))
    if(is_other){
      arg_other = args[grepl("^other\\(", args)][1]
      other = gsub("^.+\\(|\\).*", "", arg_other)

      args = args[!grepl("^other", args)]
    }

    # Now the verb
    verb = setdiff(args, c("s", "past", "or", "start", "quote"))
    if(length(verb) == 0){
      verb = "no"
    } else {
      verb = verb[1]
    }

  } else {
    verb = as.character(verb)

    enum = match.arg(as.character(enum), c("i", "a", "1", "I", "A", "FALSE", "TRUE"))
    if(enum == "TRUE") enum = "i"
  }

  n = length(x)

  # Ensuring it's not too long
  if(n > nmax){

    nmax = nmax - 1

    other = trimws(other)
    if(nchar(other) > 0){
      other = paste0(other, " ", n - nmax, " others")
    } else {
      other = paste0(n - nmax, " others")
    }

    if(quote){
      x = c(paste0("'", x[1:nmax], "'"), other)
    } else {
      x = c(x[1:nmax], other)
    }

    n = length(x)
  } else if(quote){
    x = paste0("'", x, "'")
  }

  # The verb
  if(verb == "FALSE"){
    verb = "no"
  } else if(verb %in% c("be", "are")){
    verb = "is"
  } else if(verb == "have"){
    verb = "has"
  } else if(verb == "does"){
    verb = "do"
  } else if(verb %in% c("do not", "does not", "don't", "doesn't")){
    verb = "do not"
  } else if(verb %in% c("is not", "are not", "isn't", "aren't")){
    verb = "is not"
  } else if(verb %in% c("was", "were")){
    verb = "is"
    past = TRUE
  }

  if(past){
    if(verb %in% c("no", "is", "is not", "has", "do", "do not")){
      verb_format = switch(verb, is = ifunit(n, " was", " were"), "is not" = ifunit(n, " wasn't", " weren't"), no = "", has=" had", do = "did", "do not" = " didn't")
    } else {
      verb_format = paste0(" ", verb, "ed")
    }
  } else {
    if(verb %in% c("no", "is", "is not", "has", "do", "do not")){
      verb_format = switch(verb, is = ifunit(n, " is", " are"), "is not" = ifunit(n, " isn't", " aren't"), no = "", has = ifunit(n, " has", " have"), do = ifunit(n, " does", " do"), "do not" = ifunit(n, " doesn't", " don't"))
    } else {
      verb_format = ifelse(n == 1, paste0(" ", verb, "s"), paste0(" ", verb))
    }

  }

  if (s) {
    startWord = ifelse(n == 1, " ", "s ")
  } else {
    startWord = ""
  }

  if(enum != "FALSE"){
    enum_comma = ","

    if(enum == "i"){
      enum_all = strsplit("i.ii.iii.iv.v.vi.vii.viii.ix.x.xi.xii.xiii.xiv.xv", "\\.")[[1]][1:n]
    } else if(enum == "I"){
      enum_all = toupper(strsplit("i.ii.iii.iv.v.vi.vii.viii.ix.x.xi.xii.xiii.xiv.xv", "\\.")[[1]][1:n])
    } else if(enum == "a"){
      enum_all = base::letters[1:n]
    } else if(enum == "A"){
      enum_all = base::LETTERS[1:n]
    } else if(enum == "1"){
      enum_all = 1:n
    }

    enum_all = paste0(enum_all, ") ")
  } else {
    enum_comma = ""
    enum_all = rep("", n)
  }

  if (n == 1) {
    if(!start_verb){
      res = paste0(startWord, x, verb_format)
    } else {
      res = paste0(startWord, gsub(" ", "", verb_format), " ", x)
    }

  } else {
    and_or = ifelse(or, " or ", " and ")
    if(!start_verb){
      res = paste0(startWord, paste0(enum_all[-n], x[-n], collapse = ", "), enum_comma, and_or, enum_all[n], x[n], verb_format)
    } else {
      res = paste0(startWord, gsub(" ", "", verb_format), " ", paste0(x[-n], collapse = ", "), and_or, x[n])
    }

  }

  res
}


#' Conditional element selection
#'
#' Tiny functions shorter, and hopefully more explicit, than \code{ifelse}.
#'
#' @param x A vector (\code{ifsingle}) or a numeric of length 1 (\code{ifunit}).
#' @param yes Something of length 1. Result if the condition is fulfilled.
#' @param no Something of length 1. Result if the condition is not fulfilled.
#'
#' @details
#' Yes, \code{ifunit} is identical to \code{ifelse(test == 1, yes, no)}. And regarding \code{ifsingle}, it is identical to \code{ifelse(length(test) == 1, yes, no)}.
#'
#' Why writing these functions then? Actually, I've found that they make the code more explicit, and this helps!
#'
#' @author
#' Laurent Berge
#'
#' @return
#' Returns something of length 1.
#'
#' @examples
#'
#' # Let's create an error message when NAs are present
#' my_crossprod = function(mat){
#'  if(anyNA(mat)){
#'    row_na = which(rowSums(is.na(mat)) > 0)
#'    n_na = length(row_na)
#'    stop("In argument 'mat': ", n_letter(n_na), " row", plural(n_na, "s.contain"),
#'         " NA values (", ifelse(n_na<=3, "", "e.g. "), "row",
#'         enumerate_items(head(row_na, 3), "s"), ").
#'         Please remove ", ifunit(n_na, "it", "them"), " first.")
#'  }
#'  crossprod(mat)
#' }
#'
#' mat = matrix(rnorm(30), 10, 3)
#' mat4 = mat1 = mat
#' mat4[c(1, 7, 13, 28)] = NA
#' mat1[7] = NA
#'
#' # Error raised because of NA: informative (and nice) messages
#' try(my_crossprod(mat4))
#' try(my_crossprod(mat1))
#'
#'
ifsingle = function(x, yes, no){
  if(length(x) == 1){
    return(yes)
  } else {
    return(no)
  }
}

#' @describeIn ifsingle Conditional element selection depending on whether \code{x} is equal to unity or not.
ifunit = function(x, yes, no){
  if(x[1] <= 1){
    return(yes)
  } else {
    return(no)
  }
}


#' Adds an s and/or a singular/plural verb depending on the argument's length
#'
#' Utilities to write user-level messages. These functions add an \sQuote{s} or a verb at the appropriate form depending on whether the argument is equal to unity (\code{plural}) or of length one (\code{plural_len}).
#'
#' @param x An integer of length one (\code{plural}) or a vector \code{plural_len}.
#' @param type Character string, default is missing. If \code{type = "s.is.past"} it means that an "s" will be added if \code{x} is greater than 1 (or of length greater than one for \code{plural_len}); it will be followed by the verb "to be" in past tense in singular or plural form depending on \code{x}. This argument must be made of keywords separated by points without space, the keywords are "s", "past" and a verb (i.e. any thing different than "s" and "past"). Missing keywords mean their value is equal to \code{FALSE}.
#' @param s Logical, used only if the argument type is missing. Whether to add an "s" if the form of \code{x} is plural. Default is missing: equals to \code{TRUE} if no other argument is provided, \code{FALSE} otherwise.
#' @param verb Character string or \code{FALSE}, used only if the argument type is missing. The verb to be inserted in singular or plural depending on the value of \code{x}. default is \code{FALSE}.
#' @param past Logical, used only if the argument type is missing. Whether the verb should be in past tense. Default is \code{FALSE}.
#'
#' @author
#' Laurent Berge
#'
#' @return
#' Returns a character string of length one.
#'
#' @examples
#'
#' # Let's create an error message when NAs are present
#' my_crossprod = function(mat){
#'  if(anyNA(mat)){
#'    row_na = which(rowSums(is.na(mat)) > 0)
#'    n_na = length(row_na)
#'    stop("In argument 'mat': ", n_letter(n_na), " row", plural(n_na, "s.contain"),
#'         " NA values (", ifelse(n_na<=3, "", "e.g. "), "row",
#'         enumerate_items(head(row_na, 3), "s"),
#'         "). Please remove ", ifunit(n_na, "it", "them"), " first.")
#'  }
#'  crossprod(mat)
#' }
#'
#' mat = matrix(rnorm(30), 10, 3)
#' mat4 = mat1 = mat
#' mat4[c(1, 7, 13, 28)] = NA
#' mat1[7] = NA
#'
#' # Error raised because of NA: informative (and nice) messages
#' try(my_crossprod(mat4))
#' try(my_crossprod(mat1))
#'
plural = function(x, type, s, verb = FALSE, past = FALSE){
  # adds s if x > 1, can alternatively add a verb

  PLURAL = x[1] > 1

  plural_core(PLURAL, type, s, verb, past)
}

# @title Adds an s and/or a singular/plural verb depending on the argument's length
#' @describeIn plural Adds an s and conjugate a verb depending on the length of \code{x}
plural_len = function(x, type, s, verb = FALSE, past = FALSE){
  # adds s if length(x) > 1
  PLURAL = length(x) > 1

  plural_core(PLURAL, type, s, verb, past)
}


plural_core = function(PLURAL, type, s, verb = FALSE, past = FALSE){

  if(!missing(type)){
    args = strsplit(type, "\\.")[[1]]
    s = "s" %in% args
    past = "past" %in% args
    verb = setdiff(args, c("s", "past"))
    if(length(verb) == 0){
      verb = FALSE
    } else {
      verb = verb[1]
    }
  }

  if(isFALSE(verb)){
    res = ifelse(PLURAL, "s", "")
  } else {

    if(verb %in% c("be", "are")){
      verb = "is"
    } else if(verb == "have"){
      verb = "has"
    } else if(verb == "does"){
      verb = "do"
    } else if(verb %in% c("do not", "does not", "don't", "doesn't")){
      verb = "do not"
    } else if(verb %in% c("is not", "are not", "isn't", "aren't")){
      verb = "is not"
    } else if(verb %in% c("was", "were")){
      verb = "is"
      past = TRUE
    }

    if(past){
      if(verb %in% c("is", "is not", "has", "do", "do not")){
        verb_format = switch(verb, is = ifelse(!PLURAL, "was", "were"), "is not" = ifelse(!PLURAL, "wasn't", "weren't"), has = "had", do = "did", "do not" = "didn't")
      } else {
        verb_format = paste0(verb, "ed")
      }
    } else {
      if(verb %in% c("is", "is not", "has", "do", "do not")){
        verb_format = switch(verb, is = ifelse(!PLURAL, "is", "are"), "is not" = ifelse(!PLURAL, "isn't", "aren't"), has = ifelse(!PLURAL, "has", "have"), do = ifelse(!PLURAL, "does", "do"), "do not" = ifelse(!PLURAL, "doesn't", "don't"))
      } else {
        verb_format = ifelse(PLURAL, verb, paste0(verb, "s"))
      }
    }

    if(!missing(s) && isTRUE(s)){
      res = paste0(ifelse(PLURAL, "s ", " "), verb_format)
    } else {
      res = verb_format
    }

  }

  res
}


#' Numbers in letters
#'
#' Set of (tiny) functions that convert integers into words.
#'
#' @param n An integer vector.
#'
#' @author
#' Laurent Berge
#'
#' @return
#' It returns a character vector of length one.
#'
#' @examples
#'
#' find = function(v, x){
#'   if(x %in% v){
#'     message("The number ", n_letter(x), " appears ", n_times(sum(v == x)),
#'             ", the first occurrence is the ", n_th(which(v==x)[1]), " element.")
#'   } else message("The number ", n_letter(x), " was not found.")
#' }
#'
#' v = sample(100, 500, TRUE)
#' find(v, 6)
#'
#'
#'
n_times = function(n){

  if(length(n) == 0) return("")

  dict = c("once", "twice", "three times", "four times")

  res = as.character(n)

  qui = n <= 4
  res[qui] = dict[n[qui]]

  if(any(!is.na(qui) & !qui)){
    res[!qui] = paste0(n[!qui], " times")
  }

  res
}

#' @describeIn n_times Transforms the integer \code{n} to \code{nth} appropiately.
n_th = function(n){

  if(length(n) == 0) return("")

  dict = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "nineth", "tenth", "eleventh", "twelfth", "thirteenth")

  res = as.character(n)

  qui = n <= 13
  res[qui] = dict[n[qui]]

  if(any(!is.na(qui) & !qui)){
    other = n[!qui]
    rest = other %% 10
    rest[rest == 0 | rest >= 4] = 4
    postfix = c("st", "nd", "rd", "th")
    other = paste0(other, postfix[rest])
    res[!qui] = other
  }

  res
}

#' @describeIn n_times Transforms small integers to words.
n_letter = function(n){

  if(length(n) == 0) return("")

  dict = strsplit("one.two.three.four.five.six.seven.eight.nine.ten.eleven.twelve.thirtheen.fourteen.fifteen.sixteen.seventeen.eighteen.nineteen", "\\.")[[1]]

  res = as.character(n)

  qui = n <= length(dict) & n > 0
  res[qui] = dict[n[qui]]

  res
}




#' Formatting numbers with display of significant digits
#'
#' Formatting of numbers, when they are to appear in messages. Displays only significant digits in a "nice way" and adds commas to separate thousands. It does much less than the \code{\link[base]{format}} function, but also a bit more though.
#'
#' @param x A numeric vector.
#' @param s The number of significant digits to be displayed. Defaults to 2. All digits not in the decimal are always shown.
#' @param r For large values, the number of digits after the decimals to be displayed (beyond the number of significant digits). Defaults to 0. It is useful to suggest that a number is not an integer.
#' @param commas Whether or not to add commas to separate thousands. Defaults to \code{TRUE}.
#'
#' @return
#' It returns a character vector of the same length as the input.
#'
#' @examples
#'
#' x = rnorm(1e5)
#' x[sample(1e5, 1e4, TRUE)] = NA
#'
#' # Dumb function telling the number of NA values
#' tell_na = function(x) message("x contains ", fsignif(sum(is.na(x))), " NA values.")
#'
#' tell_na(x)
#'
#' # Some differences with signif:
#' show_diff = function(x, d = 2) cat("signif(x, ", d, ") -> ", signif(x, d),
#'                                    " vs fsignif(x, ", d, ") -> ",
#'                                    fsignif(x, d), "\n", sep = "")
#'
#' # Main difference is for large numbers
#' show_diff(95123.125)
#' show_diff(95123.125, 7)
#'
#' # Identical for small numbers
#' show_diff(pi / 500)
#'
#'
fsignif = signif_plus = function (x, s = 2, r = 0, commas = TRUE){
  # This is not intended to be applied to large vectors (not efficient)
  # Only for the in-print formatting of some numbers

  # It would supa dupa simple in c++...

  if(is.character(x)){
    return(x)
  }

  if(!is.numeric(x)){
    stop("The argumnent 'x' must be numeric.")
  }

  commas_single = function(x, s, r){

    if(!is.finite(x) || abs(x) < 1) return(as.character(x))

    if((p <- ceiling(log10(abs(x)))) < s){
      r = max(r, s - p)
    }

    x_sign = sign(x)
    x = abs(x)

    x_str = deparse(x)
    dec_string = ""

    if(grepl(".", x_str, fixed = TRUE)){
      decimal = gsub(".*\\.", ".", x_str)
      dec_string = substr(decimal, 1, 1 + r)

      if(dec_string == ".") dec_string = ""
    }

    entier = sprintf("%.0f", floor(x))
    quoi = rev(strsplit(entier, "")[[1]])
    n = length(quoi)
    sol = c()
    for (i in 1:n) {
      sol = c(sol, quoi[i])
      if (i%%3 == 0 && i != n) sol = c(sol, ",")
    }

    res = paste0(ifelse(x_sign == -1, "-", ""), paste0(rev(sol), collapse = ""), dec_string)

    res
  }

  signif_single = function(x, s, r) {
    if(is.na(x)) {
      return(NA)
    }

    if(abs(x) >= 10^(s - 1)){
      return(round(x, r))
    } else {
      return(signif(x, s))
    }
  }

  res = sapply(x, signif_single, s = s, r = r)

  if(commas) res = sapply(res, commas_single, s = s, r = r)

  qui0 = which(grepl("^0.", res, perl = TRUE))
  if(length(qui0) > 0){
    qui_short = nchar(res[qui0]) < s + 2
    qui2fix = qui0[qui_short]
    for(i in qui2fix){
      res[i] = as.vector(sprintf("%s%.*s", res[i], s + 2 - nchar(res[i]), "0000000000000000"))
    }
  }

  res
}

#' @rdname fsignif
"signif_plus"



#' Nicely fits a message in the current R console
#'
#' Utility to display long messages with nice formatting. This function cuts the message to fit the current screen width of the R console. Words are never cut in the middle.
#'
#' @param msg Text message: character vector.
#' @param width A number between 0 and 1, or an integer. The maximum width of the screen the message should take. 
#' Numbers between 0 and 1 represent a fraction of the screen. You can also refer to the 
#' screen width with the special variable `.sw`. Integers represent the number of characters 
#' and cannot be lower than 15. Default is `min(120, 0.95*.sw)` (the min between 120 characters and
#' 90% of the screen width).
#' @param leading_ws Logical, default is \code{TRUE}. Whether to keep the leading 
#' white spaces when the line is cut.
#' @param leader Character scalar, default is the empty string. If provided, this 
#' value will be placed in front of every line.
#'
#' @details
#' This function does not handle tabulations.
#'
#' @return
#' It returns a single character vector with line breaks at the appropriate width.
#'
#' @examples
#'
#' # A long message of two lines with a few leading spaces
#' msg = enumerate_items(state.name, nmax = Inf)
#' msg = paste0("     ", gsub("Michigan, ", "\n", msg))
#'
#' # by default the message takes 95% of the screen
#' cat(fit_screen(msg))
#'
#' # Now we reduce it to 50%
#' cat(fit_screen(msg, 0.5))
#'
#' # we add leading_ws = FALSE to avoid the continuation of leading WS
#' cat(fit_screen(msg, 0.5, FALSE))
#' 
#' # We add "#> " in front of each line
#' cat(fit_screen(msg, 0.5, leader = "#> "))
#'
#'
fit_screen = function(msg, width = NULL, leading_ws = TRUE, leader = ""){
  # makes a message fit the current screen, by cutting the text at the appropriate location
  # msg must be a character string of length 1
  
  if(length(msg) == 0) return(msg)

  # Note that \t are NOT handled
  
  # eval
  width = check_set_width(substitute(width))

  N_LEAD = nchar(leader)

  if(width > 1){
    MAX_WIDTH = width
  } else {
    MAX_WIDTH = getOption("width") * width
  }

  MAX_WIDTH = max(MAX_WIDTH, 15)

  res = c()

  msg_split = strsplit(msg, "\n", fixed = TRUE)[[1]]

  for(m in msg_split){
    if(nchar(m) <= MAX_WIDTH){
      res = c(res, paste0(leader, m))
    } else {
      # we apply a splitting algorithm

      lead_ws = gsub("^([ \t]*).*", "\\1", m, perl = TRUE)
      m = trimws(m)
      N_LEAD_WS = nchar(lead_ws)
      add_lead = TRUE
      first = TRUE

      m_split = strsplit(m, "(?<=[^ ]) ", perl = TRUE)[[1]]

      while(TRUE){

        if(add_lead){
          width = MAX_WIDTH - N_LEAD_WS - N_LEAD
          prefix = paste0(leader, lead_ws)
        } else {
          width = MAX_WIDTH - N_LEAD
          prefix = leader
        }

        if(sum(nchar(m_split) + 1) - 1 <= width){
          res = c(res, paste0(prefix, paste(m_split, collapse = " ")))
          break
        }

        where2split = which.max(cumsum(nchar(m_split) + 1) - 1 > width) - 1
        res = c(res, paste0(prefix, paste(m_split[1:where2split], collapse = " ")))
        m_split = m_split[-(1:where2split)]

        if(!leading_ws && first){
          add_lead = FALSE
          first = FALSE
        }

      }
    }
  }

  paste(res, collapse = "\n")
}

check_set_width = function(width_expr){
  sw = getOption("width") 
  data = list(.sw = sw)
  width = eval(width_expr, data, parent.frame(2))
  
  if(isFALSE(width)){
    width = Inf
  }
  
  if(is.null(width)){
    width = min(120, 0.95 * sw)
  }
  
  width
}



#' Fills a string vector with a symbol
#'
#' Fills a string vector with a user-provided symbol, up to the required length.
#'
#' @param x A character vector.
#' @param n A positive integer giving the total expected length of each character string. Can be NULL (default). If \code{NULL}, then \code{n} is set to the maximum number of characters in \code{x} (i.e. \code{max(nchar(x))}).
#' @param symbol Character scalar, default to \code{" "}. The symbol used to fill.
#' @param right Logical, default is \code{FALSE}. Whether the character vector should be filled on the left( default) or on the right.
#' @param na Character that will replace any NA value in input. Default is "NA".
#' @param anchor Character scalar, can be missing. If provided, the filling is done up to this anchor. See examples.
#'
#' @return
#' Returns a character vector of the same length as \code{x}.
#'
#' @examples
#'
#' # Some self-explaining examples
#' x = c("hello", "I", "am", "No-one")
#' cat(sep = "\n", sfill(x))
#' cat(sep = "\n", sfill(x, symbol = "."))
#' cat(sep = "\n", sfill(x, symbol = ".", n = 15))
#' cat(sep = "\n", sfill(x, symbol = ".", right = TRUE))
#'
#' cat(sep = "\n", paste(sfill(x, symbol = ".", right = TRUE), ":", 1:4))
#'
#' # Argument 'anchor' can be useful when using numeric vectors
#' x = c(-15.5, 1253, 32.52, 665.542)
#' cat(sep = "\n", sfill(x))
#' cat(sep = "\n", sfill(x, anchor = "."))
#'
sfill = function(x = "", n = NULL, symbol = " ", right = FALSE, anchor, na = "NA"){
  # Character vectors starting with " " are not well taken care of

  check_set_arg(x, "l0 character vector conv")
  if(length(x) == 0) return(character(0))

  check_arg(n, "NULL integer scalar GE{0}")
  check_arg(symbol, "character scalar")
  check_arg(right, "logical scalar")
  check_arg(anchor, "character scalar")

  x[is.na(x)] = na

  if(nchar(symbol) != 1) stop("Argument 'symbol' must be a single character (currenlty it is of length ", nchar(symbol), ").")

  IS_ANCHOR = FALSE
  if(!missing(anchor)){
    if(nchar(anchor) != 1){
      stop("If provided, argument 'anchor' must be a single character (currenlty it is of length ", nchar(anchor), ").")
    }
    IS_ANCHOR = TRUE
    x_origin = x
    is_x_anchor = grepl(anchor, x, fixed = TRUE)
    x_split = strsplit(x, anchor, fixed = TRUE)
    if(right){
      x = sapply(x_split, function(v) v[max(length(v), 1)])
    } else {
      x = sapply(x_split, function(v) v[1])
    }

    x[is.na(x)] = ""

  }

  if(!is.null(n) && n == 0) return(x)

  n_all = nchar(x)
  if(is.null(n)) n = max(n_all)

  n2fill = n - n_all
  qui = which(n2fill > 0)
  if(length(qui) == 0 && !IS_ANCHOR) return(x)

  if(symbol == " "){
    if(right == TRUE){
      x_new = sprintf("%-*s", n, x[qui])
    } else {
      x_new = sprintf("%*s", n, x[qui])
    }
  } else {
    pattern = rep(symbol, n)
    value2append = sapply(n2fill[qui], function(nmax) paste(pattern[1:nmax], collapse = ""))
    if(right == TRUE){
      x_new = paste0(x[qui], value2append)
    } else {
      x_new = paste0(value2append, x[qui])
    }
  }

  res = x
  res[qui] = x_new

  if(IS_ANCHOR){
    for(i in seq_along(x_split)){
      if(is_x_anchor[i]){
        if(right){
          x_split[[i]][length(x_split[[i]])] = res[i]
        } else {
          x_split[[i]][1] = res[i]
        }

        res[i] = paste(x_split[[i]], collapse = anchor)
      }
    }
  }

  res
}





#' Provides package statistics
#'
#' Summary statistics of a packages: number of lines, number of functions, etc...
#'
#' @details
#' This function looks for files in the \code{R/} and \code{src/} folders and gives some stats. If there is no \code{R/} folder directly accessible from the working directory, there will be no stats displayed.
#'
#' Why this function? Well, it's just some goodies for package developers trying to be user-friendly!
#'
#' The number of documentation lines (and number of words) corresponds to the number of non-empty roxygen documentation lines. So if you don't document your code with roxygen, well, this stat won't prompt.
#'
#' Code lines correspond to non-commented, non-empty lines (by non empty: at least one letter must appear).
#'
#' Comment lines are non-empty comments.
#'
#' @return
#' Doesn't return anything, just a prompt in the console.
#'
#' @examples
#'
#' package_stats()
#'
package_stats = function(){
  r_files = list.files("./R", full.names = TRUE)
  r_files = r_files[!grepl("Rcpp", r_files)]

  n_r = length(r_files)

  if(n_r == 0){
    message("Your project doesn't look like a package... Sorry, no stats!")
    return(invisible(NULL))
  }

  my_file = c()
  for(i in 1:n_r){
    my_file = c(my_file, readLines(r_files[i]))
  }

  head_3 = function(x) if(length(x) <= 3) return(x) else return(x[1:3])

  # some stats
  n_r_lines = length(my_file)

  qui_code = !grepl("^( |\t)*#", my_file) & grepl("[[:alnum:]]", my_file)
  all_code = my_file[qui_code]
  all_assign = all_code[grepl("^( |\t)*[[:alpha:]][[:alnum:]\\._]* *(=|<-)", all_code)]

  vars = gsub("^( |\t)*| *(=|<-).*", "", all_assign)
  tvars = head_3(sort(table(vars), decreasing = TRUE))
  fav_var = paste0(names(tvars), " [", tvars, "]")

  n_r_code = sum(qui_code)
  n_r_comment = sum(grepl("^( |\t)*#[^']", my_file))

  qui_doc = grepl("^#'", my_file)
  all_doc = my_file[qui_doc & grepl("[[:alpha:]]", my_file)]
  n_doc_words = sum(lengths(strsplit(all_doc, "( |\\(|\\{)[[:alpha:]]")) - 1)
  n_r_doc = sum(qui_doc)
  n_r_funs = sum(grepl("^[\\.[:alnum:]][\\.[:alnum:]_]* *(=|<-) *function", my_file))


  c_files = list.files("./src", full.names = TRUE)
  c_files = c_files[!grepl("Rcpp", c_files) & grepl("\\.c(pp)?$", c_files)]

  n_c = length(c_files)
  if(n_c > 0){
    my_c_file = c()
    for(i in 1:n_c){
      my_c_file = c(my_c_file, readLines(c_files[i]))
    }

    n_c_lines = length(my_c_file)
    n_c_code = sum(!grepl("^( |\t)*//", my_c_file) & grepl("[[:alnum:]]", my_c_file))
    n_c_comment = sum(grepl("^( |\t)*//", my_c_file))
    n_c_funs = sum(grepl("^[\\.[:alnum:]_]* [\\.[:alnum:]_]*\\(", my_c_file))
    n_c_fun_c_export = sum(grepl("^// \\[\\[Rcpp", my_c_file))
  }

  line_doc = ifelse(n_doc_words > 0, paste0("\n........doc: ", signif_plus(n_r_doc), " (", signif_plus(n_doc_words), " words)"), "")

  cat("-----------------------
|| Package Statistics ||
------------------------

R code:",
      "\n# Lines: ", signif_plus(n_r_lines),
      "\n.......code: ", signif_plus(n_r_code), " (fav. vars: ", enumerate_items(fav_var), ")",
      "\n...comments: ", signif_plus(n_r_comment),
      line_doc,
      "\n# Functions: ", signif_plus(n_r_funs), sep = "")


  if(n_c > 0){
    # Rcpp specific
    line_export = ifelse(n_c_fun_c_export > 0, paste0("\n# Functions (Rcpp exports): ", signif_plus(n_c_fun_c_export)), "")
    intro = "C++ code:"
    if(!any(grepl("\\.cpp$", c_files))){
      intro = "C code:"
    } else if(any(grepl("\\.c$", c_files))){
      intro = "C/C++ code:"
    }
    cat("\n\n", intro,
        "\n# Lines: ", signif_plus(n_c_lines),
        "\n.......code: ", signif_plus(n_c_code),
        "\n...comments: ", signif_plus(n_c_comment),
        "\n# Functions: ", signif_plus(n_c_funs),
        line_export, sep = "")

    cat("\n\nTOTAL:",
        "\n# Lines: ", signif_plus(sum(n_c_lines) + sum(n_r_lines)),
        "\n# Code: ", signif_plus(sum(n_c_code) + sum(n_r_code)),
        "\n# Functions: ", signif_plus(sum(n_c_funs) + sum(n_r_funs)), sep = "")
  }
}






#' Suggest the the closest elements from a string vector
#' 
#' Compares a character scalar to the elements from a character vector and 
#' returns the elements that are the closest to the input.
#' 
#' @param x Character scalar, must be provided. This reference will be compared 
#' to the elements of the string vector in the argument `items`.
#' @param items Character vector, must be provided. Elements to which the value in 
#' argument `x` will be compared.
#' @param msg.write Logical scalar, default is `FALSE`. If `TRUE`, a message is returned,
#' equal to `"Maybe you meant {enum.bq.or ? matches}?"` (see [stringmagic](https://lrberge.github.io/stringmagic/articles/guide_string_magic.html)
#' for information on the interpolation) if there were matches. If no matches were found,
#' the message is `"FYI the {msg.item}{$s, are, enum.bq ? items}."`.
#' @param msg.newline Logical scalar, default is `TRUE`. Only used if `msg.write = TRUE`.
#' Whether to add a new line just before the message.
#' @param msg.item Character scalar, default is `"variable"`. Only used if `msg.write = TRUE`.
#' What does the `items` represent?
#' 
#' @details 
#' This function is useful when used internally to guide the user to relevant choices.
#' 
#' The choices to which the user is guided are in decreasing quality. First light mispells
#' are checked. Then more important mispells. Finally very important mispells. Completely 
#' off potential matches are not reported.
#' 
#' If the argument `msg.write` is `TRUE`, then a character scalar is returned containing
#' a message suggesting the matches.
#' 
#' @return 
#' It returns a vector of matches. If no matches were found 
#' 
#' @author 
#' Laurent Berge
#' 
#' @examples 
#' 
#' # function reporting the sum of a variable
#' sum_var = function(data, var){
#'   # var: a variable name, should be part of data
#'   if(!var %in% names(data)){
#'     suggestion = suggest_item(var, names(data), msg.write = TRUE)
#'     stopi("The variable `{var}` is not in the data set. {suggestion}")
#'   }
#' 
#'   sum(data[[var]])
#' }
#' 
#' # The error message guides us to a suggestion
#' try(sum_var(iris, "Petal.Le"))
#' 
suggest_item = function(x, items, msg.write = FALSE, msg.newline = TRUE, msg.item = "variable"){
  # typical use: x is not in items
  #              we want to suggest possible values
  # returns vector of length 0 if no suggestion
  
  check_set_arg(x, "character scalar conv mbt")
  check_arg(items, "character vector no na mbt")
  check_arg(msg.write, msg.newline, "logical scalar")
  check_arg(msg.item, "character scalar")
  
  items_origin = items

  # 1: with case
  nx = nchar(x)
  items_nx = substr(items, 1, nx)

  qui = items_nx == x
  if(any(qui)){
    res = items[qui]

  } else {
    # 2: without case

    x = tolower(x)
    items_nx = tolower(items_nx)

    qui = items_nx == x
    if(any(qui)){
      res = items[qui]
    } else {
      # 3: with spelling mistakes, keeping the order
      # only if x > 3 characters

      if(nx < 3) return(character(0))

      # lazy algorithm
      score = numeric(length(items))
      for(i in 1:nx){
        score = score + (substr(items_nx, i, i) == substr(x, i, i))
      }

      if(any(score > (nx / 2))){
        s_order = order(score, decreasing = TRUE)
        score = score[s_order]
        items = items[s_order]
        
        res = items[score > nx / 2]
        
      } else {
        # 4: with spelling mistakes, ignoring the order
        x_letters = strsplit(x, "")[[1]]
        score = numeric(length(items))
        for(i in 1:nx){
          score = score + (substr(items_nx, i, i) %in% x_letters)
        }

        s_order = order(score, decreasing = TRUE)
        score = score[s_order]
        items = items[s_order]
        
        res = items[score > (nx * 0.65)]
      }
    }
  }  

  if(msg.write){
    if(length(res) == 0){
      if(length(items_origin) <= 5){
        res = sma("FYI the {msg.item}{$s, are, enum.bq ? items_origin}.")
      } else {
        res = sma("\nFYI the {msg.item}s are: {sort, ', 'c ? items_origin}.")
      }
    } else {
      res = sma("Maybe you meant {enum.bq.or ? res}?")
    }

    if(msg.newline && !grepl("^\n", res)){
      res = paste0("\n", res)
    }
  }

  res
}



####
#### pkgdown ####
####


renvir_get = function(key){
  # Get the values of envir variables
  # we also evaluate them

  value_raw = Sys.getenv(key)

  if(value_raw == ""){
      return(NULL)
  }

  # Any default value should be able to be evaluated "as such"
  value_clean = gsub("__%%;;", "\n", value_raw)
  value_clean = gsub("&quot;", '"', value_clean)
  value_clean = gsub("&apos;", "'", value_clean)

  value = eval(str2lang(value_clean))

  return(value)
}

is_package_root = function(){
  isTRUE(renvir_get("package_ROOT"))
}

fix_pkgwdown_path = function(){
    # https://github.com/r-lib/pkgdown/issues/1218
    # just because I use google drive... it seems pkgdown cannot convert to relative path...

    # This is to ensure it only works for me
    if(!is_package_root()) return(NULL)

    all_files = list.files("docs/articles/", full.names = TRUE, pattern = "html$")

    for(f in all_files){
        my_file = file(f, "r", encoding = "UTF-8")
        text = readLines(f)
        close(my_file)
        if(any(grepl("../../../", text, fixed = TRUE))){
            # We embed the images directly: safer
            
            message("pkgdown images updated: ", gsub(".+/", "", f))

            # A) we get the path
            # B) we transform to URI
            # C) we replace the line

            pat = "<img.+\\.\\./.+/dreamerr/.+/images/"
            qui = which(grepl(pat, text))
            for(i in qui){
                # ex: line = "<img src = \"../../../Google drive/dreamerr/dreamerr/vignettes/images/etable/etable_tex_2021-12-02_1.05477838.png\">"
                line = text[i]
                line_split = strsplit(line, "src *= *\"")[[1]]
                path = gsub("\".*", "", line_split[2])
                # ROOT is always dreamerr
                path = gsub(".+dreamerr/", "", path)
                path = gsub("^articles", "vignettes", path)

                URI = knitr::image_uri(path)

                rest = gsub("^[^\"]+\"", "", line_split[2])
                new_line = paste0(line_split[1], ' src = "', URI, '"', rest)

                text[i] = new_line
            }

            my_file = file(f, "w", encoding = "UTF-8")
            writeLines(text, f)
            close(my_file)
        }
    }

}






























