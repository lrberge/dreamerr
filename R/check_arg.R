#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Sat Feb 29 10:04:10 2020
# ~: Ultimate control function
#----------------------------------------------#

# roxygen2::roxygenise(roclets = "rd")


# type = "scalarNumericGT{5}LT{7}|NA|scalarLogical|matrix nrow(,5) ncol(2,5) GT{log(5)}"

send_error = function(all_reasons, x_name, type, message, choices = NULL, up, .value, .data){

  # Retro compatibility of .call_up + "up" with set_up
  mc_origin = sys.call(sys.parent(2))
  if(".call_up" %in% names(mc_origin)){
    # I do that to avoid having a new argument to send_error
    up_value = get(".call_up", parent.frame(2))
    up = up_value[[1]]

  } else if(!".up" %in% names(mc_origin)){
    up_value = mget("DREAMERR__UP", parent.frame(3), ifnotfound = 0)
    up = up_value[[1]]
  }

  all_types = strsplit(type, "|", fixed = TRUE)[[1]]

  if(grepl("(?i)match|charin", type)){
    sysOrigin = sys.parent(up + 3)
    choices = extract_choices(x_name, type, choices, sysOrigin)
  }

  IS_DOTS = FALSE
  if(grepl("__dots__", x_name, fixed = TRUE)){
    IS_DOTS = TRUE
    text_problem = arg_name_header(x_name, problem = TRUE)
  } else {
    text_problem = " Problem: "
  }

  if(!missing(message) && grepl("__arg_name__", message, fixed = TRUE)){
    x_name = extract_par(message, "__arg_name__")
    message = NULL
  }

  # We check the type is well formed
  type_clean = gsub("(?i)(safe ?)?null(\\{[^\\}]*\\})?|eval(set)?|dotnames|mbt", "", type)

  all_types_clean = strsplit(type_clean, "|", fixed = TRUE)[[1]]
  all_types_clean = all_types[grepl("[[:alpha:]]", all_types_clean)]

  N_TYPES = length(all_types_clean)
  if(N_TYPES == 0){
    # MEANS only globals
    return(NULL)
  }

  #  Additional information on the main classes
  all_main_types = attr(all_reasons, "all_main_types")
  if(length(all_main_types) == 0) all_main_types = rep("", length(all_types))
  all_main_types_ok = nchar(all_main_types) > 0

  # First, we create the error message with
  # the most possible precision

  all_requested_types = c()

  for(i in seq_along(all_types)){

    add_len = add_dim = add_equality = FALSE

    my_type_raw = all_types[i]
    my_type = tolower(my_type_raw)

    if(grepl("formula", my_type)){
      #
      # FORMULAS
      #

      if(!grepl("os|ts", my_type)){
        req_type = "a formula"
      } else {
        req_type = ifelse(grepl("os", my_type), "a one-sided formula", "a two-sided formula")
      }

      IS_RIGHT = FALSE
      if(grepl("right\\(", my_type) && all_main_types_ok[i]){
        # We add information on the number of parts
        IS_RIGHT = TRUE
        n_expected = extract_par(my_type, "right", int = TRUE)
        msg = message_in_between(n_expected, "right", .value)
        req_type = paste0(req_type, " ", msg)
      }

      if(grepl("left\\(", my_type) && all_main_types_ok[i]){
        # We add information on the number of parts
        n_expected = extract_par(my_type, "left", int = TRUE)
        msg = message_in_between(n_expected, "left", .value)
        req_type = paste0(req_type, ifelse(IS_RIGHT, " and ", " "), msg)
      }

      all_requested_types[i] = req_type

      next
    } else if(grepl("class\\(", my_type)){
      #
      # CUSTOM CLASS
      #

      all_classes = extract_par(my_type_raw, "class")
      all_classes = gsub(" ", "", all_classes)

      req_type = paste0("an object of class ", enumerate_items(all_classes, "or.quote"))

      add_len = add_dim = TRUE
    } else if(grepl("scalar", my_type)){
      #
      # SCALAR
      #

      if(grepl("scalar\\(", my_type)){
        subtypes = extract_multi_types(my_type_raw, "scalar")
        req_type = paste0("a scalar of type ", enumerate_items(subtypes, "or.quote"))
      } else {
        subtypes = extract_type(my_type)
        if(is.null(subtypes)){
          all_requested_types[i] = "a scalar"
          next
        } else {
          req_type = switch(subtypes,
                            numeric = "a numeric scalar",
                            integer = "an integer scalar",
                            factor = "a factor of length 1",
                            character = "a character scalar",
                            logical = "a logical scalar",
                            strict_logical = "a scalar strictly logical")
        }
      }

      precision_numeric = ""
      if(any(c("numeric", "integer") %in% subtypes)){
        is_num = TRUE
        if(any(!c("numeric", "integer") %in% subtypes)){
          is_int = !"numeric" %in% subtypes
          precision_numeric = paste0(" (if ", ifelse(is_int, "integer", "numeric"), ")")
        }
      } else {
        all_requested_types[i] = req_type
        next
      }

      add_equality = TRUE

    } else if(grepl("list", my_type)){
      #
      # LIST
      #

      if(grepl("named", my_type)){
        req_type = "a named list"
      } else {
        req_type = "a list"
      }

      if(grepl("(?i) l0", type)){
        req_type = paste0(req_type, " (even empty)")
      }


      add_len = TRUE
    } else if(grepl("data.frame", my_type)){
      #
      # DATA FRAME
      #

      req_type = "a data.frame"

      if(grepl("vdata.frame", my_type)){
        req_type = paste0(req_type, " (or vector)")
      }

      if(grepl("nona", my_type)){
        req_type = paste0(req_type, " without NAs")
      }

      add_dim = TRUE
    } else if(grepl("matrix", my_type)){
      #
      # Matrix
      #

      delayed_typeof = NULL
      strict_logical = FALSE
      # First: the typeof
      if(grepl("matrix\\(", my_type)){
        delayed_typeof = extract_multi_types(my_type_raw, "matrix")
        req_type = "a matrix"
      } else {
        subtypes = extract_type(my_type)
        strict_logical = "strict_logical" %in% subtypes
        if(is.null(subtypes)){
          req_type = "a matrix"
        } else {
          req_type = switch(subtypes,
                            numeric = "a numeric matrix",
                            integer = "an integer matrix",
                            character = "a character matrix",
                            logical = "a logical matrix",
                            strict_logical = "a logical matrix")
          # we don't say it should be strictly logical => unless there's later problem with the type
        }
      }

      if(grepl("vmatrix", my_type)){
        req_type = paste0(req_type, " (or vector)")
      }

      if(grepl("square", my_type)){
        req_type = gsub("an? ", "a square ", req_type)
        req_type = gsub("\\)$", " of length 1)", req_type)
      }

      if(!is.null(delayed_typeof)){
        req_type = paste0(req_type, " of type ", enumerate_items(delayed_typeof, "or.quote"))
      }

      add_dim = add_equality = TRUE
    } else if(grepl("vector", my_type)){
      #
      # VECTOR
      #

      is_NAMED = grepl("named", my_type)

      if(grepl("vector\\(", my_type)){
        subtypes = extract_multi_types(my_type_raw, "vector")
        req_type = paste0("a ", ifelse(is_NAMED, "named ", ""), "vector of type ", enumerate_items(subtypes, "or.quote"))
      } else {
        subtypes = extract_type(my_type)
        if(is.null(subtypes)){
          req_type = ifelse(is_NAMED, "a named vector", "a vector")
        } else {
          if(is_NAMED){
            req_type = switch(subtypes,
                              numeric = "a named numeric vector",
                              integer = "an integer vector with names",
                              factor = "a vector of factors with names",
                              character = "a character vector with names",
                              logical = "a logical vector with names",
                              strict_logical = "a strictly logical vector with names")
          } else{
            req_type = switch(subtypes,
                              numeric = "a numeric vector",
                              integer = "an integer vector",
                              factor = "a vector of factors",
                              character = "a character vector",
                              logical = "a logical vector",
                              strict_logical = "a strictly logical vector")
          }
        }
      }

      precision_numeric = ""
      if(any(c("numeric", "integer") %in% subtypes)){
        is_num = TRUE
        if(any(!c("numeric", "integer") %in% subtypes)){
          is_int = !"numeric" %in% subtypes
          precision_numeric = paste0(" (if ", ifelse(is_int, "integer", "numeric"), ")")
        }
      }

      add_len = add_equality = TRUE
    } else if(grepl("charin", my_type)){
      #
      # CHARIN
      #

      if(grepl("charin\\(", my_type)){
        choices = extract_par(my_type_raw, "charin")
      }

      my_type_corrected = gsub("\\([^\\)]*\\)", "", my_type)
      first_msg = ifelse(grepl("multi", my_type_corrected), "a character vector with values equal to ", "a single character equal to ")
      req_type = paste0(first_msg, enumerate_items(choices, "or.quote"))

      all_requested_types[i] = req_type

      next
    } else if(grepl("match", my_type)){
      #
      # MATCH
      #

      if(grepl("match\\(", my_type)){
        choices = extract_par(my_type_raw, "match")
      }

      my_type_corrected = gsub("\\([^\\)]*\\)", "", my_type)
      first_msg = ifelse(grepl("multi", my_type_corrected), "a character vector with values equal to ", "a single character equal to ")
      req_type = paste0(first_msg, enumerate_items(choices, "or.quote"))

      if(grepl("strict", my_type_corrected)){
        req_type = paste0(req_type, " (case sensitive)")
      }

      all_requested_types[i] = req_type

      next
    } else if(grepl("function", my_type)){
      #
      # FUNCTION
      #

      req_type = "a function"

      if(grepl("arg\\(", my_type)){
        n_expected = extract_par(my_type, "arg", int = TRUE)

        msg = message_in_between(n_expected, "arg", .value)
        req_type = paste0(req_type, " ", msg)
      }

      all_requested_types[i] = req_type

      next
    } else if(grepl(" *na *", my_type)){
      #
      # special NA type
      #

      all_requested_types[i] = "equal to NA"

      next
    } else {
      all_requested_types[i] = "invalid"
      next
    }

    if(add_len){

      if(grepl("len\\(", my_type)){
        n_expected = extract_par(my_type, "len", int = TRUE)

        msg = message_in_between(n_expected, "len", .value, .data)

        if(all_main_types_ok[i]){
          req_type = paste0(req_type, " ", msg)
        }

      }
    }

    is_ROW = is_COL = FALSE
    if(add_dim){

      if(grepl("nrow(data)", my_type, fixed = TRUE) && grepl("ncol(data)", my_type, fixed = TRUE)){
        # We compact the message (it's nicer that way)
        mc = get("mc", parent.frame())

        if(!".data" %in% names(mc)){
          # This is a big problem
          stop_up(up = 2, "To evaluate the 'nrow(data)' keyword in the type '", my_type, "', the argument '.data' MUST be provided. But it is currently missing.")
        }

        if(all_main_types_ok[i]){
          is_ROW = TRUE
          end_msg = ifelse(missing(.data), " which is missing", paste0(" (", NROW(.data), " row", plural(NROW(.data)), " and ", NCOL(.data), " column", plural(NCOL(.data)), ")"))
          msg = paste0("with the same dimensions as argument '", deparse(mc[[".data"]]), "'", end_msg)
          req_type = paste0(req_type, " ", msg)
        } else {
          is_ROW = FALSE
        }

      } else {
        if(grepl("nrow\\(", my_type)){

          n_expected = extract_par(my_type, "nrow", int = TRUE)

          msg = message_in_between(n_expected, "nrow", .value, .data)

          if(all_main_types_ok[i]){
            is_ROW = TRUE
            req_type = paste0(req_type, " ", msg)
          }

        }

        if(grepl("ncol\\(", my_type)){

          n_expected = extract_par(my_type, "ncol", int = TRUE)

          msg_col = message_in_between(n_expected, "ncol", .value, .data)

          if(all_main_types_ok[i]){
            is_COL = TRUE
          }

        }
      }

    }

    is_EQ = FALSE
    if(add_equality && grepl("(ge|gt|le|lt)\\{", my_type) && all_main_types_ok[i]){

      # first we get all the values
      value_ge = value_gt = value_le = value_lt = NULL

      # greater
      if(grepl("ge{", my_type, fixed = TRUE)){
        value_ge = extract_curly(my_type_raw, "ge", as.string = TRUE)
      } else if(grepl("gt{", my_type, fixed = TRUE)){
        value_gt = extract_curly(my_type_raw, "gt", as.string = TRUE)
      }

      # lower
      if(grepl("le{", my_type, fixed = TRUE)){
        value_le = extract_curly(my_type_raw, "le", as.string = TRUE)
      } else if(grepl("lt{", my_type, fixed = TRUE)){
        value_lt = extract_curly(my_type_raw, "lt", as.string = TRUE)
      }

      if(!is.null(value_ge) || !is.null(value_gt) || !is.null(value_le) || !is.null(value_lt)){

        is_EQ = TRUE

        with_values = TRUE

        if(!is.null(value_ge) && !is.null(value_le)){
          msg_eq = paste0("in between ", value_ge, " and ", value_le)

        } else if(!is.null(value_gt) && !is.null(value_lt)){
          msg_eq = paste0("strictly in between ", value_gt, " and ", value_lt)

        } else if(!is.null(value_ge) && !is.null(value_lt)){
          msg_eq = paste0("in between ", value_ge, " and ", value_lt, " (strictly)")

        } else if(!is.null(value_gt) && !is.null(value_le)){
          msg_eq = paste0("in between ", value_gt, " (strictly) and ", value_le)

        } else if(!is.null(value_ge)){
          if(value_ge == 0){
            with_values = FALSE
            msg_eq = "with positive values"
          } else {
            msg_eq = paste0("greater than, or equal to, ", value_ge)
          }
        } else if(!is.null(value_gt)){
          if(value_gt == 0){
            with_values = FALSE
            msg_eq = "with strictly positive values"
          } else {
            msg_eq = paste0("strictly greater than ", value_gt)
          }
        } else if(!is.null(value_le)){
          if(value_le == 0){
            with_values = FALSE
            msg_eq = "with negative values"
          } else {
            msg_eq = paste0("lower than, or equal to, ", value_le)
          }

        } else if(!is.null(value_lt)){
          if(value_lt == 0){
            with_values = FALSE
            msg_eq = "with strictly negative values"
          } else {
            msg_eq = paste0("strictly lower than ", value_lt)
          }
        }

        if(grepl("matrix|vector", my_type)){
          if(with_values) {
            msg_eq = paste0("with values ", msg_eq)
          } else {

          }
        }
      }

    }

    if(is_ROW){
      if(is_COL){
        if(is_EQ){
          req_type = paste0(req_type, ", ", msg_col, ", and ", msg_eq)
        } else {
          req_type = paste0(req_type, " and ", msg_col)
        }
      } else if(is_EQ){
        req_type = paste0(req_type, ", and ", msg_eq)
      }
    } else {
      if(is_COL){
        if(is_EQ){
          req_type = paste0(req_type, " ", msg_col, ", and ", msg_eq)
        } else {
          req_type = paste0(req_type, " ", msg_col)
        }
      } else if(is_EQ){
        req_type = paste0(req_type, " ", msg_eq)
      }
    }

    all_requested_types[i] = req_type
  }

  if(all(all_requested_types == "invalid")){
    stop_up(up = 2, "You must have at least one main class. None was found in '", type, "'. Please refer to the details, the examples or the vignette.")
  }

  if(missing(message) || is.null(message)){

    add_null = ifelse(grepl("(?i)null", type), "(nullable) ", "")
    if(IS_DOTS){
      msg_start = paste0(" In argument '...', each ", add_null, "element must be ")
    } else if(grepl("$", x_name, fixed = TRUE)){
      # This means it is a list element
      l_name = gsub("\\$.+", "", x_name)
      element = gsub(".+\\$", "", x_name)

      IS_VALUE = get("IS_VALUE", parent.frame())

      if(IS_VALUE && l_name == "dots"){
        msg_start = paste0("In '...', if provided, the argument '", element, "' must be ")
      } else {
        msg_start = paste0("In the list argument '", l_name, "', the ", add_null, "element '", element, "' must be ")
      }

      x_name = ""
    } else {
      the_null_argument = ifelse(nchar(add_null) > 0, " The (nullable) argument", " Argument")
      msg_start = paste0(the_null_argument, " '", x_name, "' must be ")
    }


    # dropping the invalid types
    all_types = all_types[all_requested_types != "invalid"]
    all_requested_types = all_requested_types[all_requested_types != "invalid"]

    if(length(all_requested_types) == 1){
      message = paste0(msg_start, all_requested_types, ".")
    } else {
      message = paste0(msg_start, "either: ", enumerate_items(all_requested_types, "enum.or"), ".")
    }

  }

  # readjusting the reasons
  all_reasons = all_reasons[nchar(all_reasons) > 0]

  if(length(all_types) == 1 || length(unique(all_reasons)) == 1){
    error_msg = paste0(all_reasons, ".")

  } else if(sum(all_main_types_ok) == 1){
    error_msg = paste0(all_main_types[all_main_types_ok], all_reasons[all_main_types_ok], ".")

  } else if(length(unique(all_main_types[all_main_types_ok])) == 1){
    error_msg = paste0(all_main_types[all_main_types_ok][1], enumerate_items(all_reasons[all_main_types_ok], "or.enum a"), ".")

  } else {

    end_line = gsub(".+ |\\)", "", all_reasons)
    if(all(grepl("^it is not", all_reasons)) && length(unique(end_line)) == 1){
      # We can shorten the message => more elegant
      start_msg = gsub(" \\(.+|, .+", "", all_reasons)
      end_msg = substr(all_reasons[1], nchar(start_msg[1]) + 1, nchar(all_reasons[1]))
      if(!grepl("\\(", end_msg)) end_msg = paste0(" (instead ", gsub("^, ", "", end_msg), ")")
      start_msg = unique(start_msg)

      error_msg = start_msg[1]
      n = length(start_msg)
      if(n > 1){
        # should be always greater than 1: just to be sure
        for(i in 2:n){
          if(i < n){
            error_msg = paste0(error_msg, gsub("it is not", ", not", start_msg[i]))
          } else {
            error_msg = paste0(error_msg, gsub("it is not", " nor", start_msg[i]))
          }
        }
      }

      error_msg = paste0(error_msg, end_msg, ".")

    } else {
      error_msg = paste0(enumerate_items(all_reasons, "or.enum"), ".")
    }
  }


  #
  # The call to stop
  #

  # The original call
  my_call = deparse(sys.calls()[[sys.nframe()-(up + 3)]])[1] # call can have svl lines
  nmax = 40
  if(nchar(my_call) > nmax) my_call = paste0(substr(my_call, 1, nmax-1), "...")

  if(grepl("^[^ ]", message)) message = paste0(" ", message)

  # The formatted message
  if(grepl("REASON", message)){
    msg_split = strsplit(message, " ?REASON ?")[[1]]
    full_msg = c(msg_split[1], error_msg, msg_split[-1])
    full_msg = paste(full_msg, collapse = " ")
  } else {
    full_msg = paste0(message, text_problem, error_msg)
  }


  stop("in ", my_call, ":\n", fit_screen(full_msg), call. = FALSE)

}


extract_curly = function(type, x, as.string = FALSE){
  # feed: gt{log(5)} => gives c(2, 3)
  # type = "scalarNumericgt{log(5)}lt{exp(3)}" ; x = "gt"

  value2parse = gsub(paste0("(?i).*", x, "\\{([^\\}]+)\\}.*"), "\\1", type)

  if(as.string){
    # we want the raw value
    res = value2parse
  } else {

    if(!grepl("(", value2parse, fixed = TRUE)){
      res = as.numeric(value2parse)
    } else {
      res = eval(parse(text = value2parse))
    }
  }

  res
}

extract_par = function(type, x, int = FALSE, item = "\\1"){
  # feed: len(2,3) => gives c(2, 3)
  # feed: class(ctest, bisou) => gives c("ctest", "bisou")

  value2parse = gsub(paste0("(?i).*", x, "\\(([^\\)]*)\\).*"), item, type)

  if(grepl(",", value2parse, fixed = TRUE)){
    value_split = strsplit(value2parse, ",", fixed = TRUE)[[1]]

    if(int){
      if(length(value_split) == 1){
        res = c(as.numeric(value_split), NA)
      } else {
        if(nchar(value_split[1]) == 0){
          res = c(NA, as.numeric(value_split[2]))
        } else {
          res = as.numeric(value_split)
        }
      }
    } else {
      if(any(grepl(" ", value_split, fixed = TRUE))){
        value_split = gsub("^ +| +$", "", value_split)
      }
      res = value_split
    }

  } else {
    if(int){
      if(value2parse == "data"){
        res = -2
      } else if(value2parse == "value"){
        res = -1
      } else {
        res = as.numeric(value2parse)
      }
    } else {
      if(grepl(" ", value2parse, fixed = TRUE)){
        res = gsub("^ +| +$", "", value2parse)
      } else {
        res = value2parse
      }
    }
  }

  res
}

extract_type = function(x){

  if(grepl("numeric", x, fixed = TRUE)){
    return("numeric")

  } else if(grepl("integer", x, fixed = TRUE)){
    return("integer")

  } else if(grepl("character", x, fixed = TRUE)){
    return("character")

  } else if(grepl("factor", x, fixed = TRUE)){
    return("factor")

  } else if(grepl("logical", x, fixed = TRUE)){
    if(grepl("loose", x, fixed = TRUE)){
      return("logical")
    } else if(!grepl("strict", x, fixed = TRUE)){
      return("logical")
    } else {
      return("strict_logical")
    }
  }
  return(NULL)
}

extract_multi_types = function(type, main_type){

  values = extract_par(type, main_type)

  for(i in seq_along(values)){
    if(grepl("(?i)character", values[i])){
      values[i] = "character"

    } else if(grepl("(?i)integer", values[i])){
      values[i] = "integer"

    } else if(grepl("(?i)numeric", values[i])){
      values[i] = "numeric"

    } else if(grepl("(?i)logical", values[i])){
      values[i] = "logical"
    }
  }


  values
}


extract_choices = function(x_name, type, .choices, sysOrigin){

  IS_CHARIN = grepl("(?i)charin", type)

  if(!is.null(.choices)){
    res = .choices
  } else if(IS_CHARIN){
    if(grepl("(?i)charin\\(", type)){
      res = extract_par(type, "charin")
    } else {
      res = paste0("In type '", type, "', the choices were not found. Either: i) provide the argument '.choices', or ii) include them in parentheses: e.g. match(choice1, choice2, etc).")
      stop_up(res, up = 3)
    }
  } else if(grepl("(?i)match\\(", type)){
    res = extract_par(type, "match")
  } else {
    formal.args = formals(sys.function(sysOrigin))

    fa = formal.args[[x_name]]

    if(missing(fa)){
      res = paste0("In type '", type, "', the choices were not found. Either: i) provide the argument '.choices', ii) include them in parentheses: e.g. match(choice1, choice2, etc), or iii) set the argument's default with the choices, e.g. ", x_name, " = c(\"choice1\", \"choice2\", etc).")
      stop_up(res, up = 3)
    } else {
      res = eval(fa, envir = sys.frame(sysOrigin))
    }
  }

  res
}


message_in_between = function(n_expected, code, .value, .data){
  # => message of what is expected

  res = NULL
  if(length(n_expected) == 1){

    if(n_expected == -2){
      # MEANS DATA

      if(!code %in% c("len", "nrow", "ncol")){
        stop_up(up = 3, "The keyword '", code, "(data)' is not valid. It can only be used for 'len', 'nrow', or 'ncol'.")
      }

      mc = get("mc", parent.frame(2))
      if(!".data" %in% names(mc)){
        # This is a big problem
        my_type = get("my_type", parent.frame())
        stop_up(up = 3, "To evaluate the '", code, "(data)' keyword in the type '", my_type, "', the argument '.data' MUST be provided. But it is currently missing.")
      }

      if(missing(.data)){
        N_TYPES = get("N_TYPES", parent.frame())
        if(N_TYPES == 1){
          stop_up(up = 3, "You have only one main class in which you use the keyword '", code, "(data)'. In such cases the argument '.data' must NEVER be missing, which is currently the case. This is a big problem, please revise your code such that it never happens.")
        }


        if(code == "len"){
          res = paste0("of the same length as argument '", deparse(mc[[".data"]]), "' (which is missing)")
        } else {
          row_col = switch(code, ncol = "column", nrow = "row")
          res = paste0("with the same number of ", row_col, "s as argument '", deparse(mc[[".data"]]), "' (which is missing)")
        }

        return(res)
      }

      if(code == "ncol"){
        n_expected = NCOL(.data)
      } else {
        n_expected = NROW(.data)
      }

      # The message
      if(code == "len"){
        if(is.null(dim(.data))){
          res = paste0("of the same length as argument '", deparse(mc[[".data"]]), "' (", n_expected, ")")
        } else {
          res = paste0("of the same number of observations as argument '", deparse(mc[[".data"]]), "' (", n_expected, ")")
        }

      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("with the same number of ", row_col, "s as argument '", deparse(mc[[".data"]]), "' (", n_expected, ")")

      }

    } else if(n_expected == -1){
      # MEANS VALUE

      if(missing(.value)){
        stop_up(up = 3, "To evaluate the '", code, "(value)' keyword in the type '", my_type, "', the argument '.value' MUST be provided. But it is currently missing.")
      } else if(is.list(.value)){
        if(!code %in% names(.value)){
          stop_up(up = 3, "The argument '.value', if a list, must have names equal to the keywords it is associated to. Here a value for ", code, " is missing.")
        }
        .value = .value[[code]]
      }

      if(!is.numeric(.value) || length(.value) > 1 || .value <= 0 || .value - floor(.value) != 0){
        stop_up(up = 3, "The argument '.value' must be either: i) a strictly positive integer scalar, or ii) a list of positive integers (that's currently not the case--no integers found).")
      }

      n_expected = .value

      if(code == "len"){
        res = paste0("with, in this context, a length equal to ", n_expected)

      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("with, in this context, ", n_expected, " ", row_col, plural(n_expected))

      } else if(code == "arg"){
        res = paste0("with, in this context, ", n_expected, " argument", plural(n_expected))

      } else if(code %in% c("left", "right")){
        res = paste0("with, in this context, ", n_expected, " part", plural(n_expected), " in the ", code, "-hand-side")

      }

    } else {
      # NORMAL CASE

      if(code == "len"){
        res = paste0("of length ", n_expected)

      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("with ", n_expected, " ", row_col, plural(n_expected))

      } else if(code == "arg"){
        res = paste0("with ", n_expected, " argument", plural(n_expected))

      } else if(code %in% c("left", "right")){
        res = paste0("with ", n_expected, " part", plural(n_expected), " in the ", code, "-hand-side")

      }

    }

  } else if(all(!is.na(n_expected))){

    if(code == "len"){
      res = paste0("of length in between ", n_expected[1], " and ", n_expected[2])

    } else if(code %in% c("nrow", "ncol")){
      row_col = switch(code, nrow = "rows", ncol = "columns")

      res = paste0("with a number of ", row_col, " in between ", n_expected[1], " and ", n_expected[2])

    } else if(code == "arg"){
      res = paste0("with a number of arguments in between ", n_expected[1], " and ", n_expected[2])

    } else if(code %in% c("left", "right")){
      res = paste0("with a number of parts in the ", code, "-hand-side between ", n_expected[1], " and ", n_expected[2])

    }

  } else if(!is.na(n_expected[1])){

    if(code == "len"){
      res = paste0("of length at least equal to ", n_expected[1])

    } else if(code %in% c("nrow", "ncol")){
      row_col = switch(code, nrow = "row", ncol = "column")

      res = paste0("with at least ", n_expected[1], " ", row_col, plural(n_expected[1]))

    } else if(code == "arg"){
      res = paste0("with at least ", n_expected[1], " argument", plural(n_expected[1]))

    } else if(code %in% c("left", "right")){
      res = paste0("with at least ", n_expected[1], " part", plural(n_expected[1]), " in the ", code, "-hand-side")

    }

  } else if(!is.na(n_expected[2])){

    if(code == "len"){
      res = paste0("of length not greater than ", n_expected[2])

    } else if(code %in% c("nrow", "ncol")){
      row_col = switch(code, nrow = "row", ncol = "column")

      res = paste0('with no more than ', n_expected[2], " ", row_col, plural(n_expected[2]))

    } else if(code == "arg"){
      res = paste0("with no more than ", n_expected[2], " argument", plural(n_expected[2]))

    } else if(code %in% c("left", "right")){
      res = paste0("with no more than ", n_expected[2], " part", plural(n_expected[2]), " in the ", code, "-hand-side")

    }

  } else {
    # check_arg error
    msg = switch(code, len = "length", nrow = "number of rows", ncol = "number of columns", arg = "number of arguments", left = "number of parts in the left-hand-side", right = "number of parts in the right-hand-side")

    end_msg = ifelse(code %in% c("len", "nrow", "ncol"), "v] __CODE__(data), or vi) __CODE__(value)", "or v) __CODE__(value)")

    res = gsub("_CODE_", code, paste0("Argument 'type' is malformed: in ", my_type, ", _CODE_() is not valid. It must be either: i] _CODE_(n1), ii] _CODE_(n1, n2), iii] _CODE_(n1, ), iv] _CODE_(, n2), ", end_msg, "; with n1 and n2 integers representing the ", msg, " the argument should have (case i] is equality)."))

    stop_up(up = 3, res)
  }

  res
}

error_in_between = function(n_x, n_expected, code, my_type, .value, .data){
  # => curated error message if lengths don't match
  # my_type is here only to form the final error message

  res = NULL
  if(length(n_expected) == 1){

    if(n_expected == -2){
      # MEANS DATA

      if(!code %in% c("len", "nrow", "ncol")){
        stop_up(up = 2, "The keyword '", code, "(data)' is not valid. It can only be used for 'len', 'nrow', or 'ncol'.")
      }

      mc = get("mc", parent.frame())
      if(!".data" %in% names(mc)){
        # This is a big problem
        stop_up(up = 2, "To evaluate the '", code, "' keyword in the type '", my_type, "', the argument '.data' MUST be provided. But it is currently missing.")
      }

      if(missing(.data)){
        if(code == "len"){
          res = paste0("it is of length ", n_x, " but the expected length is unknown (the reference data is missing)")
        } else {
          row_col = switch(code, ncol = "column", nrow = "row")
          res = paste0("it has ", n_x, " ", row_col, plural(n_x), " but the expected number of ", row_col, "s is unknown (the reference data is missing)")
        }

        return(res)
      }

      if(code == "ncol"){
        n_expected = NCOL(.data)
      } else {
        n_expected = NROW(.data)
      }

    } else if(n_expected == -1){
      # MEANS VALUE

      if(missing(.value)){
        stop_up(up = 2, "To evaluate the '", code, "(value)' keyword in the type '", my_type, "', the argument '.value' MUST be provided. But it is currently missing.")
      } else if(is.list(.value)){
        if(!code %in% names(.value)){
          stop_up(up = 2, "The argument '.value', if a list, must have names equal to the keywords it is associated to. Here a value for ", code, " is missing.")
        }
        .value = .value[[code]]
      }

      if(!is.numeric(.value) || length(.value) > 1 || .value <= 0 || .value - floor(.value) != 0){
        stop_up(up = 2, "The argument '.value' must be either: i) a strictly positive integer scalar, or ii) a list of positive integers (that's currently not the case--no integers found).")
      }

      n_expected = .value

    }

    if(n_expected != n_x){

      if(code == "len"){
        res = paste0("it is of length ", n_x, " instead of ", n_expected)
      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("it has ", n_x, " ", row_col, plural(n_x)," instead of ", n_expected)

      } else if(code == "arg"){
        res = paste0("it has ", n_x, " argument", plural(n_x), " instead of ", n_expected)

      } else if(code %in% c("right", "left")) {
        res = paste0("it has ", n_letter(n_x), " part", plural(n_x), " in the ", code, "-hand-side instead of ", n_letter(n_expected))

      }
    }

  } else if(length(n_expected) == 2 && !all(is.na(n_expected))){

    if(!is.na(n_expected[1]) && n_x < n_expected[1]){

      if(code == "len"){
        res = paste0("it is of length ", n_x, ", while it should be at least of length ", n_expected[1])

      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("it has ", n_x, " ", row_col, plural(n_x)," while it should have at least ", n_expected[1], " ", row_col, "s")

      } else if(code == "arg"){
        res = paste0("it has ", n_x, " argument", plural(n_x)," while it should have at least ", n_expected[1], " argument", plural(n_expected[1]))
      } else if(code %in% c("right", "left")) {
        res = paste0("it has ", n_x, " part", plural(n_x), " in the ", code, "-hand-side while it should have at least ", n_expected[1], " part", plural(n_expected[1]))

      }

    } else if(!is.na(n_expected[2]) && n_x > n_expected[2]){

      if(code == "len"){
        res = paste0("it is of length ", n_x, ", while it should have a maximum length of ", n_expected[2])

      } else if(code %in% c("nrow", "ncol")){
        row_col = switch(code, nrow = "row", ncol = "column")

        res = paste0("it has ", n_x, " ", row_col, plural(n_x)," while it should have a maximum of ", n_expected[2], " ", row_col, plural(n_expected[2]))

      } else if(code == "arg"){
        res = paste0("it has ", n_x, " argument", plural(n_x)," while it should have a maximum of ", n_expected[2], " argument", plural(n_expected[2]))
      } else if(code %in% c("right", "left")) {
        res = paste0("it has ", n_x, " part", plural(n_x), " in the ", code, "-hand-side while it should have a maximum of ", n_expected[2], " part", plural(n_expected[2]))

      }

    }
  } else {
    # check_arg error
    msg = switch(code, len = "length", nrow = "number of rows", ncol = "number of columns", arg = "number of arguments", left = "number of parts in the left-hand-side", right = "number of parts in the right-hand-side")

    end_msg = ifelse(code %in% c("len", "nrow", "ncol"), "v] __CODE__(data), or vi) __CODE__(value)", "or v) __CODE__(value)")

    res = gsub("_CODE_", code, paste0("Argument 'type' is malformed: in ", my_type, ", _CODE_() is not valid. It must be either: i] _CODE_(n1), ii] _CODE_(n1, n2), iii] _CODE_(n1, ), iv] _CODE_(, n2), ", end_msg, "; with n1 and n2 integers representing the ", msg, " the argument should have (case i] is equality)."))

    stop_up(up = 2, res)
  }

  res
}


inform_class = function(x, short = FALSE){

  its = ifelse(short, "it's", "it is")
  if(is.function(x)){
    res = paste0(its, " a function")
  } else if(is.matrix(x)){
    res = paste0(its, " a matrix")
  } else if(is.array(x)){
    res = paste0(its, " an array")
  } else if(is.data.frame(x)){
    res = paste0(its, " a data.frame")
  } else if(is.list(x)){
    res = paste0(its, " a list")
  } else if(is.atomic(x)){
    res = paste0(its, " a vector")
  } else {
    res = paste0("it is of class ", enumerate_items(class(x), quote = TRUE))
  }

  res
}

arg_name_header = function(x_name, problem = FALSE, nullable = FALSE){

  if(!grepl("__dots__", x_name, fixed = TRUE)){
    add_null = ifelse(nullable, "(nullable) ", "")
    res = paste0("The ", add_null, "argument '", x_name, "'")
  } else {
    n = extract_curly(x_name, "__dots__")

    # information on the name of the argument
    if(grepl("__dotnames__", x_name, fixed = TRUE)){
      info_name = extract_curly(x_name, "__dotnames__", as.string = TRUE)
      info_name = paste0(" (named '", info_name, "')")
    } else {
      info_name = ""
    }

    if(problem){
      res = paste0(" Problem in the ", n_th(n), " element", info_name, ": ")
    } else {
      res = paste0("In argument '...', the ", n_th(n), " element", info_name)
    }

  }

  res
}


#' Checks arguments and informs the user appropriately
#'
#' Full-fledged argument checking. Checks that the user provides arguments of the requested type (even complex) in a very simple way for the developer. Provides detailed and informative error messages for the user.
#'
#' @param .x An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .type A character string representing the requested type(s) of the arguments. This is a bit long so please look at the details section or the vignette for explanations. Each type is composed of one main class and restrictions (optional). Types can be separated with pipes (\code{|}). The main classes are: i) \code{"scalar"} for scalars, i.e. vectors of length one, ii) \code{"vector"}, iii) \code{"matrix"}, iv) \code{"data.frame"}, v) \code{"list"}, vi) \code{formula}, vii) \code{function}, viii) \code{charin}, i.e. a character string in a set of choices, viii) \code{"match"}, i.e. a character scalar that should partially match a vector of choices, x) \code{"class(my_class1, my_class2)"}, i.e. an object whose class is any of the ones in parentheses, xi) \code{"NA"}, something identical to \code{NA}.
#' You can then add optional restrictions: 1) \code{len(a, b)}, i.e. the object should be of length between \code{a} and \code{b} (you can leave \code{a} or \code{b} missing, \code{len(a)} means length *equal* to \code{a}), \code{len(data)} and \code{len(value)} are also possible (see details), 2) \code{nrow(a,b)} or \code{ncol(a,b)} to specify the expected number of rows or columns, 3) \code{arg(a,b)}, only for functions, to retrict the number of arguments, 4) \code{"na ok"} to allow the object to have NAs (for "scalar", "vector", "matrix" types), or \code{"no na"} to restrict the object to have no NA (for "data.frame" only), 5) \code{GE}, \code{GT}, \code{LE} and \code{LT}: for numeric scalars/vectors/matrices, \code{GE{expr}} restrics the object to have only values striclty greater than (greater or equal/strictly lower than/lower or equal) the value in curly brackets, 6) e.g. \code{scalar(type1, type2)}, for scalars/vectors/matrices you can restrict the type of the object by adding the expected type in parentheses: should it be numeric, logical, etc.
#' @param .x1 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x2 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x3 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x4 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x5 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x6 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x7 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x8 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param .x9 An argument to be checked. Must be an argument name. Can also be the type, see details/examples.
#' @param ... Only used to check \code{'...'} (dot-dot-dot) arguments.
#' @param .message A character string, optional. By default, if the user provides a wrong argument, the error message stating what type of argument is required is automatically formed. You can alternatively provide your own error message, maybe more tailored to your function. The reason of why there is a problem is appended in the end of the message.
#' @param .choices Only if one of the types (in argument \code{type}) is \code{"match"}. The values the argument can take. Note that even if the \code{type} is \code{"match"}, this argument is optional since you have other ways to declare the choices.
#' @param .data Must be a data.frame, a list or a vector. Used in three situations. 1) if the global keywords \code{eval} or \code{evalset} are present: the argument will also be evaluated in the data (i.e. the argument can be a variable name of the data set). 2) if the argument is expected to be a formula and \code{var(data)} is included in the type: then the formula will be expected to contain variables from \code{.data}. 3) if the keywords \code{len(data)}, \code{nrow(data)} or \code{ncol(data)} are requested, then the required length, number of rows/columns, will be based on the data provided in \code{.data}.
#' @param .value An integer scalar or a named list of integers scalars. Used when the keyword \code{value} is present (like for instance in \code{len(value)}). If several values are to be provided, then it must be a named list with names equal to the codes: for instance if \code{nrow(value)} and \code{ncol(value)} are both present in the type, you can use (numbers are an example) \code{.value = list(nrow = 5, ncol = 6)}. See Section IV) in the examples.
#' @param .arg_name A character scalar. If \code{.message} is not provided, an automatic error message will be generated using \code{.arg_name} as the argument name.
#' @param .env An environment defaults to the frame where the user called the original function. Only used in two situations. 1) if the global keywords \code{eval} or \code{evalset} are present: the argument will also be evaluated in this environment. 2) if the argument is expected to be a formula and \code{var(env)} is included in the type: then the formula will be expected to contain variables existing in \code{.env}.
#' @param .up Integer, default is 0. If the user provides a wrong argument, the error message will integrate the call of the function from which \code{check_arg} has been called. If \code{check_arg} is  called in a non-user level sub function of a main user-level function, then use \code{.up = 1} to make the error message look like it occured in the main function (and not in the sub function). Of course you can have values higher than 1.
#' @param .call_up Deprecated, only kept for retro-compatibility. Use argument \code{.up} instead.
#'
#' @section How to form a type:
#'
#' To write the expected type of an argument, you need to write the main class in combination with the class's options and restrictions (if any).
#'
#' The syntax is: \code{"main_class option(s) restriction(s)"}
#'
#' A type MUST have at least one main class. For example: in the type \code{"logical vector len(,2) NA OK"}, \code{vector} is the main class, \code{NA OK} is the option, and \code{logical} and \code{len(,2)} are restrictions
#'
#' There are 13 main classes that can be checked. On the left the keyword, on the right what is expected from the argument, and in square brackets the related section in the examples:
#' \itemize{
#' \item \code{scalar}: an atomic vector of length 1 [Section I)]
#' \item \code{vector}: an atomic vector [Section IV)]
#' \item \code{matrix}: a matrix [Section IV)]
#' \item \code{vmatrix}: a matrix or vector [Section IV)]
#' \item \code{data.frame}: a data.frame [Section VI)]
#' \item \code{vdata.frame}: a data.frame or vector [Section VI)]
#' \item \code{list}: a list [Section V)]
#' \item \code{formula}: a formula [Section VIII)]
#' \item \code{function}: a function [Section V)]
#' \item \code{charin}: a character vector with values in a vector of choices [Section III)]
#' \item \code{match}: a character vector with values in a vector of choices, partial matching enabled and only available in \code{check_arg_plus} [Section III)]
#' \item \code{class}: a custom class [Section VI)]
#' \item \code{NA}: a vector of length 1 equal to NA--does not support options nor restrictions, usually combined with other main classes (see Section on combining multiple types) [Section VI)]
#' }
#'
#' There are seven type options, they are not available for each types. Here what they do and the types to which they are associated:
#' \itemize{
#' \item \code{NA OK} (or \code{NAOK}): Tolerates the presence of NA values. Available for \code{scalar}, \code{vector}, \code{matrix}, \code{vmatrix}.
#' \item \code{NO NA} (or \code{NONA}): Throws an error if NAs are present. Available for \code{data.frame}, \code{vdata.frame}.
#' \item \code{square}: Enforces the matrix to be square. Available for \code{matrix}, \code{vmatrix}.
#' \item \code{named}: Enforces the object to have names. Available for \code{vector}, \code{list}.
#' \item \code{multi}: Allows multiple matches. Available for \code{charin}, \code{match}.
#' \item \code{strict}: Makes the matching case-sensitive. Available for \code{match}.
#' \item \code{os} and \code{ts}: Available for \code{formula}. Option \code{os} (resp. \code{ts}) enforces that the formula is one-sided (resp. two-sided).
#' }
#'
#' You can further add restrictions. There are roughly six types of restrictions. Here what they do and the types to which they are associated:
#' \itemize{
#' \item sub-type restriction: For atomic types (\code{scalar}, \code{vector}, \code{matrix} or \code{vmatrix}), you can restrict the underlying data to be of a specific sub-type. The simple sub-types are: i) \code{integer} (numeric without decimals and logicals), i') \code{strict integer} (numeric that can be converted to integer with \code{as.integer}, and not logicals), ii) \code{numeric}, iii) \code{factor}, iv) \code{logical} (a logical or a numeric equal to 0 or 1) and iv') \code{strict logical} (a logical only). Simply add the sub-type in the type string (e.g. \code{"integer scalar"}), or if you allow multiple types, put them in parentheses rigth after the main class: e.g. \code{"scalar(character, integer)"}. See Section XI) in the examples. See also the section below for more information on the sub-types. Some types (\code{character}, \code{integer}, \code{numeric}, \code{logical} and \code{factor}) also support the keyword \code{"conv"} in \code{check_arg_plus}.
#' \item \code{GE}/\code{GT}/\code{LE}/\code{LT}: For atomic types with numeric data, you can check the values in the object. The GE/GT/LE/LT mean respectively greater or equal/greater than/lower or equal/lower than. The syntax is \code{GE{expr}}, with expr any expression. See Section IV) in the examples.
#' \item \code{len(a, b)}: You can restrict the length of objects with \code{len(a, b)} (with \code{a} and \code{b} integers). Available for \code{vector} and \code{list}. Then the length must be in between \code{a} and \code{b}. Either \code{a} or \code{b} can be missing which means absence of restriction. If \code{len(a)}, this means must be equal to \code{a}. You can also use the keywords len(data) which ensures that the length is the same as the length of the object given in the argument \code{.data}, or \code{len(value)} which ensures the length is equal to the value given in \code{.value}. See Section IV) in the examples.
#' \item \code{nrow(a, b)}, \code{ncol(a, b)}: To restrict the number of rows and columns. Available for \code{matrix}, \code{vmatrix}, \code{data.frame}, \code{vdata.frame}. Tolerates the \code{data} and \code{value} keywords (see in \code{len}). See Section IV) in the examples.
#' \item var(data, env): Available only for \code{formula}. \code{var(data)} ensures that the variables in the formula are present in the data set given by the extra argument \code{.data}. \code{var(env)} ensures they are present in the environment, and \code{var(data, env)} in either the environment or the data set. See Section VIII) in the examples.
#' \item \code{arg(a, b)}: Available only for \code{function}. Ensures that the function has a number of arguments between \code{a} and \code{b}, both integers (possibly missing). Tolerates the \code{value} keyword (see in \code{len}). See Section V) in the examples.
#' \item \code{left(a, b)} and \code{right(a, b)}: Only available for \code{formula}. Restricts the number of parts in the left-hand-side or in the right-hand-side of the formula. Tolerates the \code{value} keyword (see in \code{len}). See Section VIII) in the examples.
#' }
#'
#'
#'
#' @section Global keywords:
#'
#' There are eight global keywords that can be placed anywhere in the type. They are described in Section II) in the examples.
#' \itemize{
#' \item \code{NULL}: allows the argument to be equal to \code{NULL}.
#' \item \code{safe NULL}: allows the argument to be equal to \code{NULL}, but an error is thrown if the argument is of the type \code{base$variable} or \code{base[["variable"]]}. This is to prevent oversights from the user, especially useful when the main class is a vector.
#' \item \code{NULL{expr}}: allows the argument to be equal to \code{NULL}, if the argument is \code{NULL}, then it assigns the value of expr to the argument.
#' \item \code{MBT}: (means "must be there") an error is thrown if the argument is not provided by the user.
#' \item \code{L0}: allows 0-length vectors--overrides the default which requires that any argument should have a positive length
#' \item \code{eval}: used in combination with the extra argument \code{.data}. Evaluates the value of the argument both in the data set and in the environment (this means the argument can be a variable name).
#' \item \code{evalset}: like \code{eval}, but after evaluation, assigns the obtained value to the argument. Only available in \code{check_arg_plus}.
#' \item \code{dotnames}: only when checking \code{'...'} argument (see the related section below). Enforces that each object in \code{'...'} has a name.
#' }
#'
#'
#'
#' @section The \code{match} and \code{charin} types:
#'
#' The main classes \code{match} and \code{charin} are similar to \code{\link[base]{match.arg}}. These two types are detailed in the examples Section III).
#'
#' By default, the main class \code{match} expects a single character string whose value is in a set of choices. By default, there is no case sensitity (which can be turned on with the option \code{strict}) and there is always partial matching. It can expect a vector (instead of a single element) if the option \code{multi} is present.
#'
#' You have three different ways to set the choices:
#' \itemize{
#' \item by setting the argument default: e.g. \code{fun = function(x = c("Tom", "John")) check_arg(x, "match")}
#' \item by providing the argument \code{.choices}: e.g. \code{fun = function(x) check_arg(x, "match", .choices = c("Tom", "John"))}
#' \item by writing the choices in parentheses: e.g. \code{fun = function(x) check_arg(x, "match(Tom, John)")}
#' }
#'
#' When the user doesn't provide the argument, the default is set to the first choice.
#' Since the main class \code{match} performs a re-assignment of the variable, it is only available in \code{check_arg_plus}.
#'
#' The main class \code{charin} is similar to \code{match} in that it expects a single character string in a set of choices. The main differences are: i) there is no partial matching, ii) the choices cannot be set by setting the argument default, and iii) its checking can be turned off with setDreamer_check(FALSE) [that's the main difference between \code{check_arg} and \code{check_arg_plus}].
#'
#'
#'
#' @section Combining multiple types:
#'
#' You can combine multiple types with a pipe: '|'. The syntax is as follows:
#'
#' \code{"main_type option(x) restriction(s) | main_type option(x) restriction(s) | main_type option(x) restriction(s)"}
#'
#' You can combine as many types as you want. The behavior is as follows: if the argument matches any of the types, then that's fine.
#'
#' For example, say you require an argument to be either a logical scalar, either a data.frame, then you can write: \code{check_arg(x, "logical scalar | data.frame")}. See Section X) in the examples for a more complex example.
#'
#'
#'
#' @section Tips on the type:
#'
#' The type MUST be a character string of length 1. Two main classes must be separated by a pipe. Otherwise the order of the keywords, the spaces, or the case don't matter. Further the global keywords can be placed anywhere and need not be separated by a pipe.
#'
#' Note that a rare but problematic situation is when you set a default with the global \code{NULL{default}} and that default contains a keyword. For example in the type \code{"NULL{list()} numeric matrix"} \code{list} should not be considered as a main class, but only \code{matrix}. To be on the safe side, then just separate them with a pipe: \code{"NULL{list()} | numeric matrix"} would work appropriately.
#'
#'
#'
#' @section Checking multiple arguments:
#'
#' You can check multiple arguments at once provided they are of the same type. Say variables \code{x1} to \code{x5} should be single logicals. Just use: \code{check_arg(x1, x2, x3, x4, x5, "logical scalar")}. It is always more efficient to check multiple arguments of the same type \emph{at once}.
#'
#' It is important to note that in case of multiple arguments, you can place the type anywhere you want provided it is a character literal (and not in a variable!). This means that \code{check_arg("logical scalar", x1, x2, x3, x4, x5)} would also work.
#'
#' If your type is in a variable, then you must explicitly provide the argument \code{.type} (like in \code{check_arg(x, .type = my_type)}).
#'
#'
#'
#' @section Nesting argument checking (\code{.up}):
#'
#' When you develop several functions that share common features, it is usually good practice to pool the common computations into an internal function (to avoid code duplication).
#'
#' When you do so, you can do all the argument checking in the internal function. Then use the argument \code{.up = 1} so that if the user provdes a wrong argument, the error message will refer to the user-level function and NOT to the internal function, making it much clearer for the user.
#'
#' This is detailed in Section XII) in the examples.
#'
#'
#'
#' @section Checking the \code{...} (dot-dot-dot) argument:
#'
#' \code{check_arg} offers the possibility to check the \code{...}, provided each expected object in \code{...} should be of the same type. To do that, just add \code{...} as the first argument in \code{check_arg}, that's it! For example, you want all elements of \code{...} to be numeric vectors, then use \code{check_arg(..., "numeric vector")}.
#'
#' When checking \code{...}, you have the special global argument \code{dotnames} which enforces that each element in \code{...} has a name. Further, the other global \code{MBT} (must be there) now means that at least one element in \code{...} must be provided.
#'
#' This is detailed in Section XIV) in the examples.
#'
#'
#'
#' @section What's the difference between \code{check_arg} and \code{check_arg_plus}?:
#'
#' The function \code{check_arg_plus} extends \code{check_arg} in several ways. First it offers new keywords:
#' \itemize{
#' \item \code{evalset}: evaluates the argument in a data set (i.e. the argument can be variables names of a data set), then re-assigns back its value.
#' \item \code{NULL{default}}: if the argument is \code{NULL}, then the value in curly brackets is assigned to the argument.
#' \item \code{match}: if the argument partially matches the choices, then the matches are assigned to the argument.
#' \item \code{conv}: in atomic main classes (\code{scalar}, \code{vector} and \code{matrix}), the data can be converted to a given sub-type (currently \code{integer}, \code{numeric}, \code{logical}, \code{character} and \code{factor}), then assigned back to the argument.
#' }
#'
#' As you can see, it's all about assignment: these special keywords of \code{check_arg_plus} will modify the arguments \emph{in place}. You have such examples in Section II), III) and XI) of the examples.
#'
#' Second, it allows to check arguments that are themselves list of arguments (note that \code{conv} also works in that case). For example, one argument of your function is \code{plot.opts}, a list of arguments to be passed to plot. You can check the elements of \code{plot.opts} (e.g. \code{plot.opts$main}) with \code{check_arg_plus}. It also re-assigns the values of the list given the special keywords just described. List element checking is described in Section XIII) of the examples.
#'
#' Then why creating two functions? If the user runs a function in which the arguments were checked with \code{check_arg} and it works, then argument checking can be safely disabled, and it would also work. On the other hand, since \code{check_arg_plus} does value re-assignment, it cannot be safely turned-off--therefore cannot be disabled with \code{\link[dreamerr]{setDreamerr_check}}. Distinguishing between the two allows the user to disable argument checking and gain (although very modest) perfomance in large loops. Therefore, when you create functions, I suggest to use always \code{check_arg}, unless you need the extra features of \code{check_arg_plus}.
#'
#'
#'
#' @section \code{check_value}:
#'
#' The functions \code{check_value} and \code{check_value_plus} are almost identical to the respective functions \code{check_arg} and \code{check_arg_plus}. The key differences are as follows:
#'
#' \itemize{
#' \item They can check values instead of arguments. Indeed, if you try to check a value with \code{check_arg}, nothing will happen (provided the name of the value is not an argument). Why? Because it will consider it as a missing argument. Therefore, you are can check anything with \code{check_value}.
#' \item You can check only one item at a time (whereas you can check up to 10 arguments in \code{check_arg}).
#' }
#'
#' The main reason for using \code{check_value} is that sometimes you only know if an argument is valid after having perfomed some modifications on it. For instance, the argument may be a formula, but you also require that the variables in the formula are numeric. You cannot check all that at once with \code{check_arg}, but you can first check the formula with it, then extract the values from the formula and use \code{check_value} to ensure that the variables from the formula are numeric.
#'
#' \code{check_value} is detailed in Section XVI) in the examples.
#'
#'
#' @section Disabling argument checking:
#'
#' Although the argument checking offered by \code{check_arg} is highly optimized and fast (it depends on the type [and your computer], but it is roughly of the order of 80 micro seconds for non-missing arguments, 20 micro seconds for missing arguments), you may want to disable it for small functions in large loops (>100K iterations although this practice is not really common in R). If so, just use the function \code{\link[dreamerr]{setDreamerr_check}}, by typing \code{setDreamerr_check(FALSE)}. This will disable any call to \code{check_arg}.
#'
#' Note that the argument checking of \code{check_arg_plus} cannot be disabled because the special types it allows perform reassignment in the upper frame. That's the main difference with \code{check_arg}.
#'
#'
#'
#' @section The developer mode:
#'
#' If you're new to check_arg, given the many types available, it's very common to make mistakes when creating check_arg calls. But no worry, the developer mode is here to help!
#'
#' The developer mode ensures that any problematic call is spotted and the problem is clearly stated. It also refers to the related section in the examples if appropriate. To turn the developer mode on, use \code{setDreamerr_dev.mode(TRUE)}.
#'
#' Note that since this mode ensures a detailed cheking of the call it is thus a strain on performance and should be always turned off otherwise needed. See Section XV) in the examples.
#'
#' @author
#' Laurent Berge
#'
#'
#' @return
#' In case the \code{type} is \code{"match"}, it returns the matched value. In any other case, \code{NULL} is returned.
#'
#' @examples
#'
#' # check_arg is only used within functions
#'
#' #
#' # I) Example for the main class "scalar"
#' #
#'
#' test_scalar = function(xlog, xnum, xint, xnumlt, xdate){
#'   # when forming the type: you can see that case, order and spaces don't matter
#'   check_arg(xlog, "scalarLogical")
#'   check_arg(xnum, "numeric scalar")
#'   check_arg(xint, "  scalar Integer GE{0}  ")
#'   check_arg(xnumlt, "numeric scalar lt{0.15}")
#'
#'   # Below it is critical that there's no space between scalar and the parenthesis
#'   check_arg(xdate, "scalar(Date)")
#'   invisible(NULL)
#' }
#'
#' # Following should be OK
#' test_scalar()
#' test_scalar(xlog = FALSE, xnum = 55, xint = 5, xnumlt = 0.11, xdate = Sys.Date())
#' test_scalar(xlog = 1) # OK
#'
#' #
#' # Now errors, all the following are wrong arguments, leading to errors
#' # Please note the details in the error messages.
#'
#' # logical
#' try(test_scalar(xlog = NA))
#' try(test_scalar(xlog = 2))
#' try(test_scalar(xlog = sum))
#' try(test_scalar(xlog = pi))
#' try(test_scalar(xlog = faefeaf5))
#' try(test_scalar(xlog = c(TRUE, FALSE)))
#' try(test_scalar(xlog = c()))
#'
#' # numeric
#' try(test_scalar(xnum = NA))
#' try(test_scalar(xnum = 1:5))
#' try(test_scalar(xnum = Sys.Date()))
#'
#' # integer
#' try(test_scalar(xint = 5.5))
#' try(test_scalar(xint = -1))
#'
#' # num < 0.15
#' try(test_scalar(xnumlt = 0.15))
#' try(test_scalar(xnumlt = 0.16))
#' try(test_scalar(xnumlt = Sys.Date()))
#'
#' # Date
#' try(test_scalar(xdate = 0.15))
#'
#'
#' #
#' # II) Examples for the globals: NULL, L0, MBT, eval, evalset
#' #
#'
#' test_globals = function(xnum, xlog = TRUE, xint){
#'
#'   # Default setting with NULL is only available in check_arg_plus
#'   # MBT (must be there) throws an error if the user doesn't provide the argument
#'   check_arg_plus(xnum, "numeric vector NULL{1} MBT")
#'
#'   # NULL allows NULL values
#'   check_arg(xlog, "logical scalar safe NULL")
#'
#'   # use L0 to accept length-0 objects
#'   check_arg(xint, "integer vector L0")
#'
#'   list(xnum = xnum, xlog = xlog)
#' }
#'
#' # xnum is required because of MBT option
#' try(test_globals())
#'
#' # NULL{expr} sets the value of xnum to expr if xnum = NULL
#' # Here NULL{1} sets it to 1
#' test_globals(xnum = NULL)
#'
#' # NULL (not NULL{expr}) does not reassign
#' test_globals(xnum = NULL, xlog = NULL)
#'
#' # safe NULL: doesn't accept NULL from data.frame (DF) subselection
#' # ex: the variable 'log' does not exist in the iris DF
#' try(test_globals(5, xlog = iris$log))
#' # but xnum accepts it
#' test_globals(iris$log)
#'
#' # L0 means not NULL, 0-length vectors are OK
#' # 0-length is OK for xint:
#' test_globals(xnum = 2, xint = integer(0))
#' # L0 still checks the type:
#' try(test_globals(2, xint = numeric(0)))
#'
#' #
#' # eval and evalset
#' #
#'
#' test_eval = function(x1, x2, data = list(), i = c()){
#'   check_arg(x1, "eval numeric vector", .data = data)
#'
#'   # evalset is in check_arg_plus
#'   check_arg_plus(x2, "evalset numeric vector", .data = data)
#'
#'  # We show the variables
#'  if(1 %in% i){
#'    cat("x1:\n")
#'    print(as.character(try(x1, silent = TRUE)))
#'  }
#'
#'  if(2 %in% i){
#'    cat("x2:\n")
#'    print(as.character(try(x2, silent = TRUE)))
#'  }
#'
#' }
#'
#' # eval: evaluates the argument both in the environment and the data
#' test_eval(x1 = Sepal.Length, data = iris) # OK
#' # if we use a variable not in the environment nor in the data => error
#' try(test_eval(x1 = Sopal.Length, data = iris))
#'
#' # but eval doesn't reassign back the value of the argument:
#' test_eval(x1 = Sepal.Length, data = iris, i = 1)
#'
#' # evaset does the same as eval, but also reasssigns the value obtained:
#' test_eval(x2 = Sepal.Length, data = iris, i = 2)
#'
#'
#' #
#' # III) Match and charin
#' #
#'
#' # match  => does partial matching, only available in check_arg_plus
#' # charin => no partial matching, exact values required, but in check_arg
#'
#' #
#' # match
#' #
#'
#' # Note the three different ways to provide the choices
#' #
#' # If the argument has no default, it is kept that way (see x2)
#' # If the argument is not provided by the user,
#' #  it is left untouched (see x3)
#'
#' test_match = function(x1 = c("bonjour", "Au revoir"), x2, x3 = "test"){
#'   # 1) choices set thanks to the argument default (like in match.arg)
#'   check_arg_plus(x1, "strict match")
#'
#'   # 2) choices set with the argument .choices
#'   check_arg_plus(x2, "match", .choices = c("Sarah", "Santa", "Santa Fe", "SANTA"))
#'
#'   # 3) choices set with the parentheses
#'   check_arg_plus(x3, "multi match(Orange, Juice, Good)")
#'
#'   cat("x1:", x1, "\nx2:", ifelse(missing(x2), "[missing]", x2), "\nx3:", x3, "\n")
#' }
#'
#' # Everything below is OK
#' test_match()
#' test_match(x1 = "Au", x2 = "sar", x3 = c("GOOD", "or"))
#' test_match(x2 = "Santa")
#'
#' # Errors caught:
#' try(test_match(x1 = c("Au", "revoir")))
#' try(test_match(x1 = "au"))
#' try(test_match(x1 = sum))
#' try(test_match(x1 = list(a = 1:5)))
#'
#' try(test_match(x2 = "san"))
#' try(test_match(x2 = "santa"))
#'
#' # Same value as x3's default, but now provided by the user
#' try(test_match(x3 = "test"))
#' try(test_match(x3 = c("or", "ju", "bad")))
#'
#' # You can check multiple arguments at once
#' # [see details for multiple arguments in Section X)]
#' # Note that now the choices must be set in the argument
#' #  and they must have the same options (ie multi, strict)
#'
#' test_match_multi = function(x1 = c("bonjour", "Au revoir"), x2 = c("Sarah", "Santa"),
#'                             x3 = c("Orange", "Juice", "Good")){
#'
#'   # multiple arguments at once
#'   check_arg_plus(x1, x2, x3, "match")
#'
#'   cat("x1:", x1, "\nx2:", x2, "\nx3:", x3, "\n")
#' }
#'
#' test_match_multi()
#'
#' #
#' # charin
#' #
#'
#' # charin is similar to match but requires the user to provide the exact value
#' # only the multi option is available
#'
#' test_charin = function(x1 = "bonjour", x2  = "Sarah"){
#'
#'   # 1) set the choices with .choices
#'   check_arg(x1, "charin", .choices = c("bonjour", "au revoir"))
#'
#'   # 2) set the choices with the parentheses
#'   check_arg(x2, "multi charin(Sarah, Santa, Santa Fe)")
#'
#'   cat("x1:", x1, "\nx2:", x2, "\n")
#' }
#'
#' # Now we need the exact values
#' test_charin("au revoir", c("Santa", "Santa Fe"))
#'
#' # Errors when partial matching tried
#' try(test_charin("au re"))
#'
#'
#' #
#' # IV) Vectors and marices, equalities, dimensions and lengths
#' #
#'
#' # You can restrict the length of objects with len(a, b)
#' #    - if len(a, b) length must be in between a and b
#' #    - if len(a, ) length must be at least a
#' #    - if len(, b) length must be at most b
#' #    - if len(a) length must be equal to a
#' # You can also use the special keywords len(data) or len(value),
#' #  but then the argument .data or .value must also be provided.
#' #  (the related example comes later)
#' #
#' # You can restrict the number of rows/columns with nrow(a, b) and ncol(a, b)
#' #
#' # You can restrict a matrix to be square with the 'square' keyword
#' #
#' # You can restrict the values an element can take with GE/GT/LE/LT,
#' # respectively greater or equal/greater than/lower or equal/lower than
#' # The syntax is GE{expr}, with expr any expression
#' # Of course, it only works for numeric values
#' #
#' # You can tolerate NA values (default is no tolerance in vector/matrices)
#' # with the keyword 'NAOK' or 'NA OK'
#' #
#'
#' test_vmat = function(xvec, xmat, xvmat, xstmat, xnamed){
#'   # vector of integers with values between 5 and exp(3)
#'   check_arg(xvec, "integer Vector GE{5} LT{exp(3)}")
#'
#'   # logical matrix with at least two rows and with 3 columns
#'   check_arg(xmat, "logicalMatrix NROW(2,) NCOL(3)")
#'
#'   # vector or matrix (vmatrix) of integers or character strings
#'   # with at most 3 observations, NAs are OK
#'   check_arg(xvmat, "vmatrix(character, integer) nrow(,3) naok")
#'
#'   # square matrix of logicals, the values must be TRUE/FALSE,
#'   # i.e. values equal to 0 or 1 are not OK
#'   check_arg(xstmat, "strict Logical square Matrix")
#'
#'   # A vector with names of length 2
#'   check_arg(xnamed, "named Vector len(2)")
#'   invisible(NULL)
#' }
#'
#' # OK
#' test_vmat(xvec = 5:20, xmat = matrix(TRUE, 3, 3), xvmat = c(NA, 4, 3),
#'           xstmat = matrix(FALSE, 2, 2), xnamed = c(bon=1, jour=2))
#'
#' # Vector checks:
#' try(test_vmat(xvec = c(NA, 2:5)))
#' try(test_vmat(xvec = 2))
#' try(test_vmat(xvec = 21))
#' try(test_vmat(xvec = 5.5))
#'
#' # Matrix checks:
#' try(test_vmat(xmat = matrix(1, 3, 4)))
#' try(test_vmat(xmat = matrix(2, 3, 3)))
#' try(test_vmat(xmat = matrix(1, 1, 3)))
#' try(test_vmat(xmat = iris))
#'
#' try(test_vmat(xvmat = iris))
#'
#' try(test_vmat(xstmat = matrix(1, 1, 3)))
#' try(test_vmat(xstmat = matrix(1, 3, 3)))
#' try(test_vmat(xstmat = matrix(c(TRUE, FALSE, NA), 3, 3)))
#'
#' # Named vector checks:
#' try(test_vmat(xnamed = 1:3))
#' try(test_vmat(xnamed = c(bon=1, jour=2, les=3)))
#'
#' #
#' # Illustration of the keywords 'data', 'value'
#' #
#'
#' # 'value'
#' # Matrix multiplication X * Y * Z
#' test_dynamic_restriction = function(x, y, z){
#'   check_arg(x, "mbt numeric matrix")
#'   check_arg(y, "mbt numeric matrix nrow(value)", .value = ncol(x))
#'   check_arg(z, "mbt numeric matrix nrow(value)", .value = ncol(y))
#'
#'   # An alternative to the previous two lines:
#'   # check_arg(z, "mbt numeric matrix")
#'   # check_arg(y, "mbt numeric matrix nrow(value) ncol(value)",
#'   #           .value = list(nrow = ncol(x), ncol = nrow(z)))
#'
#'   x %*% y %*% z
#' }
#'
#' x = matrix(1, 2, 3)
#' y = matrix(2, 3, 5)
#' z = matrix(rnorm(10), 5,  2)
#'
#' test_dynamic_restriction(x, y, z)
#'
#' # Now error
#' try(test_dynamic_restriction(x, matrix(5, 1, 2), z))
#'
#' # 'data'
#' # Computing maximum difference between two matrices
#' test_dynamic_bis = function(x, y){
#'   check_arg(x, "mbt numeric matrix")
#'   # we require y to be of the same dimension as x
#'   check_arg(y, "mbt numeric matrix nrow(data) ncol(data)", .data = x)
#'
#'   max(abs(x - y))
#' }
#'
#' test_dynamic_bis(x, x)
#'
#' # Now error
#' try(test_dynamic_bis(x, y))
#'
#'
#' #
#' # V) Functions and lists
#' #
#'
#' # You can restrict the number of arguments of a
#' # function with arg(a, b) [see Section IV) for details]
#'
#' test_funlist = function(xfun, xlist){
#'   check_arg(xfun, "function arg(1,2)")
#'   check_arg(xlist, "list len(,3)")
#'   invisible(NULL)
#' }
#'
#' # OK
#' test_funlist(xfun = sum, xlist = iris[c(1,2)])
#'
#' # function checks:
#' try(test_funlist(xfun = function(x, y, z) x + y + z))
#'
#' # list checks:
#' try(test_funlist(xlist = iris[1:4]))
#' try(test_funlist(xlist = list()))
#'
#'
#' #
#' # VI) Data.frame and custom class
#' #
#'
#' test_df = function(xdf, xvdf, xcustom){
#'   # data.frame with at least 100 observations
#'   check_arg(xdf, "data.frame nrow(100,)")
#'
#'   # data.frame or vector (vdata.frame)
#'   check_arg(xvdf, "vdata.frame")
#'
#'   # Either: i) object of class glm or lm
#'   # ii) NA
#'   # iii) NULL
#'   check_arg(xcustom, "class(lm, glm)|NA|null")
#'   invisible(NULL)
#' }
#'
#' # OK
#' m = lm(Sepal.Length~Species, iris)
#' test_df(xdf = iris, xcustom = m)
#' test_df(xvdf = iris$Sepal.Length)
#' test_df(xcustom = NULL)
#'
#' # data.frame checks:
#' try(test_df(xdf = iris[1:50,]))
#' try(test_df(xdf = iris[integer(0)]))
#' try(test_df(xdf = iris$Sepal.Length))
#' # Note that the following works:
#' test_df(xvdf = iris$Sepal.Length)
#'
#' # Custom class checks:
#' try(test_df(xcustom = iris))
#'
#' #
#' # VIII) Formulas
#' #
#'
#' # The keyword is 'formula'
#' # You can restrict the formula to be:
#' # - one sided with 'os'
#' # - two sided with 'ts'
#' #
#' # You can restrict that the variables of a forumula must be in
#' # a data set or in the environment with var(data, env)
#' # - var(data) => variables must be in the data set
#' # - var(env) => variables must be in the environment
#' # - var(data, env) => variables must be in the data set or in the environment
#' # Of course, if var(data), you must provide a data set
#' #
#' # Checking multipart formulas is included. You can use left(a, b)
#' #  and right(a, b) to put restrictions in the number of parts allowed
#' #  in the left and right-hand-sides
#' #
#'
#' test_formulas = function(fml1, fml2, fml3, fml4, data = iris){
#'   # Regular formula, variables must be in the data set
#'   check_arg(fml1, "formula var(data)", .data = data)
#'
#'   # One sided formula, variables in the environment
#'   check_arg(fml2, "os formula var(env)")
#'
#'   # Two sided formula, variables in the data set or in the env.
#'   check_arg(fml3, "ts formula var(data, env)", .data = data)
#'
#'   # One or two sided, at most two parts in the RHS, at most 1 in the LHS
#'   check_arg(fml4, "formula left(,1) right(,2)")
#'
#'   invisible(NULL)
#' }
#'
#' # We set x1 in the environment
#' x1 = 5
#'
#' # Works
#' test_formulas(~Sepal.Length, ~x1, Sepal.Length~x1, a ~ b, data = iris)
#'
#' # Now let's see errors
#' try(test_formulas(Sepal.Length~x1, data = iris))
#'
#' try(test_formulas(fml2 = ~Sepal.Length, data = iris))
#' try(test_formulas(fml2 = Sepal.Length~x1, data = iris))
#'
#' try(test_formulas(fml3 = ~x1, data = iris))
#' try(test_formulas(fml3 = x1~x555, data = iris))
#'
#' try(test_formulas(fml4 = a ~ b | c | d))
#' try(test_formulas(fml4 = a | b ~ c | d))
#'
#'
#'
#' #
#' # IX) Multiple types
#' #
#'
#' # You can check multiple types using a pipe: '|'
#' # Note that global keywords (like NULL, eval, l0, etc) need not be
#' # separated by pipes. They can be anywhere, the following are identical:
#' #  - "character scalar | data.frame NULL"
#' #  - "NULL character scalar | data.frame"
#' #  - "character scalar NULL | data.frame"
#' #  - "character scalar | data.frame | NULL"
#' #
#'
#' test_mult = function(x){
#'   # x must be either:
#'   # i) a numeric vector of length at least 2
#'   # ii) a square character matrix
#'   # iii) an integer scalar (vector of length 1)
#'   check_arg(x, "numeric vector len(2,) | square character matrix | integer scalar")
#'   invisible(NULL)
#' }
#'
#' # OK
#' test_mult(1)
#' test_mult(1:2)
#' test_mult(matrix("ok", 1, 1))
#'
#' # Not OK, notice the very detailed error messages
#' try(test_mult(matrix("bonjour", 1, 2)))
#' try(test_mult(1.1))
#'
#'
#' #
#' # X) Multiple arguments
#' #
#'
#' # You can check multiple arguments at once if they have the same type.
#' # You can add the type where you want but it must be a character literal.
#' # You can check up to 10 arguments with the same type.
#'
#' test_multiarg = function(xlog1, xlog2, xnum1, xnum2, xnum3){
#'   # checking the logicals
#'   check_arg(xlog1, xlog2, "logical scalar")
#'
#'   # checking the numerics
#'   # => Alternatively, you can add the type first
#'   check_arg("numeric vector", xnum1, xnum2, xnum3)
#'
#'   invisible(NULL)
#' }
#'
#' # Let's throw some errors
#' try(test_multiarg(xlog2 = 4))
#' try(test_multiarg(xnum3 = "test"))
#'
#'
#' #
#' # XI) Multiple sub-stypes
#' #
#'
#' # For atomic arguments (like vector or matrices),
#' # you can check the type of underlying data: is it integer, numeric, etc?
#' # There are five simple sub-types:
#' # - integer
#' # - numeric
#' # - factor
#' # - logical (of type logical or a numeric equal to 0 or 1)
#' # - strict logical (of type logical only)
#' #
#' # If you require that the data is of one sub-type only:
#' # - a) if it's one of the simple sub-types: add the keyword directly in the type
#' # - b) otherwise: add the sub-type in parentheses
#' #
#' # Note that the parentheses MUST follow the main class directly.
#' #
#' # Example:
#' # - a) "integer scalar"
#' # - b) "scalar(Date)"
#' #
#' # If you want to check multiple sub-types: you must add them in parentheses.
#' # Again, the parentheses MUST follow the main class directly.
#' # Examples:
#' # "vector(character, factor)"
#' # "scalar(integer, strict logical)"
#' # "matrix(Date, integer, logical)"
#' #
#' # In check_arg_plus, you can use the keyword "conv" to convert to the
#' # desired type
#' #
#'
#' test_multi_subtypes = function(x, y){
#'   check_arg(x, "scalar(integer, strict logical)")
#'   check_arg(y, "vector(character, factor, Date)")
#'   invisible(NULL)
#' }
#'
#' # What follows doesn't work
#' try(test_multi_subtypes(x = 5.5))
#'
#' # Note that it works if x = 5
#' #  (for check_arg 5 is integer although is.integer(5) returns FALSE)
#' test_multi_subtypes(x = 5)
#'
#' try(test_multi_subtypes(y = 5.5))
#'
#' # Testing the "conv" keyword:
#'
#' test_conv = function(x, type){
#'   check_arg_plus(x, .type = type)
#'   x
#' }
#'
#' class(test_conv(5L, "numeric scalar conv"))
#' class(test_conv(5, "integer scalar conv"))
#' class(test_conv(5, "integer scalar"))
#'
#' # You can use the "conv" keyword in multi-types
#' # Remember that types are checked in ORDER! (see the behavior)
#' test_conv(5:1, "vector(logical, character conv)")
#' test_conv(c(TRUE, FALSE), "vector(logical, character conv)")
#'
#'
#' #
#' # XII) Nested checking: using .up
#' #
#'
#' # Say you have two user level functions
#' # But you do all the computation in an internal function.
#' # The error message should be at the level of the user-level function
#' # You can use the argument .up to do that
#' #
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
#'   # The error messages will be at the level of the user-level functions
#'   # which are 1 up the stack
#'   check_arg(x, y, "numeric scalar mbt", .up = 1)
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
#' # The errors are at the level of sum_fun although
#' # the arguments have been checked in my_internal.
#' # => much easier for the user to understand the problem
#'
#'
#' #
#' # XIII) Using check_arg_plus to check and set list defaults
#' #
#'
#' # Sometimes it is useful to have arguments that are themselves
#' # list of arguments.
#' # Witch check_arg_plus you can check the arguments nested in lists
#' # and easily set default values at the same time.
#' #
#' # When you check a list element, you MUST use the syntax argument$element
#' #
#'
#' # Function that performs a regression then plots it
#' plot_cor = function(x, y, lm.opts = list(), plot.opts = list(), line.opts = list()){
#'
#'   check_arg(x, y, "numeric vector")
#'
#'   # First we ensure the arguments are lists (even of 0-length)
#'   check_arg(lm.opts, plot.opts, line.opts, "named list L0")
#'
#'   # The linear regression
#'   lm.opts$formula = y ~ x
#'   reg = do.call("lm", lm.opts)
#'
#'   # plotting the correlation, with defaults
#'   check_arg_plus(plot.opts$main, "character scalar NULL{'Correlation between x and y'}")
#'
#'   # you can use variables created in the function when setting the default
#'   x_name = deparse(substitute(x))
#'   check_arg_plus(plot.opts$xlab, "character scalar NULL{x_name}")
#'   check_arg_plus(plot.opts$ylab, "character scalar NULL{'y'}")
#'
#'   # we restrict to only two plotting types: p or h
#'   check_arg_plus(plot.opts$type, "NULL{'p'} match(p, h)")
#'
#'   plot.opts$x = x
#'   plot.opts$y = y
#'   do.call("plot", plot.opts)
#'
#'   # with the fit
#'   check_arg_plus(line.opts$col, "NULL{'firebrick'}") # no checking but default setting
#'   check_arg_plus(line.opts$lwd, "integer scalar GE{0} NULL{2}") # check + default
#'   line.opts$a = reg
#'   do.call("abline", line.opts)
#' }
#'
#' sepal_length = iris$Sepal.Length ; y = iris$Sepal.Width
#' plot_cor(sepal_length, y)
#'
#' plot_cor(sepal_length, y, plot.opts = list(col = iris$Species, main = "Another title"))
#'
#' # Now throwing errors
#' try(plot_cor(sepal_length, y, plot.opts = list(type = "l")))
#' try(plot_cor(sepal_length, y, line.opts = list(lwd = -50)))
#'
#'
#' #
#' # XIV) Checking '...' (dot-dot-dot)
#' #
#'
#' # You can also check the '...' argument if you expect all objects
#' # to be of the same type.
#' #
#' # To do so, you MUST place the ... in the first argument of check_arg
#' #
#'
#' sum_check = function(...){
#'   # we want each element of ... to be numeric vectors without NAs
#'   # we want at least one element to be there (mbt)
#'   check_arg(..., "numeric vector mbt")
#'
#'   # once the check is done, we apply sum
#'   sum(...)
#' }
#'
#' sum_check(1:5, 5:20)
#'
#' # Now let's compare the behavior of sum_check() with that of sum()
#' # in the presence of errors
#' x = 1:5 ; y = pt
#' try(sum_check(x, y))
#' try(sum(x, y))
#'
#' # As you can see, in the first call, it's very easy to spot and debug the problem
#' # while in the second call it's almost impossible
#'
#'
#'
#' #
#' # XV) Developer mode
#' #
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
#' # Setting the developer mode off:
#' setDreamerr_dev.mode(FALSE)
#'
#'
#' #
#' # XVI) Using check_value
#' #
#'
#' # The main function for checking arguments is check_arg.
#' # But sometimes you only know if an argument is valid after
#' #  having perfomed some modifications on it.
#' # => that's when check_value kicks in.
#' #
#' # It's better with an example.
#' #
#' # In this example we'll construct a plotting function
#' # using a formula, with a rock-solid argument checking.
#' #
#'
#' # Plotting function, but using a formula
#' # You want to plot only numeric values
#' plot_fml = function(fml, data, ...){
#'   # We first check the arguments
#'   check_arg(data, "data.frame mbt")
#'   check_arg(fml, "ts formula mbt var(data)", .data = data)
#'
#'   # We extract the values of the formula
#'   y = fml[[2]]
#'   x = fml[[3]]
#'
#'   # Now we check that x and y are valid => with check_value
#'   # We also use the possibility to assign the value of y and x directly
#'   # We add a custom message because y/x are NOT arguments
#'   check_value_plus(y, "evalset numeric vector", .data = data,
#'                    .message = "In the argument 'fml', the LHS must be numeric.")
#'   check_value_plus(x, "evalset numeric vector", .data = data,
#'                    .message = "In the argument 'fml', the RHS must be numeric.")
#'
#'   # The dots => only arguments to plot are valid
#'   args_ok = c(formalArgs(plot.default), names(par()))
#'   validate_dots(valid_args = args_ok, stop = TRUE)
#'
#'   # We also set the xlab/ylab
#'   dots = list(...) # dots has a special meaning in check_value (no need to pass .message)
#'   check_value_plus(dots$ylab, "NULL{deparse(fml[[2]])} character vector conv len(,3)")
#'   check_value_plus(dots$xlab, "NULL{deparse(fml[[3]])} character vector conv len(,3)")
#'
#'   dots$y = y
#'   dots$x = x
#'
#'   do.call("plot", dots)
#'
#' }
#'
#' # Let's check it works
#' plot_fml(Sepal.Length ~ Petal.Length + Sepal.Width, iris)
#' plot_fml(Sepal.Length ~ Petal.Length + Sepal.Width, iris, xlab = "Not the default xlab")
#'
#' # Now let's throw some errors
#' try(plot_fml(Sepal.Length ~ Species, iris))
#' try(plot_fml(Sepal.Length ~ Petal.Length, iris, xlab = iris))
#' try(plot_fml(Sepal.Length ~ Petal.Length, iris, xlab = iris$Species))
#'
#'
#'
check_arg = function(.x, .type, .x1, .x2, .x3, .x4, .x5, .x6, .x7, .x8, .x9, ..., .message, .choices = NULL, .data = list(), .value, .env, .up = 0, .call_up = 0){

  if(!getOption("dreamerr_check")) return(NULL)

  mc = match.call(expand.dots = FALSE)

  if(getOption("dreamerr_dev.mode")){
    check_dreamerr_calls(.x = .x, .type = .type, .x1 = .x1, .x2 = .x2, .x3 = .x3, .x4 = .x4, .x5 = .x5, .x6 = .x6, .x7 = .x7, .x8 = .x8, .x9 = .x9, ..., .message = .message, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up)
  }

  check_arg_core(.x = .x, .type = .type, .x1 = .x1, .x2 = .x2, .x3 = .x3, .x4 = .x4, .x5 = .x5, .x6 = .x6, .x7 = .x7, .x8 = .x8, .x9 = .x9, ..., .message = .message, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up, .mc = mc, .is_plus = FALSE, .is_value = FALSE)

}

#' @describeIn check_arg Same as \code{check_arg}, but includes in addition: i) default setting, ii) type conversion, iii) partial matching, and iv) checking list elements. (Small drawback: cannot be turned off.)
check_arg_plus = function(.x, .type, .x1, .x2, .x3, .x4, .x5, .x6, .x7, .x8, .x9, ..., .message, .choices = NULL, .data = list(), .value, .env, .up = 0, .call_up = 0){

  mc = match.call(expand.dots = FALSE)

  if(getOption("dreamerr_dev.mode")){
    check_dreamerr_calls(.x = .x, .type = .type, .x1 = .x1, .x2 = .x2, .x3 = .x3, .x4 = .x4, .x5 = .x5, .x6 = .x6, .x7 = .x7, .x8 = .x8, .x9 = .x9, ..., .message = .message, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up)
  }

  check_arg_core(.x = .x, .type = .type, .x1 = .x1, .x2 = .x2, .x3 = .x3, .x4 = .x4, .x5 = .x5, .x6 = .x6, .x7 = .x7, .x8 = .x8, .x9 = .x9, ..., .message = .message, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up, .mc = mc, .is_plus = TRUE, .is_value = FALSE)

}

#' @describeIn check_arg Checks if a (single) value is of the appropriate type
check_value = function(.x, .type, .message, .arg_name, .choices = NULL, .data = list(), .value, .env, .up = 0, .call_up = 0){

  if(!getOption("dreamerr_check")) return(NULL)

  mc = match.call(expand.dots = FALSE)

  if(getOption("dreamerr_dev.mode")){
    check_dreamerr_calls(.x = .x, .type = .type, .message = .message, .arg_name = .arg_name, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up)
  }

  check_arg_core(.x = .x, .type = .type, .message = .message, .arg_name = .arg_name, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up, .mc = mc, .is_plus = FALSE, .is_value = TRUE)

}

#' @describeIn check_arg Same as \code{check_value}, but includes in addition: i) default setting, ii) type conversion, iii) partial matching, and iv) checking list elements. (Small drawback: cannot be turned off.)
check_value_plus = function(.x, .type, .message, .arg_name, .choices = NULL, .data = list(), .value, .env, .up = 0, .call_up = 0){

  mc = match.call(expand.dots = FALSE)

  if(getOption("dreamerr_dev.mode")){
    check_dreamerr_calls(.x = .x, .type = .type, .message = .message, .arg_name = .arg_name, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up)
  }

  check_arg_core(.x = .x, .type = .type, .message = .message, .arg_name = .arg_name, .choices = .choices, .data = .data, .value = .value, .env = .env, .up = .up, .call_up = .call_up, .mc = mc, .is_plus = TRUE, .is_value = TRUE)

}




#### .===================== ####

####
#### CORE FUNCTION ####
####

check_arg_core = function(.x, .type, .x1, .x2, .x3, .x4, .x5, .x6, .x7, .x8, .x9, ..., .message, .choices = NULL, .data = list(), .value, .env, .up = 0, .call_up = 0, .arg_name, .mc, .is_plus = FALSE, .is_value = FALSE){

  # NOTA: the price to pay to using a core function called by user-level functions is about 4us. I think that's fair for the
  # clarity it adds to the code (and I hate duplication anyway).

  # BEWARE:
  # .type => input type
  # type => type in the function

  # setDreamerr_check(FALSE):
  # Direct call to if(!getOption("dreamerr_check")) return(NULL) is about 1us
  # But doing so is not safe if the type is 'match' or 'NULL{stg}' because these two types assign values in the upper frame
  # Checking for these types requires a) finding the types, and b) checking for them. This is about 13us => 10 times slower than no check
  # One solution is to create two argument checking functions:
  #  1) check_arg => safe function with direct outing if necessary
  #  2) check_arg_plus => same as check_arg but accomodates NULL{default} and partial matching. But if setDreamerr_check(FALSE), no direct outing
  #

  # To avoid code duplication (and to avoid creating new sub functions), I created chunks of
  #  code that are a pattern replicated where appropriate.
  # The chunk is defined by # START :: CHUNK(chunk_name) // # END :: CHUNK(chunk_name)
  #  code that are a pattern replicated where appropriate.
  # The copies are defined by # START :: COPY(chunk_name) // # END :: COPY(chunk_name)
  # Of course only the code in the main chunk is to be modified.
  #

  # Arg plus types:
  # - match
  # - NULL{default}
  # - evalset

  IS_VALUE = .is_value
  IS_PLUS = .is_plus

  mc = .mc

  # Retro compatibility .call_up (dammit!) + set_up
  if(!is.na(match(".call_up", names(mc)))){
    .up = .call_up
  } else if(is.na(match(".up", names(mc)))){
    up_value = mget("DREAMERR__UP", parent.frame(2), ifnotfound = 0)
    .up = up_value[[1]]
  }

  if(IS_VALUE == FALSE){
    #
    # CHECK ARG ####
    #

    # We need the current call (both to identify the dots and to get the type)
    current_call = sys.call(sys.nframe() - 1)

    # To check the dots arguments, '...' must be first. Otherwise, too costly to check
    IS_DOTS = identical(current_call[[2]], quote(...)) # 1.2 us => the price to pay for '...' auto-check

    # What's the difference between dots vs no dots (default)?
    #  - by default we check that the arguments are in the original call => not possible with ... and makes no sense
    #  - since each argument has a name, we evaluate each separately => not possible with '...'
    #
    # Why using the arguments .x, .type, .x1, etc, .x9 instead of ...?
    # Because, using list(...) has big disadvantages:
    #  i) it's slower than accessing directly the arguments,
    # ii) (biggest problem) either the evaluation of the arguments is not possible separately,
    #     either it is super slow (since eval(parse(text = stuff)) is needed)
    #
    # So in the end using ... as a default for checking arguments was a no go. Now it is only used to check "..." arguments
    #

    #
    # Stuff used in both dots/no dots
    #

    # matched current call
    # we save 500ns by using match instead of %in%
    # mc_arg = mc[names(mc) %in% c(".x", ".type", ".x1", ".x2", ".x3", ".x4", ".x5", ".x6", ".x7", ".x8", ".x9")]
    mc_arg = mc[match(names(mc), c(".x", ".type", ".x1", ".x2", ".x3", ".x4", ".x5", ".x6", ".x7", ".x8", ".x9"), nomatch = 0) > 0]

    # The original call (nedded to identify missing arguments OR for the '...' case)
    sysOrigin = sys.parent(.up + 2)
    mc_origin = match.call(definition = sys.function(sysOrigin), call = sys.call(sysOrigin), expand.dots = FALSE)

    if(!IS_DOTS){
      #
      # DEFAULT (no dots) ####
      #

      if(length(mc_arg) < 2){
        stop_up("Problem in the arguments used to call check_arg(), at least '.x' and '.type' should be provided.")
      } else {

        if(any(".type" == names(current_call))){
          type = .type
          mc_arg = mc_arg[!names(mc_arg) == ".type"]

        } else {

          type = NULL
          for(i in length(mc_arg):1){
            if(is.character(mc_arg[[i]])){
              type = mc_arg[[i]]
              mc_arg = mc_arg[-i]
              break
            }
          }

          if(is.null(type)){
            stop_up("Argument '.type' could not be identified. There is a problem in the call to check_arg. Try using explicitely .type = \"stg\". Please see the details/examples/vignette.")
          }
        }

        args = names(mc_arg)
        n = length(mc_arg)
        x_names = character(n)

        if(IS_PLUS){
          # deparse costs more but it is required for lists
          for(i in 1:n) x_names[[i]] = deparse(mc_arg[[i]])

          IS_LIST = grepl("$", x_names, fixed = TRUE)

        } else {
          for(i in 1:n) x_names[[i]] = as.character(mc_arg[[i]])

        }

        is_done = rep(FALSE, n)
      }

      type_low = tolower(type)
      # if(IS_PLUS && !getOption("dreamerr_check") && !grepl("null{", type_low, fixed = TRUE) && !grepl("match", type_low, fixed = TRUE)) return(NULL)

      # if(FLAG == "setup") return(NULL)

      #
      # Missing ####
      #

      # Missing:
      # - the missingness is at the level of the function the user called!
      #   * this means that default values (args that the user doesn't pass) won't be checked!
      #   * missingness is checked at the user-level function // not at the internal function level
      # - type == match is special because missing values should be evaluated and the arguments should be set

      args_origin = names(mc_origin)
      formal.args = NULL

      IS_MBT = NULL
      if(IS_PLUS){
        IS_MATCH = NULL
        IS_NULL_DEFAULT = NULL
      } else {
        IS_MATCH = FALSE
      }

      if(IS_PLUS){
        # list elements are NEVER missing, but can be NULL. List elements that are NULL are considered like missing
        is_missing = (match(x_names, args_origin, nomatch = 0) == 0) & !IS_LIST
      } else {
        # is_missing = x_names %in% args_origin
        is_missing = match(x_names, args_origin, nomatch = 0) == 0
      }

      for(i in seq_along(x_names)){

        if(is_missing[i]){

          if(is.null(IS_MBT)) IS_MBT = grepl("mbt", type_low, fixed = TRUE)

          if(IS_MBT){
            if(missing(.message)){
              .message = paste0(" Argument '", x_names[i], "' is required.")
            }
            reason = "it is missing"
            send_error(reason, x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
          } else {

            is_done[i] = TRUE

            # if IS_PLUS => we set NULL{default} if needed + match

            if(IS_PLUS){

              if(is.null(IS_NULL_DEFAULT)) IS_NULL_DEFAULT = grepl("null{", type_low, fixed = TRUE)

              if(IS_NULL_DEFAULT){
                # We set the default is needed

                if(is.null(formal.args)){
                  formal.args = formals(sys.function(sysOrigin))
                }

                fa = formal.args[[x_names[i]]]

                if(!missing(fa) && "NULL" == deparse(fa)){
                  # We set the value
                  value2eval = extract_curly(type, "null", as.string = TRUE)
                  value = eval(parse(text = value2eval), parent.frame(2))

                  assign(x_names[i], value, parent.frame(2))

                  # We go to the next iteration without checking the "match" that follows
                  next
                }

              }

              # Match type => we get the choices and set the default

              if(is.null(IS_MATCH)){
                # Beware of the behavior:
                # if match:
                #  * if there is a default value:
                #    + if it is multi type (i.e. contains a |) => we go through the full checking
                #    + if NOT multi type => we set the default here
                #  * if there is NO default value:
                #    + we set it here and stop
                #
                # To identify match => fixed = TRUE, so if we have class(pmatch), we will have TRUE while we don't want it
                #
                if(grepl("class", type_low, fixed = TRUE)){
                  if(grepl("|", type_low, fixed = TRUE)){
                    # costly call to grepl in last resort:
                    IS_MATCH = grepl("(^|\\|)[^\\(]*match", type_low)
                  } else {
                    IS_MATCH = FALSE
                  }

                } else {
                  IS_MATCH = grepl("match", type_low, fixed = TRUE)
                }
              }

              if(IS_MATCH && is.null(.choices) && !grepl("match(", type_low, fixed = TRUE)){
                # We need to take care of this special type
                # if the choices are provided in match() or in .choices, we skip
                # otherwise, same behavior as match.arg

                if(is.null(formal.args)){
                  formal.args = formals(sys.function(sysOrigin))
                }

                fa = formal.args[[x_names[i]]]

                if(missing(fa)){
                  NO_DEFAULT = TRUE
                } else {
                  NO_DEFAULT = FALSE
                }

                if(NO_DEFAULT){
                  # BUG
                  if(length(value_all) == 0){
                    stop_up("Type 'match' could not be set since the choices were not found. Argument ", x_names[i], " is missing with no default. Either: i) provide the argument '.choices', ii) include the choices directly in the type in parentheses: e.g. match(choice1, choice2, etc), or iii) set the choices in the argument's default, e.g. ", x_names[i], " = c(\"choice1\", \"choice2\", etc).")
                  }

                } else {
                  # There is a default value
                  # if the type is multi type => we go through all the checking (ex: NA | match(value1, value2) with NA default)

                  if(grepl("|", type_low, fixed = TRUE)){
                    next
                  }

                  value_all = eval(fa, envir = sys.frame(sysOrigin))

                  if(!is.character(value_all)){
                    stop_up("The default values of argument ", x_names[i], " is not of type character (instead it is of type ", enumerate_items(class(value_all[1]), "quote"), "). To initialize the class 'match', it must be of type character.")
                  }

                  value = value_all[1]
                }

                assign(x_names[i], value, parent.frame(2))

              }
            }
          }
        }
      }

      if(all(is_done)) return(NULL)

      # if(FLAG == "mbt") return(NULL)

    } else {
      #
      # dot-dot-dot ####
      #

      RM_TYPE = FALSE
      if(any(".type" == names(current_call))){
        type = .type
        mc_arg = mc_arg[!names(mc_arg) == ".type"]

      } else {

        RM_TYPE = TRUE
        type = NULL
        if(!is.null(names(current_call))){
          current_call = current_call[!names(current_call) %in% c(".message", ".choices", ".data", ".env", ".up")]
        }

        if(length(current_call) > 0){
          for(i in length(current_call):1){
            if(is.character(current_call[[i]])){
              type = current_call[[i]]
              break
            }
          }
        }

        if(is.null(type)){
          stop_up("Argument .type could not be identified. There is a problem in the call to check_arg. Try using explicitely .type = \"stg\". Please see the details/examples/vignette.")
        }

      }

      type_low = tolower(type)

      # The original call => to find the order of the ...
      dots_origin = mc_origin[["..."]]

      n_dots = length(dots_origin)

      # Missing
      if(n_dots == 0){
        if(grepl("mbt", type_low, fixed = TRUE)){
          stop_up("In argument '...', no value is provided. Problem: at least one value is required.", up = .up + 2)
        } else {
          return(NULL)
        }
      }

      # Evaluation => creation of x_all

      if(grepl("eval", type_low, fixed = TRUE)) stop_up("Type 'eval' is not available when checking dot-dot-dot arguments.")

      x_dots = list()
      if("..." %in% names(mc)){
        x_dots = try(list(...), silent = TRUE)

        if("try-error" %in% class(x_dots)){
          reason = gsub("^.+:( \n )? |\n$", "", as.character(x_dots))
          stop_up("In argument '...', ", ifsingle(dots_origin, "the element", "some elements"), " could not be evaluated. Problem: ", reason, up = .up + 2)
        }

      }

      # Some numbers
      # naked_order: order of the elements in ... without names
      if(!is.null(names(dots_origin))){
        naked_order = (1:n_dots)[nchar(names(dots_origin)) == 0]
        n_named_dots = sum(nchar(names(dots_origin)) > 0)
      } else {
        n_named_dots = 0
        naked_order = 1:n_dots
      }

      # Option dotnames requires the arguments in dots to be named
      if(grepl("dotnames", type_low, fixed = TRUE)){
        if(n_named_dots != n_dots){
          if(is.null(names(dots_origin))){
            reason = "none is named."
          } else {
            qui_pblm = which(nchar(names(dots_origin)) == 0)
            reason = paste0("the ", enumerate_items(n_th(qui_pblm)), " element", plural_len(qui_pblm, "s.isn't"), " named.")
          }

          stop_up("In argument '...': all elements must be named. Problem: that's not the case, ", reason, up = .up + 2)

        } else if(nchar(type_low) <= 9){
          return(NULL)
        }
      }

      args = names(mc_arg)
      if(length(args) > 0 && n_named_dots < n_dots){

        # we check the consistency (developer-side mistake)
        if(n_dots - n_named_dots)

        rm_adj = 0

        x_all_tmp = list()
        for(i in seq_along(args)){
          x = switch(args[i], ".x" = try(.x, silent = TRUE), ".type" = try(.type, silent = TRUE), ".x1" = try(.x1, silent = TRUE), ".x2" = try(.x2, silent = TRUE), ".x3" = try(.x3, silent = TRUE), ".x4" = try(.x4, silent = TRUE), ".x5" = try(.x5, silent = TRUE), ".x6" = try(.x6, silent = TRUE), ".x7" = try(.x7, silent = TRUE), ".x8" = try(.x8, silent = TRUE), ".x9" = try(.x9, silent = TRUE))

          # If error => it's an undefined evaluation => we're out
          if(any(class(x) == "try-error")){
            reason = gsub("^.+:( \n )? |\n$", "", as.character(x))

            if(grepl("^argument \".+\" is missing, with no default", reason)){
              # Developer side error
              arg_name = gsub("^[^\"]+\"|\"[^\"]+$", "", reason)
              fun_name = deparse(sys.call(sys.parent(2))[[1]])
              stop_up("Problem in the call to the internal function '", fun_name, "'. The argument '", arg_name, "' has been mistakenly passed in '...'.")
            } else {
              stop_up("In argument '...', the ", n_th(naked_order[i - rm_adj]), " element could not be evaluated. Problem: ", reason, up = .up + 2)
            }


          }

          if(RM_TYPE && identical(x, type)){
            rm_adj = 1
            RM_TYPE = FALSE
          } else {
            x_all_tmp[[length(x_all_tmp) + 1]] = x
          }
        }

        # We put everything in the proper order
        x_all = list()
        x_all[1:n_dots] = NA

        x_all[naked_order[1:length(x_all_tmp)]] = x_all_tmp
        if(n_dots > length(x_all_tmp)){
          x_all[-naked_order[1:length(x_all_tmp)]] = x_dots
        }

      } else {
        x_all = x_dots
      }

      n = length(x_all)

      if(n_named_dots == n_dots){
        names(x_all) = names(dots_origin)
        d_names = paste0("__dotnames__{", names(dots_origin), "}")
      } else {
        d_names = ""
      }

      x_names = paste0("__dots__{", 1:n, "}", d_names)

      is_done = rep(FALSE, n)

      # we need is match information
      if(IS_PLUS){
        IS_MATCH = grepl("match", type_low, fixed = TRUE)
      } else {
        IS_MATCH = FALSE
      }

      #
      # We now check nullity and 0-length
      #

      # Since we've already evaluated the values of x_all, we create a shorter loop here
      # => it avoids checking for multiple IS_DOTS in the default loop
      # However, the checking of L0 is identical => so we copy the code here
      # => do no edit it by hand!
      #


      for(i in seq_along(x_names)){

        x = x_all[[i]]

        if(is.null(x)){
          if(grepl("null", type_low, fixed = TRUE)){

            if(grepl("safe", type_low, fixed = TRUE)){
              value_dp = deparse(mc_origin[[x_names[i]]])
              if(grepl("$", value_dp, fixed = TRUE)){
                msg = paste0("it is NULL (fine) but you entered '", value_dp, "'. If you really want it to be NULL, please use NULL directly or a variable containing NULL")
                send_error(msg, x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
              }
            }

            if(grepl("null{", type_low, fixed = TRUE)){
              stop_up("The type NULL{default} is not available when checking '...'.")
            } else {
              is_done[i] = TRUE
              next
            }

          }  else {
            send_error("it is NULL", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
          }
        }

        # DO NOT EDIT BY HAND! => edit in CHUNK(L0)
        # START::COPY(L0)
        if(length(x) == 0){
          if(grepl("l0", type_low, fixed = TRUE)){

            if(is.list(x)){
              if(grepl("list", type_low, fixed = TRUE)){
                is_done[i] = TRUE
                next
              } else {
                send_error("it is a list", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
              }
            } else {
              is_int = grepl("integer", type_low, fixed = TRUE)
              is_num = grepl("numeric", type_low, fixed = TRUE)
              is_log = grepl("logical", type_low, fixed = TRUE)
              n_types = is_int + is_num + is_log
              if(n_types == 3){
                is_done[i] = TRUE
                next

              } else if(n_types == 0){
                if(grepl("list", type_low, fixed = TRUE)){
                  send_error("it is a list", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
                } else {
                  is_done[i] = TRUE
                  next
                }

              } else {
                ok = class(x)[1] %in% c("integer", "numeric", "logical")[c(is_int, is_num, is_log)]
                if(ok){
                  is_done[i] = TRUE
                  next
                } else {
                  msg = paste0("it is of length 0 and of type '", class(x)[1], "'")
                  send_error(msg, x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
                }
              }
            }

          } else {
            send_error("it is of length 0, while it should have a positive length", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
          }
        }
        # END::COPY(L0)
      }

    }

  } else {
    #
    # CHECK VALUE ####
    #

    if(missing(.type) || !is.character(.type) || length(.type) != 1){
      stop_up("Argument '.type' must be a character of length 1 providing the type to be tested.")
    }

    if(missing(.x)){
      stop_up("You must provide the argument '.x'. Problem: it is currently missing.")
    }

    type = .type
    type_low = tolower(type)

    IS_DOTS = FALSE

    # If IS_PLUS:
    # - we find out if the object is an element in a list
    # - we enforce that the argument is a name or a list
    #

    if(IS_PLUS){

      sysOrigin = sys.parent(.up + 2)

      IS_LIST = FALSE
      if(!is.name(mc[[".x"]])){
        mc_x = mc[[".x"]]
        is_list = is.call(mc_x) && grepl("^[\\.[:alpha:]][[:alnum:]\\._]*\\$", deparse(mc_x))
        if(is_list){
          IS_LIST = TRUE
        } else {
          stop_up("In check_value_plus(), the argument '.x' must be a variable name or a list-element of the form x$element. Currently it is neither.")
        }
      }

      IS_MATCH = NULL
    } else {
      IS_MATCH = FALSE
    }

    is_done = FALSE
    args = ".x"
    n = 1
    if(is.name(mc[[".x"]])){
      x_names = as.character(mc[[".x"]])
    } else {
      x_names = deparse(mc[[".x"]])
    }

    if(!missing(.arg_name) && missing(.message)) .message = paste0("__arg_name__(", .arg_name, ")")

    IS_EVAL = grepl("eval", type_low, fixed = TRUE)

    if(IS_EVAL){
      mc_origin = list()
      if(!(is.name(.x) || is.call(.x))){
        stop_up("To use the keywords 'eval' or 'evalset', the argument '.x' must be a name or a call (like for instance if fml = a ~ b + c, then .x = fml[[2]] would be valid).")
      }
      mc_origin[[x_names]] = .x
    }

    x_all = list()

  }



  if(!IS_DOTS){
    # IS_DOTS has been already evaluated

    #
    # Evaluation problems ####
    #

    # Evaluation MUST be done, it is something that will save a lot of time to the user
    # However error catching is rather costly, especially if we need to parse it (5 times more costly)...
    # This is the reason I chose to add the arguments .x1 to .x9 instead of using ...
    # Let me explain, if we use ..., then either:
    #   1) we don't do evaluation checking => this was a no go
    #   2) we evaluate with eval(parse(text = "try(is.null(the name of the object), silent=TRUE)"), parent.frame())
    #      => but this evaluation, although more general, was 5 times more coslty than a direct evaluation
    #
    # Conclusion: this is why there are the arguments .x1 to .x9 which replace the ...
    #

    IS_EVAL = grepl("eval", type_low, fixed = TRUE)

    x_all = list()

    for(i in which(!is_done)){

      # Evaluation of the argument
      if(IS_EVAL){

        if(IS_PLUS && IS_LIST[i]) stop_up("The keywords 'eval' and 'evalset' are not available when checking list elements.")

        if(missing(.env)){
          .env = parent.frame(.up + 3)
        }

        value_dp = deparse(mc_origin[[x_names[i]]])
        if(missing(.data)){
          x = try(eval(parse(text = value_dp), .env), silent = TRUE)
        } else {
          x = try(eval(parse(text = value_dp), .data, .env), silent = TRUE)
        }


        # We try to extract some precise information if error
        if(any(class(x) == "try-error")){
          .message = paste0("Argument '", x_names[i], "' (equal to '", value_dp, "') could not be evaluated.")

          reason = gsub("^.+:( \n )? |\n$", "", as.character(x))
          if(length(.data) > 0 && !is.null(names(.data))){
            x_vars = all.vars(parse(text = value_dp))
            if(length(x_vars) > 0){
              x_pblm = setdiff(x_vars, names(.data))
              x_real_pblm = c()
              for(v in x_pblm){
                if(!exists(v, envir = .env)) x_real_pblm = c(x_real_pblm, v)
              }
              if(length(x_real_pblm) > 0){
                reason = paste0("The variable", enumerate_items(x_real_pblm, "s.is.quote"), " not in the data set (given in argument '", deparse(mc[[".data"]]), "') nor in the environment")
              }
            }
          }

          send_error(reason, x_name = x_names[i], type = type, message = .message, up = .up, .value = .value, .data = .data)

        }

        # No real benefit to dealy assignment:
        # - if type is OK => we will assign anyway
        # - if type not OK => it's longer but since it will lead to an error, that's fine
        #
        if(grepl("evalset", type_low, fixed = TRUE)){

          if(!IS_PLUS){
            FUN_NAME = ifelse(IS_VALUE, "check_value", "check_arg")
            stop_up("The type evalset is not available in ", FUN_NAME, "(), use ", FUN_NAME, "_plus() instead.")
          }

          assign(x_names[i], x, parent.frame(2))
        }

      } else {
        x = switch(args[i], ".x" = try(.x, silent = TRUE), ".type" = try(.type, silent = TRUE), ".x1" = try(.x1, silent = TRUE), ".x2" = try(.x2, silent = TRUE), ".x3" = try(.x3, silent = TRUE), ".x4" = try(.x4, silent = TRUE), ".x5" = try(.x5, silent = TRUE), ".x6" = try(.x6, silent = TRUE), ".x7" = try(.x7, silent = TRUE), ".x8" = try(.x8, silent = TRUE), ".x9" = try(.x9, silent = TRUE))

        # If error => it's an undefined evaluation => we're out
        if(any(class(x) == "try-error")){

          if(IS_VALUE){
            # if in value => it's an internal error (developer side, big error: this should NEVER happen)
            stop_up("The value '", x_names[i], "' could not be evaluated. This should never happen, please revise the code accordingly so that the value passed to check_value", ifelse(IS_PLUS, "_plus", ""), " always exists.")

          } else {
            .message = paste0("Argument '", x_names[i], "' (equal to '", deparse(mc_origin[[x_names[i]]]), "') could not be evaluated.")
            reason = gsub("^.+:( \n )? |\n$", "", as.character(x))

            send_error(reason, x_name = x_names[i], type = type, message = .message, up = .up, .value = .value, .data = .data)
          }

        }

      }

      if(is.null(x)){
        if(grepl("null", type_low, fixed = TRUE)){

          if(grepl("safe", type_low, fixed = TRUE)){
            value_dp = deparse(mc_origin[[x_names[i]]])
            if(grepl("$", value_dp, fixed = TRUE)){
              msg = paste0("it is NULL (fine) but you entered '", value_dp, "'. If you really want it to be NULL, please use NULL directly or a variable containing NULL")
              send_error(msg, x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
            }
          }

          if(grepl("null{", type_low, fixed = TRUE)){

            if(!IS_PLUS){
              FUN_NAME = ifelse(IS_VALUE, "check_value", "check_arg")
              stop_up("The type NULL{default} is not available in ", FUN_NAME, "(), use ", FUN_NAME, "_plus() instead.")
            }

            value2eval = extract_curly(type, "null", as.string = TRUE)
            value = eval(parse(text = value2eval), parent.frame(2))

            if(IS_LIST[i]){
              # we get the list, assign the value to the list, reassign back in the parent frame
              l_name = gsub("\\$.+", "", x_names[i])
              my_list = get(l_name, parent.frame(2))
              var_name = gsub(".+\\$", "", x_names[i])
              my_list[[var_name]] = value
              assign(l_name, my_list, parent.frame(2))

            } else {
              assign(x_names[i], value, parent.frame(2))
            }

            is_done[i] = TRUE
            next

          } else {
            is_done[i] = TRUE
            next
          }

        } else if(IS_PLUS && IS_LIST[i]){
          # List elements that are NULL are like missing

          if(is.null(IS_MBT)) IS_MBT = grepl("mbt", type_low, fixed = TRUE)

          if(IS_MBT){
            l_name = gsub("\\$.+", "", x_names[i])
            var_name = gsub(".+\\$", "", x_names[i])
            msg = paste0("In the list argument '", l_name, "', the element '", var_name, "' must be provided and cannot be NULL.")
            stop_up(msg, up = .up + 2)
          } else {
            is_done[i] = TRUE
            next
          }
        } else {
          send_error("it is NULL", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
        }
      }

      # START::CHUNK(L0)
      if(length(x) == 0){
        if(grepl("l0", type_low, fixed = TRUE)){

          if(is.list(x)){
            if(grepl("list", type_low, fixed = TRUE)){
              is_done[i] = TRUE
              next
            } else {
              send_error("it is a list", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
            }
          } else {
            is_int = grepl("integer", type_low, fixed = TRUE)
            is_num = grepl("numeric", type_low, fixed = TRUE)
            is_log = grepl("logical", type_low, fixed = TRUE)
            n_types = is_int + is_num + is_log
            if(n_types == 3){
              is_done[i] = TRUE
              next

            } else if(n_types == 0){
              if(grepl("list", type_low, fixed = TRUE)){
                send_error("it is a list", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
              } else {
                is_done[i] = TRUE
                next
              }

            } else {
              ok = class(x)[1] %in% c("integer", "numeric", "logical")[c(is_int, is_num, is_log)]
              if(ok){
                is_done[i] = TRUE
                next
              } else {
                msg = paste0("it is of length 0 and of type '", class(x)[1], "'")
                send_error(msg, x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
              }
            }
          }

        } else {
          send_error("it is of length 0, while it should have a positive length", x_name = x_names[i], type = type, message = .message, choices = .choices, up = .up, .value = .value, .data = .data)
        }
      }
      # END::CHUNK(L0)

      # if here we will perform the full check, so we save the value of x
      x_all[[i]] = x

    }



  }

  if(all(is_done)) return(NULL)

  # if(FLAG == "null") return(NULL)

  #
  # Longer checks ####
  #

  if(grepl("|", type, fixed = TRUE)){
    all_types = strsplit(type, "|", fixed = TRUE)[[1]]
  } else {
    all_types = type
  }


  # Reason & main class will be later used when error is called
  all_reasons = list()
  all_main_types = list()

  n_types = length(all_types)
  for(i in which(!is_done)){
    all_reasons[[i]] = rep("", n_types)
    all_main_types[[i]] = rep("", n_types)
  }

  #
  # Main loop
  #

  # Checking for NAs is a coslty operation => we do it only once across different types
  x_omit = list()
  x_omit_done = rep(FALSE, n)
  any_NA = rep(FALSE, n)
  any_NA_done = rep(FALSE, n)

  choices_all = list()

  for(i in seq_along(all_types)){
    # => we stop at the first valid type

    my_type_raw = all_types[i]
    my_type = tolower(my_type_raw)

    #
    # MAIN CLASSES ####
    #

    subtypes = c()
    check_len = check_equality = check_dim = check_typeof = check_NAOK = check_NONA = FALSE
    dim_loose = FALSE

    is_done_or_fail = is_done

    if(grepl("class(", my_type, fixed = TRUE)){
      #
      # __CUSTOM CLASS ####
      #

      all_classes = strsplit(gsub(".*class\\(([[:alnum:], _\\.]+)\\).*", "\\1", my_type), ",")[[1]]
      all_classes = gsub(" ", "", all_classes)

      for(k in which(!is_done)){
        class_ok = intersect(tolower(class(x_all[[k]])), all_classes)

        if(length(class_ok) == 0){
          all_reasons[[k]][i] = paste0("the object is not of the appropriate class (instead ", inform_class(x_all[[k]]), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = paste0("it is an object of class '", class_ok, "' but ")
      }

      if(all(is_done_or_fail)) next

      check_len = check_dim = TRUE
    } else if(grepl("scalar", my_type, fixed = TRUE)){
      #
      # __SCALAR ####
      #

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(!is.atomic(x) || !is.null(dim(x))){
          all_reasons[[k]][i] = paste0("it is not a scalar, ", inform_class(x, TRUE))
          is_done_or_fail[k] = TRUE
          next
        } else if(length(x) != 1){
          all_reasons[[k]][i] = paste0("it is not of length 1 (currenlty: ", length(x), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = "it is a scalar but "

      }

      if(all(is_done_or_fail)) next

      if(grepl("scalar(", my_type, fixed = TRUE)){
        subtypes = extract_par(my_type, "scalar")
        check_typeof = TRUE
      } else {
        check_typeof = nchar(my_type) >= 10
      }

      check_equality = check_NAOK = TRUE
    } else if(grepl("vector", my_type, fixed = TRUE)){
      #
      # __VECTOR ####
      #

      # A vector is tricky to define.
      # For me a vector is something unidimentional of elements of length 1

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(!is.atomic(x) || !is.null(dim(x))){
          all_reasons[[k]][i] = paste0("it is not a vector, ", inform_class(x, TRUE))
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = "it is a vector but "

        if(grepl("named", my_type, fixed = TRUE)){
          # special type for vectors
          if(is.null(names(x))){
            all_reasons[[k]][i] = "it does not have a name attribute"
            is_done_or_fail[k] = TRUE
            next
          }
        }

      }

      if(all(is_done_or_fail)) next

      if(grepl("vector(", my_type, fixed = TRUE)){
        subtypes = extract_par(my_type, "vector")
        check_typeof = TRUE
      } else {
        check_typeof = nchar(my_type) >= 11
      }

      check_len = check_equality = check_NAOK = TRUE
    } else if(grepl("list", my_type, fixed = TRUE)){
      #
      # __LIST ####
      #

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(!is.list(x)){
          all_reasons[[k]][i] = paste0("it is not a list (instead ", inform_class(x), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = "it is a list but "

        if(grepl("named", my_type, fixed = TRUE)){
          # special type for lists
          if(is.null(names(x))){
            all_reasons[[k]][i] = "it does not have a name attribute"
            is_done_or_fail[k] = TRUE
            next
          }
        }

      }

      if(all(is_done_or_fail)) next

      check_len = TRUE
    } else if(grepl("data.frame", my_type, fixed = TRUE)){
      #
      # __DATA FRAME ####
      #

      if(grepl("vdata.frame", my_type, fixed = TRUE)){
        dim_loose = TRUE
      }

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(dim_loose){
          if(!(is.data.frame(x) || (is.atomic(x) && is.null(dim(x))))){
            all_reasons[[k]][i] = paste0("it is not a data.frame nor a vector (instead ", inform_class(x), ")")
            is_done_or_fail[k] = TRUE
            next
          }
        } else if(!is.data.frame(x)){
          all_reasons[[k]][i] = paste0("it is not a data.frame (instead ", inform_class(x), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = ifelse(is.null(dim(x)), "it is a vector but ", "it is a data.frame but ")

      }

      if(all(is_done_or_fail)) next

      check_dim = check_NONA = TRUE
    } else if(grepl("matrix", my_type, fixed = TRUE)){
      #
      # __MATRIX ####
      #

      if(grepl("vmatrix", my_type, fixed = TRUE)){
        dim_loose = TRUE
      }

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(dim_loose){
          if(!(is.matrix(x) || (is.atomic(x) && is.null(dim(x))))){
            all_reasons[[k]][i] = paste0("it is not a matrix nor a vector (instead ", inform_class(x), ")")
            is_done_or_fail[k] = TRUE
            next
          }
        } else if(!is.matrix(x)){
          all_reasons[[k]][i] = paste0("it is not a matrix (instead ", inform_class(x), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = ifelse(is.null(dim(x)), "it is a vector but ", "it is a matrix but ")

        if(grepl("square", my_type, fixed = TRUE)){
          if(NROW(x) != NCOL(x)){
            all_reasons[[k]][i] = paste0("it is not a square matrix (", NROW(x), " row", plural(NROW(x)), " and ", NCOL(x), " column", plural(NCOL(x)), ")")
            is_done_or_fail[k] = TRUE
            next
          }
        }

      }

      if(all(is_done_or_fail)) next

      if(grepl("matrix(", my_type, fixed = TRUE)){
        subtypes = extract_par(my_type, "matrix")
        check_typeof = TRUE
      } else {
        check_typeof = nchar(my_type) >= 11
      }

      check_dim = check_equality = check_NAOK = TRUE
    } else if(grepl("formula", my_type, fixed = TRUE)){
      #
      # __FORMULAS ####
      #

      for(k in which(!is_done)){

        if(!"formula" %in% class(x_all[[k]])){
          all_reasons[[k]][i] = paste0("it is not a formula (instead ", inform_class(x_all[[k]]), ")")
          next
        } else {
          all_main_types[[k]][i] = "it is a formula but "
          if(grepl("os", my_type, fixed = TRUE) && length(x_all[[k]]) == 3){
            all_reasons[[k]][i] = "it is currently two-sided"
            next
          } else if(grepl("ts", my_type, fixed = TRUE) && length(x_all[[k]]) == 2){
            all_reasons[[k]][i] = "it is currently only one-sided"
            next
          }
        }

        # Multiparts formula

        fml_len = NULL
        if(grepl("right(", my_type, fixed = TRUE)){
          n_expected = extract_par(my_type, "right", int = TRUE)

          fml_len = length(Formula(x))
          n_right = fml_len[2]

          my_error = error_in_between(n_right, n_expected, "right", my_type, .value)

          if(!is.null(my_error)){
            all_reasons[[k]][i] = my_error
            is_done_or_fail[k] = TRUE
            next
          }
        }

        if(grepl("left(", my_type, fixed = TRUE)){
          n_expected = extract_par(my_type, "left", int = TRUE)

          if(is.null(fml_len)) fml_len = length(Formula(x))
          n_left = fml_len[1]

          my_error = error_in_between(n_left, n_expected, "left", my_type, .value)

          if(!is.null(my_error)){
            all_reasons[[k]][i] = my_error
            is_done_or_fail[k] = TRUE
            next
          }
        }

        if(grepl("var(", my_type, fixed = TRUE)){
          where = extract_par(my_type, "var")
          x_vars = all.vars(x_all[[k]])
          x_pblm = x_vars
          if(length(x_vars) > 0){
            # In here, if there are errors, we send them directly, since no way there are two formulas in type
            # and even so => it would be bad practice

            if("data" %in% where){
              if(length(where) == 1){

                if(missing(.data) || length(.data) == 0){
                  data_dp = deparse(mc[[".data"]])

                  if(missing(.data) || data_dp == "NULL"){
                    stop_up("You cannot use the type 'var(data)' (in '.type = ", my_type_raw, "') when the argument '.data' is missing. Please provide the argument '.data'.")
                  } else {

                    info_arg = arg_name_header(x_names[i])
                    stop_up(info_arg, " is a formula that must contain variables from the data set given in argument '", data_dp, "'. Problem: this data set has no variable.", up = .up + 2)
                  }

                } else if(is.null(names(.data))){
                  info_arg = arg_name_header(x_names[i])
                  msg = ifelse(is.list(.data), "is a list but has no names attribute.", "is not a data.frame nor a list.")
                  stop_up(info_arg, " is a formula that must contain variables from the data set given in argument '", deparse(mc[[".data"]]), "'. Problem: this data set ", msg, up = .up + 2)

                } else {
                  x_pblm = setdiff(x_vars, names(.data))
                  if(length(x_pblm) > 0){
                    info_arg = arg_name_header(x_names[i])
                    stop_up(info_arg, " is a formula whose variables must be in the data set given in argument '", deparse(mc[[".data"]]), "'. Problem: the variable", enumerate_items(x_pblm, "s.is.quote"), " not in the data.", up = .up + 2)
                  }
                }
              } else if(!missing(.data) && !is.null(names(.data))){
                x_pblm = setdiff(x_vars, names(.data))
              }
            }

            if(length(x_pblm) > 0 && "env" %in% where){

              if(missing(.env)){
                .env = parent.frame(.up + 3)
              }

              is_ok = sapply(x_pblm, exists, envir = .env)
              if(any(!is_ok)){
                x_real_pblm = x_pblm[!is_ok]

                if("data" %in% where){

                  data_dp = deparse(mc[[".data"]])

                  if(length(.data) == 0){
                    if(data_dp == "NULL"){
                      stop_up("You cannot use 'data' in 'var(data, env)' (in '.type = ", my_type_raw, "') when the argument '.data' is missing. Please provide the argument '.data'.")
                    }
                  }

                  msg = paste0("in the data set (given in argument '", data_dp, "') or in the environment")
                } else {
                  msg = "in the environment"
                }

                info_arg = arg_name_header(x_names[i])
                stop_up(info_arg, " is a formula whose variables must be ", msg, ". Problem: the variable", enumerate_items(x_real_pblm, "s.isn't.quote"), " there.", up = .up + 2)

              }

            }

          }
        }

        is_done[k] = TRUE

      }

      if(all(is_done)) return(NULL)

      # We don't check further with formula
      next

    } else if(grepl("charin", my_type, fixed = TRUE)){
      #
      # __CHARIN ####
      #

      if(grepl("charin(", my_type, fixed = TRUE)){
        .choices = choices = extract_par(my_type_raw, "charin")
      }

      if(is.null(.choices)){
        # the next two lines are from the function match.arg
        stop_up("In type '", my_type, "', the choices were not found. Either: i) provide the argument '.choices', or ii) include them in parentheses: e.g. charin(choice1, choice2, etc).")
      } else {
        choices = .choices
      }

      # multi KW
      if(grepl("multi", my_type, fixed = TRUE)){
        if(grepl("(", my_type, fixed = TRUE)){
          is_multi = grepl("multi", gsub("\\([^\\)]*\\)", "", my_type), fixed = TRUE)
        } else {
          is_multi = TRUE
        }

      } else {
        is_multi = FALSE
      }

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(!is.atomic(x) || !is.null(dim(x))){
          all_reasons[[k]][i] = paste0("it is not a vector, ", inform_class(x, TRUE))
          is_done_or_fail[k] = TRUE
          next
        } else if(is_multi == FALSE && length(x) > 1){
          all_reasons[[k]][i] = paste0("it is not of length 1 (currenlty: ", length(x), ")")
          is_done_or_fail[k] = TRUE
          next
        } else if(anyNA(x)){
          all_reasons[[k]][i] = "it contains NAs while it should be NA-free"
          is_done_or_fail[k] = TRUE
          next
        }

        # The check
        if(!all(x %in% choices)){
          x_pblm = unique(x[!x %in% choices])
          msg = ifelse(any(!is.na(pmatch(tolower(x_pblm), tolower(choices)))), " (note that no partial matching is performed)", "")
          all_reasons[[k]][i] = paste0("the value", enumerate_items(x_pblm, "quote.s.don't"), " match any choice", msg)
          is_done_or_fail[k] = TRUE
          next
        } else {
          is_done = TRUE
          next
        }

      }

      if(all(is_done)) return(NULL)

      next

    } else if(grepl("match", my_type, fixed = TRUE)){
      #
      # __MATCH ####
      #
      IS_MATCH = TRUE

      if(!IS_PLUS){
        FUN_NAME = ifelse(IS_VALUE, "check_value", "check_arg")
        stop_up("The main class 'match' is not available in ", FUN_NAME, "(), use ", FUN_NAME, "_plus() instead.")
      }

      if(IS_DOTS) stop_up("The main class 'match' is not available when checking dot-dot-dot ('...').")

      if(grepl("match(", my_type, fixed = TRUE)){
        .choices = choices = extract_par(my_type_raw, "match")
      }

      # multi + strict KW
      is_multi = is_strict = FALSE
      if(grepl("multi", my_type, fixed = TRUE)){
        if(grepl("(", my_type, fixed = TRUE)){
          my_type_corrected = gsub("\\([^\\)]*\\)", "", my_type)
        } else {
          my_type_corrected = my_type
        }
        is_multi = grepl("multi", my_type_corrected, fixed = TRUE)
        is_strict = grepl("strict", my_type_corrected, fixed = TRUE)

      } else if(grepl("strict", my_type, fixed = TRUE)){
        if(grepl("(", my_type, fixed = TRUE)){
          is_strict = grepl("strict", gsub("\\([^\\)]*\\)", "", my_type), fixed = TRUE)
        } else {
          is_strict = TRUE
        }
      }

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(is.null(.choices)){
          # the next two lines are from the function match.arg
          if(is.null(formal.args)){
            formal.args = formals(sys.function(sysOrigin))
          }

          fa = formal.args[[x_names[k]]]

          if(missing(fa)){
            stop_up("In type '", my_type, "', the choices were not found. Either: i) provide the argument '.choices', ii) include them in parentheses: e.g. match(choice1, choice2, etc), or iii) set the argument's default with the choices, e.g. ", x_names[k], " = c(\"choice1\", \"choice2\", etc).")
          }

          choices = eval(fa, envir = sys.frame(sysOrigin))

        } else {
          choices = .choices
        }

        choices_all[[k]] = choices

        if(!is.atomic(x) || !is.null(dim(x))){
          all_reasons[[k]][i] = paste0("it is not a vector, ", inform_class(x, TRUE))
          is_done_or_fail[k] = TRUE
          next
        } else if(is_multi == FALSE && length(x) > 1){
          all_reasons[[k]][i] = paste0("it is not of length 1 (currenlty: ", length(x), ")")
          is_done_or_fail[k] = TRUE
          next
        } else if(anyNA(x)){
          all_reasons[[k]][i] = "it contains NAs while it should be NA-free"
          is_done_or_fail[k] = TRUE
          next
        }

        # Character coercion
        if(!is.character(x)) x = as.character(x)



        pblm_match = FALSE
        if(is_strict){
          res_int = pmatch(x, choices, duplicates.ok = TRUE)

          if(anyNA(res_int)){
            # This is an error for sure => now we provide information
            pblm_match = TRUE
            j = which.max(is.na(res_int))
            if(nchar(x[j]) == 0){
              all_reasons[[k]][i] = "empty strings are not accepted"
            } else {
              res_int_j = pmatch(tolower(x[j]), tolower(choices), duplicates.ok = TRUE)
              if(!is.na(res_int_j)){
                all_reasons[[k]][i] = paste0("no match was found for '", x[j], "' (note that it is case sensitive, maybe you meant '", choices[res_int_j], "'?)")
              } else {
                # Let's fnd out the reason
                choices_current = substr(choices, 1, nchar(x[j]))
                is_ok = grepl(x[j], choices, fixed = TRUE)
                if(!any(is_ok)){
                  all_reasons[[k]][i] = paste0("no match was found for '", x[j], "'")
                } else {
                  is_ok_bis = is_ok & nchar(choices) == nchar(as.character(x[j]))
                  if(!any(is_ok_bis)){
                    all_reasons[[k]][i] = paste0("more than one value was matched for '", x[j], "': ", enumerate_items(choices[is_ok], quote = TRUE))
                  } else {
                    # The algorithm should NEVER end here
                    all_reasons[[k]][i] = paste0("no match was found for '", x[j], "'")
                  }
                }
              }
            }
          } else {
            res = choices[res_int]
          }

        } else {
          # Matching with no case sensitivity
          #

          res_int = pmatch(x, choices, duplicates.ok = TRUE)
          res = choices[as.integer(res_int)]

          if(anyNA(res_int)){
            # we keep trying to match it!

            x_low = tolower(x)
            choices_low = tolower(choices)

            if(any(nchar(x) == 0)){
              j = which(nchar(x) == 0)[1]
              all_reasons[[k]][i] = "empty strings are not accepted"
              pblm_match = TRUE

            } else {
              # More complex matching

              for(j in which(is.na(res_int))){

                choices_low_current = substr(choices_low, 1, nchar(x[j]))

                is_ok = grepl(x_low[j], choices_low_current, fixed = TRUE)
                if(!any(is_ok)){
                  all_reasons[[k]][i] = paste0("no match was found for '", x[j], "'")
                  pblm_match = TRUE
                  break

                } else if(sum(is_ok) == 1){
                  # Good
                  res[j] = choices[is_ok]

                } else {

                  is_ok_bis = is_ok & nchar(choices) == nchar(x[j])
                  if(sum(is_ok_bis) == 1){
                    res[j] = choices[is_ok_bis]

                  } else if(sum(is_ok_bis) == 0){
                    all_reasons[[k]][i] = paste0("more than one value was matched for '", x[j], "': ", enumerate_items(choices[is_ok], quote = TRUE))
                    pblm_match = TRUE
                    break

                  } else {
                    # in case two identical items but with different case
                    is_ok_ter = choices == x[j]
                    if(sum(is_ok_ter) == 1){
                      res[j] = choices[is_ok_ter]

                    } else {
                      all_reasons[[k]][i] = paste0("more than one value was matched for '", x[j], "': ", enumerate_items(choices[is_ok_bis], quote = TRUE), ", please check the case")
                      pblm_match = TRUE
                      break
                    }

                  }

                }
              }
            }
          }
        }

        if(pblm_match){
          if(is_multi && length(x) > 1){
            all_reasons[[k]][i] = paste0(all_reasons[[k]][i], " [", n_th(j), " element]")
          }
          is_done_or_fail[k] = TRUE
          next
        } else {

          if(IS_LIST[k]){
            # we get the list, assign the value to the list, reassign back in the parent frame
            l_name = gsub("\\$.+", "", x_names[k])
            my_list = get(l_name, parent.frame(2))
            var_name = gsub(".+\\$", "", x_names[k])
            my_list[[var_name]] = res
            assign(l_name, my_list, parent.frame(2))

          } else {
            assign(x_names[k], res, parent.frame(2))
          }

          is_done[k] = TRUE
          next
        }
      }

      if(all(is_done)) return(NULL)

      next

    } else if(grepl("(^| )na( |$)", my_type)){
      #
      # __special NA type ####
      #

      # if here: na ok an no na have been taken care of before
      # so the check is valid

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(length(x) > 1){
          all_reasons[[k]][i] = paste0("it is not of length 1 (currenlty: ", length(x), ")")
          is_done_or_fail[k] = TRUE
          next
        }

        # to catch na to non closure
        is_NA = tryCatch(is.na(x), warning = function(x) x)
        if(!isTRUE(is_NA)){
          all_reasons[[k]][i] = "is.na returns FALSE"
          is_done_or_fail[k] = TRUE
          next
        } else {
          is_done = TRUE
        }
      }

      if(all(is_done)) return(NULL)

      next
    } else if(grepl("function", my_type, fixed = TRUE)){
      #
      # __FUNCTION ####
      #

      # Functions are pretty rare as arguments: so last

      for(k in which(!is_done)){

        x = x_all[[k]]

        if(!is.function(x)){
          all_reasons[[k]][i] = paste0("it is not a function, ", inform_class(x, TRUE))
          is_done_or_fail[k] = TRUE
          next
        }

        all_main_types[[k]][i] = "it is a function but "

        if(grepl("arg(", my_type, fixed = TRUE)){
          n_expected = extract_par(my_type, "arg", int = TRUE)
          n_arg = length(formals(args(x)))

          my_error = error_in_between(n_arg, n_expected, "arg", my_type, .value)

          if(!is.null(my_error)){
            all_reasons[[k]][i] = my_error
            is_done_or_fail[k] = TRUE
            next
          }

        }

        is_done[k] = TRUE

      }

      if(all(is_done)) return(NULL)

      next

    } else {
      next
    }

    #
    # Checking sub types ####
    #

    is_num = FALSE

    #
    # ...len ####
    #

    if(check_len && grepl("len(", my_type, fixed = TRUE)){

      for(k in which(!is_done_or_fail)){

        x = x_all[[k]]

        n_len = length(x)
        n_expected = extract_par(my_type, "len", int = TRUE)

        my_error = error_in_between(n_len, n_expected, "len", my_type, .value, .data)

        if(!is.null(my_error)){
          all_reasons[[k]][i] = my_error
          is_done_or_fail[k] = TRUE
          next
        }

      }

      if(all(is_done_or_fail)) next

    }

    #
    # ...ncol + nrow ####
    #

    if(check_dim && (grepl("nrow(", my_type, fixed = TRUE) || grepl("ncol(", my_type, fixed = TRUE))){

      if(dim_loose){
        fun_ncol = base::NCOL
        fun_nrow = base::NROW
      } else {
        fun_ncol = base::ncol
        fun_nrow = base::nrow
      }

      # ROW
      if(grepl("nrow(", my_type, fixed = TRUE)){

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          n_row = fun_nrow(x)
          n_expected = extract_par(my_type, "nrow", int = TRUE)

          if(is.null(n_row)){
            all_reasons[[k]][i] = "it has no row attribute"
            is_done_or_fail[k] = TRUE
            next
          }

          my_error = error_in_between(n_row, n_expected, "nrow", my_type, .value, .data)

          if(!is.null(my_error)){
            all_reasons[[k]][i] = my_error
            is_done_or_fail[k] = TRUE
            next
          }
        }

        if(all(is_done_or_fail)) next

      }

      # COL
      if(grepl("ncol(", my_type, fixed = TRUE)){

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          n_col = fun_ncol(x)
          n_expected = extract_par(my_type, "ncol", int = TRUE)

          if(is.null(n_col)){
            all_reasons[[k]][i] = "it has no column attribute"
            is_done_or_fail[k] = TRUE
            next
          }

          my_error = error_in_between(n_col, n_expected, "ncol", my_type, .value, .data)

          if(!is.null(my_error)){
            all_reasons[[k]][i] = my_error
            is_done_or_fail[k] = TRUE
            next
          }
        }

        if(all(is_done_or_fail)) next

      }

    }

    #
    # ...NA ####
    #

    if(check_NAOK){
      if(!(grepl("na ok", my_type, fixed = TRUE) || grepl("naok", my_type, fixed = TRUE))){

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          if(!any_NA_done[k]){
            any_NA[k] = anyNA(x)
            any_NA_done[k] = TRUE
          }

          if(any_NA[k]){
            if(length(all_types) == 1 && length(x) > 1){
              n_na = sum(is.na(x))
              all_reasons[[k]][i] = paste0("it contains ", signif_plus(n_na), " NA", plural(n_na), " while it should be NA-free")
            } else {
              all_reasons[[k]][i] = "it contains NAs while it should be NA-free"
            }

            is_done_or_fail[k] = TRUE
            next
          }

        }

        if(all(is_done_or_fail)) next

      }

      if(check_typeof && (grepl("integer", my_type, fixed = TRUE) || grepl("logical", my_type, fixed = TRUE))){
        # We set x_omit => needed to check the integer/logical type

        for(k in which(!is_done_or_fail)){

          if(!x_omit_done[k]){
            x = x_all[[k]]

            if(!any_NA_done[k]){
              any_NA[k] = anyNA(x)
              any_NA_done[k] = TRUE
            }

            if(any_NA[k]){
              x_omit[[k]] = as.vector(x)[!is.na(as.vector(x))]
              if(length(x_omit[[k]]) == 0){
                is_done[k] = is_done_or_fail[k] = TRUE
                next
              }
            } else {
              x_omit[[k]] = x
            }
            x_omit_done[k] = TRUE

          }
        }

        if(all(is_done)) return(NULL)
        if(all(is_done_or_fail)) next

      }

    } else if(check_NONA){

      if(grepl("no na", my_type, fixed = TRUE) || grepl("nona", my_type, fixed = TRUE)){

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          if(!any_NA_done[k]){
            any_NA[k] = anyNA(x)
            any_NA_done[k] = TRUE
          }

          if(any_NA[k]){
            all_reasons[[k]][i] = "it contains NAs while it should be NA-free"
            is_done_or_fail[k] = TRUE
            next
          }

        }

        if(all(is_done_or_fail)) next

      }
    }

    #
    # ...Typeof ####
    #

    if(check_typeof){

      if(length(subtypes) > 0){

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          is_num = is.numeric(x) || is.logical(x)

          ok_subtypes = FALSE
          for(my_subtype in subtypes){
            if(my_subtype == "numeric"){
              if(is_num){
                ok_subtypes = TRUE

                if(IS_PLUS && (is.logical(x) || is.integer(x)) && grepl("conv", my_subtype, fixed = TRUE)){
                  # we coerce logical and integers into numeric
                  x = as.numeric(x)

                  # START::CHUNK(conv_assign)
                  if(IS_LIST[k]){
                    # we get the list, assign the value to the list, reassign back in the parent frame
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
                  # END::CHUNK(conv_assign)
                }

                break
              }
            } else if(my_subtype == "integer"){
              # Integer accomodates integers larger than 2B
              # Except if conversion is needed

              if(is.integer(x)){
                ok_subtypes = TRUE
                break
              } else if((is_num && max(abs(x_omit[[k]] - floor(x_omit[[k]]))) == 0)) {
                # If here: integer, but not necessarily strict

                ok_conv = TRUE
                if(grepl("strict", my_subtype, fixed = TRUE)){
                  # strict integer => something such that as.integer works and NOT logical
                  # must be usable as indices
                  if(!is.logical(x) && max(abs(x_omit[[k]])) < 2147483647){
                    ok_subtypes = TRUE
                  }
                } else if(grepl("large", my_subtype, fixed = TRUE)){
                  ok_subtypes = TRUE
                  ok_conv = FALSE
                } else if(max(abs(x_omit[[k]])) < 2147483647){
                  ok_subtypes = TRUE
                }

                if(ok_subtypes){
                  # If here: type is OK

                  # We check if conversion is needed
                  if(IS_PLUS && grepl("conv", my_subtype, fixed = TRUE)){
                    # We coerce logicals and numeric to integer

                    if(!ok_conv){
                      stop_up("In the type '", my_type, "', for the sub-type integer, the keyword 'large' is not compatible with the keyword 'conv' (since large integers cannot be converted with as.integer(x)).")
                    }

                    x = as.integer(x)

                    # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
                    # END::COPY(conv_assign)
                  }

                  break
                }

              }
            } else if(grepl("character", my_subtype, fixed = TRUE)){
              if(is.character(x)){
                ok_subtypes = TRUE
                break

              } else if(IS_PLUS && grepl("conv", my_subtype, fixed = TRUE)){
                # Anything atomic CAN be converted
                x = as.character(x)
                is_done[k] = TRUE

                # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
                # END::COPY(conv_assign)

                ok_subtypes = TRUE
                break
              }
            } else if(my_subtype == "factor"){
              if(is.factor(x)){
                ok_subtypes = TRUE
                break
              } else if(IS_PLUS && grepl("conv", my_subtype, fixed = TRUE)){
                # Anything atomic CAN be converted
                x = as.factor(x)
                is_done[k] = TRUE

                # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
                # END::COPY(conv_assign)

                ok_subtypes = TRUE
                break
              }
            } else if(grepl("logical", my_subtype, fixed = TRUE)){
              if(is.logical(x)){
                ok_subtypes = TRUE
                break
              } else if(!grepl("strict", my_subtype, fixed = TRUE) && (is_num && all(x_omit[[k]] %in% c(0, 1)))){
                ok_subtypes = TRUE

                if(IS_PLUS && grepl("conv", my_subtype, fixed = TRUE)){
                  # we coerce logical and integers into numeric
                  x = as.logical(x)

                  # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
                  # END::COPY(conv_assign)
                }

                break
              }
            } else {
              if(my_subtype %in% tolower(typeof(x)) || my_subtype %in% tolower(class(x))){
                ok_subtypes = TRUE
                break
              }
            }
          }

          if(ok_subtypes == FALSE){
            all_reasons[[k]][i] = paste0("it is not of the appropriate type (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
            is_done_or_fail[k] = TRUE
            next
          }
        }

      } else {

        for(k in which(!is_done_or_fail)){

          x = x_all[[k]]

          is_num = is.numeric(x) || is.logical(x)

          if(grepl("numeric", my_type, fixed = TRUE)){
            if(!is_num){
              all_reasons[[k]][i] = paste0("it is not numeric (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
              is_done_or_fail[k] = TRUE
              next
            }

            if(IS_PLUS && (is.integer(x) || is.logical(x)) && grepl("conv", my_type, fixed = TRUE)){
              x = as.numeric(x)
              # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
              # END::COPY(conv_assign)
            }

          } else if(grepl("integer", my_type, fixed = TRUE) && !is.integer(x)){
            # large Integer accomodates integers larger than 2B

            if(!is_num){
              is_scalar = grepl("scalar", my_type, fixed = TRUE)
              intro = ifelse(is_scalar, "it is not an integer", "it is not of type integer")
              all_reasons[[k]][i] = paste0(intro, ", instead it is of type ", enumerate_items(class(x[1]), quote = TRUE))

              is_done_or_fail[k] = TRUE
              next
            }

            # Default is the "normal" integer (logical or numeric values with no decimal lower than 2B)
            int_check_large = TRUE

            if(grepl("strict", my_type, fixed = TRUE)){
              if(is.logical(x)){
                is_scalar = grepl("scalar", my_type, fixed = TRUE)
                intro = ifelse(is_scalar, "it is not strictly an integer", "it is not strictly of type integer")
                all_reasons[[k]][i] = paste0(intro, ", instead it is a logical")

                is_done_or_fail[k] = TRUE
                next
              }
            } else if(grepl("large", my_type, fixed = TRUE)){
              int_check_large = FALSE
            }


            # Checking all decimals are 0
            if(any(x_omit[[k]] != as.integer(x_omit[[k]]))){
              is_scalar = grepl("scalar", my_type, fixed = TRUE)
              intro = ifelse(is_scalar, "it is not an integer", "it is not of type integer")
              all_reasons[[k]][i] = paste0(intro, ", although it is numeric")
              is_done_or_fail[k] = TRUE
              next
            }

            # checking large integers
            if(int_check_large && max(abs(x_omit[[k]])) > 2147483647){
              all_reasons[[k]][i] = "it is a numeric that cannot be converted to integer (it exceeds 2,147,483,647)"
              is_done_or_fail[k] = TRUE
              next
            }

            # If here => that's fine, now we check for conversion
            if(IS_PLUS && grepl("conv", my_type, fixed = TRUE)){

              if(int_check_large == FALSE){
                stop_up("In the type '", my_type, "', for the sub-type integer, the keyword 'large' is not compatible with the keyword 'conv' (since large integers cannot be converted to 32bit integers with as.integer(x)).")
              }

              x = as.integer(x)

              # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
              # END::COPY(conv_assign)
            }

          } else if(grepl("character", my_type, fixed = TRUE)){
            if(IS_PLUS && grepl("conv", my_type, fixed = TRUE)){
              # Every atomic element can be converted to character
              x = as.character(x)
              is_done[k] = TRUE

              # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
              # END::COPY(conv_assign)

              next

            } else if(!is.character(x)){
              all_reasons[[k]][i] = paste0("it is not of type character (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
              is_done_or_fail[k] = TRUE
              next
            }
          } else if(grepl("logical", my_type, fixed = TRUE) && !is.logical(x)){

            if(grepl("strict", my_type, fixed = TRUE)){
              all_reasons[[k]][i] = paste0("it is not of type logical (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
              is_done_or_fail[k] = TRUE
              next

            } else {
              # Not a strict logical => ok if numeric in 0/1
              if(!is.numeric(x)){
                all_reasons[[k]][i] = paste0("it is not of type logical (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
                is_done_or_fail[k] = TRUE
                next

              } else if(!all(x_omit[[k]] %in% c(0, 1))){
                all_reasons[[k]][i] = paste0("it is not of type logical (it is a numeric with values different from 0 or 1)")
                is_done_or_fail[k] = TRUE
                next
              }
            }

            # if here => fine
            if(IS_PLUS && grepl("conv", my_type, fixed = TRUE)){
              x = as.logical(x)

              # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
              # END::COPY(conv_assign)
            }

          } else if(grepl("factor", my_type, fixed = TRUE)){
            if(IS_PLUS && grepl("conv", my_type, fixed = TRUE)){
              # Every atomic element can be converted to character
              x = as.factor(x)
              is_done[k] = TRUE

              # START::COPY(conv_assign)
                  if(IS_LIST[k]){
                    l_name = gsub("\\$.+", "", x_names[k])
                    my_list = get(l_name, parent.frame(2))
                    var_name = gsub(".+\\$", "", x_names[k])
                    my_list[[var_name]] = x
                    assign(l_name, my_list, parent.frame(2))
                  } else {
                    assign(x_names[k], x, parent.frame(2))
                  }
              # END::COPY(conv_assign)

              next

            } else if(!is.factor(x)){
              all_reasons[[k]][i] = paste0("it is not of type factor (instead it is of type ", enumerate_items(class(x[1]), quote = TRUE), ")")
              is_done_or_fail[k] = TRUE
              next
            }
          }
        }

      }

      if(all(is_done)) return(NULL)
      if(all(is_done_or_fail)) next
    }

    #
    # ...Equality ####
    #

    if(check_equality && (grepl("ge{", my_type, fixed = TRUE) || grepl("gt{", my_type, fixed = TRUE) || grepl("le{", my_type, fixed = TRUE) || grepl("lt{", my_type, fixed = TRUE))){
      # fixed is still faster than (ge|gt|le|lt)\\{

      for(k in which(!is_done_or_fail)){
        # we need to set x_omit if not yet set
        x = x_all[[k]]

        is_num = is.numeric(x) || is.logical(x)

        # Of course, we only check equality for numerics
        if(!is_num) next

        if(!x_omit_done[k]){

          if(!any_NA_done[k]){
            any_NA[k] = anyNA(x)
            any_NA_done[k] = TRUE
          }

          if(any_NA[k]){
            x_omit[[k]] = as.vector(x)[!is.na(as.vector(x))]
            if(length(x_omit[[k]]) == 0){
              is_done_or_fail[k] = TRUE
              next
            }
          } else {
            x_omit[[k]] = x
          }
          x_omit_done[k] = TRUE
        }

        msg = ifelse(length(x) == 1, "it is ", "it contains values ")

        # GE
        if(grepl("ge{", my_type, fixed = TRUE)){
          # return(NULL)
          value = extract_curly(my_type_raw, "ge")

          if(is.na(value)){
            stop_up("Problem in the evaluation of ge{expr} in ", my_type_raw, ". The expression in ge{} is ill-formed.")
          }

          # return(NULL)
          if(any(x_omit[[k]] < value)){
            all_reasons[[k]][i] = paste0(msg, "strictly lower than ", signif_plus(value, 5, commas = FALSE))
            is_done_or_fail[k] = TRUE
            next
          }
        } else if(grepl("gt{", my_type, fixed = TRUE)){
          # GT
          value = extract_curly(my_type_raw, "gt")

          if(is.na(value)){
            stop_up("Problem in the evaluation of gt{expr} in ", my_type_raw, ". The expression in gt{} is ill-formed.")
          }

          if(any(x_omit[[k]] <= value)){
            if(any(x_omit[[k]] < value)){
              all_reasons[[k]][i] = paste0(msg, "lower than ", signif_plus(value, 5, commas = FALSE))
            } else {
              all_reasons[[k]][i] = paste0(msg, "equal to ", signif_plus(value, 5, commas = FALSE), " (while it should be *striclty* greater than it)")
            }
            is_done_or_fail[k] = TRUE
            next
          }
        }

        # LE
        if(grepl("le{", my_type, fixed = TRUE)){
          value = extract_curly(my_type_raw, "le")

          if(is.na(value)){
            stop_up("Problem in the evaluation of le{expr} in ", my_type_raw, ". The expression in le{} is ill-formed.")
          }

          if(any(x_omit[[k]] > value)){
            all_reasons[[k]][i] = paste0(msg, "strictly greater than ", signif_plus(value, 5, commas = FALSE))
            is_done_or_fail[k] = TRUE
            next
          }
        } else if(grepl("lt{", my_type, fixed = TRUE)){
          # LT
          value = extract_curly(my_type_raw, "lt")

          if(is.na(value)){
            stop_up("Problem in the evaluation of lt{expr} in ", my_type_raw, ". The expression in lt{} is ill-formed.")
          }

          if(any(x_omit[[k]] >= value)){
            if(any(x_omit[[k]] > value)){
              all_reasons[[k]][i] = paste0(msg, "greater than ", signif_plus(value, 5, commas = FALSE))
            } else {
              all_reasons[[k]][i] = paste0(msg, "equal to ", signif_plus(value, 5, commas = FALSE), " (while it should be *striclty* lower than it)")
            }
            is_done_or_fail[k] = TRUE
            next
          }
        }
      }

      if(all(is_done_or_fail)) next

    }

    # If we're here, this means that's fine!
    is_done[!is_done_or_fail] = TRUE
    # Out of the loop
    if(all(is_done)) return(NULL)

  }

  #
  # Send error ####
  #

  # If we're here, well... it means there's an error!

  qui = which.max(!is_done)
  all_reasons = all_reasons[[qui]]
  all_main_types = all_main_types[[qui]]
  if(!is.null(IS_MATCH) && IS_MATCH) .choices = choices_all[[qui]]

  attr(all_reasons, "all_main_types") = all_main_types

  # If here => it means no type has been matched => error

  send_error(all_reasons, x_name = x_names[qui], type, .message, .choices, .up, .value = .value, .data = .data)

}




























