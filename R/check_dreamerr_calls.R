#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Apr 06 09:16:56 2020
# ~: Functions checking the call to check_arg
#----------------------------------------------#



check_dreamerr_calls = function(.x, .type, .x1, .x2, .x3, .x4, .x5, .x6, .x7, .x8, .x9, ..., .message, .choices = NULL, .data = list(), .value, .env, .up, .call_up, .arg_name){
  # This internal function tries to fully check the call to check_arg
  # in particular errors/warnings will pop when the types are ill-formed

  #
  # The calls
  #

  # We get the call to check arg or check value

  current_call = sys.call(sys.nframe() - 1)

  FUN_NAME_FULL = deparse(current_call[[1]])

  IS_VALUE = grepl("value", FUN_NAME_FULL)
  IS_PLUS = grepl("plus", FUN_NAME_FULL)

  FUN_NAME = ifelse(IS_VALUE, "check_value", "check_arg")

  not_missing = function(arg) deparse(substitute(arg)) %in% names(current_call)

  #
  # Basic Arguments ####
  #

  # .message: character scalar
  if(not_missing(.message)){
    if(length(.message) != 1){
      stop_up(up = 1, "Argument '.message' must be a character string of length 1. Currently it is of length ", length(.message), ".")
    }
    if(!is.character(.message)){
      stop_up(up = 1, "Argument '.message' must be a character string of length 1. Currently it is not of type character.")
    }
  }

  # .choices: character vector
  if(not_missing(.choices) && !is.null(.choices)){
    if(!is.character(.choices)){
      stop_up(up = 1, "Argument '.choices' must be a character vector. Currently it is not of type character.")
    }
  }

  # .env: an environment
  if(not_missing(.env)){
    if(!is.environment(.env)){
      stop_up(up = 1, "Argument '.env' must be an environment (default it is the environment from the main call of the function). Currently it is not an environment.")
    }
  }

  # .up: integer scalar
  if(not_missing(.call_up)){
    warn_up("Argument '.call_up' is deprecated, please use '.up' instead ('.call_up' will be removed in the future).")
    .up = .call_up
  }

  if(not_missing(.up)){
    if(length(.up) != 1){
      stop_up(up = 1, "Argument '.up' must be a positive integer scalar. Currently it is of length ", length(.up), ".")
    }
    if(!is.numeric(.up)){
      stop_up(up = 1, "Argument '.up' must be a positive integer scalar. Currently it is not numeric.")
    }
    if((.up - floor(.up)) != 0){
      stop_up(up = 1, "Argument '.up' must be a positive integer scalar. Currently it is not an integer although numeric.")
    }
    if(.up < 0){
      stop_up(up = 1, "Argument '.up' must be a positive integer scalar. Currently it is not positive.")
    }
  }

  # .value: integer scalar
  if(not_missing(.value)){
    if(length(.value) != 1){
      stop_up(up = 1, "Argument '.value' must be a positive integer scalar. Currently it is of length ", length(.value), ".")
    }
    if(!is.numeric(.value)){
      stop_up(up = 1, "Argument '.value' must be a positive integer scalar. Currently it is not numeric.")
    }
    if((.value - floor(.value)) != 0){
      stop_up(up = 1, "Argument '.value' must be a positive integer scalar. Currently it is not an integer although numeric.")
    }
    if(.value < 0){
      stop_up(up = 1, "Argument '.value' must be a positive integer scalar. Currently it is not positive.")
    }
  }

  # .arg_name: character scalar
  if(not_missing(.arg_name)){
    if(length(.arg_name) != 1){
      stop_up(up = 1, "Argument '.arg_name' must be a character string of length 1. Currently it is of length ", length(.arg_name), ".")
    }
    if(!is.character(.arg_name)){
      stop_up(up = 1, "Argument '.arg_name' must be a character string of length 1. Currently it is not of type character.")
    }
  }



  if(IS_VALUE == FALSE){
    #
    # CHECK ARG ####
    #

    IS_DOTS = identical(current_call[[2]], quote(...))
    sysUp = sys.parent()
    mc = match.call(definition = sys.function(sysUp), call = sys.call(sysUp), expand.dots = FALSE)
    mc_arg = mc[match(names(mc), c(".x", ".type", ".x1", ".x2", ".x3", ".x4", ".x5", ".x6", ".x7", ".x8", ".x9"), nomatch = 0) > 0]

    sysOrigin = sys.parent(.up + 2)
    mc_origin = match.call(definition = sys.function(sysOrigin), call = sys.call(sysOrigin), expand.dots = FALSE)

    #
    # Finding the type
    #

    # - 2: the function name and the type so that nb_args is really the number of arguments
    if(is.null(names(current_call))){
      nb_args = length(current_call) - 2
    } else {
      # We count the number of unnamed args or with names in .x[d] .type
      nb_args = length(current_call[!names(current_call) %in% c(".message", ".choices", ".data", ".env", ".up")]) - 2
    }

    if(nb_args < 1){
      stop_up(up = 1, "Problem in the arguments used to call check_arg, at least '.x' and '.type' should be provided.")
    }


    is_dots = sapply(current_call, identical, quote(...))
    if(any(is_dots) && !IS_DOTS){
      stop_up(up = 1, "Problem in the arguments passed to check_arg(). If you want to check '...', then '...' must be the first argument of check_arg (currently it is the ", n_th(which(is_dots) - 1), ").")
    }

    if(IS_DOTS){
      # We check that the user didn't provide .x etc

      if(any(grepl(".x", names(current_call), fixed = TRUE))){
        stop_up(up = 1, "When checking the argument '...', you cannot add any other argument '.x' to '.x9'.")
      }

    } else {
      # We check that the user didn't provide too many args
      if(nb_args > 10){
        stop_up(up = 1, "You cannot check more than 10 arguments. Make another call to check_arg.")
      }
    }

    # Finding the type
    if(any(".type" == names(current_call))){
      type = .type
      mc_arg = mc_arg[!names(mc_arg) == ".type"]

      if(length(type) != 1){
        stop_up(up = 1, "Argument '.type' must be a character string of length 1. Currently it is of length ", length(type), ".")
      }
      if(!is.character(type)){
        stop_up(up = 1, "Argument '.type' must be a character string of length 1. Currently it is not of type character.")
      }

    } else {
      type = NULL
      if(!is.null(names(current_call))){
        current_call = current_call[!names(current_call) %in% c(".message", ".choices", ".data", ".env", ".up")]
      }

      if(IS_DOTS){
        is_char = sapply(current_call, is.character)
      } else {
        is_char = sapply(mc_arg, is.character)
      }

      if(sum(is_char) == 1){
        if(IS_DOTS){
          type = current_call[[which(is_char)]]
        } else {
          type = mc_arg[[which(is_char)]]
          mc_arg = mc_arg[-which(is_char)]
        }

      } else {
        if(sum(is_char) == 0){
          stop_up(up = 1, "Argument '.type' could not be identified: no character literal was found. There is a problem in the call to check_arg. Try using explicitely .type = \"stg\". Please see the details/examples/vignette.")
        } else {
          stop_up(up = 1, "Argument '.type' could not be identified: several character literals were found. There is a very big problem in the call to check_arg which should consist of only i) argument names and ii) the type. Please have a look at the details/examples/vignette.")
        }
      }
    }

    # checking that arguments are names, first we find if check_arg plus

    if(!IS_DOTS){
      if(!IS_PLUS){
        # => all names
        is_name = sapply(mc_arg, is.name)
        if(any(!is_name)){
          # we check if it's because it's alist
          is_list = sapply(mc_arg, function(x) is.call(x) && grepl("^[\\.[:alpha:]][[:alnum:]\\._]*\\$", deparse(x)))
          if(all(!is_name & is_list)){
            stop_up(up = 1, "You cannot check list elements in check_arg, but you can in check_arg_plus. Please refer to Section XIII) in the examples.")
          } else {
            stop_up(up = 1, "In check_arg, the arguments '.x' to '.x9' must be argument names. This is not the case for '", deparse(mc_arg[[which(!is_name)[1]]]), "'. Please refer to the details/examples/vignette.")
          }

        }
      } else {
        # => all names and lists base$var
        is_name = sapply(mc_arg, is.name)
        is_list = sapply(mc_arg, function(x) is.call(x) && grepl("^[\\.[:alpha:]][[:alnum:]\\._]*\\$", deparse(x)))
        is_ok = is_name | is_list
        if(any(!is_ok)){
          stop_up(up = 1, "In check_arg_plus, the arguments '.x' to '.x9' must be argument names (or list elements). This is not the case for '", deparse(mc_arg[[which(!is_ok)[1]]]), "'. Please refer to the details/examples/vignette.")
        }
      }
    }

  } else {
    #
    # CHECK VALUE ####
    #

    if(missing(.type)){
      stop_up(up = 1, "The argument '.type' is required. Problem: it is currently missing.")
    }

    if(length(.type) != 1){
      stop_up(up = 1, "Argument '.type' must be a character string of length 1. Currently it is of length ", length(.type), ".")
    }
    if(!is.character(.type)){
      stop_up(up = 1, "Argument '.type' must be a character string of length 1. Currently it is not of type character.")
    }

    type = .type

    if(missing(.x)){
      stop_up(up = 1, "The argument '.x' is required. Problem: it is currently missing.")
    }

  }

  #
  # TYPE ####
  #


  #
  # Checking the validity of the type
  #

  if(!IS_PLUS){
    if(grepl("(?i)evalset", type)){
      stop_up("You cannot use the keyword 'evalset' in ", FUN_NAME, ", use ", FUN_NAME, "_plus instead. See Section II) or XVI) in the examples.")
    }

    if(grepl("(?i)null\\{", type)){
      stop_up("You cannot use the keyword 'NULL{expr}' in ", FUN_NAME, ", use ", FUN_NAME, "_plus instead. See Section II), XIII) or XVI) in the examples.")
    }
  }

  # First we delete all the globals
  type_clean = gsub("(?i)(safe ?)?null(\\{[^\\}]*\\})?|eval(set)?|dotnames|mbt|l0", "", type)

  all_types = strsplit(type_clean, "|", fixed = TRUE)[[1]]
  all_types = all_types[grepl("[[:alpha:]]", all_types)]

  for(i in seq_along(all_types)){

    my_type_raw = all_types[i]
    my_type = tolower(my_type_raw)

    is_there = function(x) grepl(x, my_type, fixed = TRUE)

    make_error_warning = function(x, my_type_raw){

      my_type = tolower(my_type_raw)

      other_main = function() grepl("class\\(|scalar|vector|list|data\\.frame|matrix|formula|charin|match|(^| )na( |$)|function", my_type)
      is_done = function() grepl("^ *$", my_type)
      clean_kw = function(x) gsub(x, "", my_type)
      clean_par = function(x) gsub(paste0(x, "\\([^\\)]+\\)"), "", my_type)
      clean_curl = function(x) gsub(paste0(x, "\\{[^\\}]+\\}"), "", my_type)
      is_there = function(x) grepl(x, my_type, fixed = TRUE)

      kw = strsplit(x, ".", fixed = TRUE)[[1]]

      if(length(kw) >=2 && kw[2] == "frame"){
        new_kw = kw[-1]
        new_kw[1] = "data.frame"
        kw = new_kw
      }

      main_type = kw[1]

      # main class
      if(is_there(paste0(kw[1], "("))){
        my_type = clean_par(paste0("v?", kw[1]))
      } else {
        my_type = clean_kw(paste0("v?", kw[1]))
      }

      if(length(kw) > 1){
        for(j in 2:length(kw)){

          # Check the validity of the len(a, b) etc
          if(kw[j] %in% c("len", "arg", "dim", "right", "left")){
            kw_all = kw[j]
            if(kw_all == "dim") kw_all = c("nrow", "ncol")

            for(k in kw_all){
              if(is_there(k)){
                value = extract_par(my_type, k)

                data_type = ifelse(k %in% c("len", "ncol", "nrow"), paste0(k, "(data) or "), "")

                msg = paste0("Problem in the type. In the main class '", main_type, "' [fully equal to '", my_type_raw, "'], the ", k, " restriction is ill-formed. It MUST be of the type: A) ", k, "(a, b), ", k, "(, b), ", k, "(a, ) or ", k, "(a), with a and b integers. Or B) ", data_type, k, "(value). See Section IV) in the examples.")

                # 1) length
                if(length(value) > 2){
                  stop_up(up = 2, msg, " Currently it contains ", length(value), " elements in the parentheses.")
                } else if(length(value) == 1 && value == ""){
                  stop_up(up = 2, msg, " Currently it contains no element in the parentheses.")
                }

                # data / value
                if(any(grepl("data", value))){
                  if(length(value) != 1 || value != "data"){
                    stop_up(up = 2, msg, " To use the 'data' keyword, you MUST use the syntax ", k, "(data).")
                  } else if(!k %in% c("len", "ncol", "nrow")){
                    stop_up(up = 2, msg, " You can use the 'data' keyword only for 'len', 'nrow' or 'ncol'.")
                  } else {
                    next
                  }
                }

                if(any(grepl("value", value))){
                  if(length(value) != 1 || value != "value"){
                    stop_up(up = 2, msg, " To use the 'value' keyword, you MUST use the syntax ", k, "(value).")
                  } else {
                    next
                  }
                }

                # 2) ints
                if(any(grepl("[^[:digit:]]", value))){
                  stop_up(up = 2, msg, " Currently it is no integer in parentheses.")
                }

                if(all(nchar(value) == 0)){
                  stop_up(up = 2, msg, " Both integers a and b can't be missing at the same time.")
                }

                value_int = as.integer(value[nchar(value) > 0])
                if(any(value_int == 0)){
                  stop_up(up = 2, msg, " They should NOT be equal to 0.")
                }

                if(length(value_int) == 2 && value_int[1] > value_int[2]){
                  stop_up(up = 2, msg, " Of course b should be greater than a, which is not the case here.")
                }
              }
            }

          } else if(kw[j] == "var" && is_there("var")){
            value = extract_par(my_type, "var")
            if(length(value) > 2 || any(!value %in% c("data", "env"))){
              stop_up(up = 2, "Problem in the type. In the main class '", main_type, "' [fully equal to '", my_type_raw, "'], the restriction 'var' MUST be equal to var(data, env), var(env, data), var(data) or var(env). This is currently not the case. Please see Section VIII) in the examples.")
            }
          }

          # Cleaning the types
          if(kw[j] == "dim"){
            my_type = clean_par("nrow")
            my_type = clean_par("ncol")

          } else if(kw[j] == "equality"){

            if(is_there("ge{") && is_there("gt{")){
              stop_up(up = 2, "You cannot have the keywords greater than (gt{}) and greater or equal (ge{}) at the same time. See Section IV) in the examples.")
            }

            if(is_there("le{") && is_there("lt{")){
              stop_up(up = 2, "You cannot have the keywords lower than (gt{}) and lower or equal (ge{}) at the same time. See Section IV) in the examples.")
            }

            my_type = clean_curl("ge")
            my_type = clean_curl("le")
            my_type = clean_curl("gt")
            my_type = clean_curl("lt")

          } else if(kw[j] == "type"){
            ok_conv = TRUE
            if(is_there("integer")){
              my_type = clean_kw("integer")
              my_type = clean_kw("strict")
              my_type = clean_kw("large")

            } else if(is_there("logical")){
              my_type = clean_kw("logical")
              my_type = clean_kw("strict")

            } else if(is_there("character")){
              my_type = clean_kw("character")

            } else if(is_there("numeric")){
              my_type = clean_kw("numeric")

            } else if(is_there("factor")){
              my_type = clean_kw("factor")

            } else {
              next
            }

            if(!IS_PLUS && grepl("conv", my_type, fixed = TRUE)){
              stop_up(up = 2, "You cannot use the keyword 'conv' in ", FUN_NAME, ", use ", FUN_NAME, "_plus instead. See Section XI) in the examples.")
            }

            my_type = clean_kw("conv")

            if(grepl("integer|logical|numeric|character|factor", my_type)){
              sub_remain = gsub(".*(integer|logical|numeric|character|factor).*", "\\1", my_type)
              stop_up(up = 2, "Problem in the type. In the main class '", main_type, "' [fully equal to '", my_type_raw, "'], the following keyword(s) will not be used: '", trimws(my_type), "'.\n Further, another sub-type was found in this remainder ('", sub_remain, "'), this is not allowed. If you want to check several sub-types, please put them in parentheses after the main class. See Section XI) in the examples.")
            }

          } else if(kw[j] == "sided"){
            if(is_there("os")){
              my_type = clean_kw("os")
            } else if(is_there("ts")){
              my_type = clean_kw("ts")
            } else {
              next
            }

            if(grepl("ts|os", my_type)){
              stop_up(up = 2, "Problem in the type. In the main class '", main_type, "' [fully equal to '", my_type_raw, "'], both the keywords 'os' and 'ts' were found. It cannot be one-sided and two-sided at the same time. Please see Section VIII) in the examples.")
            }

          } else if(is_there(paste0(kw[j], "("))){
            my_type = clean_par(kw[j])

          } else {
            my_type = clean_kw(kw[j])
          }

        }
      }

      if(!is_done()){
        my_type = gsub("^ +| +$", "", my_type)

        my_type = trimws(my_type)
        my_type_raw = trimws(my_type_raw)

        if(other_main()){
          main_type_remain = trimws(gsub(".*(class|scalar|vector|list|data\\.frame|matrix|formula|charin|match|(^| )na( |$)|function).*", "\\1", my_type))
          stop_up(up = 2, "Problem in the type. In the main class '", main_type, "' [fully equal to '", my_type_raw, "'], the following keyword(s) will not be used: '", my_type, "'.\n Further, another main class was found in this remainder ('", main_type_remain, "'), this is not allowed. Please separate main classes with pipes. See Section IX) in the examples.")
        } else {
          warn_up(up = 2, "Problem in the type: in '", my_type_raw, "', the following keyword(s) are not valid: '", my_type, "'.")
        }
      }
    }

    if(is_there("class(")){
      make_error_warning("class.len.dim", my_type_raw)

    } else if(is_there("scalar")){
      make_error_warning("scalar.equality.na ?ok.type", my_type_raw)

    } else if(is_there("vector")){
      make_error_warning("vector.len.equality.na ?ok.type.named", my_type_raw)

    } else if(is_there("list")){
      make_error_warning("list.named.len", my_type_raw)

    } else if(is_there("data.frame")){
      # make_error_warning("data.frame.no ?na.dim",  "vdata.frame nrow(100,) no na len(30)")
      make_error_warning("data.frame.no ?na.dim", my_type_raw)

    } else if(is_there("matrix")){
      make_error_warning("matrix.type.square.dim.equality.na ?ok", my_type_raw)

    } else if(is_there("formula")){
      make_error_warning("formula.sided.var.right.left", my_type_raw)

    } else if(is_there("charin")){
      make_error_warning("charin.multi", my_type_raw)

    } else if(is_there("match")){

      if(!IS_PLUS) stop_up("You cannot use the main class 'match' in ", FUN_NAME, ", you must use ", FUN_NAME, "_plus instead.")

      make_error_warning("match.multi.strict", my_type_raw)

    } else if(grepl("(^| )na( |$)", my_type)){
      make_error_warning("na", my_type_raw)

    } else if(is_there("function")){
      make_error_warning("function.arg", my_type_raw)

    } else {
      warn_up(up = 1, "The following type: '", my_type_raw, "' does not relate to any main class. Please refer to the details/examples to see how to form types.")
    }
  }

}




