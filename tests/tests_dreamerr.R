#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Aug 31 09:07:52 2020
# ~: test script for dreamerr
#----------------------------------------------#


# Two kinds of checks:
# 1) no error when there should be no error
# 2) errors when there should be errors
#

# To do:
# * add complex calls (multiple types)
# * add check_value
# * check returns from check_arg and check_value
# * specifics from check_set_arg (conv, NULL, etc)

library(dreamerr)

test_err = function(x){
  a = try(x, silent = TRUE)
  if(!any(class(a) == "try-error")) stop("Expected an error that did not occur.")
  invisible(NULL)
}


####
#### Main Types ####
####


####
#### ...scalar ####
####

# To check: types / equality / NA

test_scalar = function(x1, x2, x3, x4, x5){
  check_arg(x1, "numeric scalar")
  check_arg(x2, "integer scalar na ok")
  check_arg(x3, "scalar(factor, logical)")
  check_arg(x4, "scalar(integer, character, logical)")
  check_arg(x5, "integer scalar GT{-3} LE{3}")
  invisible(NULL)
}

#
# Should work
#

# x1: numeric scalar
test_scalar(x1 = 1)
test_scalar(x1 = -1)
test_scalar(x1 = exp(55))

# x2: integer scalar na ok
test_scalar(x2 = 1)
test_scalar(x2 = NA)

# x3: scalar(factor, logical)
test_scalar(x3 = factor("a", "a"))
test_scalar(x3 = TRUE)

# x4: scalar(integer, character, logical)
test_scalar(x4 = 5)
test_scalar(x4 = "bon")
test_scalar(x4 = TRUE)

# x5: integer scalar GT{-3} LE{3}
test_scalar(x5 = -2)
test_scalar(x5 = 3)


#
# Should **not** work
#

# x1: numeric scalar
test_err(test_scalar("5"))
test_err(test_scalar(NA))
test_err(test_scalar(iris))

# x2: integer scalar na ok
test_err(test_scalar(x2 = 1.1))
test_err(test_scalar(x2 = c(NA, 1)))

# x3: scalar(factor, logical)
test_err(test_scalar(x3 = "5"))
test_err(test_scalar(x3 = 1))

# x4: scalar(integer, character, logical)
test_err(test_scalar(x4 = 5.5))

# x5: integer scalar GT{-3} LE{3}
test_err(test_scalar(x5 = -3))
test_err(test_scalar(x5 = 3.1))



####
#### ...vector ####
####

# To check: types / equality / NA / len

test_vec = function(x1, x2, x3, x4, x5, x6){
  check_arg(x1, "integer vector")
  check_arg(x2, "numeric vector no na GE{0}")
  check_arg(x3, "vector(logical, character)")
  check_arg(x4, "vector len(2,5)")
  check_arg(x5, "numeric vector len(data)", .data = iris)
  check_arg(x6, "integer vector len(value) no na GT{5}", .value = 3)
  invisible(NULL)
}


#
# should work
#

# x1: integer vector
test_vec(x1 = 5)
test_vec(x1 = 5:6)
test_vec(x1 = TRUE)

# x2: numeric vector no na GE{0}
test_vec(x2 = 1.1)
test_vec(x2 = iris$Sepal.Length)
test_vec(x2 = c(55, 32))

# x3: vector(logical, character)
test_vec(x3 = c(TRUE, NA, FALSE))
test_vec(x3 = TRUE)
test_vec(x3 = c("bon", NA, "jour"))

# x4: vector len(2,5)
test_vec(x4 = c(TRUE, NA))
test_vec(x4 = c("bon", NA, "jour", NA, NA))

# x5: numeric vector len(data)
test_vec(x5 = iris$Sepal.Width)

# x6: integer vector len(value) no na GT{5} // .value = 3
test_vec(x6 = c(6, 7, 8))

#
# should **not** work
#

# x1: integer vector
test_err(test_vec(x1 = 5.5))
test_err(test_vec(x1 = 0.5 + 1:2))

# x2: numeric vector no na GE{0}
test_err(test_vec(x2 = -1.1))
test_err(test_vec(x2 = c(NA, 55)))

# x3: vector(logical, character)
test_err(test_vec(x3 = 55))

# x4: vector len(2,5)
test_err(test_vec(x4 = TRUE))

# x5: numeric vector len(data)
test_err(test_vec(x5 = iris$Species))
test_err(test_vec(x5 = 1:5))

# x6: integer vector len(value) no na GT{5} // .value = 3
test_err(test_vec(x6 = c(6, NA, 8)))
test_err(test_vec(x6 = c(5, 7, 8)))
test_err(test_vec(x6 = c(7, 8)))


####
#### ...list ####
####

# len

test_list = function(x1, x2, x3){
  check_arg(x1, "list")
  check_arg(x2, "list len(2)")
  check_arg(x3, "list len(value)", .value = 2)
  invisible(NULL)
}


#
# should work
#

# x1: list len 0
test_list(x1 = iris)
test_list(x1 = list())

# x2: list len(2)
test_list(x2 = iris[1:2])
test_list(x2 = list(a = 5, b = 6))

# x3: list len(value)
test_list(x3 = list(a = 5, b = 6))


#
# should **not** work
#

# x1: list
test_err(test_list(x1 = 1:5))

# x2: list len(2)
test_err(test_list(x2 = iris[1]))

# x3: list len(value)
test_err(test_list(x3 = list(a = 5, b = 6, c = 9)))


####
#### ...data.frame ####
####

# nrow / ncol / no na / vdata.frame

test_df = function(x1, x2){
  check_arg(x1, "data.frame nrow(10,) ncol(,2)")
  check_arg(x2, "vdata.frame no na")
  invisible(NULL)
}


#
# should work
#

# x1: data.frame nrow(10,) ncol(,2)
test_df(x1 = iris[, 1:2])
test_df(x1 = data.frame(a = 1:10))

# x2: vdata.frame no na
test_df(x2 = iris[, 1:2])
test_df(x2 = 1:5)
test_df(x2 = iris$Sepal.Length)


#
# should **not** work
#

# x1: data.frame nrow(10,) ncol(,2)
test_err(test_df(x1 = iris[1:5, 1:2]))
test_err(test_df(x1 = iris))

# x2: vdata.frame no na
test_err(test_df(x2 = data.frame(a = c(NA, 1:5))))
test_err(test_df(x2 = c(NA, 1:5)))



####
#### ...matrix ####
####

# type / no na / nrow / ncol / square / equality / vmatrix

test_mat = function(x1, x2, x3, x4, x5){
  check_arg(x1, "square numeric matrix GT{0}")
  check_arg(x2, "vmatrix no na nrow(10, )")
  invisible(NULL)
}


#
# should work
#

# x1: square numeric matrix GT{0}
test_mat(x1 = matrix(5, 5, 5))
test_mat(x1 = matrix(c(NA, 5), 6, 6))

# x2: vmatrix no na nrow(10, )
test_mat(x2 = 1:10)
test_mat(x2 = matrix(rnorm(20*25), 20, 25))


#
# should **not** work
#

# x1: square numeric matrix GT{0}
test_err(test_mat(x1 = matrix(-5, 5, 5)))
test_err(test_mat(x1 = matrix(5, 6, 5)))
test_err(test_mat(x1 = matrix("bonjour", 6, 5)))
test_err(test_mat(x1 = 1))

# x2: vmatrix no na nrow(10, )
test_err(test_mat(x2 = 1:5))
test_err(test_mat(x2 = c(NA, 1:10)))
test_err(test_mat(x2 = matrix(55, 5, 20)))

####
#### ...formula ####
####

# os / ts / left / right

test_fml = function(x1, x2, x3, x4, x5){
  check_arg(x1, "ts formula")
  check_arg(x2, "os formula right(2)")
  check_arg(x3, "formula left(, 1)")
  check_arg(x4, "formula var(data)", .data = iris)
  check_arg(x5, "formula var(env)")
  invisible(NULL)
}


#
# should work
#

# x1: ts formula
test_fml(x1 = a ~ b + c)
test_fml(x1 = a | b ~ b + c | d + e)

# x2: os formula right(2)
test_fml(x2 = ~ b + c | d)

# x3: formula left(, 1)
test_fml(x3 = a ~ b + c)
test_fml(x3 = a ~ 1 | 0 | z)

# x4: formula var(data), .data = iris
test_fml(x4 = Petal.Length ~ Sepal.Length | Species)

# x5: formula var(env)
x = y = 5
test_fml(x5 = y ~ x)

#
# should **not** work
#

# x1: ts formula
test_err(test_fml(x1 = iris))
test_err(test_fml(x1 = 1:5))
test_err(test_fml(x1 = ~ b + c))

# x2: os formula right(2)
test_err(test_fml(x2 = ~ b + c | d | e))
test_err(test_fml(x2 = a ~ b + c | d))

# x3: formula left(, 1)
test_err(test_fml(x3 = a | b ~ b + c))

# x4: formula var(data), .data = iris
test_err(test_fml(x4 = Petol.Length ~ Sepal.Length | Species))
test_err(test_fml(x4 = Petol.Length ~ Sepal.Length | species))

# x5: formula var(env)
x = y = 5
test_err(test_fml(x5 = y ~ xxx))


####
#### ...charin ####
####

# multi

test_charin = function(x1, x2){
  check_arg(x1, "charin", .choices = c("bon", "jour", "so", "leil"))
  check_arg(x2, "multi charin(bon, jour, so, leil)")
  invisible(NULL)
}


#
# should work
#

# x1: charin
test_charin(x1 = "bon")
test_charin(x1 = "jour")

# x2: strict charin
test_charin(x2 = c("bon", "jour"))

#
# should **not** work
#

# x1: charin
test_err(test_charin(x1 = "bonj"))
test_err(test_charin(x1 = "Bon"))
test_err(test_charin(x1 = c("bon", "jour")))

# x2: multi charin
test_err(test_charin(x2 = 55))


####
#### ...match ####
####

# strict / multi / different inits

test_match = function(x1 = c("bon", "jour", "soleil"), x2, x3){
  mc = match.call()
  check_set_arg(x1, "match")
  if("x1" %in% names(mc)) return(x1)

  check_set_arg(x2, "strict match(bon, jour, soleil)")
  if("x2" %in% names(mc)) return(x2)

  check_set_arg(x3, "multi match", .choices = c("bon", "jour", "soleil"))
  if("x3" %in% names(mc)) return(x3)

  return(x1)
}


#
# should work
#

# x1: match
test_match() == "bon"
test_match(x1 = "jour") == "jour"
test_match(x1 = "s") == "soleil"
test_match(x1 = "So") == "soleil"
test_match(x1 = "Bo") == "bon"

# x2: strict match(bon, jour, soleil)
test_match(x2 = "jour") == "jour"
test_match(x2 = "s") == "soleil"

# x3: multi match, .choices = c("bon", "jour", "soleil")
test_match(x3 = c("jour", "bo")) %in% c("bon", "jour")

#
# should **not** work
#

# x1: match
test_err(test_match(x1 = "jouro"))
test_err(test_match(x1 = 55))
test_err(test_match(x1 = ".+"))

# x2: strict match(bon, jour, soleil)
test_err(test_match(x2 = "Jour"))
test_err(test_match(x2 = c("jour", "b")))

# x3: multi match, .choices = c("bon", "jour", "soleil")
test_err(test_match(x3 = NA))


####
#### ...NA ####
####

test_na = function(x1){
  check_arg(x1, "NA")
  invisible(NULL)
}


#
# should work
#

# x1: NA
test_na(x1 = NA)

#
# should **not** work
#

# x1: NA
test_err(test_na(x1 = 5))
test_err(test_na(x1 = 5:55))
test_err(test_na(x1 = iris))
test_err(test_na(x1 = c(NA, 2)))
test_err(test_na(x1 = lis(NA)))


####
#### ...function ####
####

# arg

test_fun = function(x1, x2, x3, x4, x5){
  check_arg(x1, "function")
  check_arg(x2, "function arg(1,)")
  invisible(NULL)
}


#
# should work
#

# x1: function
test_fun(x1 = sum)

# x2: function arg(1,)
test_fun(x2 = merge)

#
# should **not** work
#

# x1: function
test_err(test_fun(x1 = 1:5))

# x2: function arg(1,)
test_err(test_fun(x2 = function() 5))


































































