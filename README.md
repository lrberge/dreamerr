
## dreamerr: Error Handling Made Easy


This package is addressed to developers or anyone writing functions (therefore almost everyone!).

It intends to increase R's user-friendliness and has two main objectives: 

1. to provide to the developer a simple and intuitive, yet powerful and flexible, way to check the arguments passed to a function
2. to offer informative error messages to the user, in the case s/he passes arguments of the wrong type, stating clearly the problem, therefore saving their precious time in debugging

These two goals can be achieved with a single line: `check_arg(arg_name, "expected type")`.

This package aims to be the end of R's sometimes cryptic error messages, like the infamous

```
Error in xj[i] : invalid subscript type 'closure'
```

## Motivating example

To illustrate the usefulness, let's rewrite the `lm()` function with some error handling and compare the results without/with error handling. In this simple example, we just add argument checking to the first two arguments of `lm`.

```r
lm_check = function(formula, data){

  # data: can be missing, if provided, it must be a data.frame or a list with names
  # (a data.frame is a named list, but the two types are added for clarity)
  check_arg(data, "data.frame | named list") 
  
  # formula: 
  # - must be a two-sided formula (ts)
  # - must be given by the user (mbt: must be there)
  # - the variables of the formula must be in the data set or in the environment (var(data, env))
  check_arg(formula, "ts formula var(data, env) mbt", .data = data)

  # Now the all to lm
  lm(formula, data)
}
```

As we can see each argument is checked with one line, and the types are rather explicit since natural language is used.

Now let's compare the two functions. First without error handling (`lm`'s default):

```r
# No argument
lm() 
#> Error in terms.formula(formula, data = data) : 
#>   argument is not a valid model

# Problems in the formula
lm(~Sepal.Width, iris) 
#> Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#>   incompatible dimensions

lm(1+1, iris) 
#> Error in formula.default(object, env = baseenv()) : invalid formula

lm(1+"a", iris) 
#> Error in 1 + "a" : non-numeric argument to binary operator

lm(moien, iris) 
#> Error in stats::model.frame(formula = moien, data = iris, drop.unused.levels = TRUE) : 
#>   object 'moien' not found

# Problem in the data
lm(Sepal.Length~Sepal.Width, pt) 
#> Error in as.data.frame.default(data, optional = TRUE) : 
#>   cannot coerce class ‘"function"’ to a data.frame

lm(Sepal.Length ~ Sepal.Width, 1:5) 
#> Error in eval(predvars, data, env) : 
#>   numeric 'envir' arg not of length one

# Mistake in the variable name
lm(Sopal.Length~Sepal.Width, iris) 
#> Error in eval(predvars, data, env) : object 'Sopal.Length' not found

```

As we can see, the error messages are not really explicit. It's not easy to find: i) what the problem is, ii) which function really drives the error, iii) which argument is involved. It is therefore at best difficult, at worst impossible, to solve the problem from the error messages.

Now with error handling:

```r
# No argument
lm_check() 
#> Error : in lm_check():
#>  Argument 'formula' is required. Problem: it is missing.

# Problems in the formula
lm_check(~Sepal.Width, iris) 
#> Error : in lm_check(~Sepal.Width, iris):
#>  Argument 'formula' must be a two-sided formula. Problem: it is currently only one-sided.

lm_check(1 + 1, iris) 
#> Error : in lm_check(1 + 1, iris):
#>  Argument 'formula' must be a two-sided formula. Problem: it is not a formula (instead it is a vector).

lm_check(1 + "a", iris) 
#> Error : in lm_check(1 + "a", iris):
#>  Argument 'formula' (equal to '1 + "a"') could not be evaluated. Problem: non-numeric argument to binary operator.

lm_check(moien, iris) 
#> Error : in lm_check(moien, iris):
#>  Argument 'formula' (equal to 'moien') could not be evaluated. Problem: object 'moien' not found.

# Problem in the data
lm_check(Sepal.Length ~ Sepal.Width, pt) 
#> Error : in lm_check(Sepal.Length ~ Sepal.Width, pt):
#>  Argument 'data' must be either: i) a data.frame, or ii) a named list. Problem: it is not a data.frame nor a list
#> (instead it is a function).

lm_check(Sepal.Length ~ Sepal.Width, 1:5) 
#> Error : in lm_check(Sepal.Length ~ Sepal.Width, 1:...:
#>  Argument 'data' must be either: i) a data.frame, or ii) a named list. Problem: it is not a data.frame nor a list
#> (instead it is a vector).

# Mistake in the variable name
lm_check(Sopal.Length ~ Sepal.Width, iris) 
#> Error : in lm_check(Sopal.Length ~ Sepal.Width, iris):
#>  The argument 'formula' is a formula whose variables must be in the data set (given in argument 'data') or in the
#> environment. Problem: the variable 'Sopal.Length' isn't there.

```

Now the error messages are explicit: we know where the problem comes from, the function `lm_check` is always shown as the root cause of the problem. The problematic argument is also explicitly mentioned. What the argument is expected to be is reminded and the mismatch with the actual argument is clearly mentioned after `Problem:`. Note that even nasty evaluation problems are caught.

Debugging a call to `lm_check` is a piece of cake (especially compared to a direct call to `lm`): a lot of time is saved, the function looks a lot more user-friendly... with just two damn lines of code. 



## A note on performance

You may wonder: how much argument checking would slow down my code? Well, not much. Even not at all to be honest. 

Although the function `check_arg` supports literally over hundreds of different types, it has been highly optimized, meaning its performance cost is close to 0. Talking numbers, in a moderately slow computer (2.6GHz) it is in the ballpark of 20 micro seconds for missing arguments (i.e. not provided by the user) and of 70 micro seconds for simple checks of non-missing arguments. So you end up with a cost of 1s if you check 20K to 50K arguments, which seems reasonnably fast.

In any case, the user can always set argument checking off with `setDreamerr_check(FALSE)`. So no reason not to make your functions user-friendly!

