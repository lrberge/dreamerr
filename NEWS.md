
# dreamerr 1.5.0

## New features

- the argument `.message` of the `check_` functions is now interpolated with `string_magic()`

## User-level changes

- the type `l0` has been removed. Now `check_arg(x, "vector")` (or the like) receiving something 0-length in input will not send an error (in the previous version, `l0` was required). Vector length can still be checked with the `len(a,b)` mechanism (thanks to @kylebutts for early bug reports).

## Minor changes

- to increase clarity, now the problem leading to an error is displayed in a newline with the prefix `PROBLEM` in uppercase

- improve clarity of error messages involving lower than/greater than

- improve the number of items suggested by `suggest_item`

## Bug fixes

- fix display problem in `fsignif`

- increase the requirement of `stringmagic` version to be compatible with R v3.5.0

- the `check_arg` family of functions now correctly reports the full call stack when requested

- fix bug: wrong argument name displayed when `check_value` was used with the argument `.arg_name` and an error was produced because a variable was missing in a formula. Thanks to @kylebutts, #4.

# dreamerr 1.4.0

## New functions

- New set of `hook` error and warning functions for reporting in complex internal functions. These are `set_hook` to set a function as the hook and `stop_hook`/`warn_hook` error and warning functions used to report as if they were called from the hooked function.

- `warni` and `stopi` for warning and stop functions with character interpolation using [stringmagic](https://github.com/lrberge/stringmagic)

## New type

- The type `path` has been added to `check_arg`. It is used to check paths.

## Improvements

- All warning and stop functions gain string interpolation with [stringmagic](https://github.com/lrberge/stringmagic).


# dreamerr 1.3.0

## Bug fixes

 - fixes `fit_screen` so that it does not mess with white spaces.

## Name changes

  - Functions `check_arg_plus` and `check_value_plus` become `check_set_arg` and `check_set_value` to improve clarity.

## Improvements

 - `fsignif` now displays trailing 0s appropriately and is neutral to character vectors (instead of throwing an error).
 
 - the comparison types now can evaluate values from the calling frame:
```r
z = 32
x = 77
try(check_value(x, "numeric scalar LE{z}"))
#> Error: in check_value(x, "numeric scalar LE{z}"):
#>  Value 'x' must be a numeric scalar lower than, or equal to, 32. Problem: it is strictly greater than 32.
```

 - `stop_up` now accepts the argument msg which is an extra message displayed right after the error message.

# dreamerr 1.2.3

#### Bug fixes

  - in `check_arg_plus` conversion of factors to characters now works properly.
  
  - Fix bug in `stop_up` when the argument `up` was not appropriately set by the user.

  - fix bug in `sfill` regarding the combined usage of `anchor` and `right` in special circumstances.
  
#### New features
  
  - now `sfill` accepts 0-length vectors.
  
  - new exported function: `fit_screen`.


# dreamerr 1.2.2

#### Bug fixes

  - fix bugs in `sfill`:
  
    * when an anchor was provided and all characters before that were of the same length.
    
    * when an anchor was provided and the fill was from the right.

#### User visible changes

 - `check_`family: objects are now returned invisibly.
 
 - new function `fsignif` which is an alias of `signif_plus`.

# dreamerr 1.2.1 

#### Bug fixes

 - Bug when using commas or parentheses in arguments `.prefix` or `.argname` of `check_value`.
 - Bug in the error message in `check_value` when the argument could not be evaluated.
 - Bug introduced in the previous version, when checking types or equality with the option "no na"
 
 
#### User visible changes

  - `validate_dots`: nicer messages.

# dreamerr 1.2.0 

#### Important changes (no retro compatibility)

 - IMPORTANT CHANGE: Now by default, there is `NA` tolerance for the main classes `vector` and `matrix` (opposite to the previous behavior). The new keywords `NO NA` replace the former keywords `NA OK` for these classes.
```
test = function(x){
  check_arg(x, "numeric vector")
}
# Before (version <= 1.1.0)
test(c(5, NA)) # => Error
# Now (version >= 1.2.0)
test(c(5, NA)) # => is OK
```

 - IMPORTANT CHANGE: values of 0 or 1 are not valid by default anymore when the `logical` type is requested. 
```
test = function(x){
    check_arg(x, "logical scalar")
}
# Before (version <= 1.1.0)
test(0) # => is OK
# Now (version >= 1.2.0)
test(0) # => Error, must be logical
```
  The new behavior is equivalent to the previous behavior when the type was `strict logical`. Now strict logical has been removed and `loose logical` has been introduced which accepts 0 and 1. Why this change? I remarked that when you allow arguments of very different types, one of which was a logical scalar, it was really useful to distinguish between them safely using `isTRUE` or `is.logical`. With the previous default behavior you could have a 0 or 1 that would prevent you from doing that. Of course you could use `strict logical` to trigger that behavior but it was non-intuitive. 

#### New features

 - In `check_value`: you can use the new argument `.prefix` to design the error message. 
 
 - Function `check_value_plus` now returns the value of the element it has checked and possibly modified. 
```
x = "t"
y = check_value_plus(x, "match(This, Is, Raw)")
y # => "This"
fml = ~head(Petal.Length)
z = check_value_plus(fml[[2]], "evalset numeric vector", .data = iris)
z
```
 
 - `sfill` now accepts NAs.
 
#### Other changes

 - Error messages in `check_value` have been improved.
 
 - `enumerate_items` now returns the empty string when 0-length vectors are in input.
  
#### Bug correction

 - The main class `match` could lead the argument to lose its attributes (like names), now corrected.
 
 - Vectors containing only NAs when NAs are tolerated could lead to a wrong error message, now corrected.
 
 - Bug when `check_value_plus` was called from the global environment and an error was triggered.
 
 - Bug when `check_value` was called and `.arg_name` was provided.
 
 - Bug when the "`...`" argument contained `NULL` values.
 
 - The developer mode now never applies to functions from packages using dreamerr
 
 - Behavior of argument `nmax` in `enumerate_items` is fixed.
 
 - Type conversion of matrices returned a vector, now corrected.


# dreamerr 1.1.0 

#### New functions

 - `sfill`: formatting function for strings. Fills a character string up to the required length. Helps to form nice messages.

 - `set_up`: sets the argument `up` semi-globally (i.e. throughout all calls within a function).
 
 - `set_check`: turns on/off argument checking semi-globally (i.e. throughout all calls within a function).

#### New features

 - When devising your own message (with argument `.message`), you can use the special character `__ARG__`. If found, it will be replaced by the appropriate argument name.
 
#### User visible changes

 - Message informing no NA tolerance in the types is prompted only when the error comes from NA values.
 
 - The developer mode catches more errors and provide more suggestions.
 
 - `check_value` now doesn't throw an error for missing `.x`.

#### Change in argument names

 - `.call_up` becomes `.up` -- retro-compatibility is NOT ensured.

#### Bug correction

 - Small bug that could occur for the types `"integer scalar na ok"` with the argument equal to `NA`.
 
 - Important bug when using features requiring evaluations in the environment. Now the default environment is set up appropriately.
 
 - Bug when using `var(data,env)` in a `formula` class with missing `data`.
 
 - Very sneaky bug when badly forming a call to `check_arg` to check `...` in nested functions is now caught.
 
 - In `enumerate_items`, when there were many elements, `quote = TRUE` led to quote the `XX others` that shouldn't be quoted. Now corrected.
 
 - In the `formula` class, if a variable was passed as a `Formula` (notice the capital F), this caused an error if one-sidedness or two-sidedness were to be checked.
 
 - Bug when in `n_letter` when negative numbers are in. This bug propagated into `check_arg`.


# dreamerr 1.0.0 

This package is the outcome of over a decade of coding and developing packages in R. As a package developer I always wanted my code to be "safe": so that if a user provides arguments of the good type the functions always work, and if there's an argument of the wrong type then an informative error message is shown. 

The big problem is that error handling is extremely time consuming, especially if you want to specify informative error messages stating clearly where the problem comes from. This problem is compounded when you want to offer the user flexible arguments that can be of many types. To do error handling properly in such situations is a nightmare: you end up with big if/else trees with a lot of different messages. And it doesn't improve the speed of your code! So you have little incentives to invest time in proper error handling. 

This is why I developed this function. It's one line of code, and it does all the work for you. From the developer side it's super simple, and from the user side, s/he ends up with extremely informative error messages therefore saving a lot of time in debugging.







