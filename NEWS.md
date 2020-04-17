
## Version 1.xx.xx 

#### New functions

`fill_symbol`: formatting function for strings. Fills a character string up to the required length. Helps to form nice messages.

`set_up`: sets the argument `up` semi-globally (i.e. throughout all calls within a function).

#### Change in argument names

`.call_up` becomes `.up` -- retro-compatibility is ensured.

#### Bug correction

 . Small bug that could occur for the types "integer scalar na ok" with the argument equal to NA.
 
 . Important bug when using features requiring evaluations in the environment. Now the default environment is set up appropriately.
 
 . Bug when using `var(data,env)` in a `formula` class with missing `data`.
 
 . Very sneaky bug when badly forming a call to \code{check_arg} to check "\code{...}" in nested functions in now caught.


## First version: 1.0.0 (2020-04-12)

This package is the outcome of over a decade of coding and developing packages in R. As a package developer I always wanted my code to be "safe": so that if a user provides arguments of the good type the functions always work, and if there's an argument of the wrong type then an informative error message is shown. 

The big problem is that error handling is extremely time consuming, especially if you want to specify informative error messages stating clearly where the problem comes from. This problem is compounded when you want to offer the user flexible arguments that can be of many types. To do error handling properly in such situations is a nightmare: you end up with big if/else trees with a lot of different messages. And it doesn't improve the speed of your code! So you have little incentives to invest time in proper error handling. 

This is why I developed this function. It's one line of code, and it does all the work for you. From the developer side it's super simple, and from the user side, s/he ends up with extremely informative error messages therefore saving a lot of time in debugging.







