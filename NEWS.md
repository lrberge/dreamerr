
## First version: 1.0.0

This package is the outcome of over a decade of coding and developing packages in R. As a package developer I always wanted my code to be "safe": so that if a user provides arguments of the good type the functions always work, and if there's an argument of the wrong type then an informative error message is shown. 

The big problem is that error handling is extremely time consuming, especially if you want to specify informative error messages stating clearly where the problem comes from. This problem is compounded when you want to offer the user flexible arguments that can be of many types. To do error handling properly in such situations is a nightmare: you end up with big if/else trees with a lot of different messages. And it doesn't improve the speed of your code! So you have little incentives to invest time in proper error handling. 

This is why I developed this function. It's one line of code, and it does all the work for you. From the developer side it's super simple, and from the user side, s/he ends up with extremely informative error messages therefore saving a lot of time in debugging.







