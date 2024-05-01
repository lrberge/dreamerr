

#
# compatibility
#

if(!exists("str2lang", asNamespace("base"))){
  str2lang = function(x){
    res = parse(text = x, keep.source = FALSE)
    if(is.expression(res)){
      res = res[[1]]
    }
    res
  }
}


#
# startup
#


.onLoad <- function(libname, pkgname){
	# setting some options

	options("dreamerr_check" = TRUE)
	options("dreamerr_dev.mode" = FALSE)
	
	fix_pkgwdown_path()

	invisible()
}

