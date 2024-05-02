

#
# compatibility
#

if(!exists("str2lang", asNamespace("base"))){
  str2lang = function(x){
    parse(text = x, keep.source = FALSE)[[1]]
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

