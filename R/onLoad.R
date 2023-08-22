


.onLoad <- function(libname, pkgname){
	# setting some options

	options("dreamerr_check" = TRUE)
	options("dreamerr_dev.mode" = FALSE)
	
	fix_pkgwdown_path()

	invisible()
}

