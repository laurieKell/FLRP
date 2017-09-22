# generics.R - New generic methods
# FLRP/R/generics.R

# constructors {{{
#' @rdname FLBRP
#' @aliases FLBRP FLBRP-methods
setGeneric('FLBRP', function(object, sr, ...)
		standardGeneric('FLBRP'))
# }}}

# methods
setGeneric('biomass.obs', function(object, ...)
		standardGeneric('biomass.obs'))

setGeneric('brp', function(object, ...)
		standardGeneric('brp'))

setGeneric('catch.obs', function(object, ...)
		standardGeneric('catch.obs'))

setGeneric('computeFbar', function(object, ...)
		standardGeneric('computeFbar'))

setGeneric('hcrYield', function(object, fbar, ...)
		standardGeneric('hcrYield'))

setGeneric('refpts', function(object, ...)
		standardGeneric('refpts'))

setGeneric('refpts<-', function(object, ..., value)
		standardGeneric('refpts<-'))

setGeneric('spr', function(object, ...)
		standardGeneric('spr'))

setGeneric('yield.obs', function(object, ...)
		standardGeneric('yield.obs'))

setGeneric('ypr', function(object, ...)
		standardGeneric('ypr'))

setGeneric('msyRange', function(object, ...)
		standardGeneric('msyRange'))

setGeneric("fwdWindow", function(x, y, ...)
    standardGeneric("fwdWindow"))
