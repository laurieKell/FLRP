# spr.R - DESC
# /spr.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# spr {{{

#' @rdname spr
#' @aliases spr,FLBRP-method
#' @examples
#' data(ple4brp)
#'
#' spr(ple4brp)

setMethod('spr', signature(object='FLBRP'),
  function(object) {

    # SET sr to rec = 1, i.e. per recruit
    params(object) <- FLPar(a=1)
    model(object) <- formula(rec ~ a)
    
    res <- .Call("spr", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)), dimnames=dimnames(params(object))), 
      PACKAGE = "FLRP")

    return(res)
  }
) # }}}

# spr0 {{{

#' @rdname spr
#' @aliases spr0,FLBRP,missing,missing-method
#' @examples
#' data(ple4brp)
#'
#' spr0(ple4brp)

setMethod('spr0', signature(ssb='FLBRP', rec='missing', fbar='missing'),
  function(ssb) {

    # SET sr to rec = 1, i.e. per recruit
    params(ssb) <- FLPar(a=1)
    model(ssb) <- formula(rec ~ a)

    # SET fbar = 0
    fbar(ssb) <- FLQuant(0, quant=quant(fbar(ssb)))
    
    res <- .Call("spr", ssb, SRNameCode(SRModelName(ssb@model)),
      FLQuant(c(params(ssb)),dimnames=dimnames(params(ssb))),
      PACKAGE = "FLRP")

    return(res)
  }
) # }}}
