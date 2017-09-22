# fwdWindow.R - DESC
# FLRP/R/fwdWindow.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' @title fwdWindow
#' @rdname fwdWindow
#' @md
#' @description
#'   Extends an object representing a fish population for projecting into the
#'   future using the assumed equilibirum values used in the calculation of
#'   reference points 
#' @param x The population object, for example of class \code{\link{FLStock}}
#' @param y The reference points object, of class \code{\link{FLBRP}}
#' @param end Final year of the extended object, always interpreted as a *character*
#' @return An object of the same class a *x*, extended to *year=end*
#' @details
#'     slts <- c("stock.wt", "landings.wt", "discards.wt", "catch.wt",
#'        "m", "mat", "harvest.spwn", "m.spwn")
#' @examples
#' data(ple4)
#'
#' # Create the FLSR and FLBRP objects
#' psr <- fmle(as.FLSR(ple4, model="bevholt"))
#' prp <- brp(FLBRP(ple4, sr=psr))
#'
#' res <- fwdWindow(ple4, prp, end=2014)

setMethod("fwdWindow", signature(x="FLStock", y="FLBRP"),
  function(x, y, end=dims(x)$maxyear) {

    # EXTEND x with window
    res <- window(x, end=end, extend=TRUE, frequency=1)

    # NEW window years
    wyrs <- setdiff(dimnames(m(res))$year, dimnames(m(x))$year)
      
    # CHECKS

    # COMPLETE *.wt, landings.n, discards.n, mat, m, harvest, *.spwn
    slts <- c("stock.wt", "landings.wt", "discards.wt", "catch.wt",
       "m", "mat", "harvest.spwn", "m.spwn")

    for(s in slts) {
      slot(res, s)[,wyrs] <- do.call(s, list(y))
    }

    # COMPLETE landings.n, discards.n, harvest, any year will do
    
    landings.n(res)[,wyrs] <- landings.n(y)[,3]
    discards.n(res)[,wyrs] <- discards.n(y)[,3]
    harvest(res)[,wyrs] <- harvest(y)[,3]

    return(res)
  }
)
