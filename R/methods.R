# methods.R - DESC
# /methods.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@kell.es>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# brp {{{
setMethod('brp', signature(object='FLBRP'),
  function(object)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ") in FLBRP object can not be used by brp. See ?ab"))

    # check needed slots are filled up
    for(i in c('landings.sel', 'discards.sel', 'bycatch.harvest', 'stock.wt',
      'landings.wt','discards.wt', 'bycatch.wt','m','mat','harvest.spwn', 'm.spwn',
      'availability'))
      if(all(is.na(slot(object, i))))
        stop("missing necessary information in slot ", i)

    # check dims in object and params
    iter <- c(dims(object)$iter, length(dimnames(params(object))$iter))
    # if > 1, they should be equal
    if(all(iter > 1))
      if(iter[1] != iter[2])
        stop('iter in FLQuant slots and params do not match, ',
          paste(iter, collapse=' vs. '))

    # extend refpts as needed
    iter <- max(iter)
    if(iter > 1 && dims(refpts(object))$iter == 1)
      refpts <- propagate(refpts(object), iter, fill.iter=TRUE)
    else if(iter > 1 && dims(refpts(object))$iter != iter)
      stop("iters in refpts and object slots do not match")
    else
      refpts <- refpts(object)

    if ("virgin" %in% dimnames(refpts)$refpt){
      refpts@.Data["virgin",,         ] <- as.numeric(NA)
      refpts@.Data["virgin", "harvest",] <- 0}

    res <- .Call("brp", object, refpts, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLRP")

    return(res)
  }) # }}}

# landings.n {{{
setMethod('landings.n', signature(object='FLBRP'),
  function(object) {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    .Call('landings_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# discards.n {{{
setMethod('discards.n', signature(object='FLBRP'),
  function(object) {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

   .Call('discards_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# stock.n {{{
setMethod('stock.n', signature(object='FLBRP'),
  function(object)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    .Call('stock_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
  }
) # }}}

# catch.n {{{
setMethod('catch.n', signature(object='FLBRP'),
  function(object) {
    res <- landings.n(object) + discards.n(object)
    if (units(discards.n(object)) == units(landings.n(object)))
		  units(res) <- units(discards.n(object))
    else
      warning("unts of discards.n and landings.n do not match")
      
    return(res)
  }
) # }}}

# catch.wt {{{
setMethod('catch.wt', signature(object='FLBRP'),
  function(object) {

      denom<-landings.sel(object) + discards.sel(object)
      denom[denom==0]<-1
      
      res <- (landings.wt(object) * landings.sel(object) +
              discards.wt(object) * discards.sel(object)) / denom

    test=units(discards.wt(object)) == units(landings.wt(object))
    if (!is.na(test))  
    if (test)
				units(res) <- units(discards.wt(object))

    return(res)
  }
) # }}}

# catch.sel {{{
setMethod('catch.sel', signature(object='FLBRP'),
  function(object) {
    return(landings.sel(object) + discards.sel(object))
  }) # }}}

# catch.obs {{{
setMethod('catch.obs', signature(object='FLBRP'),
  function(object) {
    return(discards.obs(object)+landings.obs(object))
  }) # }}}

# biomass.obs {{{
setMethod('biomass.obs', signature(object='FLBRP'),
  function(object) {
    return(stock.obs(object))
  }) # }}}

# yield.obs {{{
setMethod('yield.obs', signature(object='FLBRP'),
  function(object) {
    return(landings.obs(object))
  }) # }}}

# computeFbar {{{
setMethod('computeFbar', signature(object='FLBRP'),
  function(object) {
    return(apply(harvest(object)[
      ac(object@range["minfbar"]:object@range["maxfbar"])], c(2:6),mean))
  }) # }}}

# rec {{{
setMethod('rec', signature(object='FLBRP'),
  function(object) {
    return(stock.n(object)[1,])
  }) # }}}

# harvest {{{
setMethod("harvest", signature(object="FLBRP", catch="missing"),
	function(object){
    # selectivity
    sel<-expand(landings.sel(object) + discards.sel(object),year=dims(discards.sel(object))$minyear+(1:dim(fbar(object))[2])-1)
    dmns<-dimnames(sel)
    dmns$year<-dimnames(fbar(object))$year
    sel<-FLQuant(sel,dimnames=dmns)
    
    sel[,] <- sel[,1]
    sel <- sweep(sel, 2:6, fbar(object), '*')
    units(sel) <- 'f'
    
    return(sel)
  }) # }}}

# ypr {{{
setMethod('ypr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res<-.Call("ypr", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLRP")

    return(res)
  }) # }}}

# hcrYield {{{
setMethod('hcrYield', signature(object='FLBRP', fbar='FLQuant'),
  function(object, fbar)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    # check input fbar dims
    if(!identical(dim(fbar), dim(fbar(object))))
      stop("input fbar must be the same length as fbar(object)")

    if(dims(object)$iter!=1 && dims(object@params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")

    res <- .Call("hcrYield", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      fbar, PACKAGE = "FLBRP")
    
    # propagate landings.wt
    if(dims(res)$iter != dims(landings.wt(object))$iter)
      landings.wt(object) <- propagate(landings.wt(object), dims(res)$iter)

    return(quantSums(res %*% landings.wt(object)))
   }
)
setMethod('hcrYield', signature(object='FLBRP', fbar='numeric'),
  function(object, fbar)
    hcrYield(object, FLQuant(fbar)))
# }}}
