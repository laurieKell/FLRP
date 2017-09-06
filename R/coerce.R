# coerce.R - DESC
# /coerce.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# To FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from) {

	  rec.age  <- dims(from)$min
	  recYrCls <-as.numeric(dimnames(rec.obs(from))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(from))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR from rec.age
    rec <- rec.obs(from)[,ac(recYrCls)]
    ssb <- ssb.obs(from)[,ac(ssbYrCls)]

   # create the FLSR from
   res=FLSR(name=from@name,
            desc=from@desc,
	          rec     =rec,
            ssb     =ssb,
            params  =params(from),
            model   =model(from))
    
    res@params=params(from)

    residuals(res)=log(rec(res)/predict(res))
    units(residuals(res))="NA"
    
    return(res)
  }
) # }}}
