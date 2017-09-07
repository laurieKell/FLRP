# plots.R - DESC
# /plots.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


setMethod("plot", signature("FLBRP", "missing"),
  function(x, obs=FALSE, refpts=TRUE, ...) {

    # EXTRACT metrics
    df <- model.frame(metrics(x,
      list(ssb=ssb, f=fbar, rec=rec, catch=catch, profit=profit)), drop=TRUE)

    # NO economics
    panels <- list(
      P1=c(x="f", y="ssb", panel="Equilibrium SSB v. F"),
      P2=c(x="ssb", y="rec", panel="Equilibrium Recruitment v. SSB"),
      P3=c(x="f", y="catch", panel="Equilibrium Yield v. F"),
      P4=c(x="ssb", y="catch", panel="Equilibrium Yield v. SSB"))

    # WITH economics
    panels <- c(panels, list(
      P5=c(x="f", y="profit", panel="Equilibrium Profit v. F"),
      P6=c(x="ssb", y="profit", panel="Equilibrium Profit v. SSB")))
                             
    # APPLY over panels to extract x, y and panel for each element
    dat <- lapply(panels, function(x) {
      data.frame(x=df[,x['x']], y=df[,x['y']], panel=x['panel'], row.names=NULL)
    })

    # RBIND into single df
    dat <- do.call(rbind, c(dat, list(make.row.names = FALSE)))

    # PLOT

    ggplot(dat, aes(x=x, y=y)) + geom_line() +
      facet_wrap(~panel, scales="free", ncol=2)
  }
)
