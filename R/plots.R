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
      list(ssb=ssb, harvest=fbar, rec=rec, yield=landings, profit=profit)), drop=TRUE)

    # refpts
    rps <- refpts(x)

    # SUBSET df
    df <- subset(df, harvest <= c(rps['crash', 'harvest']))

    # NO economics
    panels <- list(
      P1=c(x="harvest", y="ssb", panel="Equilibrium SSB v. F"),
      P2=c(x="ssb", y="rec", panel="Equilibrium Recruitment v. SSB"),
      P3=c(x="harvest", y="yield", panel="Equilibrium Yield v. F"),
      P4=c(x="ssb", y="yield", panel="Equilibrium Yield v. SSB"))

    # WITH economics
    if(!all(is.na(rps[, 'profit']))) {
      panels <- c(panels, list(
        P5=c(x="harvest", y="profit", panel="Equilibrium Profit v. F"),
        P6=c(x="ssb", y="profit", panel="Equilibrium Profit v. SSB")))
    }
                             
    # APPLY over panels to extract x, y and panel for each element
    dat <- lapply(panels, function(p) {
      data.frame(x=df[,p['x']], y=df[,p['y']], panel=p['panel'], row.names=NULL)
    })

    # RBIND into single df
    dat <- do.call(rbind, c(dat, list(make.row.names = FALSE)))

    # PLOT
    p <- ggplot(dat, aes(x=x, y=y)) + geom_line() +
      facet_wrap(~panel, scales="free", ncol=2) +
      xlab("") + ylab("")

    # PLOT refpts
    if(refpts) {
      rpdat <- lapply(panels, function(p) {
        # CBIND x, + refpt, iter ...
        cbind(as(rps[, p['x']], 'data.frame')[, -2],
        # ... y, panel
        y=c(rps[,p['y']]), panel=unname(p['panel']))
      })
      rpdat <- do.call(rbind, c(rpdat, list(make.row.names = FALSE)))

      p <- p + geom_point(data=rpdat, aes(x=data, y=y, colour=refpt))
    }

    # PLOT observations
    if(obs) {
    
      dfo <- model.frame(metrics(x,
        list(ssb=ssb.obs, harvest=fbar.obs, rec=rec.obs, yield=landings.obs)),
        drop=TRUE)
    
      # APPLY over panels to extract x, y and panel for each element
      dato <- lapply(panels[1:4], function(p)
        data.frame(x=dfo[,p['x']], y=dfo[,p['y']], panel=p['panel'], row.names=NULL))
      
      dato <- do.call(rbind, c(dato, list(make.row.names = FALSE)))

      p <- p + geom_point(data=dato)
    }

    return(p)
  }
)
