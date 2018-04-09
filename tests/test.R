# test.R - DESC
# /test.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

library(FLRP)

data(ple4)

ple4sr <- fmle(as.FLSR(ple4, model="bevholt"))

ple4brp <- FLBRP(ple4, sr=ple4sr, fbar=FLQuant(seq(0, 3, length=101)),
  price=FLQuant(1.50, dimnames=list(age=1:10)))

ple4brp <- brp(ple4brp)


# accessors

availability(ple4brp)
bycatch.harv(ple4brp)
bycatch.wt(ple4brp)
discards.obs(ple4brp)
discards.sel(ple4brp)
discards.wt(ple4brp)
fbar(ple4brp)
fbar.obs(ple4brp)
fcost(ple4brp)
harvest.spwn(ple4brp)
landings.obs(ple4brp)
landings.sel(ple4brp)
landings.wt(ple4brp)
m(ple4brp)
m.spwn(ple4brp)
mat(ple4brp)
price(ple4brp)
profit.obs(ple4brp)
rec.obs(ple4brp)
ssb.obs(ple4brp)
stock.obs(ple4brp)
stock.wt(ple4brp)
vcost(ple4brp)
