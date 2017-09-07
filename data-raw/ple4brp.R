# ple4brp.R - DESC
# /ple4brp.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@kell.es>
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

save(ple4brp, file="../data/ple4brp.RData", compress="xz")
