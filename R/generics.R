# generics.R - New generic methods
# FLRP/R/generics.R

# Copyright European Union, 2017
# Authors: Laurie Kell <laurie@kell.es>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# constructors {{{
setGeneric('FLBRP', function(object, sr, ...)
		standardGeneric('FLBRP'))
# }}}

# accessors
