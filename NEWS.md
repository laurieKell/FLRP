# FLRP 0.0.3

## NEW FEATURES
- New code for fwdWindow(FLStock, FLBRP)
- spr(FLBRP) and spr0(FLBRP) are now implemented
- A bare vignette now points at FLBRP/FLRP tutorial in http://flr-project.org

## USER-VISIBLE CHANGES

## BUG FIXES

## UTILITIES

## DOCUMENTATION

## DEPRECATED & DEFUNCT
- fwdwindow(FLStock, FLBRP) only has 'end' argument for call to window(FLStock)
  as 'start' and 'frequency' are set to the first year of the FLStock object and
  1 respectively.
