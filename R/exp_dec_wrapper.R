exp_dec_wrapper <-
function(thermal_units, chill_days, parms) {
  calc <- exp_dec(thermal_units, chill_days, b = parms[1], m = parms[2])
  return(calc)
}
