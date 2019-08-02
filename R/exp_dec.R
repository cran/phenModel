exp_dec <-
function(thermal_units, chill_days, b, m) {
  y_pred <- b * exp(m * chill_days)
  if(thermal_units >= y_pred) {
    return(1)
  } else {
    return(0)
  }
}
