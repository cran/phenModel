get_thermal_units <-
function(Tmin, Tmax, Tbase, Tmean) {
  alpha <- (Tmax - Tmin)/2
  theta <- asin((Tbase - Tmean)/alpha)
  theta[is.nan(theta)] <- 0
  thermal_units <- 1/pi * ((Tmean - Tbase) * (pi/2 - theta) + (alpha * cos(theta)))
  thermal_units[Tmin >= Tbase] <- Tmean[Tmin >= Tbase] - Tbase
  thermal_units[Tmax < Tbase] <- 0
  return(cumsum(thermal_units))
}
