get_total_info <-
function(Tmax, Tmin, Tbase, lat, day) {
  day_length <- get_day_length(lat, day)
  day_length[1:240] <- 100
  Tmean <- get_Tmean(Tmin, Tmax)
  thermal_units <- get_thermal_units(Tmin, Tmax, Tbase, Tmean)
  chill_days <- get_chill_days(Tmean, Tbase)
  info_data <- data.frame(day_length, Tmean, thermal_units, chill_days)
  return(info_data)
}
