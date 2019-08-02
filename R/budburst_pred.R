budburst_pred <-
function(thermal_units, chill_days, bud_pars) {
  bud_data <- data.frame(thermal_units, chill_days)
  bud_data$budburst <- apply(bud_data, 1, function(x) exp_dec_wrapper(x[1], x[2], bud_pars))
  budburst_day <- min(which(bud_data$budburst == 1))
  return(as.numeric(budburst_day))
}
