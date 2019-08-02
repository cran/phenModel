cum_dev_rate <-
function(Tmean, thermal_units, chill_days, day_length,
                         bud_pars, weib_pars, lactin_pars, max_day_length, 
                         pop_quantiles = .5, data) {
  cdr <- phen_model(Tmean = Tmean, thermal_units = thermal_units, chill_days = chill_days,
                    day_length = day_length, bud_pars = bud_pars, weib_pars = weib_pars,
                    lactin_pars = lactin_pars, max_day_length = max_day_length,
                    pop_quantiles = pop_quantiles, data = data, save.l = TRUE)
  return(data.frame(data, "cum_devrate" = cdr))
}
