phen_model <-
function(Tmean, thermal_units, chill_days, day_length,
                       bud_pars, weib_pars, lactin_pars, max_day_length, 
                       pop_quantiles = c(.05, .5, .95),
                       print.l = FALSE, save.l = FALSE,
                       data) {
  
  Tmean <- data[,Tmean]
  thermal_units <- data[,thermal_units]
  chill_days <- data[,chill_days]
  day_length <- data[,day_length]
  
  threshold <- apply(weib_pars, 2, weibull3p_inverse_wrapper, cum_prob = pop_quantiles)
  budburst <- budburst_pred(thermal_units, chill_days, bud_pars)
  
  ## day of budburst
  budburst_day <- rep(budburst, length(pop_quantiles))
  
  if(save.l) {
    post_diapause_day <- stage_change_pred(Tmean, threshold, budburst_day+1, lactin_pars, 1,
                                           day_length, max_day_length, print.l, save.l)
    ovipositing_day <- stage_change_pred(Tmean, threshold, post_diapause_day[[1]]+1, lactin_pars, 2,
                                         day_length, max_day_length, print.l, save.l)
    larva_emergence_day <- stage_change_pred(Tmean, threshold, ovipositing_day[[1]]+1, lactin_pars, 3,
                                             day_length, max_day_length, print.l, save.l)
    pupa_emergence_day <- stage_change_pred(Tmean, threshold, larva_emergence_day[[1]]+1, lactin_pars, 4,
                                            day_length, max_day_length, print.l, save.l)
    adult_emergence_day <- stage_change_pred(Tmean, threshold, pupa_emergence_day[[1]]+1, lactin_pars, 5,
                                             day_length, max_day_length, print.l,save.l)
    sexual_maturation_day <- stage_change_pred(Tmean, threshold, adult_emergence_day[[1]]+1, lactin_pars, 6,
                                               day_length, max_day_length, print.l, save.l)
    full_eval <- c(rep(NA, budburst_day[1]),
                   post_diapause_day[[2]], ovipositing_day[[2]],
                   larva_emergence_day[[2]], pupa_emergence_day[[2]],
                   adult_emergence_day[[2]], sexual_maturation_day[[2]])
    full_eval <- c(full_eval, rep(NA, nrow(data) - length(full_eval)))
    return(full_eval)
  }
  
  ## life-cycle stages
  post_diapause_day <- stage_change_pred(Tmean, threshold, budburst_day+1, lactin_pars, 1,
                                         day_length, max_day_length, print.l, save.l)[[1]]
  ovipositing_day <- stage_change_pred(Tmean, threshold, post_diapause_day+1, lactin_pars, 2,
                                       day_length, max_day_length, print.l, save.l)[[1]]
  larva_emergence_day <- stage_change_pred(Tmean, threshold, ovipositing_day+1, lactin_pars, 3,
                                           day_length, max_day_length, print.l, save.l)[[1]]
  pupa_emergence_day <- stage_change_pred(Tmean, threshold, larva_emergence_day+1, lactin_pars, 4,
                                          day_length, max_day_length, print.l, save.l)[[1]]
  adult_emergence_day <- stage_change_pred(Tmean, threshold, pupa_emergence_day+1, lactin_pars, 5,
                                           day_length, max_day_length, print.l,save.l)[[1]]
  sexual_maturation_day <- stage_change_pred(Tmean, threshold, adult_emergence_day+1, lactin_pars, 6,
                                             day_length, max_day_length, print.l, save.l)[[1]]
  
  ## return all information
  full_eval <- data.frame(budburst_day, post_diapause_day, ovipositing_day, larva_emergence_day,
                          pupa_emergence_day, adult_emergence_day, sexual_maturation_day)
  rownames(full_eval) <- as.character(pop_quantiles)
  return(full_eval)
}
