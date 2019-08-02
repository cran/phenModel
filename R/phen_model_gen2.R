phen_model_gen2 <-
function(Tmean, day_length,
                            bud_pars, weib_pars, lactin_pars, 
                            sex_mat_day, pop_quantiles = .5, max_day_length = 14.92,
                            data) {
  if(is.na(sex_mat_day)) {
    ovipositing_day <- larva_emergence_day <-
      pupa_emergence_day <- adult_emergence_day <-
      sexual_maturation_day <- rep(NA, length(pop_quantiles))
    full_eval <- data.frame(ovipositing_day, larva_emergence_day,
                            pupa_emergence_day, adult_emergence_day, sexual_maturation_day) 
    rownames(full_eval) <- as.character(pop_quantiles)
    return(full_eval)
  }
  
  Tmean <- data[,Tmean]
  day_length <- data[,day_length]
  
  threshold <- apply(weib_pars, 2, weibull3p_inverse_wrapper, cum_prob = pop_quantiles)
  
  ## life-cycle stages
  ovipositing_day <- stage_change_pred_gen2(Tmean, threshold, sex_mat_day+1, lactin_pars, 2)
  larva_emergence_day <- stage_change_pred_gen2(Tmean, threshold, ovipositing_day+1, lactin_pars, 3)
  pupa_emergence_day <- stage_change_pred_gen2(Tmean, threshold, larva_emergence_day+1, lactin_pars, 4)
  adult_emergence_day <- stage_change_pred_gen2(Tmean, threshold, pupa_emergence_day+1, lactin_pars, 5)
  if(is.na(adult_emergence_day)) {
    sexual_maturation_day <- NA
  } else if(day_length[adult_emergence_day] < max_day_length) {
    sexual_maturation_day <- NA
  } else {
    sexual_maturation_day <- stage_change_pred_gen2(Tmean, threshold, adult_emergence_day+1, lactin_pars, 6)
  }
  
  ## return all information
  full_eval <- data.frame(ovipositing_day, larva_emergence_day,
                          pupa_emergence_day, adult_emergence_day, sexual_maturation_day)
  rownames(full_eval) <- as.character(pop_quantiles)
  return(full_eval)
}
