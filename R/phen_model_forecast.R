phen_model_forecast <-
function(Tmean, thermal_units, chill_days, day_length,
                                bud_pars, weib_pars, lactin_pars, max_day_length = 14.92,
                                pop_quantile, year, data) {
  
  if(length(pop_quantile) > 1) stop("please choose only one quantile")
  
  ## note that year must start on 1st Nov and end on 31st Oct of the following year
  year <- data[,year]
  data_split <- split(data, year)
  nyears <- length(data_split)
  
  phen <- matrix(NA, ncol = 7, nrow = nyears)
  for(i in 1:nyears) {
    phen[i,] <- as.matrix(
      phen_model(Tmean = Tmean, thermal_units = thermal_units,
                 chill_days = chill_days, day_length = day_length,
                 bud_pars = bud_pars, weib_pars = weib_pars,
                 lactin_pars = lactin_pars, max_day_length = max_day_length,
                 pop_quantiles = pop_quantile, data = data_split[[i]])
    )
  }
  
  rownames(phen) <- 1:nyears
  colnames(phen) <- c("budburst_day","post_diapause_day","ovipositing_day","larva_emergence_day",
                      "pupa_emergence_day","adult_emergence_day","sexual_maturation_day")
  phen <- as.data.frame(phen)
  
  ## code subsequent generations - keep going as long as they can fit within 365 days
  phen2 <- matrix(NA, ncol = 5, nrow = nyears)
  for(i in 1:nyears) {
    phen2[i,] <- as.matrix(
      phen_model_gen2(Tmean = Tmean, day_length = day_length,
                      bud_pars = bud_pars, weib_pars = weib_pars,
                      lactin_pars = lactin_pars, sex_mat_day = phen[i,7],
                      pop_quantiles = pop_quantile, max_day_length = max_day_length,
                      data = data_split[[i]])
    )
  }
  
  colnames(phen2) <- paste(colnames(phen)[3:7], "_gen2", sep = "")
  phen2 <- as.data.frame(phen2)
  
  phen <- data.frame(phen, phen2)
  
  gen <- 2
  while(sum(is.na(phen[,7+5*(gen-1)])) < nyears) {
    gen <- gen + 1
    phen_new <- matrix(NA, ncol = 5, nrow = nyears)
    for(i in 1:nyears) {
      phen_new[i,] <- as.matrix(
        phen_model_gen2(Tmean = Tmean, day_length = day_length,
                        bud_pars = bud_pars, weib_pars = weib_pars,
                        lactin_pars = lactin_pars, sex_mat_day = phen[i,7+5*(gen-2)],
                        pop_quantiles = pop_quantile, max_day_length = max_day_length,
                        data = data_split[[i]])
      )
    }
    
    colnames(phen_new) <- paste(colnames(phen)[3:7], "_gen", gen, sep = "")
    phen_new <- as.data.frame(phen_new)
    
    phen <- data.frame(phen, phen_new)
  }
  
  return(phen)
}
