assign_observed_data <-
function(obj, eggs = NA, larvae = NA, pupae = NA, adults) {
  obj_mean <- apply(obj, 2, function(x) mean(na.omit(x)))  
  obj_sd <- apply(obj, 2, function(x) sd(na.omit(x)))
  
  PDprob <- pnorm(adults,
                  obj_mean["post_diapause_day"], obj_sd["post_diapause_day"],
                  lower.tail = FALSE)
  OVprob <- pnorm(adults,
                  obj_mean["ovipositing_day"], obj_sd["ovipositing_day"],
                  lower.tail = FALSE)
  AEprob <- pnorm(adults,
                  obj_mean["adult_emergence_day"], obj_sd["adult_emergence_day"])
  
  PD_points <- PDprob > AEprob
  OV_points <- (OVprob > AEprob) & !PD_points
  AE_points <- !(PD_points | OV_points)
  
  obs_lengths <- c(length(eggs) + sum(OV_points), length(larvae), length(pupae), sum(PD_points), sum(AE_points))
  max_length <- max(obs_lengths)
  
  post_diapause <- c(adults[PD_points], rep(NA, max_length - sum(PD_points)))
  eggs <- c(eggs, adults[OV_points], rep(NA, max_length - length(eggs) - sum(OV_points)))
  larvae <- c(larvae, rep(NA, max_length - length(larvae)))
  pupae <- c(pupae, rep(NA, max_length - length(pupae)))
  adults <- c(adults[AE_points], rep(NA, max_length - sum(AE_points)))
  
  observed_data <- data.frame(post_diapause, eggs, larvae, pupae, adults)
  return(observed_data)
}
