get_chill_days <-
function(Tmean, Tbase) {
  chill_days <- rep(0, length(Tmean))
  chill_days[Tmean < Tbase] <- 1
  return(cumsum(chill_days))
}
