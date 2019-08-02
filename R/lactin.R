lactin <-
function(Tmean, rho, Tmax, deltaT, lambda) {
  exp(rho * Tmean) - exp(rho * Tmax - (Tmax - Tmean)/deltaT) + lambda
}
