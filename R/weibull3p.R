weibull3p <-
function(norm_time, gamma, eta, beta) {
  1 - exp(-((norm_time - gamma)/eta)^beta) 
}
