weibull3p_inverse <-
function(cum_prob, gamma, eta, beta) {
  gamma + eta * (- log(1 - cum_prob))^(1/beta)
}
