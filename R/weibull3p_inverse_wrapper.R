weibull3p_inverse_wrapper <-
function(cum_prob, parms) {
  calc <- weibull3p_inverse(cum_prob, parms[1], parms[2], parms[3]) 
  return(as.numeric(calc))
}
