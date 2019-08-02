lactin_wrapper <-
function(Tmean, parms) {
  calc <- lactin(Tmean, parms[1], parms[2], parms[3], parms[4])
  if(calc < 0) calc <- 0
  return(as.numeric(calc))
}
