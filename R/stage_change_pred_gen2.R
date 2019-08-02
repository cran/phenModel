stage_change_pred_gen2 <-
function(Tmean, threshold, earlier_stage_day, lactin_pars, stage_code) {
  if(is.na(earlier_stage_day)) return(NA)
  nq <- nrow(threshold)
  if(is.null(nq)) {
    nq <- 1
    threshold <- t(matrix(threshold))
  }
  change_day <- NULL
  for(q in 1:nq) {
    i <- earlier_stage_day[q]
    l <- lactin_wrapper(Tmean[i], lactin_pars[,stage_code])
    cum_devtime <- l
    if(cum_devtime < 0) cum_devtime <- 0
    while(cum_devtime < threshold[q,stage_code] & i < 365) {
      i <- i + 1
      l <- l + lactin_wrapper(Tmean[i], lactin_pars[,stage_code])
      cum_devtime <- l
      if(cum_devtime < 0) cum_devtime <- 0
    }
    if(i == 365) {
      change_day[q] <- NA
    } else {
      change_day[q] <- i
    }
  }
  return(change_day)
}
