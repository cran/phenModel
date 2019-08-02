stage_change_pred <-
function(Tmean, threshold, earlier_stage_day, lactin_pars, stage_code,
                              day_length, max_day_length, print.l = FALSE, save.l = FALSE) {
  nq <- nrow(threshold)
  if(is.null(nq)) {
    nq <- 1
    threshold <- t(matrix(threshold))
  }
  change_day <- NULL
  for(q in 1:nq) {
    i <- earlier_stage_day[q]
    if(save.l) saved_cum_devtime <- NA
    if(is.na(i) || (stage_code == 6 & day_length[i] < max_day_length) |
       (i > 365)) {
      change_day[q] <- NA
      if(save.l) saved_cum_devtime <- c(saved_cum_devtime, NA)
    } else {
      l <- lactin_wrapper(Tmean[i], lactin_pars[,stage_code])
      cum_devtime <- l
      if(cum_devtime < 0) cum_devtime <- 0
      if(save.l) saved_cum_devtime <- cum_devtime
      while(cum_devtime < threshold[q,stage_code]) {
        if(print.l) cat("Day: ", i, ", Cumulative dev. rate: ", cum_devtime, "\n", sep = "")
        i <- i + 1
        if(i > 365) {
          i <- NA
          break
        }
        l <- l + lactin_wrapper(Tmean[i], lactin_pars[,stage_code])
        cum_devtime <- l
        if(cum_devtime < 0) cum_devtime <- 0
        if(save.l) saved_cum_devtime <- c(saved_cum_devtime, cum_devtime)
      }
      change_day[q] <- i
    }
  }
  if(save.l) return(list("change_day" = change_day, "cum_devtime" = saved_cum_devtime))
  return(list("change_day" = change_day))
}
