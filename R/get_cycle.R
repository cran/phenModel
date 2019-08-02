get_cycle <-
function(data, day, month, day.end, month.end) {
  cycle <- numeric(nrow(data))
  i <- 1
  j <- 1
  while(j < nrow(data)) {
    while(!(data$month[j] == month.end & data$day[j] == day.end)) {
      cycle[j] <- i
      j <- j + 1
    }
    cycle[j] <- i
    j <- j + 1
    i <- i + 1
  }
  return(cycle)
}
