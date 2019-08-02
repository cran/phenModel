cdl_check <- function(cdl, quantiles, max_day_length) {
  
  cdl_grid <- value <- NULL
  
  ## generate grid
  x <- seq(7, 21, length = 1000)
  
  ## obtain cut-off lines (plotted in black)
  y <- sapply(cdl, function(a) x > a)
  z <- melt(y)[,-1]
  names(z) <- c("quantile","value")
  z$cdl_grid <- x
  z$value <- as.factor(z$value)
  levels(z$value) <- c("no cut","cut")
  
  ## obtain sensitivity lines (plotted in red)
  ## flag FALSE: red to the right of max_day_length
  direction_flag <- sapply(cdl, function(a) max_day_length > a)
  
  ## for flag FALSE, make all values 1, then
  ## change values < max_day_length to NA,
  ## then change values > corresponding cdl to NA
  y2 <- y
  y2[,!direction_flag] <- 1
  y2[x < max_day_length,!direction_flag] <- NA
  for(i in which(!direction_flag)) {
    y2[x > cdl[i],i] <- NA
  }
  ## for flag TRUE, make all values 2, then
  ## change values > max_day_length to NA,
  ## then change values < corresponding cdl to NA
  y2[,direction_flag] <- 2
  y2[x > max_day_length,direction_flag] <- NA
  for(i in which(direction_flag)) {
    y2[x < cdl[i],i] <- NA
  }
  
  z2 <- melt(y2)[,-1]
  names(z2) <- c("quantile","value")
  z2$cdl_grid <- x
  z2 <- na.omit(z2)
  
  p <- ggplot(data = z, mapping = aes(x = cdl_grid, y = value)) +
    theme_bw() +
    geom_line(lwd = 1) +
    geom_line(data = z2, col = 2, lwd = 1) +
    geom_vline(xintercept = max_day_length, lty = 2) +
    facet_wrap(~ quantile) +
    xlab("Critical day length") +
    ylab("")
  
  return(p)
}