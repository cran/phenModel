bp_maxmin <-
function(x, probs = c(0,0,.5,1,1)) {
  r <- quantile(x, probs = probs, na.rm = TRUE)
  names(r) <- c("ymin","lower","middle","upper","ymax")
  r
}
