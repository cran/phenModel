plot_phen_forecast <-
function(obj, year_from = 1,
                               xlab = "Year", ylab = "Days from 1st November") {
  
  year <- day <- Generation <- Stage <- NULL
  
  ngen <- (ncol(obj) - 7) / 5 + 1
  ncycles <- nrow(obj)
  obj_long <- melt(obj)
  names(obj_long) <- c("Stage","day")
  obj_long$year <- 1:nrow(obj) + year_from - 1
  obj_long$Generation <- factor(c(rep(1, 7*ncycles),
                                            rep(2:ngen, each = 5*ncycles)))
  levels(obj_long$Stage) <- c("Budburst","Post-Diapause",
                              rep(c("Ovipositing Period (Eggs)",
                                    "Larvae Emergence","Pupae Emergence",
                                    "Adult Emergence","Sexual Maturation"), ngen))
  
  p <- ggplot(data = obj_long,
              mapping = aes(x = year, y = day,
                            colour = Generation,
                            pch = Stage)) +
    theme_bw() + geom_line() + geom_point() +
    ylab(ylab) + xlab(xlab) +
    theme(text = element_text(size = 20)) +
    scale_y_continuous(breaks = seq(0, 400, 50)) +
    scale_shape_manual(values = c(16,17,15,3,7,8,0))
  
  #print(p)
  return(p)
}
