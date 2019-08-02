plot_phen_model <-
function(obj, obj_gen2, binwidth = 8,
                            forecast = FALSE, observed_data = NULL,
                            ylab = "Days from 1st November",
                            xlab = "Modelled Life-Cycle Stages",
                            labels = c("Budburst","Post-Diapause",
                                       "Ovipositing Period (Eggs)",
                                       "Larvae Emergence","Pupae Emergence",
                                       "Adult Emergence","Sexual Maturation")) {
  
  Stage2 <- Day <- Stage <- NULL
  
  n_quantiles <- nrow(obj)
  if(n_quantiles < 3) stop("please use a longer sequence of quantiles in phen_model")
  
  obj_long <- melt(obj)
  levels(obj_long$variable) <- c("Budburst","Post-Diapause","Ovipositing Period (Eggs) (1st Gen)",
                                 "Larvae Emergence (1st Gen)","Pupae Emergence (1st Gen)",
                                 "Adult Emergence (1st Gen)","Sexual Maturation (1st Gen)")
  names(obj_long) <- c("Stage2","Day")
  obj_long$Stage <- obj_long$Stage2
  levels(obj_long$Stage) <- c("1 Budburst","2 Post-Diapause","3 Ovipositing Period (Eggs)",
                              "4 Larvae Emergence","5 Pupae Emergence",
                              "6 Adult Emergence","7 Sexual Maturation")
  
  if(!missing(obj_gen2)) {
    obj_gen2_long <- melt(obj_gen2)
    levels(obj_gen2_long$variable) <- c("Ovipositing Period (Eggs) (2nd Gen)",
                                        "Larvae Emergence (2nd Gen)","Pupae Emergence (2nd Gen)",
                                        "Adult Emergence (2nd Gen)","Sexual Maturation (2nd Gen)")
    names(obj_gen2_long) <- c("Stage2","Day")
    obj_gen2_long$Stage <- obj_gen2_long$Stage2
    levels(obj_gen2_long$Stage) <- c("3 Ovipositing Period (Eggs)",
                                     "4 Larvae Emergence","5 Pupae Emergence",
                                     "6 Adult Emergence","7 Sexual Maturation")
    obj_gen2_long <- rbind(obj_gen2_long, obj_long[1,])
    if(!forecast) {
      obj_full <- rbind(obj_long[-c(2:n_quantiles),], obj_gen2_long)
    } else {
      obj_full <- rbind(obj_long, obj_gen2_long)
    }
    max.y <- max(c(375, max(obj), max(obj_gen2)))
  } else {
    if(!forecast) {
      obj_full <- obj_long[-c(2:n_quantiles),]
    } else {
      obj_full <- obj_long
    }
    max.y <- max(c(375, max(obj)))
  }
  
  min.y <- min(100, min(obj) - 50)
  
  p <- ggplot(data = obj_full, mapping = aes(x = Stage2, y = Day, fill = Stage)) +
    theme_bw() + coord_flip() +
    geom_flat_violin(scale = "width", bw = binwidth)
  if(!missing(obj_gen2)) {
    if(!forecast) p <- p + geom_point(data = obj_gen2_long, pch = 21, cex = 4)
  } else {
    if(!forecast) p <- p + geom_point()
  }
  p <- p +
    ylab(ylab) + xlab(xlab) +
    theme(text = element_text(size = 20)) +
    scale_fill_discrete(labels = labels) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    ylim(c(min.y, max.y))
  
  if(!is.null(observed_data)) {
    colnames(observed_data) <- c("Post-Diapause",
                                 "Ovipositing Period (Eggs) (1st Gen)",
                                 "Larvae Emergence (1st Gen)",
                                 "Pupae Emergence (1st Gen)",
                                 "Adult Emergence (1st Gen)")
    obs_data <- melt(observed_data)  
    names(obs_data) <- c("Stage2","Day")
    obs_data$Stage <- obs_data$Stage2
    levels(obs_data$Stage) <- c("2 Post-Diapause",
                                "3 Ovipositing Period (Eggs)",
                                "4 Larvae Emergence",
                                "5 Pupae Emergence",
                                "6 Adult Emergence")
    
    p <- p + geom_point(data = obs_data,
                        mapping = aes(x = Stage2, y = Day,
                                      fill = NULL),
                        pch = 21, cex = 3, fill = 1, alpha = .5)
  }
  
  #print(p)
  return(p)
}
