sensitivity <- function(Tmax, Tmin, Tbase, lat, day,
                        bud_pars, weib_pars, lactin_pars, max_day_length, 
                        pop_quantile = .5, data,
                        study_type = c("Tbase","lactin","weibull","bud_days","bud_parms","cdl"),
                        percent_variation = .1, resolution = 1000) {
  
  if(length(pop_quantile) > 1) stop("sensitivity analysis is only available for a single population quantile")
  
  ## getting original evaluation
  info <- get_total_info(Tmax = Tmax,
                         Tmin = Tmin,
                         Tbase = Tbase,
                         lat = lat,
                         day = day)
  
  full_data <- data.frame(data, info)
  
  original_eval <- phen_model(Tmean = "Tmean",
                              thermal_units = "thermal_units",
                              chill_days = "chill_days",
                              day_length = "day_length",
                              bud_pars = bud_pars,
                              weib_pars = weib_pars,
                              lactin_pars = lactin_pars,
                              max_day_length = max_day_length,
                              pop_quantiles = pop_quantile,
                              data = full_data)
  
  if(study_type == "cdl") {
    original_eval_gen2 <- phen_model_gen2(Tmean = "Tmean",
                                          day_length = "day_length",
                                          bud_pars = bud_pars,
                                          weib_pars = weib_pars,
                                          lactin_pars = lactin_pars,
                                          sex_mat_day = original_eval[1,"sexual_maturation_day"],
                                          max_day_length = max_day_length,
                                          pop_quantiles = pop_quantile,
                                          data = full_data)
    
    original_eval <- cbind(original_eval, original_eval_gen2)
  }
  
  ## switching for each type of sensitivity study
  sensitivity_results <- switch(study_type,
                                "Tbase" = sensitivity_Tbase(percent_variation = percent_variation,
                                                            resolution = resolution, Tbase = Tbase,
                                                            Tmax = Tmax, Tmin = Tmin, lat = lat, day = day,
                                                            bud_pars = bud_pars, weib_pars = weib_pars,
                                                            lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                            pop_quantile = pop_quantile, data = data),
                                "lactin" = sensitivity_lactin(percent_variation = percent_variation,
                                                              resolution = resolution, full_data = full_data,
                                                              bud_pars = bud_pars, weib_pars = weib_pars,
                                                              lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                              pop_quantile = pop_quantile),
                                "weibull" = sensitivity_weibull(percent_variation = percent_variation,
                                                                resolution = resolution, full_data = full_data,
                                                                bud_pars = bud_pars, weib_pars = weib_pars,
                                                                lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                                pop_quantile = pop_quantile),
                                "bud_days" = sensitivity_bud_days(percent_variation = percent_variation,
                                                                  resolution = resolution, thermal_units = "thermal_units",
                                                                  chill_days = "chill_days", day_length = "day_length",
                                                                  bud_pars = bud_pars, weib_pars = weib_pars,
                                                                  lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                                  pop_quantile = pop_quantile, full_data = full_data),
                                "bud_parms" = sensitivity_bud_parms(percent_variation = percent_variation,
                                                                    resolution = resolution, full_data = full_data,
                                                                    bud_pars = bud_pars, weib_pars = weib_pars,
                                                                    lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                                    pop_quantile = pop_quantile),
                                "cdl" = sensitivity_cdl(percent_variation = percent_variation,
                                                        resolution = resolution, full_data = full_data,
                                                        bud_pars = bud_pars, weib_pars = weib_pars,
                                                        lactin_pars = lactin_pars, max_day_length = max_day_length,
                                                        pop_quantile = pop_quantile))
  
  ## returning results
  return(list("sensitivity_results" = sensitivity_results, "original_eval" = original_eval))
}

sensitivity_Tbase <- function(percent_variation, resolution, Tbase, Tmax, Tmin, lat, day,
                              bud_pars, weib_pars, lactin_pars, max_day_length,
                              pop_quantile, data) {
  Tbase_values <- seq(Tbase * (1 - percent_variation),
                      Tbase * (1 + percent_variation),
                      length = resolution)
  
  info <- get_total_info(Tmax = Tmax,
                         Tmin = Tmin,
                         Tbase = Tbase_values[1],
                         lat = lat,
                         day = day)
  full_data <- data.frame(data, info)
  all_model_evals <- phen_model(Tmean = "Tmean",
                                thermal_units = "thermal_units",
                                chill_days = "chill_days",
                                day_length = "day_length",
                                bud_pars = bud_pars,
                                weib_pars = weib_pars,
                                lactin_pars = lactin_pars,
                                max_day_length = max_day_length,
                                pop_quantiles = pop_quantile,
                                data = full_data)
  rownames(all_model_evals) <- paste(Tbase_values[1])
  
  for(i in Tbase_values[-1]) {
    info <- get_total_info(Tmax = Tmax,
                           Tmin = Tmin,
                           Tbase = i,
                           lat = lat,
                           day = day)
    full_data <- data.frame(data, info)
    model_eval <- phen_model(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = bud_pars,
                             weib_pars = weib_pars,
                             lactin_pars = lactin_pars,
                             max_day_length = max_day_length,
                             pop_quantiles = pop_quantile,
                             data = full_data)
    rownames(model_eval) <- paste(i)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  return(all_model_evals)
}

sensitivity_lactin <- function(percent_variation, resolution,
                               full_data, bud_pars, weib_pars, lactin_pars,
                               max_day_length, pop_quantile) {
  
  all_pars <- as.numeric(lactin_pars)
  all_pars_min <- all_pars * (1 - percent_variation)
  all_pars_max <- all_pars * (1 + percent_variation)
  
  all_pars_sample <- numeric(length(all_pars))
  for(i in 1:length(all_pars)) {
    min_i <- min(all_pars_min[i], all_pars_max[i])
    max_i <- max(all_pars_min[i], all_pars_max[i])
    all_pars_sample[i] <- runif(1, min_i, max_i)
  }
  
  lactin_pars_sim <- matrix(all_pars_sample, nrow = 4)
  dimnames(lactin_pars_sim) <- dimnames(lactin_pars)
  
  all_model_evals <- phen_model(Tmean = "Tmean",
                                thermal_units = "thermal_units",
                                chill_days = "chill_days",
                                day_length = "day_length",
                                bud_pars = bud_pars,
                                weib_pars = weib_pars,
                                lactin_pars = lactin_pars_sim,
                                max_day_length = max_day_length,
                                pop_quantiles = pop_quantile,
                                data = full_data)
  rownames(all_model_evals) <- "1"
  
  for(k in 2:resolution) {
    all_pars_sample <- numeric(length(all_pars))
    for(i in 1:length(all_pars)) {
      min_i <- min(all_pars_min[i], all_pars_max[i])
      max_i <- max(all_pars_min[i], all_pars_max[i])
      all_pars_sample[i] <- runif(1, min_i, max_i)
    }
    
    lactin_pars_sim <- matrix(all_pars_sample, nrow = 4)
    dimnames(lactin_pars_sim) <- dimnames(lactin_pars)
    
    model_eval <- phen_model(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = bud_pars,
                             weib_pars = weib_pars,
                             lactin_pars = lactin_pars_sim,
                             max_day_length = max_day_length,
                             pop_quantiles = pop_quantile,
                             data = full_data)
    rownames(model_eval) <- paste(k)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  return(all_model_evals)
}

sensitivity_weibull <- function(percent_variation, resolution,
                                full_data, bud_pars, weib_pars, lactin_pars,
                                max_day_length, pop_quantile) {
  
  all_pars <- as.numeric(weib_pars)
  all_pars_min <- all_pars * (1 - percent_variation)
  all_pars_max <- all_pars * (1 + percent_variation)
  
  all_pars_sample <- numeric(length(all_pars))
  for(i in 1:length(all_pars)) {
    min_i <- min(all_pars_min[i], all_pars_max[i])
    max_i <- max(all_pars_min[i], all_pars_max[i])
    all_pars_sample[i] <- runif(1, min_i, max_i)
  }
  
  weib_pars_sim <- matrix(all_pars_sample, nrow = 3)
  dimnames(weib_pars_sim) <- dimnames(weib_pars)
  
  all_model_evals <- phen_model(Tmean = "Tmean",
                                thermal_units = "thermal_units",
                                chill_days = "chill_days",
                                day_length = "day_length",
                                bud_pars = bud_pars,
                                weib_pars = weib_pars_sim,
                                lactin_pars = lactin_pars,
                                max_day_length = max_day_length,
                                pop_quantiles = pop_quantile,
                                data = full_data)
  rownames(all_model_evals) <- "1"
  
  for(k in 2:resolution) {
    all_pars_sample <- numeric(length(all_pars))
    for(i in 1:length(all_pars)) {
      min_i <- min(all_pars_min[i], all_pars_max[i])
      max_i <- max(all_pars_min[i], all_pars_max[i])
      all_pars_sample[i] <- runif(1, min_i, max_i)
    }
    
    weib_pars_sim <- matrix(all_pars_sample, nrow = 3)
    dimnames(weib_pars_sim) <- dimnames(weib_pars)
    
    model_eval <- phen_model(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = bud_pars,
                             weib_pars = weib_pars_sim,
                             lactin_pars = lactin_pars,
                             max_day_length = max_day_length,
                             pop_quantiles = pop_quantile,
                             data = full_data)
    rownames(model_eval) <- paste(k)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  return(all_model_evals)
}

sensitivity_bud_parms <- function(percent_variation, resolution,
                                  full_data, bud_pars, weib_pars, lactin_pars,
                                  max_day_length, pop_quantile) {
  
  all_pars <- as.numeric(bud_pars)
  all_pars_min <- all_pars * (1 - percent_variation)
  all_pars_max <- all_pars * (1 + percent_variation)
  
  all_pars_sample <- numeric(length(all_pars))
  for(i in 1:length(all_pars)) {
    min_i <- min(all_pars_min[i], all_pars_max[i])
    max_i <- max(all_pars_min[i], all_pars_max[i])
    all_pars_sample[i] <- runif(1, min_i, max_i)
  }
  
  bud_pars_sim <- matrix(all_pars_sample, nrow = 3)
  dimnames(bud_pars_sim) <- dimnames(bud_pars)
  
  all_model_evals <- phen_model(Tmean = "Tmean",
                                thermal_units = "thermal_units",
                                chill_days = "chill_days",
                                day_length = "day_length",
                                bud_pars = bud_pars_sim,
                                weib_pars = weib_pars,
                                lactin_pars = lactin_pars,
                                max_day_length = max_day_length,
                                pop_quantiles = pop_quantile,
                                data = full_data)
  rownames(all_model_evals) <- "1"
  
  for(k in 2:resolution) {
    all_pars_sample <- numeric(length(all_pars))
    for(i in 1:length(all_pars)) {
      min_i <- min(all_pars_min[i], all_pars_max[i])
      max_i <- max(all_pars_min[i], all_pars_max[i])
      all_pars_sample[i] <- runif(1, min_i, max_i)
    }
    
    bud_pars_sim <- matrix(all_pars_sample, nrow = 3)
    dimnames(bud_pars_sim) <- dimnames(bud_pars)
    
    model_eval <- phen_model(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = bud_pars_sim,
                             weib_pars = weib_pars,
                             lactin_pars = lactin_pars,
                             max_day_length = max_day_length,
                             pop_quantiles = pop_quantile,
                             data = full_data)
    rownames(model_eval) <- paste(k)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  return(all_model_evals)
}

sensitivity_cdl <- function(percent_variation, resolution,
                            full_data, bud_pars, weib_pars, lactin_pars,
                            max_day_length, pop_quantile) {
  
  cdl_values <- seq(max_day_length * (1 - percent_variation),
                    max_day_length * (1 + percent_variation),
                    length = resolution)
  
  all_model_evals <- phen_model(Tmean = "Tmean",
                                thermal_units = "thermal_units",
                                chill_days = "chill_days",
                                day_length = "day_length",
                                bud_pars = bud_pars,
                                weib_pars = weib_pars,
                                lactin_pars = lactin_pars,
                                max_day_length = cdl_values[1],
                                pop_quantiles = pop_quantile,
                                data = full_data)
  rownames(all_model_evals) <- paste(cdl_values[1])
  
  for(i in cdl_values[-1]) {
    model_eval <- phen_model(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = bud_pars,
                             weib_pars = weib_pars,
                             lactin_pars = lactin_pars,
                             max_day_length = i,
                             pop_quantiles = pop_quantile,
                             data = full_data)
    rownames(model_eval) <- paste(i)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  all_model_evals_gen2 <- phen_model_gen2(Tmean = "Tmean",
                                          day_length = "day_length",
                                          bud_pars = bud_pars,
                                          weib_pars = weib_pars,
                                          lactin_pars = lactin_pars, 
                                          sex_mat_day = all_model_evals[1,"sexual_maturation_day"],
                                          pop_quantiles = pop_quantile,
                                          max_day_length = cdl_values[1], 
                                          data = full_data)
  rownames(all_model_evals_gen2) <- paste(cdl_values[1])
  
  for(k in 2:nrow(all_model_evals)) {
    model_eval_gen2 <- phen_model_gen2(Tmean = "Tmean",
                                       day_length = "day_length",
                                       bud_pars = bud_pars,
                                       weib_pars = weib_pars,
                                       lactin_pars = lactin_pars,
                                       sex_mat_day = all_model_evals[k,"sexual_maturation_day"],
                                       max_day_length = cdl_values[k],
                                       pop_quantiles = pop_quantile,
                                       data = full_data)
    rownames(model_eval_gen2) <- paste(cdl_values[k])
    all_model_evals_gen2 <- rbind(all_model_evals_gen2, model_eval_gen2)
  }
  
  all_model_evals <- cbind(all_model_evals, all_model_evals_gen2)
  return(all_model_evals)
}

sensitivity_bud_days <- function(percent_variation, resolution, 
                                 thermal_units, chill_days, day_length,
                                 bud_pars, weib_pars, lactin_pars, max_day_length,
                                 pop_quantile, full_data) {
  
  phen_model_local <-
    function(Tmean, day_length,
             bud_pars, weib_pars, lactin_pars, max_day_length, 
             pop_quantiles, data, budburst) {
      
      print.l <- save.l <- FALSE
      
      Tmean <- data[,Tmean]
      day_length <- data[,day_length]
      
      threshold <- apply(weib_pars, 2, weibull3p_inverse_wrapper, cum_prob = pop_quantiles)
      
      ## day of budburst
      budburst_day <- rep(budburst, length(pop_quantiles))
      
      ## life-cycle stages
      post_diapause_day <- stage_change_pred(Tmean, threshold, budburst_day+1, lactin_pars, 1,
                                                         day_length, max_day_length, print.l, save.l)[[1]]
      ovipositing_day <- stage_change_pred(Tmean, threshold, post_diapause_day+1, lactin_pars, 2,
                                                       day_length, max_day_length, print.l, save.l)[[1]]
      larva_emergence_day <- stage_change_pred(Tmean, threshold, ovipositing_day+1, lactin_pars, 3,
                                                           day_length, max_day_length, print.l, save.l)[[1]]
      pupa_emergence_day <- stage_change_pred(Tmean, threshold, larva_emergence_day+1, lactin_pars, 4,
                                                          day_length, max_day_length, print.l, save.l)[[1]]
      adult_emergence_day <- stage_change_pred(Tmean, threshold, pupa_emergence_day+1, lactin_pars, 5,
                                                           day_length, max_day_length, print.l,save.l)[[1]]
      sexual_maturation_day <- stage_change_pred(Tmean, threshold, adult_emergence_day+1, lactin_pars, 6,
                                                             day_length, max_day_length, print.l, save.l)[[1]]
      
      ## return all information
      full_eval <- data.frame(budburst_day, post_diapause_day, ovipositing_day, larva_emergence_day,
                              pupa_emergence_day, adult_emergence_day, sexual_maturation_day)
      rownames(full_eval) <- as.character(pop_quantiles)
      return(full_eval)
    }
  
  budburst_orig <- budburst_pred(full_data[thermal_units], full_data[chill_days], bud_pars)
  
  bud_values <- seq(budburst_orig * (1 - percent_variation),
                    budburst_orig * (1 + percent_variation),
                    length = resolution)
  bud_values <- unique(floor(bud_values))
  
  all_model_evals <- phen_model_local(Tmean = "Tmean",
                                      day_length = "day_length",
                                      bud_pars = bud_pars,
                                      weib_pars = weib_pars,
                                      lactin_pars = lactin_pars,
                                      max_day_length = max_day_length,
                                      pop_quantiles = pop_quantile,
                                      data = full_data,
                                      budburst = bud_values[1])
  rownames(all_model_evals) <- paste(bud_values[1])
  
  for(i in bud_values[-1]) {
    model_eval <- phen_model_local(Tmean = "Tmean",
                                   day_length = "day_length",
                                   bud_pars = bud_pars,
                                   weib_pars = weib_pars,
                                   lactin_pars = lactin_pars,
                                   max_day_length = max_day_length,
                                   pop_quantiles = pop_quantile,
                                   data = full_data,
                                   budburst = i)
    rownames(model_eval) <- paste(i)
    all_model_evals <- rbind(all_model_evals, model_eval)
  }
  
  return(all_model_evals)
}

summary_sensitivity <- function(obj) {
  sens <- obj$sensitivity_results
  orig <- obj$original_eval
  
  cdl_flag <- ncol(sens) > 7
  
  if(!cdl_flag) {
    summaries <- apply(sens, 2, function(x) quantile(na.omit(x), probs = c(.025, .5, .975)))
    summaries <- rbind(summaries, orig)
    rownames(summaries)[4] <- "original evaluation"
  } else {
    summaries <- apply(sens[,c(7,12)], 2, function(x) round(
      as.numeric(
        rownames(sens)[min(which(is.na(x)))]
        ), 2))
    cat("Original run:", "\n")
    colnames(orig)[c(7,12)] <- c("sexual_maturation_gen1","sexual_maturation_gen2")
    print(orig[,c(7,12)])
    cat("CDL threshold from which sexual maturation does not happen:", "\n")
    names(summaries) <- c("sexual_maturation_gen1","sexual_maturation_gen2")
  }
  return(summaries)
}

boxplot_sensitivity <- function(obj) {
  
  value <- variable <- NULL
  
  sens <- melt(obj$sensitivity_results)
  orig <- melt(obj$original_eval)
  levels(sens$variable) <- levels(orig$variable) <-
    c("Budburst", "Post-Diapause", "Ovipositing Period", 
      "Larvae Emergence", "Pupae Emergence", "Adult Emergence",
      "Sexual Maturation")
  
  p <- ggplot(data = sens, mapping = aes(x = variable, y = value)) +
    theme_bw() +
    geom_boxplot(fill = "#2dbde7", alpha = .8) +
    geom_point(data = orig, cex = 5, pch = 21, bg = "orange") +
    xlab("Stage") +
    ylab("Days from 1st November") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}