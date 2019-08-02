\name{valentia}
\alias{valentia}
\docType{data}
\encoding{UTF-8}
\title{
Valentia Data
}
\description{
Daily minimum and maximum temperature data obtained at the Valentia synoptic weather station for the period of 1st November 1939 to 31st October 2000.
}
\usage{data("valentia")}
\format{
  A data frame with 22281 observations on the following 7 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{day}}{a numeric vector}
    \item{\code{Lat}}{a numeric vector}
    \item{\code{max_temp}}{a numeric vector}
    \item{\code{min_temp}}{a numeric vector}
    \item{\code{day_of_year}}{a numeric vector}
  }
}
\details{
The data was obtained from the Irish National Meteorological Agency (Met \enc{Éireann}{Eireann}).
}
\source{
https://www.met.ie/climate/available-data/historical-data
}
\examples{
data(valentia)

valentia$cycle <- get_cycle(data = valentia, day = "day", month = "month",
                            day.end = 31, month.end = 10)
valentia_split <- split(valentia, valentia$cycle)
valentia_split <- lapply(valentia_split,
                         function(x) data.frame(x,
                                                with(x, get_total_info(Tmax = x$max_temp,
                                                                       Tmin = x$min_temp,
                                                                       Tbase = 7,
                                                                       lat = x$Lat,
                                                                       x$day_of_year))))
valentia <- do.call("rbind", valentia_split)

data(pvulg_budburst_parameters)
data(pvulg_weibull_parameters)
data(pvulg_lactin_parameters)

phen_forecast <- phen_model_forecast(Tmean = "Tmean",
                                     thermal_units = "thermal_units",
                                     chill_days = "chill_days",
                                     day_length = "day_length",
                                     bud_pars = pvulg_budburst_parameters,
                                     weib_pars = pvulg_weibull_parameters,
                                     lactin_pars = pvulg_lactin_parameters,
                                     pop_quantile = .5,
                                     year = "cycle",
                                     data = valentia)

plot_phen_model(obj = phen_forecast[,1:7], obj_gen2 = phen_forecast[,8:12],
                forecast = TRUE)

plot_phen_forecast(obj = phen_forecast, year_from = 1939)
}