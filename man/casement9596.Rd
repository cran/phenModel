\name{casement9596}
\alias{casement9596}
\docType{data}
\encoding{UTF-8}
\title{
Casement 1995-1996 Data
}
\description{
Daily minimum and maximum temperature data obtained at the Casement Aerodrome weather station for the period of 1st November 1995 to 31st October 1996.
}
\usage{data("casement9596")}
\format{
  A data frame with 366 observations on the following 7 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Month}}{a numeric vector}
    \item{\code{Day}}{a numeric vector}
    \item{\code{Lat}}{a numeric vector}
    \item{\code{Tmin}}{a numeric vector}
    \item{\code{Tmax}}{a numeric vector}
    \item{\code{day}}{a numeric vector}
  }
}
\details{
The data was obtained from the Irish National Meteorological Agency (Met \enc{Éireann}{Eireann}).
}
\source{
https://www.met.ie/climate/available-data/historical-data
}
\examples{
data(casement9596)

casement1995 <- data.frame(casement9596, 
                           with(casement9596, 
                                get_total_info(Tmax, Tmin, Tbase = 7, Lat, day)))

## evaluating model
quantiles_seq <- seq(.001, .95, .001)

## loading fitted parameters
data(pvulg_budburst_parameters)
data(pvulg_weibull_parameters)
data(pvulg_lactin_parameters)

phen95 <- phen_model(Tmean = "Tmean",
                     thermal_units = "thermal_units",
                     chill_days = "chill_days",
                     day_length = "day_length",
                     bud_pars = pvulg_budburst_parameters,
                     weib_pars = pvulg_weibull_parameters,
                     lactin_pars = pvulg_lactin_parameters,
                     max_day_length = 14.92,
                     pop_quantiles = quantiles_seq,
                     data = casement1995)

phen95_gen2 <- phen_model_gen2(Tmean = "Tmean",
                               day_length = "day_length",
                               bud_pars = pvulg_budburst_parameters,
                               weib_pars = pvulg_weibull_parameters,
                               lactin_pars = pvulg_lactin_parameters,
                               sex_mat_day = phen95["0.05","sexual_maturation_day"],
                               pop_quantiles = .05,
                               data = casement1995)

plot_phen_model(obj = phen95, obj_gen2 = phen95_gen2)
}
\keyword{datasets}
