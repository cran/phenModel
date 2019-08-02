\name{phen_model_forecast}
\alias{phen_model_forecast}
\title{
Multi-Year Forecast Based on the Phenology Model
}
\description{
A forecast based on the phenology model for multiple years, based on daily temperatures.
}
\usage{
phen_model_forecast(Tmean, thermal_units, chill_days, day_length,
                    bud_pars, weib_pars, lactin_pars,
                    max_day_length = 14.92, pop_quantile, year, data)
}
\arguments{
  \item{Tmean}{name of the column in the data representing the mean temperature}
  \item{thermal_units}{name of the column in the data representing the thermal units}
  \item{chill_days}{name of the column in the data representing the number of chill days}
  \item{day_length}{name of the column in the data representing the day length}
  \item{bud_pars}{vector of parameters for the budburst model}
  \item{weib_pars}{matrix of parameters for the Weibull model for each developmental stage}
  \item{lactin_pars}{matrix of parameters for the lactin model for each developmental stage}
  \item{max_day_length}{critical day length threshold}
  \item{pop_quantile}{quantile of the population to be evaluated by the phenology model}
  \item{year}{a vector of indices representing each yearly cycle (usually the output from \code{\link{get_cycle}})}
  \item{data}{a \code{data.frame}}
}
\details{
See \code{\link{phen_model}}
}
\value{
A \code{data.frame} with the number of rows equal to the total number of yearly cycles, and each column representing the developmental stage of the insect.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{plot_phen_forecast}}
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
}