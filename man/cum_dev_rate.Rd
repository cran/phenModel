\name{cum_dev_rate}
\alias{cum_dev_rate}
\title{
Cumulative Development Rates
}
\description{
Returns the cumulative development rates calculated when the phenology model is being evaluated.
}
\usage{
cum_dev_rate(Tmean, thermal_units, chill_days, day_length,
             bud_pars, weib_pars, lactin_pars, max_day_length,
             pop_quantiles = 0.5, data)
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
  \item{pop_quantiles}{quantile of the population to be evaluated by the phenology model}
  \item{data}{a \code{data.frame}}
}
\value{
Returns the data with an appended column representing the cumulative development rates evaluated by the model.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{phen_model}}
}
\examples{
data(casement9596)

casement1995 <- data.frame(casement9596, 
                           with(casement9596, 
                                get_total_info(Tmax, Tmin, Tbase = 7, Lat, day)))

## loading fitted parameters
data(pvulg_budburst_parameters)
data(pvulg_weibull_parameters)
data(pvulg_lactin_parameters)

casement1995 <- cum_dev_rate(Tmean = "Tmean",
                             thermal_units = "thermal_units",
                             chill_days = "chill_days",
                             day_length = "day_length",
                             bud_pars = pvulg_budburst_parameters,
                             weib_pars = pvulg_weibull_parameters,
                             lactin_pars = pvulg_lactin_parameters,
                             max_day_length = 14.92,
                             data = casement1995)
}