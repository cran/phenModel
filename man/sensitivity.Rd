\name{sensitivity}
\alias{sensitivity}
\alias{sensitivity_Tbase}
\alias{sensitivity_lactin}
\alias{sensitivity_weibull}
\alias{sensitivity_bud_days}
\alias{sensitivity_bud_parms}
\alias{sensitivity_cdl}
\alias{summary_sensitivity}
\alias{boxplot_sensitivity}
\title{
Sensitivity Analysis for Evaluated Phenology Model
}
\description{
Runs a sensitivity analysis on the evaluated phenology model, by simulating random changes to fitted parameters and/or specifying a range of base temperatures to determine a chill day.
}
\usage{
sensitivity(Tmax, Tmin, Tbase, lat, day,
            bud_pars, weib_pars, lactin_pars, max_day_length, 
            pop_quantile = .5, data,
            study_type = c("Tbase","lactin","weibull","bud_days","bud_parms","cdl"),
            percent_variation = .1, resolution = 1000)

summary_sensitivity(obj)

boxplot_sensitivity(obj)
}
\arguments{
  \item{obj}{an output from a call to the \code{sensitivity} function}
  \item{Tmax}{a vector of daily maximum temperatures}
  \item{Tmin}{a vector of daily minimum temperatures}
  \item{Tbase}{the base temperature}
  \item{lat}{the latitude}
  \item{day}{day of the year (vector)}
  \item{bud_pars}{vector of parameters for the budburst model}
  \item{weib_pars}{matrix of parameters for the Weibull model for each developmental stage}
  \item{lactin_pars}{matrix of parameters for the lactin model for each developmental stage}
  \item{max_day_length}{critical day length threshold}
  \item{pop_quantile}{quantile of the population to be evaluated by the phenology model}
  \item{data}{a \code{data.frame}}
  \item{study_type}{a character string indicating what type of sensitivity study to be performed. \code{"Tbase"} runs a sensitivity study for the temperature threshold that classifies a chill day; \code{"lactin"} runs a sensitivity study on variations of the values for the parameters estimated by the lactin model; \code{"weibull"} does the same as the latter, but for the Weibull model; \code{"bud_days"} changes the budburst day while \code{"bud_parms"} randomly changes the parameter estimates for the exponential decay function associated with budburst prediction; \code{"cdl"} changes the critical day length threshold}
  \item{percent_variation}{percentage of variation of the values to be changed for the sensitivity analysis}
  \item{resolution}{number of model evaluations to be performed}
}
\value{
A \code{list} containing the sensitivity analysis results as the first component, and the original model evaluation results as the second component.
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

data(pvulg_budburst_parameters)
data(pvulg_weibull_parameters)
data(pvulg_lactin_parameters)

sens_casement1995 <- sensitivity(Tmax = casement9596$Tmax,
                                 Tmin = casement9596$Tmin,
                                 Tbase = 7,
                                 lat = casement9596$Lat,
                                 day = casement9596$day,
                                 bud_pars = pvulg_budburst_parameters,
                                 weib_pars = pvulg_weibull_parameters,
                                 lactin_pars = pvulg_lactin_parameters,
                                 max_day_length = 14.92, 
                                 pop_quantile = .5,
                                 data = casement9596,
                                 study_type = "lactin",
                                 percent_variation = .1,
                                 resolution = 100)
                                 
summary_sensitivity(sens_casement1995)

boxplot_sensitivity(sens_casement1995)
}