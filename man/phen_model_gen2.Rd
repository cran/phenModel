\name{phen_model_gen2}
\alias{phen_model_gen2}
\title{
Phenology Model Evaluation for Further Generations
}
\description{
Evaluates the phenology model for specified population quantiles, based on daily temperatures for further generations.
}
\usage{
phen_model_gen2(Tmean, day_length,
                bud_pars, weib_pars, lactin_pars,
                sex_mat_day, pop_quantiles = 0.5,
                max_day_length = 14.92, data)
}
\arguments{
  \item{Tmean}{name of the column in the data representing the mean temperature}
  \item{day_length}{name of the column in the data representing the day length}
  \item{bud_pars}{vector of parameters for the budburst model}
  \item{weib_pars}{matrix of parameters for the Weibull model for each developmental stage}
  \item{lactin_pars}{matrix of parameters for the lactin model for each developmental stage}
  \item{sex_mat_day}{the day individuals from the first generation reached the sexual maturation stage}
  \item{pop_quantiles}{vector of quantiles of the population to be evaluated by the phenology model}
  \item{max_day_length}{critical day length threshold}
  \item{data}{a \code{data.frame}}
}
\details{
See \code{\link{phen_model}}
}
\value{
A \code{data.frame} containing model evaluations for each life cycle (columns) and population quantile (rows).
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{plot_phen_model}}
}
\examples{
data(casement9596)

casement1995 <- data.frame(casement9596, 
                           with(casement9596, 
                                get_total_info(Tmax, Tmin, Tbase = 7, Lat, day)))

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
                     data = casement1995)
           
phen95_gen2 <- phen_model_gen2(Tmean = "Tmean",
                               day_length = "day_length",
                               bud_pars = pvulg_budburst_parameters,
                               weib_pars = pvulg_weibull_parameters,
                               lactin_pars = pvulg_lactin_parameters,
                               sex_mat_day = phen95["0.05","sexual_maturation_day"],
                               pop_quantiles = .05,
                               data = casement1995)
}