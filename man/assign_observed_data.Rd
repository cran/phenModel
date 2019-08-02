\name{assign_observed_data}
\alias{assign_observed_data}

\title{
Observed Data Preparation
}
\description{
Prepares observed data for plotting with model evaluation.
}
\usage{
assign_observed_data(obj, eggs = NA, larvae = NA, pupae = NA, adults)
}

\arguments{
  \item{obj}{an object generated from \code{\link{phen_model}}}
  \item{eggs}{vector of days in which eggs were observed}
  \item{larvae}{vector of days in which larvae were observed}
  \item{pupae}{vector of days in which pupae were observed}
  \item{adults}{vector of days in which adults were observed}
}
\details{
Reads in the data and returns an object ready to be passed to \code{\link{plot_phen_model}} for plotting. Uses a simple probability checking to determine whether the observations for the adults category belong to the post-diapause, ovipositing or adult emergence stage, based on normal distributions.
}
\value{
Returns a \code{data.frame} with five columns, one for each stage (post-diapause, eggs, larvae, pupae and adults).
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}

\seealso{
\code{\link{phen_model}}, \code{\link{plot_phen_model}}
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

## evaluating model
quantiles_seq <- seq(.001, .95, .001)

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

## adding observed points
made_up_data <- assign_observed_data(obj = phen95,
                                     eggs = c(182,184,204,227,233,242),
                                     larvae = c(204,227,233,242,249),
                                     adults = c(204,212,220,221,222,224,227,233,242,269,283,298,
                                                305,310,311,312,313,314,315,316,317,318,
                                                319,320,321,322,323,334,325,326,327,328,
                                                329,330,331,332))

plot_phen_model(obj = phen95, obj_gen2 = phen95_gen2,
                observed_data = made_up_data)
}