\name{cdl_check}
\alias{cdl_check}
\title{
Plots for Checking Sensitivity to CDL
}
\description{
Produces plots based on sensitivity analysis for the critical day length threshold.
}
\usage{
cdl_check(cdl, quantiles, max_day_length)
}
\arguments{
  \item{cdl}{a vector with the CDL thersholds calculated by \code{\link{sensitivity}}}
  \item{quantiles}{the quantiles corresponding to each value from \code{cdl}}
  \item{max_day_length}{the maximum day length set when running \code{\link{sensitivity}}}
}
\value{
A \code{ggplot} object.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{sensitivity}}
}
\examples{
data(casement9596)

casement1995 <- data.frame(casement9596, 
                           with(casement9596, 
                                get_total_info(Tmax, Tmin, Tbase = 7, Lat, day)))

data(pvulg_budburst_parameters)
data(pvulg_weibull_parameters)
data(pvulg_lactin_parameters)

test_quantiles <- c(.01,.05,.1,.25,.5,.75,.9,.95,.99)
cdl_thresholds <- NULL
for(q in test_quantiles) {
  sens_casement_cdl <- sensitivity(Tmax = casement9596$Tmax,
                                   Tmin = casement9596$Tmin,
                                   Tbase = 7,
                                   lat = casement9596$Lat,
                                   day = casement9596$day,
                                   bud_pars = pvulg_budburst_parameters,
                                   weib_pars = pvulg_weibull_parameters,
                                   lactin_pars = pvulg_lactin_parameters,
                                   max_day_length = 14.92, 
                                   pop_quantile = q,
                                   data = casement9596,
                                   study_type = "cdl",
                                   percent_variation = 1,
                                   resolution = 20) # increase resolution for better visualisation
  cdl_thresholds <- c(cdl_thresholds, summary_sensitivity(sens_casement_cdl)[1])
}

names(cdl_thresholds) <- paste(test_quantiles)
cdl_thresholds
                                 
cdl_check(cdl = cdl_thresholds,
          quantiles = test_quantiles,
          max_day_length = 14.92) +
  ggtitle("CDL sensitivity")
}