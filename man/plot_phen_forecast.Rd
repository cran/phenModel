\name{plot_phen_forecast}
\alias{plot_phen_forecast}
\title{
Plot Phenology Model Multi-Year Forecast
}
\description{
Produces a plot of the phenology model multi-year forecasting evaluations.
}
\usage{
plot_phen_forecast(obj, year_from = 1, xlab = "Year",
                   ylab = "Days from 1st November")
}
\arguments{
  \item{obj}{an output from the \code{\link{phen_model_forecast}} function}
  \item{year_from}{the first year we are forecasting for (e.g. "2015")}
  \item{xlab}{the x-axis label}
  \item{ylab}{the y-axis label}
}
\value{
A \code{ggplot} object.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{valentia}}
}