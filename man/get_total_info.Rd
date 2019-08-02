\name{get_total_info}
\alias{get_total_info}
\encoding{UTF-8}
\title{
Compute Day Length, Mean Temperature, Thermal Units, and Cumulative Chill Days
}
\description{
Computes daily day length, mean temperature, thermal units and cumulative number of chill days for a dataset.
}
\usage{
get_total_info(Tmax, Tmin, Tbase, lat, day)
}
\arguments{
  \item{Tmax}{a vector of daily maximum temperatures}
  \item{Tmin}{a vector of daily minimum temperatures}
  \item{Tbase}{the base temperature used to define a chill day}
  \item{lat}{the latitude}
  \item{day}{day of the year (vector)}
}
\details{
Implements the method by Forsythe et al. (1995) to calculate the day length, and the single-sine degree-day method to compute the thermal units.
}
\value{
Returns a \code{data.frame} with the day lengths, mean temperatures, thermal units and cumulative number of chill days.
}
\references{
Cannell, M.G.R., Smith, R.I. (1983) Thermal time, chill days and prediction of budburst in Picea sitchensis. Journal of Applied Ecology, 20(3), 951–963.

Forsythe et al. (1995) A model comparison for daylength as a function of latitude and day of the year. Ecological Modeling 80:87-95.
}
\author{
Rafael de Andrade Moral (rafael.denadrademoral@mu.ie) and Rowan Fealy
}
\examples{
data(casement9596)

with(casement9596, get_total_info(Tmax, Tmin, Tbase = 7, Lat, day))
}