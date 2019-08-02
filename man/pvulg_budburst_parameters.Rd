\name{pvulg_budburst_parameters}
\alias{pvulg_budburst_parameters}
\docType{data}
\encoding{UTF-8}
\title{
Estimated Parameters for the Exponential Decay Model for Budburst
}
\description{
Vector of estimated parameters.
}
\usage{data("pvulg_budburst_parameters")}
\format{
  The format is:
 Named num [1:2] 540.196 -0.016
 - attr(*, "names")= chr [1:2] "b" "m"
}
\details{
The method implied is based on the Alternating Model, which is a degree-day model that accumulates thermal/chilling units above/below a base threshold from a specified date (Cannell and Smith, 1983). The b and m values correspond to the estimates for budburst for Salix viminalis (Pollard et al., submitted).
}
\source{
Chmielewski, F.M., Heider, S., Moryson, S. and Bruns, E. (2013) International phenological observation networks: concept of IPG and GPM. In M. D. Schwartz, ed. Phenology: An integrative environmental science. New York London: Springer, p. 610.
}
\references{
Cannell, M.G.R. and Smith, R.I. (1983) Thermal time, chill days and prediction of budburst in Picea sitchensis. Journal of Applied Ecology, 20(3), 951–963.

Pollard C.P., Griffin C.T., Moral R.A., Duffy C., Chuche J., Gaffney M.T., Fealy R. (submitted) phenModel: a temperature-dependent phenology model for the blue willow beetle, Phratora vulgatissima.
}
\examples{
data(pvulg_budburst_parameters)
}