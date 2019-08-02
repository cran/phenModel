\name{pvulg_lactin_parameters}
\alias{pvulg_lactin_parameters}
\docType{data}
\encoding{UTF-8}
\title{
Estimated Parameters for the Lactin Model for P. Vulgatissima
}
\description{
Matrix of estimated parameters for each stage of the life-cycle of P. vulgatissima
}
\usage{data("pvulg_lactin_parameters")}
\format{
  The format is:
 num [1:4, 1:6] 0.00864 28.94449 0.52103 -1.06402 0.002 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:4] "rho" "Tmax" "deltaT" "lambda"
  ..$ : chr [1:6] "post_diapause" "ovipositing" "larva_emergence" "pupa_emergence" ...
}
\details{
The parameters were derived based on constant temperature laboratory experiments (see Pollard, 2014) for different life-cycle stages of P. vulgatissima. Development times were converted to rates and subsequently used as input for the Lactin model (Lactin et al., 1995). The model was fitted to the entire temperature range, including where there was no development rate (at the upper temperature threshold).
}
\source{
Pollard, C. (2014) A Temperature-Dependent Development Model for Willow Beetle Species (Coleoptera: Chrysomelidae) in Ireland: Simulation of Phenology/Voltinism in Response to Climate Change. Unpublished PhD thesis, submitted to the National University of Ireland Maynooth, Maynooth, 1-373.
}
\references{
Lactin D.J., Holliday N.J., Johnson D.L. and Craigen R. (1995) Improved rate model of temperature-dependent development by arthropods. Environmental Entomology, 24, 68–75. doi:10.1093/ee/24.1.68

Pollard C.P., Griffin C.T., Moral R.A., Duffy C., Chuche J., Gaffney M.T., Fealy R. (submitted) phenModel: a temperature-dependent phenology model for the blue willow beetle, Phratora vulgatissima.
}
\examples{
data(pvulg_lactin_parameters)
}