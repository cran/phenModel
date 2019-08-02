\name{pvulg_weibull_parameters}
\alias{pvulg_weibull_parameters}
\docType{data}
\encoding{UTF-8}
\title{
Estimated Parameters for the Weibull Model for P. Vulgatissima
}
\description{
Matrix of estimated parameters for each stage of the life-cycle of P. vulgatissima
}
\usage{data("pvulg_weibull_parameters")}
\format{
  The format is:
 num [1:3, 1:6] 0.7207 0.3517 1.7918 -0.0716 0.5117 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:3] "gamma" "eta" "beta"
  ..$ : chr [1:6] "post_diapause" "ovipositing" "larva_emergence" "pupa_emergence" ...
}
\details{
The parameters were derived based on constant temperature laboratory experiments (see Pollard, 2014) for different life-cycle stages of P. vulgatissima. Development times (or rates) were derived as cumulative frequency distributions at each temperature; development times for each percentile were then divided by the respective median value of the distribution to produce a normalised ("same shape") distribution of development times at each temperature (Wagner et al., 1984; 1991). Based on the "same shape" assumption, a single cumulative distribution representing the normalised distributions for all temperature treatments was produced by averaging all the distributions, weighted by the total frequency of each distribution (Wagner et al., 1984; 1991).
}
\source{
Pollard, C. (2014) A Temperature-Dependent Development Model for Willow Beetle Species (Coleoptera: Chrysomelidae) in Ireland: Simulation of Phenology/Voltinism in Response to Climate Change. Unpublished PhD thesis, submitted to the National University of Ireland Maynooth, Maynooth, 1-373.
}
\references{
Wagner T.L., Olson R.L. and Willers J.L. (1991) Modeling arthropod development time. Journal of Agricultural Entomology, 8, 251-270.

Wagner T.L., Wu H.I., Sharpe P.J.H. and Coulson R.N. (1984) Modeling distributions of insect development time: a literature review and application of the Weibull function. Annals of the Entomological Society of America, 77, 475-487. doi:10.1093/aesa/77.2.208

Pollard C.P., Griffin C.T., Moral R.A., Duffy C., Chuche J., Gaffney M.T., Fealy R. (submitted) phenModel: a temperature-dependent phenology model for the blue willow beetle, Phratora vulgatissima.
}
\examples{
data(pvulg_weibull_parameters)
}