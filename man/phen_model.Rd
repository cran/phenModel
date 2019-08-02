\name{phen_model}
\alias{phen_model}
\encoding{UTF-8}
\title{
Phenology Model Evaluation
}
\description{
Evaluates the phenology model for specified population quantiles, based on daily temperatures.
}
\usage{
phen_model(Tmean, thermal_units, chill_days, day_length,
           bud_pars, weib_pars, lactin_pars, max_day_length,
           pop_quantiles = c(0.05, 0.5, 0.95),
           print.l = FALSE, save.l = FALSE, data)
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
  \item{pop_quantiles}{vector of quantiles of the population to be evaluated by the phenology model}
  \item{print.l}{logical. If \code{TRUE}, prints the cumulative development rates as the model iterates.}
  \item{save.l}{logical. If \code{TRUE}, returns the cumulative development rates instead of model evaluations.}
  \item{data}{a \code{data.frame}}
}
\details{
As inputs, the model requires site latitude and daily minimum and maximum temperatures from the 1st November of the preceding year. Development, quantified by a temperature dependent development rate model defined for each life cycle stage, is initialised in the model following the simulated date of budburst, a biofix dependent on the accumulation of required thermal and chilling days from the 1st November of the preceding year, described below. Once budburst is simulated to have occurred, post-diapause pre-oviposition development commences. Following completion of the post-diapause pre-oviposition stage, the first day of egg-lay is returned for adult females emerging from overwintering. Oviposition period stage advancement occurs when the required development for this life cycle stage has been completed. Egg, larval and pupal development stages are then initiated sequentially. Following completion of the pupal development stage, the emerging adults enter a sexual maturation or post-eclosion pre-oviposition period. This life-cycle stage is hypothesised to be sensitive to a diapause-inducing stimulus - photoperiod (Tauber et al., 1986; Danks, 1987; Dalin, 2011; Hodek 2012; Pollard, 2014) - defined in the model as a critical day length (CDL) threshold. New generation adults that finish sexual maturation after the critical day length (CDL) enter into a state of reproductive diapause. P. vulgatissima adults that complete development prior to the CDL threshold can begin ovipositing to initiate subsequent generations.
}
\value{
A \code{data.frame} containing model evaluations for each life cycle (columns) and population quantile (rows).
}
\references{
Dalin P. (2011) Diapause induction and termination in a commonly univoltine leaf beetle (Phratora vulgatissima). Insect Science, 18, 443-450. doi:10.1111/j.1744-7917.2011.01417.x
 
Danks, H.V. (1987) Insect dormancy: an ecological perspective 1st ed. H. V. Danks, ed., Biological Survey of Canada (Terrestrial Artropods) (Ottawa).

Hodek, I. (2012) Adult diapause in Coleoptera. Psyche, 2012, 1–10.

Tauber M.J., Tauber C.A. and Masaki S. (1986) Seasonal adaptations of insects. Oxford University Press, New York, USA

Pollard, C. (2014) A Temperature-Dependent Development Model for Willow Beetle Species (Coleoptera: Chrysomelidae) in Ireland: Simulation of Phenology/Voltinism in Response to Climate Change. Unpublished PhD thesis, submitted to the National University of Ireland Maynooth, Maynooth, 1-373.

Pollard C.P., Griffin C.T., Moral R.A., Duffy C., Chuche J., Gaffney M.T., Fealy R. (submitted) phenModel: a temperature-dependent phenology model for the blue willow beetle, Phratora vulgatissima.
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

phen_model(Tmean = "Tmean",
           thermal_units = "thermal_units",
           chill_days = "chill_days",
           day_length = "day_length",
           bud_pars = pvulg_budburst_parameters,
           weib_pars = pvulg_weibull_parameters,
           lactin_pars = pvulg_lactin_parameters,
           max_day_length = 14.92,
           data = casement1995)
}