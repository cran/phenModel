\name{plot_phen_model}
\alias{plot_phen_model}
\title{
Plot Phenology Model Evaluations
}
\description{
Produces a plot of the phenology model evaluations.
}
\usage{
plot_phen_model(obj, obj_gen2, binwidth = 8, forecast = FALSE,
                observed_data = NULL, ylab = "Days from 1st November",
                xlab = "Modelled Life-Cycle Stages", 
                labels = c("Budburst",
                           "Post-Diapause",
                           "Ovipositing Period (Eggs)",
                           "Larvae Emergence",
                           "Pupae Emergence",
                           "Adult Emergence",
                           "Sexual Maturation"))
}
\arguments{
  \item{obj}{an output of the \code{\link{phen_model}} or \code{\link{phen_model_forecast}} functions}
  \item{obj_gen2}{an output of the \code{\link{phen_model_gen2}} or \code{\link{phen_model_forecast}} functions}
  \item{binwidth}{binwidth of the density estimator}
  \item{forecast}{logical. Set to \code{TRUE} if you are plotting a multi-year forecast (i.e. if \code{obj} and \code{obj_gen2} are outputs of \code{\link{phen_model_forecast}})}
  \item{observed_data}{an output from the \code{\link{assign_observed_data}} function}
  \item{ylab}{the y-axis label}
  \item{xlab}{the x-axis label}
  \item{labels}{a vector of strings representing the names of the life-cycle stages}
}
\details{
Visualisation of days until stage completion for different life cycle stages and generations. Plot can incorporate observed data, where available. All dates are counted from the 1st of November of the previous year. The estimated density is based on a Gaussian kernel whose binwidth can be easily modified using argument \code{binwidth}. To add observed data points, see \code{\link{assign_observed_data}}.

Since this is a \code{ggplot} object, layers can be easily added by using standard \code{ggplot} functions.
}
\value{
A \code{ggplot} object.
}
\references{
Pollard C.P., Griffin C.T., Moral R.A., Duffy C., Chuche J., Gaffney M.T., Fealy R. (submitted) phenModel: a temperature-dependent phenology model for the blue willow beetle, Phratora vulgatissima.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\seealso{
\code{\link{phen_model}}, \code{\link{phen_model_gen2}}, \code{\link{phen_model_forecast}}
}