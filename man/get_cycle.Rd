\name{get_cycle}
\alias{get_cycle}
\title{
Obtain Yearly Cycles
}
\description{
Given a \code{data.frame} with columns representing days and months, returns a vector of indices representing which cycle each row corresponds to.
}
\usage{
get_cycle(data, day, month, day.end, month.end)
}
\arguments{
  \item{data}{a \code{data.frame}}
  \item{day}{name of the column representing the days}
  \item{month}{name of the column representing the months}
  \item{day.end}{which day determines the end of a cycle?}
  \item{month.end}{which month determines the end of a cycle?}
}
\value{
A vector of integers.
}
\author{
Rafael de Andrade Moral (rafael.deandrademoral@mu.ie) and Rowan Fealy
}
\examples{
data(valentia)

# cycle ends on 31st of October
valentia$cycle <- get_cycle(data = valentia, day = "day", month = "month",
                            day.end = 31, month.end = 10)
}