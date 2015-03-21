\name{CTPL}
\alias{CTPL}
\title{
Search Families Based on Chinese Names of Plants
}
\description{
This function prepare checklist of plants with scientific name, Kew Family and APGIII family based on Chinese Names specified in an text file.
}
\usage{
CTPL(file = NULL)
}
\arguments{
  \item{file}{
Character string of the file name.
}
}
\details{
The file containing Chinese Names must be in plain text, and encoded in ANSII, one row for one species. 
}
\value{
A file "CTPL_results.csv" will be generated in the working directory.
}
\references{
The Plant List Website. 
}
\author{
Jinlong Zhang
}
\examples{
# Do not Run
# path <- system.file("extdata", "CTPL_test.txt", package = "plantlist")
# CTPL(file = path)
}
