\name{plantlist-package}
\alias{plantlist-package}
\alias{plantlist}
\docType{package}
\title{
Looking up Plant Families based on Kew Families and the Plantlist Website
}
\description{
Looking up Higher Plant Families based on Kew Families and the plantlist website, and making taxa table for Phylomatic. 
}
\details{
\tabular{ll}{
Package: \tab plantlist\cr
Type: \tab Package\cr
Version: \tab 0.1.4\cr
Date: \tab 2015-3-31\cr
License: \tab GLP-2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jinlong Zhang

Maintainer: Jinlong Zhang <jinlongzhang01@gmail.com>
}

\keyword{ package }
\examples{

TPL("Carex")
TPL("Cherry")
splist <- c( "Ranunculus japonicus", "Hepatica henryi", 
              "Heracleum acuminatum", "Solanum nigrum", 
              "Punica sp.", "Machilus", "Today", "####" ) 
res <- TPL(splist)
taxa.table(res)

}
