\name{tailTransform-package}
\alias{tailTransform-package}
\alias{tailTransform}
\docType{package}
\title{
\packageTitle{tailTransform}
}
\description{
\packageDescription{tailTransform}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{tailTransform}
\packageIndices{tailTransform}
The package contains five items: (i) a function ttrans() that symmetrically shortens the tails of pair differences for graphical display, (ii) three
plotting functions, boxplotTT(), boxplotTTlist() and boxplotBlockDesign(), that aid in interpreting and displaying the transformed data, and (iii) an observational study aHDL with three control groups and an unaffected outcome
that are intended to reveal unmeasured confounding if it is
present.<doi:10.1080/00031305.2022.2063944>
}
\author{
\packageAuthor{tailTransform}

Maintainer: \packageMaintainer{tailTransform}
}
\references{
Rosenbaum, P. R. (2023) <doi:10.1111/biom.13558> Sensitivity analyses informed by tests for bias in observational studies. Biometrics. 9(1), 475-487.

Rosenbaum, P. R. (2022) <doi:10.1080/00031305.2022.2063944> A new transformation of treated-control
matched-pair differences for graphical display.  American Statistician, 76, 346-352.
}
\keyword{ package }
\examples{
data(aHDL)
attach(aHDL)
d<-hdl[grpL=="D"]-hdl[grpL=="N"]  # pair differences
tcks<-c(-100,-60,-40,-20,0,20,40,60,200)
boxplotTT(d,p=-1,qu=.95,tcks=tcks)
detach(aHDL)
rm(tcks,aHDL)
}
