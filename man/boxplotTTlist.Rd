\name{boxplotTTlist}
\alias{boxplotTTlist}
\title{
Parallel Boxplots After Tail Transformation For a List
}
\description{
For a list of vectors of pair differences, plots boxplots for differences after applying the same tail transformation to all differences.  See the help file for
ttrans() for information about the transformation.
}
\usage{
boxplotTTlist(ylist, p=-1, qu=.95, tcks=NULL, ylab="", xlab="", main="",
         cex.lab=.8, cex.main=.8, cex.axis=.8, symZero=TRUE)
}
\arguments{
  \item{ylist}{
A list of vectors of pair differences.  An error will result if ylist is not
a list.  The vectors need not have
the same lengths, and this makes boxplotTTlist() more flexible than
boxplotTT().
}
  \item{p}{
The power of the transformation.  See the help file for the ttrans() function.
}
  \item{qu}{
One number strictly between 0 and 1, commonly 0.9 or 0.95.  Define beta to be the qu quantile of abs(as.vector(y)).  Then, values between -beta and beta are not transformed.
}
  \item{tcks}{
A vector of untransformed values to become tick marks for the y axis after transformation.  Although the y axis was transformed, it is labeled with corresponding untransformed values given by tcks.  If is.null(tcks), then the y axis has no tick marks.
}
  \item{ylab}{
Label for the y-axis.
}
  \item{xlab}{
Label for the x-axis.
}
  \item{main}{
Title for the plot.  If main="", then no title appears, and the sample
size appears above each boxplot.  If you prefer no title and no sample
sizes, set main=" ".
}
  \item{cex.lab}{Sets the label size for the boxplot using
  the standard graphics parameter cex.lab.
}
\item{cex.main}{Sets the title size for the boxplot using
  the standard graphics parameter cex.main.
}
\item{cex.axis}{Sets the axis size for the boxplot using
  the standard graphics parameter cex.axis.
}
\item{symZero}{If TRUE, makes the y-axis symmetric about zero.
If FALSE, the maximum and minimum values in ylist determine
the y-axis.
}
}
\details{
See the help file for ttrans() for an explanation of the transformation.
}
\value{
Parallel boxplots after tail transformation.
}
\references{
Rosenbaum, P. R. (2022) <doi:10.1080/00031305.2022.2063944> A new transformation of treated-control
matched-pair differences for graphical display.  American Statistician, 76, 346-352.

Ye, T., Small, D. S. and Rosenbaum, P. R. (2022) <doi:10.1214/22-AOAS1611> Dimensions, power and factors in an observational study of behavioral problems after physical abuse of children. Annals of Applied Statistics, 16, 2732-2754.
}
\author{
Paul R. Rosenbaum
}


\seealso{\code{\link{ttrans}}}

\examples{
# Makes Figure 5(ii) in Rosenbaum (2022).
# See also Figure 1 in Ye et al. (2022).

data(aHDL)
y<-t(matrix(aHDL$hdl,4,406))
y<-y[,c(1,3,2,4)]
colnames(y)<-c("D","N","R","B")

TC<-c(y[,1]-y[,2],y[,1]-y[,3],y[,1]-y[,4])
CC<-c(y[,2]-y[,3],y[,2]-y[,4],y[,3]-y[,4])
CC<-c(CC,-CC)

ylist<-list(TC=TC,CC=CC)

boxplotTTlist(ylist,tcks=c(-80,-40,-20,0,20,40,80),ylab="HDL Difference")


}

\keyword{hplot}
\concept{Causal effect}
\concept{Matched pair difference}
\concept{Transformation}
