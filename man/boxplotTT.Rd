\name{boxplotTT}
\alias{boxplotTT}
\title{
Parallel Boxplots After Tail Transformation
}
\description{
For a matrix of pair differences, plots one or more boxplots for differences after applying the same tail transformation to all differences.  See the help file for
ttrans() for information about the transformation.  For groups of unequal sizes,
use boxplotTTlist().
}
\usage{
boxplotTT(y, p = -1, qu = 0.95, tcks = NULL, ylab = "", xlab = "",main = "")
}
\arguments{
  \item{y}{
A vector, matrix, or dataframe of differences to be transformed and plotted.  Each column becomes a different boxplot after transformation.
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
Title for the plot.
}
}
\details{
See the help file for ttrans() for an explanation of the transformation.
}
\value{
A boxplot.
}
\references{
Rosenbaum, P. R. (2022) <doi:10.1080/00031305.2022.2063944> A new transformation of treated-control
matched-pair differences for graphical display.  American Statistician, 76, 346-352.
}
\author{
Paul R. Rosenbaum
}


\seealso{\code{\link{ttrans}}}

\examples{
data(aHDL)
y<-t(matrix(aHDL$hdl,4,406))
y<-y[,c(1,3,2,4)]
colnames(y)<-c("D","N","R","B")


# 6 pairwise comparisons of 4 groups
o<-matrix(NA,dim(y)[1],6)
colnames(o)<-1:6
k<-0
for (i in 1:3)
  for (j in (i+1):4){
    k<-k+1
    colnames(o)[k]<-paste(colnames(y)[i],colnames(y)[j],sep="-")
    o[,k]<-y[,i]-y[,j]
  }
rm(i,j,k)

# Plotting tick marks.  Remember, the transformation compresses
# extremes, so unequally spaced tick marks are usually needed.
tcks<-c(-100,-60,-40,-20,0,20,40,60,200)

# tails transformed by the p=-1 power (i.e., reciprocal)
boxplotTT(o,p=-1,qu=.95,tcks=tcks,ylab="HDL",
     xlab="Pairwise Comparisons of 4 Groups",
     main="HDL Differences in 4 Alcohol Groups")

}

\keyword{hplot}
\concept{Causal effect}
\concept{Matched pair difference}
\concept{Transformation}
