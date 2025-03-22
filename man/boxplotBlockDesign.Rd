\name{boxplotTTBlockDesign}
\alias{boxplotTTBlockDesign}
\title{
Parallel Boxplots After Tail Transformation For a Block Design
}
\description{
For an IxJ block design, a boxplot of column differences after
transformation.  See the help file for
ttrans() for information about the transformation.
}
\usage{
boxplotTTBlockDesign(bd, grp, p = -1, qu = 0.95, tcks = NULL, internal = TRUE,
     ylab = "", xlab = "", main = "", cex.axis = .8, cex.lab = .8,
     cex.main = .8, symZero=TRUE)
}
\arguments{
  \item{bd}{
An I x J matrix or data.frame of outcomes with no missing outcomes.
}
\item{grp}{
A vector of length J = number of columns of bd.  If grp is 1:J, then the
plot contains choose(J,2) boxplots for the differences of choose(J,2) pairs of columns
of bd, and these are labeled 1-2, 1-3, 2-3, ....  If grp contains J distinct
short names, the same boxplots appear but labeled name1-name2, name1-name3,....
One-letter names look best.  If J=4 and grp is (1,2,2,2) and internal=FALSE,
then one boxplot
appears, comparing column 1 to each of the columns labeled 2, using
column 1 three times in comparison with columns 2, 3 and 4, so an Ix4 design
produces 3I differences.  If J=4 and grp is (1,2,2,2) and internal=TRUE,
then two boxplots
appear.  The first boxplot is the same as for internal=FALSE.  The second
boxplot displays 2I x choose(J-1,2) differences, or 2I x 3 differences if grp is (1,2,2,2),
namely bd[,2]-bd[,3], bd[,3]-bd[,2], bd[,2]-bd[,4], bd[,4]-bd[,2],
bd[,3]-bd[,4], bd[,4]-bd[,3], that is, all pair differences among distinct
columns labeled 2.  See details.  An error will result if all J entries in
grp are the same.
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
  \item{internal}{
If the J entries in grp are all different, say grp=(1,2,3,4), then changing internal has no
effect.  If one or more entries in grp are duplicated, as in grp=(1,2,3,3),
and if internal=TRUE, then three boxplots compare 1-2, 1-3 and 2-3, and a
fourth boxplot displays the 2I symmetrized differences among controls labeled
3, namely c(bd[,3]-bd[,4], bd[,4]-bd[,3]).  If grp=(1,2,3,3) and internal=FALSE,
then the fourth boxplot does not appear.  See details.
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
sizes, set main=" ".  Note that the sample size refers to the number of
differences; see details.
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

If the columns of the I x J block design bd represent different groups, say
grp=(1,2,...,J), then the choose(J,2) pairwise differences among pairs of
columns are plotted after tail transformation.

Suppose that two or more columns represent the same group, so J>length(unique(grp)),
as in grp(1,2,3,3,4,4) with a block size of J=6 but just 4 groups.  If
internal=FALSE, then there is a boxplot plot if I differences comparing
groups 1 and 2, bd[,1]-bd[,2].  There is a boxplot of 2I differences comparing groups
1 and 3, namely c(bd[,1]-bd[,3], bd[,1]-bd[,4]).  There is a boxplot of 4I
differences comparing groups 3 and 4.

In the same situation, but with internal=TRUE, the two columns for group 3 are
compared, and so are the two columns for group 4.  Both columns 5 and 6
each represent I individuals from group 4.  A boxplot shows the variation in
group 4 within blocks by plotting the 2I symmetrized differences,
c(bd[,5]-bd[,6], bd[,6]-bd[,5]).  That boxplot is symmetric about zero, and
shows how people in the same block differ by chance when they are in the
same group.  That boxplot is a benchmark for comparing two different groups.

For discussion and examples of symmetrized boxplots, see Ye et al. (2022, Figure 1) and Rosenbaum (2022, Figure 5).
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

data(aHDL)
y<-t(matrix(aHDL$hdl,4,406))
y<-y[,c(1,3,2,4)]
grp<-c("D","N","R","B")

# Figure 3 in Rosenbaum (2022)
boxplotTTBlockDesign(y,grp=grp,tcks=c(-200,-50,-20,0,20,50,200),
   ylab="HDL Difference",xlab="Group Comparisons")

# Same figure, different transformation, p=-2
boxplotTTBlockDesign(y,p=-2,grp=grp,tcks=c(-200,-50,-20,0,20,50,200),
   ylab="HDL Difference",xlab="Group Comparisons")

# Figure 5 in Rosenbaum (2022).  The three control groups have been merged.
# Note that the C-C boxplot is perfectly symmetric about zero, and less
# dispersed than the T-C boxplot.
grp<-c("D","C","C","C")
boxplotTTBlockDesign(y,grp=grp,tcks=c(-200,-50,-20,0,20,50,200),
   ylab="HDL Difference",xlab="Group Comparisons")

# The same figure can be produced explicitly using boxplotTTlist.
TC<-c(y[,1]-y[,2],y[,1]-y[,3],y[,1]-y[,4])
CC<-c(y[,2]-y[,3],y[,2]-y[,4],y[,3]-y[,4])
CC<-c(CC,-CC)

ylist<-list(TC=TC,CC=CC)

boxplotTTlist(ylist,tcks=c(-200,-50,-20,0,20,50,200),ylab="HDL Difference")

#  More variations: Keep group B separate
grp<-c("D","C","C","B")
boxplotTTBlockDesign(y,grp=grp,tcks=c(-50,-20,0,20,50),
      symZero=FALSE,main=" ",p=1/2)
boxplotTTBlockDesign(y,grp=grp,tcks=c(-200,-50,-20,0,20,50,200),
      main="",p=-1,internal=FALSE)
}

\keyword{hplot}
\concept{Causal effect}
\concept{Matched pair difference}
\concept{Transformation}
