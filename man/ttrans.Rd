\name{ttrans}
\alias{ttrans}

\title{
Symmetric Tail Transformation of Differences for Graphical Display
}
\description{
Performs a differentiable, strictly increasing, odd transformation of treated-minus-control pair differences d, or after-minus-before changes, or difference-in-differences.  The transformation allows one to see the undistorted center of a distribution that contains extreme outliers, while also seeing the outliers.  The transformation t(d) is: (i) odd, meaning
t(d )= -t(-d), so positive and negative values of d are transformed symmetrically, (ii) for some number beta>0, the transformation leaves d untouched between -beta and beta, so t(d)=d for -beta < d < beta, (iii) the transformation has derivative 1 at -beta and beta, so it is smooth at the point where the nonlinear transformation begins to take effect.
}
\usage{
ttrans(d, p = -1, qu = NULL, beta = NULL)
}
\arguments{
  \item{d}{
A vector of differences to be transformed.
}
  \item{p}{
The power to be used in the transformation of the tails, with p=0 being the log, as in the Box-Cox-Tukey transformation.
}
  \item{qu}{
If qu is specified, it is a number strictly between 0 and 1, commonly 0.9 or 0.95.  Then beta is set to be the qu quantile of abs(d).  If qu=.95, then 95 percent of the differences in d are not transformed.  You must specify either qu or beta, and you must not specify both qu and beta.
}
  \item{beta}{
The value beta mentioned in the description.  You must specify either qu or beta, and you must not specify both qu and beta.
}
}
\details{
Recall that beta>0.  Let y be one difference in d.  If y=0, then t(y)=y=0.  If 0<y<=beta, then
t(y) = y.  If y>beta, then a Box-Cox-Tukey power transformation is applied to y, with the transformation relocated and scaled so that t(y) has derivative 1 at beta.  If y<0, then t(y) = -t(|y|).  Properties of the transformation are discussed in Rosenbaum (2022).

Although t(y) is nonlinear, it is exactly linear with slope 1 between -beta and beta, and t(y) is smooth with slope 1 at -beta and beta.  The nonlinear aspect of the transformation is barely visible near -beta and beta.

If d is symmetric about zero, then the transformed values are also symmetric about zero.  If there is no effect on the differences, in the sense that they are symmetric about zero, then the transformed differences also exhibit no effect.

The transformation does not alter Wilcoxon's signed rank statistic, or other signed rank statistics.  Specifically, the transformation does not alter the ranks of |d|, and it does not alter sign(d).

The p=-1 reciprocal transformation has an upper and lower asymptote, so it limits the range of the d's, but it shows outliers clearly.
}
\value{
A vector of transformed values of d.
}
\references{
Box, George E. P. and David R. Cox. (1964) An analysis of transformations. Journal of the Royal Statistical Society: Series B 26, 211-243.
<doi:10.1111/j.2517-6161.1964.tb00553.x>

Rosenbaum, P. R. (2022). A new transformation of treated-control
matched-pair differences for graphical display.  Manuscript.

Tukey, J. W. (1949). One degree of freedom for non-additivity. Biometrics, 5, 232-242. <doi:10.2307/3001938>

Tukey, J. W. (1957). On the comparative anatomy of transformations.  Annals of Mathematical Statistics, 28, 602-632.
<doi:10.1214/aoms/1177706875>
}
\author{
Paul R. Rosenbaum
}

\examples{
data(aHDL)
attach(aHDL)
d<-hdl[grpL=="D"]-hdl[grpL=="N"]  # pair differences
detach(aHDL)
oldpar<-par()
par(mfrow=c(1,2))
boxplot(d) # untransformed
boxplot(ttrans(d,qu=.95,p=-1)) # reciprocal transformation of tails

par(mfrow=c(1,1))
# Label the transformed vertical axis with untransformed values
# Add -beta and beta on the right axis
tcks<-c(-100,-60,-40,-20,0,20,40,60,200)
boxplotTT(d,p=-1,qu=.95,tcks=tcks)

par<-oldpar
rm(aHDL,d,oldpar)
}
\concept{Box-Cox transformation}
\concept{transformation}
\concept{pair differences}
\concept{difference-in-differences}
