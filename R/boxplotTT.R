boxplotTT <-
function(y,p=-1,qu=.95,tcks=NULL,ylab="",xlab="",main=""){

  # Check input
  stopifnot(is.matrix(y)|is.vector(y)|is.data.frame(y))
  ismat<-FALSE # Is y a matrix or data frame?
  if (is.matrix(y)|is.data.frame(y)) ismat<-TRUE
  stopifnot(is.vector(qu)&(length(qu)==1)&(qu>0)&(qu<1))
  if (!is.null(tcks)) stopifnot(is.vector(tcks)&is.numeric(tcks))

  # adjust plot margins
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mar=c(5,4,4,3)+.1)

  if (ismat){
    nobs<-dim(y)[1]
    nvars<-dim(y)[2]
    if (!is.null(colnames(y))) nms<-colnames(y)
    else nms<-1:nvars
    ymat<-as.matrix(y)
    y<-as.vector(unlist(y))
  }

  # One transformation for all boxplots
  beta<-unlist(stats::quantile(abs(y),qu))
  ty<-ttrans(as.vector(unlist(y)),p=p,beta=beta)
  ylim=c(min(ty),max(ty))
  if (!is.null(tcks)) {
    tcksT<-ttrans(tcks,p=p,beta=beta)
    if(max(tcksT)>ylim[2]) ylim[2]<-max(tcksT)
    if(min(tcksT)<ylim[1]) ylim[1]<-min(tcksT)
  }

  if (ismat){
    ty<-matrix(ty,nobs,nvars)
    graphics::boxplot(ty,names=nms,yaxt="n",cex.axis=.8,cex.lab=.8,cex.main=.8,
            ylab=ylab,main=main,xlab=xlab,ylim=ylim)
    graphics::abline(h=0)
  }
  else {
    graphics::boxplot(ty,yaxt="n",cex.axis=.8,cex.lab=.8,cex.main=.8,
               ylab=ylab,main=main,xlab=xlab,ylim=ylim)
    graphics::abline(h=0)
  }

  graphics::axis(4,at=c(-beta,beta),labels=c(expression(-beta),expression(beta)),
       las=1,cex.axis=.75)
  if (!is.null(tcks)) axis(2,at=tcksT,labels=tcks,las=1,cex.axis=.75)
}
