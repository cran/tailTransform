boxplotTTlist <-
function(ylist,p=-1,qu=.95,tcks=NULL,ylab="",xlab="",main="",
         cex.lab=.8,cex.main=.8,cex.axis=.8,symZero=TRUE){

  # Check input
  stopifnot(is.list(ylist))
  #stopifnot(length(ylist)>=2)
  stopifnot(is.vector(qu)&(length(qu)==1)&(qu>0)&(qu<1))
  if (!is.null(tcks)) stopifnot(is.vector(tcks)&is.numeric(tcks))

  # adjust plot margins
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mar=c(5,4,4,3)+.1)

  if (is.null(names(ylist))) names(ylist)<-1:length(ylist)
  nms<-names(ylist)

  # Remove missing y's, if any
  if (sum(is.na(unlist(ylist)))>0) {
    yj<-ylist[[j]]
    for (j in 1:length(ylist)) ylist[[j]]<-yj[!is.na(yj)]
  }

  # Convert ylist to vector + factor
  y<-unlist(ylist)
  cnt<-lapply(ylist,length)
  grp<-NULL
  for (j in 1:length(ylist)) grp<-c(grp,rep(nms[j],cnt[j]))
  grp<-factor(grp,levels=nms,ordered=TRUE)

  # One transformation for all boxplots
  beta<-unlist(stats::quantile(abs(y),qu,na.rm=TRUE))
  ty<-ttrans(as.vector(unlist(y)),p=p,beta=beta)
  if (symZero) ylim=c(-max(ty,na.rm=TRUE),max(ty,na.rm=TRUE))
  else ylim=c(min(ty,na.rm=TRUE),max(ty,na.rm=TRUE))
  if (!is.null(tcks)) {
    tcksT<-ttrans(tcks,p=p,beta=beta)
    if(max(tcksT)>ylim[2]) ylim[2]<-max(tcksT)
    if(min(tcksT)<ylim[1]) ylim[1]<-min(tcksT)
  }

  graphics::boxplot(ty~grp,yaxt="n",cex.axis=cex.axis,cex.lab=cex.lab,
                    cex.main=cex.main,yas=1,
                    ylab=ylab,main=main,xlab=xlab,ylim=ylim)
  graphics::abline(h=0)

  graphics::axis(4,at=c(-beta,beta),labels=c(expression(-beta),expression(beta)),
       las=1,cex.axis=.75)
  if (!is.null(tcks)) axis(2,at=tcksT,labels=tcks,las=1,cex.axis=cex.axis)
  if (main=="") axis(3,at=1:length(ylist),labels=paste("n=",table(grp),sep=""),
                     cex.axis=cex.axis)
}
