boxplotTTBlockDesign<-function(bd,grp,p=-1,qu=.95,tcks=NULL,internal=TRUE,
        ylab="",xlab="",main="",cex.axis=.8,cex.lab=.8,cex.main=.8,symZero=TRUE){
  stopifnot(is.matrix(bd)|is.data.frame(bd))
  stopifnot(length(grp)==(dim(bd)[2]))
  stopifnot(length(unique(grp))>1)
  colnames(bd)<-grp
  U<-unique(colnames(bd))
  lenU<-length(U)
  nms1<-NULL
  nms2<-NULL
  ylist1<-NULL
  ylist2<-NULL
  for (i in 1:lenU){
    mati<-bd[,colnames(bd)==U[i],drop=FALSE]
    dmati<-dim(mati)[2]
    for (j in i:lenU){
      matj<-bd[,colnames(bd)==U[j],drop=FALSE]
      dmatj<-dim(matj)[2]
      if ((i!=j)|(dmati>1)) {
        if (i!=j) nms1<-c(nms1,paste(U[i],"-",U[j],sep=""))
        else nms2<-c(nms2,paste(U[i],"-",U[j],sep=""))
        dif<-NULL
        for (k in 1:dmati)
         for (l in 1:dmatj)
           if ((i!=j)|(k!=l)) dif<-c(dif,mati[,k]-matj[,l])
        if ((i!=j)&(is.null(ylist1))) ylist1<-list(dif)
        else if ((i!=j)&(!is.null(ylist1))) ylist1<-c(ylist1,list(dif))
        else if ((i==j)&(is.null(ylist2))) ylist2<-list(dif)
        else if ((i==j)&(!is.null(ylist2))) ylist2<-c(ylist2,list(dif))
      }

    }
  }
  names(ylist1)<-nms1
  names(ylist2)<-nms2
  if (internal) ylist<-c(ylist1,ylist2)
  else ylist<-ylist1
  boxplotTTlist(ylist,p=p,qu=qu,tcks=tcks,ylab=ylab,xlab=xlab,main=main,
                cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis,
                symZero=symZero)
}
