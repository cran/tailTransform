ttrans <-
function(d,p=-1,qu=NULL,beta=NULL){
  stopifnot(is.vector(d))
  stopifnot((!is.null(qu))|(!is.null(beta)))
  stopifnot((is.null(qu))|(is.null(beta)))

  ad<-abs(d)
  if (!is.null(qu)) beta <- stats::quantile(ad,qu, na.rm=TRUE)

  l<-pmin(ad,beta)
  above<-(!is.na(d))&(ad>beta)
  if (p!=0) l[above]<-l[above]+((ad[above]^p)-(beta^p))/(p*(beta^(p-1)))
  else l[above]<-l[above]+(log(ad[above])-log(beta))*beta
  l*sign(d)
}
