

cdfR <- function(x,...) UseMethod("cdfR")

cdfR.default <- function(x){
  a = getCdf(x)
  b = breakCdf(a)
  out = list(cdf = a,cdfSplit = b)
  class(out) = "cdfR"
  return(out)
}

plot.cdfR <- function(x,...){
  mx = max(x$cdfSplit$xp)
  mi = min(x$cdfSplit$xn)
  plot(x$cdfSplit$xn,x$cdfSplit$yn,xlim = c(mi,mx));points(x$cdfSplit$xp,x$cdfSplit$yp)
}

getCdf <- function(x){
  sx = sort(x)
  y  = seq(1,length (x),1)/(length (x) +1)
  return(list(x=sx,y=y))
}

breakCdf<-function(cdf){
  xp = cdf$x>=0 # consider>=
  xn = cdf$x<0
  yp = 1-cdf$y[xp]
  yn = cdf$y[xn]
  return(list(xp=cdf$x[xp],yp=yp,xn=cdf$x[xn],yn=yn))
}

rCdf<-function(cdf,n){
  ru=runif(n)
  a=approx(cdf$y,cdf$x,xout=ru)
  ptr = is.na(a$y)*(ru>0.5)
  a$y[ptr] = max(cdf$x)
  a$y[is.na(a$y)] = min(cdf$x)
  return(a$y)
}




