
# Notes to self on how the differential gene expression is determined in RaceID2.
# #####

cdiff <- list()
x     <- object@ndata
y     <- object@expdata[,names(object@ndata)]
part  <- object@cpart
for ( i in 1:max(part) ){
  if ( sum(part == i) == 0 ) next
  # (if more than 1 cell) calculate mean expression per gene outside cluster (ndata)
  m <-  if ( sum(part != i) > 1 ) apply(x[,part != i],1,mean) else x[,part != i]
  # (if more than 1 cell) calculate mean expression per gene inside cluster
  n <-  if ( sum(part == i) > 1 ) apply(x[,part == i],1,mean) else x[,part == i]
  # fc is calculated later based only on m and n
  ###
  # now for p-value calculation
  # (if >1 cell inside) calculate 
  # cellTotalCount: per cell the total raw transcript count inside cluster
  # do median(cellTotalCount_inside)/median(cellTotalCount_outside)
  no <- if ( sum(part == i) > 1 ) median(apply(y[,part == i],2,sum))/median(apply(x[,part == i],2,sum)) else sum(y[,part == i])/sum(x[,part == i])
  m <- m*no
  n <- n*no
  pv <- binompval(m/sum(m),sum(n),n)
    # I think I have some additional notes on how this calculation works 
    # on paper.
  d <- data.frame(mean.ncl=m,mean.cl=n,fc=n/m,pv=pv)[order(pv,decreasing=FALSE),]
  cdiff[[paste("cl",i,sep=".")]] <- d[d$pv < pvalue,]
}
