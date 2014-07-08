## This generates the data
n.quarters <- 100
n.stores <- 20
if (exists("test.data")){
  rm(test.data)
}
for (i in 1:n.stores){
  interval <- runif(1, 1, 200)
  new.df <- data.frame(              
    var0 = interval + c(0, cumsum(runif(49, -5, 5))),
    date = seq.Date(as.Date("1990-03-30"), by="3 month", length.out=n.quarters),
    store = rep(paste("Store", i, sep=""), n.quarters))
  if (exists("test.data")){
    test.data <- rbind(test.data, new.df)    
  } else {
    test.data <- new.df
  }
}
test.data$store <- factor(test.data$store)



## This formats the data in the right way and then generates the chart
library(lattice)
xyplot(var0 ~ date, data=test.data, groups=store, type=c("l","g"))

tw <- reshape(test.data, timevar="date", idvar="store", direction="wide")
parallelplot(tw[,-1], horizontal.axis=F, 
         scales=list(x=list(rot=45, 
                            at=seq(1,ncol(tw)-1,by=2), 
                            labels=substr(names(tw[,-1])[seq(1,ncol(tw)-1,by=2)],6,100), 
                            cex=.5)))

library(kml)
names(tw) <- c("id", paste("t", 1:(ncol(tw)-1)))
tw.cld <- cld(tw)
cld.res <- kml(tw.cld,nbRedrawing=5)
plot(tw.cld)