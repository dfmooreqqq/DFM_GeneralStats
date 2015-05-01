packages <- c("plyr")
packages <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }
})

test<-data.frame(count = as.integer(character()),
                 size =  as.integer(character()),
                 range =  as.integer(character())
                 )

a = rnorm(500)
maxsamplesize = 200
simulations = 200

for (j in 1:maxsamplesize){
    print(j)
    for (i in 1:simulations){
        testsample <- sample(a,j)
        testi<-data.frame(count = i+(j-1)*simulations,
                          size = j,
                          range = range(testsample)[2] - range(testsample)[1]
                          )
        test<-rbind(test, testi)
    }
}

test$greaterthan0.9<-test$range>0.9*(range(a)[2] - range(a)[1])

summarytest<-ddply(test, .(size), summarize, likelihood = sum(greaterthan0.9)/simulations)
plot(likelihood~size, data = summarytest)
