# x=228
# n=239
# 
# hdp = binom.bayes(x, n,
#             conf.level = 0.95,
#             prior.shape1 = 207,
#             prior.shape2 = 32,
#             type = c("highest", "central"),
#             tol = .Machine$double.eps^0.5)

packages <- c("binom")
packages <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }
})


labels = c(560,520,480,440,400,360,320,280,240,200,160,120,80)
passes = c(0,1,5,32,89,144,207,228,239,239,239,239,239)
N = 239

#Now define the priors
priorpasses = c(0.01,passes[1:length(passes)-1])
priorfails = N - priorpasses

# This will bug out in the cases where either passes or fails equal zero
passeshdp = binom.bayes(passes, N,
                        conf.level = 0.8,
                        prior.shape1 = priorpasses,
                        prior.shape2 = priorfails,
                        type = c("highest", "central"),
                        tol = .Machine$double.eps^0.5,
                        maxit = 1000)

# 

binom.bayes.densityplot(passeshdp[passeshdp$x != 0 & passeshdp$x != N, ],
                        npoints = 10000,
                        alpha = 0.8)

#passeshdp = cbind(passeshdp,labels)
passfail = passes
for (i in as.single(1:length(passes))){
  if(passeshdp$mean[i]>0.5){
    passfail[i]="p"
  }
  if(passeshdp$mean[i]<0.5){
    passfail[i]="f"
  }
}
passeshdp = cbind(passeshdp,labels,passfail)

xyplot(passeshdp$upper ~ passeshdp$labels | passeshdp$method )
xyplot(passeshdp$upper ~ passeshdp$labels | passeshdp$passfail * passeshdp$method )
