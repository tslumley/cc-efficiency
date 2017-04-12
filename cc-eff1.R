library(mitools)
library(survey)


simulate_ccnorm<-function(beta){
ccr<-replicate(10000,{
    df<-data.frame(y=rep(1:0,c(5000,100000-5000)))
    df$x<-with(df, rnorm(100000,y*beta-beta/2))

### Case-control sampling
insample<-c(which(df$y==1),sample(which(df$y==0),sum(df$y)))
sdf<-df[insample,]
Nmiss<-nrow(df)-length(insample)
w<-sum(df$y==0)/sum(sdf$y==0)
sdf$wt<-with(sdf,ifelse(y==1,1,w))

#fits
model0<-glm(y~x,family=binomial,data=df)
modelm<-glm(y~x,family=binomial,data=sdf)
models<-glm(y~x,family=quasibinomial,data=sdf,weight=wt)

beta<-c(coef((modelm)),
        coef((models))
       )-coef(model0)

beta
}
)
}

sapply((0:10)/2, function(beta) {betas<-simulate_ccnorm(beta); (mad(betas[2,])/mad(betas[4,]))^2})->effs3normal
save(effs3normal,file="effsnormal.rda")

simulate_ccexp<-function(beta){
ccr<-replicate(1000,{
    df<-data.frame(y=rep(1:0,c(500,10000-500)))
    df$x<-with(df, rexp(10000,rate=1/(y*beta+1)))

### Case-control sampling
insample<-c(which(df$y==1),sample(which(df$y==0),sum(df$y)))
sdf<-df[insample,]
Nmiss<-nrow(df)-length(insample)
w<-sum(df$y==0)/sum(sdf$y==0)
sdf$wt<-with(sdf,ifelse(y==1,1,w))

#fits
model0<-glm(y~x,family=binomial,data=df)
modelm<-glm(y~x,family=binomial,data=sdf)
models<-glm(y~x,family=quasibinomial,data=sdf,weight=wt)

beta<-c(coef((modelm)),
        coef((models))
       )-coef(model0)

beta
}
)
}
sapply((0:10)/2, function(beta) {betas<-simulate_ccexp(beta);print(beta); (mad(betas[2,])/mad(betas[4,]))^2})->effs3exp


