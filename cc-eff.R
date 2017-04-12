library(mitools)
library(survey)

ccr<-replicate(10000,{
df<-data.frame(x=rnorm(10000))
mu<-with(df,exp(x-4)/(1+exp(x-4)))
df$y<-rbinom(10000,1,mu)

### Case-control sampling
insample<-c(which(df$y==1),sample(which(df$y==0),sum(df$y)))
sdf<-df[insample,]
fdf<-df
fdf$x[-insample]<-NA
Nmiss<-nrow(df)-length(insample)
w<-sum(df$y==0)/sum(sdf$y==0)

### Parametric, control-only imputation
m<-with(sdf,mean(x[y==0]))
s<-with(sdf, sd(x[y==0]))
p1<-vector("list",10)
for(i in 1:10){
  p1[[i]]<-fdf
  p1[[i]]$x[-insample]<-rnorm(Nmiss,m,s)
}
p1list<-imputationList(p1)

### Parametric, case-control imputation
wmodel <-glm(y~x,data=sdf,weights=ifelse(y==1,1,w),family=quasibinomial)
l <- glm(x~y,data=sdf)
s<-sqrt(summary(l)$dispersion)
pred<-predict(l,newdata=fdf,type="response")
p2<-vector("list",10)
for(i in 1:10){
    p2[[i]]<-fdf
  p2[[i]]$x[-insample]<-rnorm(Nmiss,m[-insample],s)
}
p2list<-imputationList(p2)

### Resampling imputation
p3<-vector("list",10)
for(i in 1:10){
  p3[[i]]<-fdf
  p3[[i]]$x[-insample]<-with(sdf,sample(x[y==0],Nmiss,replace=TRUE))
}
p3list<-imputationList(p3)



#Residual imputation
p4<-vector("list",10)
m0<-with(sdf, mean(x[y=0]))
for(i in 1:10){
  p4[[i]]<-fdf
  p4[[i]]$x[-insample]<-with(sdf,m0+sample(x[y==0]-m0,Nmiss,replace=TRUE))
}
p4list<-imputationList(p4)

## fit
models0<-glm(y~x,family=binomial,data=sdf)
models1<-with(p1list, glm(y~x,family=binomial))
models2<-with(p2list, glm(y~x,family=binomial))
models3<-with(p3list, glm(y~x,family=binomial))
models4<-with(p4list, glm(y~x,family=binomial))

beta<-c(coef((wmodel)),
        coef((models0)),
        coef(MIcombine(models1)),
        coef(MIcombine(models2)),
        coef(MIcombine(models3)),
        coef(MIcombine(models4)))

beta
}
)
