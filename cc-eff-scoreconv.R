

beta<-1

expit<-function(eta) exp(eta)/(1+exp(eta))
logit<-function(p) log(p/(1-p))


one.sim<-function(beta,m=1) replicate(1500,{

y<-rep(0:1,c(5000*m,5000))
x<-c(rnorm(5000*m,mean=-beta/(m+1)),rnorm(5000,mean=beta*m/(m+1)))
w<-1+(1-y)*100
X<-cbind(1,x)


m1<-glm(y~x,family=binomial)
m1w<-glm(y~x,family=binomial,weights=w)

u<-(X*(y-fitted(m1)))%*%vcov(m1)
uw<-(X*(w/mean(w))*(y-fitted(m1w)))%*%vcov(m1w)


pi0<-1/100
alpha<-logit(mean(y-x*beta))+log(pi0)
mu<-expit(as.vector(X%*%c(alpha-log(pi0),beta)))
muw<-expit(as.vector(X%*%c(alpha,beta)))
u0<-(X*(y-mu))%*%solve(crossprod(X*mu*(1-mu),X))
uw0<-(X*w*(y-muw))%*%solve(crossprod(X*w*muw*(1-muw),X))



c(coef(m1)[2],coef(m1w)[2],cor(u[,2],uw[,2]),cor(u0[,2],uw0[,2]))
})

betasim<-c(-1, -0.5, 0,0.1,0.5,1,1.5,2)
rr<-lapply(betasim,one.sim)

one.inf<-function(beta,m=1) replicate(5000,{
y<-rep(0:1,c(5000*m,5000))
x<-c(rnorm(5000*m,mean=-beta/(m+1)),rnorm(5000,mean=beta*m/(m+1)))
w<-1+(1-y)*100
X<-cbind(1,x)


pi0<-1/100
alpha<-logit(mean(y-x*beta))+log(pi0)
mu<-expit(as.vector(X%*%c(alpha-log(pi0),beta)))
muw<-expit(as.vector(X%*%c(alpha,beta)))
u0<-(X*(y-mu))%*%solve(crossprod(X*mu*(1-mu),X))
uw0<-(X*w*(y-muw))%*%solve(crossprod(X*w*muw*(1-muw),X))



cor(u0[,2],uw0[,2])
})

betainf<-c(-1, -0.5, 0,0.5,1,1.5,2,2.5,3,4,5,6)
r1<-lapply(betainf,one.inf)


save(r1, rr, betasim,betainf,file="~/cc-score4efficiency.rda")

einf<-sapply(r1,function(x) mean(x^2))
esim<-sapply(rr,function(d) var(d[1,])/var(d[2,]))

plot(betasim,esim, xlim=c(-1,6),ylim=c(0,1),type="b",ylab="Efficiency",xlab=expression(beta))
points(betainf,einf,type="b",col="blue",lty=2)


betamatch<-seq(-3,3,length=50)
r2<-lapply(betamatch,function(b) c(mean(one.inf(b,m=1)^2),mean(one.inf(b,m=5)^2)))
r3<-sapply(betamatch,function(b) mean(one.inf(b,m=10)^2))
plot(betamatch,sapply(r2,function(r) r[1]),type="b",xlab=expression(beta),ylab="Efficiency relative to MLE")
points(betamatch,sapply(r2,function(r) r[2]),type="b",pch=19,col="darkgrey")
points(betamatch,r3,type="b",pch=19,col="black")
legend("topleft",pch=c(1,19,19),col=c("black","darkgrey","black"), legend=c("1 control per case","5 controls per case","10 controls per case"),bty="n")
save(r2,r3, betamatch, file="~/cc-matchingratio.rda")