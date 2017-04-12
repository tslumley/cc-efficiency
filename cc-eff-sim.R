betas<-c(-1,-.5,0,.5,1,1.5,2)


sim<-function(beta){
x0<-rnorm(3000,m=-beta/2)
x1<-rnorm(3000,m=beta/2)
x<-c(x0,x1)
y<-rep(0:1,each=3000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])
}

r<-lapply(betas, function(b) replicate(3000,sim(b)))

simgamma<-function(beta, shape,rate){
x0<-rgamma(3000,shape=shape,rate=rate+beta/2)
x1<-rgamma(3000,shape=shape,rate=rate-beta/2)
x<-c(x0,x1)
y<-rep(0:1,each=3000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])
}

rg<-lapply(betas, function(b) replicate(3000,simgamma(b,shape=3,rate=4)))

simp<-function(beta,lambda=2){
	x0<-rpois(3000, lambda*exp(-beta/2))
	x1<-rpois(3000,lambda*exp(beta/2))
x<-c(x0,x1)
y<-rep(0:1,each=3000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])
	
	
}

rp<-lapply(betas, function(b) replicate(3000,simp(b,lambda=2)))


sim4<-function(beta, p=c(1,1,1,1)){
	x0<-sample(1:4,3000,replace=TRUE, prob=p*exp(-beta*(1:4)/2))
	x1<-sample(1:4,3000,replace=TRUE, prob=p*exp(+beta*(1:4)/2))
x<-c(x0,x1)
y<-rep(0:1,each=3000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])

}


ru<-lapply(betas, function(b) replicate(3000,sim4(b,p=c(1,1,1,1))))


rl<-lapply(betas, function(b) replicate(3000,sim4(b,p=c(4,3,2,1))))
rr<-lapply(betas, function(b) replicate(3000,sim4(b,p=c(1,2,3,4))))

rll<-lapply(betas, function(b) replicate(3000,sim4(b,p=c(4,3,1,.1))))
rrr<-lapply(betas, function(b) replicate(3000,sim4(b,p=c(.1,1,3,4))))




plot(betas,lapply(r, function(x) mad(x[2,])/mad(x[1,])),type="b",ylim=c(1,2),xlab=expression(beta),ylab="Inefficiency",lwd=2,col="blue")
lines(betas,lapply(rg, function(x) mad(x[2,])/mad(x[1,])),type="b",col="orange",lwd=2)
lines(betas,lapply(rp, function(x) mad(x[2,])/mad(x[1,])),type="b",col="forestgreen",lwd=2)

legend("topleft",lty=1,lwd=2,col=c("blue","orange","forestgreen"),legend=c("Normal",expression(Gamma(3)), "Poisson"),bty="n")

plot(betas,lapply(ru, function(x) mad(x[2,])/mad(x[1,])),type="b",ylim=c(1,2),xlab=expression(beta),ylab="Inefficiency",lwd=2,col="forestgreen")
lines(betas,lapply(rr, function(x) mad(x[2,])/mad(x[1,])),type="b",col="blue",lwd=2)
lines(betas,lapply(rl, function(x) mad(x[2,])/mad(x[1,])),type="b",col="orange",lwd=2)

lines(betas,lapply(rrr, function(x) mad(x[2,])/mad(x[1,])),type="b",col="skyblue",lwd=2)
lines(betas,lapply(rll, function(x) mad(x[2,])/mad(x[1,])),type="b",col="goldenrod",lwd=2)

legend("topleft",lty=1,lwd=2,col=c("blue","forestgreen","orange"),legend=c("Outliers left","Uniform", "Outliers right"),bty="n")




sim2<-function(beta){
x0<-rnorm(6000,m=-beta/3)
x1<-rnorm(3000,m=2*beta/3)
x<-c(x0,x1)
y<-rep(c(0,0,1),each=3000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])
}

r2<-lapply(betas, function(b) replicate(3000,sim2(b)))

sim4<-function(beta){
x0<-rnorm(8000,m=-beta/5)
x1<-rnorm(2000,m=4*beta/5)
x<-c(x0,x1)
y<-rep(c(0,0,0,0,1),each=2000)
c(coef(glm(y~x,family=binomial))[2], coef(glm(y~x,family=quasibinomial, weights=((1-y)*100)+1))[2])
}

r4<-lapply(betas, function(b) replicate(3000,sim4(b)))



plot(betas,lapply(r, function(x) mad(x[2,])/mad(x[1,])),type="b",ylim=c(1,2),xlab=expression(beta),ylab="Inefficiency",lwd=2,col="blue")
lines(betas,lapply(r2, function(x) mad(x[2,])/mad(x[1,])),type="b",col="orange",lwd=2)
lines(betas,lapply(r4, function(x) mad(x[2,])/mad(x[1,])),type="b",col="forestgreen",lwd=2)

legend("topleft",lty=1,lwd=2,col=c("blue","orange","forestgreen"),legend=c("1:1","2:1", "4:1"),bty="n")

