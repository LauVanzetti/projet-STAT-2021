r2norm<- function(n=n,muX=muX,muY=muY,sX=sX,sY=sY,rho=rho,seed=SIMULATIONSEED) {
  x<- rnorm(n,muX,sX)
  z<-rnorm(n,muY,sY*sqrt(1-rho^2))
  y<- sY/sX*rho*(x-muX)+z 
  data.frame(x=x,y=y)
}






rho= -0.708
muX=0
muY=-1
sX=0.6
sY=1.9
vec.rho=(-9:9)/10
n=730
M=960
SIMULATIONSEED=41898
vec.means=rep(0,length(vec.rho))
for(j in 1:length(vec.rho)){
  stat.spear=rep(0,M)
  set.seed(SIMULATIONSEED)
  for(i in 1:M){
    data=r2norm(n, muX=muX, muY=muY, sX=sY, sY=sY, rho=vec.rho[j])
    stat.spear[i]=cor(data$x,data$y,method="spearman")
  }
  vec.means[j]=mean(stat.spear)
}
vec.bias=(vec.means-vec.rho)
par(mar=c(4,7,4,4))
plot(vec.rho,vec.bias,xlab="True correlation",type="l",ylab="Estimation bias (simulated)", cex.lab = 1.8, cex.axis = 1.8)
abline(h=0)

