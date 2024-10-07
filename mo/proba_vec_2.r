
vecsimin <- function(b=c(3,4),sigma_eps=matrix(c(1.5,-0.3,-0.3,1.5),2),n=30,
                   ar=0.2){
  require(MASS)
  
  norm <- norm(as.matrix(b), "F")
  B <- matrix(c(b[1]/norm,b[2]/norm,1,2),byrow=TRUE,ncol=2)
  L <- diag(c(ar-1,0))
  
  alfa <- solve(B)%*%L[,1]
  beta <- matrix(B[1,])
  
  alfa <- alfa/norm
  beta <- norm*beta
  P <- alfa%*%t(beta)
  
  sigmabeps <- t(beta)%*%sigma_eps%*%beta
  sigmabb <- sigmabeps/(1-(1+t(beta)%*%alfa)^2)
  betax0 <- rnorm(1,0,sigmabb)
  
  beta_ort <- c(1/beta[1,],-1/beta[2,])
  x0 <- beta_ort+c(0,betax0/beta[2,])
  x <- matrix(0,ncol=4,nrow=n+1)
  x[1,1:2] <- x0
  x[1,3:4] <- rep(NA,2)
  
  for(i in 2:(n+1)){
    x[i,3:4] <- mvrnorm(1,rep(0, 2),sigma_eps)
    x[i,1:2] <- x[i-1,1:2]%*%t(diag(2)+P)+x[i,3:4]}
  
  sim <- cbind(x,rbind(rep(NA,2),diff(x[,1:2])))
  colnames(sim) <- c("xt1","xt2","eps1","eps2","dxt1","dxt2")
  betatx=sim[,c("xt1","xt2")]%*%beta
  
  par(mfrow=c(1,1))
  reg=sim[,c("xt1","xt2")]
  plot(reg,main="Wykres rozrzutu")
  
  ts <- as.ts(sim[-1,c("xt1","xt2")])
  plot(ts,main="Poziomy")
  
  tsd <- as.ts(sim[-1,c("dxt1","dxt2")])
  plot(tsd,main="Przyrosty")
  
  plot(as.ts(betatx),main="Proces odchylen od rownowagi")
  list(norm=norm,alfa=alfa,beta=beta,P=P,sim=sim,
       betatx=betatx,sigmabeps=sigmabeps,sigmabb=sigmabb)}

oo <- vecsimin(n=1000)
l <- lm(oo$sim[,2]~oo$sim[,1]-1)




