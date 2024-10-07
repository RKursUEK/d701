library(ccgarch)

vecsimingo <- function(b = c(-3,4), sigma_eps = matrix(c(1.5,-0.3,-0.3,1.5), 2), n=30,
                     ar = 0.5, ...){
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
  betax0 <- 0
  
  beta_ort <- c(1/beta[1,],-1/beta[2,])
  x0 <- beta_ort+c(0,betax0/beta[2,])
  x <- matrix(0,ncol=4,nrow=n+1)
  x[1,1:2] <- x0
  x[1,3:4] <- rep(NA,2)
  
  nobs <- n; cut <- 1000; nu <- 8
  aa <- c(0.003, 0.005, 0.001)
  AA <- diag(c(0.2,0.3,0.15))
  BB <- diag(c(0.75, 0.6, 0.8))
  uncR <- matrix(c(1.0, 0.4, 0.3, 0.4, 1.0, 0.12, 0.3, 0.12, 1.0),3,3)
  dcc.para <- c(0.01,0.98)
  
  ## Not run: 
  
  # for normally distributed innovations
  dcc.data <- dcc.sim(nobs, aa, AA, BB, uncR, dcc.para, model="diagonal")
  deps <- dcc.data$eps
  
  for(i in 2:(n+1)){
    x[i,3:4] <- deps[i-1,-3]
    x[i,1:2] <- x[i-1,1:2]%*%t(diag(2)+P)+x[i,3:4]}
  
  sim <- cbind(x,rbind(rep(NA,2),diff(x[,1:2])))
  colnames(sim)<-c("xt1","xt2","eps1","eps2","dxt1","dxt2")
  betatx <- sim[,c("xt1","xt2")]%*%beta
  
  par(mfrow=c(1,1))
  reg <- sim[,c("xt1","xt2")]
  plot(reg,main=paste("Wykres rozrzutu: xt1, xt2"),xaxt='n', yaxt='n',
       xlab="xt1",ylab="xt2")
  abline(0,-b[1]/b[2],col="darkred",lwd=2)


pol1 <- 0.85*max(reg[,1])
pol2 <- -b[1]/b[2]*pol1*0.85
text(pol1,pol2,expression(sp*(beta["ort"])),col="darkred",cex=2)

ts <- as.ts(sim[-1,c("xt1","xt2")])
plot(ts,main="Poziomy xt",yaxt='n',xlab=NULL)

  
tsd <- as.ts(sim[-1,c("dxt1","dxt2")])
plot(tsd,main="Przyrosty xt",yaxt='n',xlab=NULL)
  
plot(as.ts(betatx),yaxt='n',ylab=expression(y[t]== beta^"'" * x[t]),
       main="Odchylenia od r?wnowagi (mispricing): yt")
list(norm=norm,alfa=alfa,beta=beta,P=P,sim=sim,
       betatx=betatx,sigmabeps=sigmabeps,sigmabb=sigmabb)}

goo <- vecsimingo(n=1000,ar=0.5)
l <- lm(goo$sim[,2]~goo$sim[,1]-1)

ar(goo$betatx,FALSE,1)

plot(as.ts(goo$betatx), yaxt='n', ylab = expression(y[t] == beta^"'" * x[t]),
     main = "Odchylenia od rownowagi (mispricing)")

############
# Funkcja cadlag

y0 <- c(50,1, 2, 4, 3,50)
sfun0  <- stepfun(c(0,0.25,0.5,0.75,1.0), y0, f = 0)

plot(sfun0,verticals=FALSE,lwd=3,pch=16,ylab="",xlim=c(0,1),
     ylim=c(0,4),main="cadlag")
points(c(0.25,0.5,0.75,1.0),c(1,2,4,3))

pol1 <- reg[round(0.9*dim(reg)[1])]
pol2 <- -b[1]/b[2]*pol*0.9
text(pol1,pol2,expression(beta["\136"]),col="darkred",lwd=2)
