###############################################################################################################
### Statystyczna teoria decyzji
##############################################################################################################
# sdt - decyzja przy Y|X
# L - macierz definiujaca funkcje straty, domyslnie wszystkie 
# pozadiagonalne równe 1, diagonalne rowne 0,
# d -dane dotyczace wartosci x, prawdopodobienstwa z rozkladu brzegowego y 
# (prawdopodobienstwa a priori), typ rozkladu warunkowego X|Y, 
# np. dnorm - normalny, ... odnosi sie do parametrów rozk³adu
# np. w przypadku rozkladu normalnego sa to mean i sd

sdt <- function(L = NULL, d, py, r,...){
  pw <- function(d) eval(parse(text=paste(r,"(x=d,...)",sep="")))
  # Warunkowa gêstoœæ x|y
  pwx <- t(sapply(d,pw))
  nr <- dim(pwx)[1]
  nc <- dim(pwx)[2]
  #Prawdopodobieñstwo laczne (x,y)
  pl <- pwx * matrix(rep(py,nr),nrow=nr,byrow=TRUE)
  # Prawdopdobienstwo warunkowe y|x
  pwy <- prop.table(pl,1)
  
  porz<-function(l,decr=FALSE) c(order(l,decreasing=decr)[1],l[order(l,decreasing=decr)[1]])
  
  if(is.matrix(L)){
    # Oczekiwana strata przy decyzji di
    el <- pwy %*% t(L)
    # Decyzja minimalizujaca oczekiwana strate
    
    dec <- t(apply(el,1,porz))}
  
  # Decyzja odpowiadajaca maksymalnemu prawdopodobienstwu y|x
  elmax <- matrix(1,nrow=nr,ncol=nc)-pwy
  decmax <- t(apply(pwy,1,porz,decr=TRUE))
  
  colnames(pwx) <- paste("p.x.y", 1:nc, sep = "")
  colnames(pl) <- paste("p.xy", 1:nc, sep = "")
  colnames(pwy) <- paste("p.y", 1:nc,".x", sep = "")
  colnames(elmax) <- paste("ro.d",1:(dim(elmax)[2]), sep = "")
  colnames(decmax) <- c("d*","p.y.x.max=1-ro*")
  if(is.matrix(L)){ 
    colnames(el) <- paste("ro.d",1:(dim(el)[2]), sep = "")
    colnames(dec) <- c("d*","ro*")}
  
  c(list(dist=r,...),list(cond.x.y=pwx,joint.xy=pl,cond.y.x=pwy),
    if(is.matrix(L))
    {
      list(EL.by.dec=el,
           dec = dec)
    }
    else{
      list(EL.by.dec=elmax,
           dec = decmax)
    }
  )
}

###############################################################################################################
### Przyklady decyzji z sdt
###############################################################################################################

args(sdt)
# Rozklad warunkowy X|Y = y: rozklad normalny (mu_1 = 1, mu_2 = 5, mu_3 = 5; sigma_1 = 2, sigma_2 = 3, sigma_3 = 4)
sdt(d = 5:10, py = c(0.2,0.3,0.5), r = "dnorm", mean = c(1,10,5), sd = 2:4)

# Rozklad warunkowy x|y: rozklad t-Studenta
sdt(d = 5:10, py = c(0.2,0.3,0.5), r = "dt", df = 2:4)
# Macierz strat
(L <- matrix(c(0,1,2,3,0,4,5,6,0), byrow = TRUE, ncol = 3))
sdt(L = L, d = 5:10, py = c(0.2,0.3,0.5), r = "dnorm", mean = c(1,10,5), sd = 2:4)

install.packages("gamlss.dist")
library(gamlss.dist)

# Rozklad warunkowy X|Y = y: uogolniony rozklad t-Studenta
# dGT(x, mu = 0, sigma = 1, nu = 3, tau = 1.5, log = FALSE)

sdt(d = 5:10, py = c(0.2,0.3,0.5), r = "dGT", mu = c(1,10,5), sigma = 2:4, tau = rep(1.5,3))

# Wykresy warunkowych gestoœci X|Y = y
plot(x = NULL, xlim = c(-5,5), ylim = c(-10,10), ylab = "")
curve(dnorm(x, 0, 1.5), from = -5, to = 5, col = "red", ylab = "")
par(new = TRUE)
curve(dnorm(x, 1, 1.5), from = -5,to = 5, col = "green", ylab = "")
par(new = TRUE)
curve(dnorm(x, 2, 1.5), from = -5, to = 5, col = "blue", ylab = "fx|y (x)")
abline(v = c(0, 1, 2), lty = 2, col = c("red", "green", "blue"))
legend("topleft", "Conditional distr. expected val.", lty = 2)

# funkcja matplot - alternatywne wykreœlanie kilkuwiêkszej iloœci krzywych na jednym wykresie
# Gestosci dla teoretycznych rozkladow warunkowych x|y, gdzie y = 1, 2, ..., K
# albo (cond = FALSE)
# (Zrzutowana) gestosc dla teoretycznego rozkladu lacznego (x, y), gdzie y = 1, 2, ..., K
mixtcondjointdens <- function(cond = TRUE, pi = rep(1 / 3, 3), mu = c(2, 6, 10), sigma = rep(1, 3)){
  fr <- min(qnorm(.01, mu, sigma))
  to <- max(qnorm(.99, mu, sigma))
  x <- seq(from = fr, to = to, length.out = 1000)
  main <- ifelse(cond, "Conditional density plot: f_x|y(x)", "Joint density plot: f_x,y(x,y)")
  #  plot(x = NULL, xlim = c(-5,5), ylim = c(-10,10), ylab = "")
  densx <- NULL
  for(i in 1:length(pi)){
    if(cond){
      densx <- cbind(densx, dnorm(x, mu[i], sigma[i]))
    }else{
      densx <- cbind(densx, pi[i] * dnorm(x, mu[i], sigma[i]))
    }
  }
  matplot(x, densx, type = "l", lty = 1, main = main)
}

mixtcondjointdens()
# Rozklad warunkowy: X|Y = k
mixtcondjointdens(pi = c(.2, .5, .3), mu = 0:2, sigma = rep(1.5, 3), cond = TRUE)
# Rozklad laczny (X,Y), przy roznych proporcjach pi_k, k = 1, 2, ..., K
mixtcondjointdens(pi = c(.2, .5, .3), mu = 0:2, sigma = rep(1.5, 3), cond = FALSE)
