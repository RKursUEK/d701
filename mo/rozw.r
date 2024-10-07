A <- matrix(c(1,6,3,3,5,2), ncol=2)
b <- c(4, 6, 8)
c <- c(5, 7)
zn <- rep("<=", 3)

library(lpSolve)

# Zadanie prymalne
pl <- lp("max", c, A, rep("<=",3), b)

pl$solution # rozwiazanie opt. x* = (0,0; 1,2)
pl$objval 	# wart. maks. funkcji celu: Z* = 8,4

# Zadanie dualne - rozwiazanie optymalne
y1 <- y3 <- 0
y2 <- c[2]/t(A)[2,2]
y <- c(y1, y2, y3)	# rozw. opt. zad. dual. (ceny dualne): y* = (0,0; 1,4; 0,0)  
K <- b%*%y		# wart. min. funk. celu: K* = 8,4 (stad Z* = K*)