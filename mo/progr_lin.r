# Instalacja i zaladowanie pakietu lpSolve implementujacego algorytmy rozwiazywania problemow PL (m.in. algorytm simpleks)
install.packages("lpSolve")
library(lpSolve)
help(package=lpSolve)

# Informacja na temat funkcji lp implementujacej algorytm simpleks
?lp

# lp (direction = "min", objective.in, const.mat, const.dir, const.rhs,
#	transpose.constraints = TRUE, int.vec, presolve=0, compute.sens=0,
#       binary.vec, all.int=FALSE, all.bin=FALSE, scale = 196, dense.const, 
#       num.bin.solns=1, use.rw=FALSE)

#########################################################
# Rozwiazanie problemu PL za pomoca algorytmu simpleks
#########################################################

# Okreslenie parametr?w zadania
(A <- matrix(c(5,3,2,4,5,2),ncol=3))
(b <- c(3,5))
(c <- c(10,8,9))
(zn <- rep(">=",2))

# n - liczba zmiennych decyzyjnych, m - liczba funkcyjnych warunkow ograniczajacych
m <- dim(A)[1]
n <- dim(A)[2]
choose(m+n,m) # g?rne ograniczenie liczby iteracji algorytmu

(opt <- lp("min", c, A, zn, b, compute.sens=1))

names(opt) 	# wypisuje elementy listy z wynikami
opt$solution 	# rozwiazanie optymalne zagadnienia minimalizacji
opt$objval 	# wartosc funkcji celu dla rozwiazania optymalnego (minimum)
opt$duals 	# rozwiazanie dualne (wiersz zerowy tablicy simpleks, 
	  	# w pierwszej kolejnosci podano wartosci odpowiadajace trzem zmiennym swobodnym)
# niezerowe wartosci w zerowym wierszu swiadcza, ze zmienne swobodne s1 i s2 sa niebazowe,
# czyli funkcyjne warunki ograniczajace 1 i 2 spelnione sa dla rozwiazania optymalnego jako r?wnosci)

# wrazliwosc na zmiany parametrow funkcji celu (analiza przedzialowa)
opt$sens.coef.from
opt$sens.coef.to

# wrazliwosc na zmiany wyrazow wolnych funkcyjnych warunkow 
# ograniczajacych (analiza przedzialowa)
opt$duals.from
opt$duals.to


# Zadanie dualne (zdefiniowanie oraz rozwiazanie zadania dualnego, dla sprawdzenia wynikow uzyskanych wyzej)
(zn1 <- rep("<=",3))
(optd <- lp("max", b, t(A), zn1, c, compute.sens=1))
optd$solution
optd$objval