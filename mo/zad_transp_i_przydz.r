library(lpSolve)

########################################################################
# Zadanie transportowe 
# zbilansowane zadanie transportowe (podaz=popyt=2000): 3 dostawcow, 4 odbiorcow

# macierz kosztow transportu od dostawcy Di do odbiorcy Oj
#     O1  O2  O3  O4
# D1  50  40  60  20
# D2  40  80  70  30
# D3  60  40  70  80

# wektor reprezentujacy podaz poszczegolnych dostawcow
# D1  700
# D2  500
# D3  800

# wektor reprezentujacy popyt poszczegolnych odbiorcow
# O1  O2  O3  O4
# 400 600 500 500

# Okreslic strukture przewozow minimalizujaca laczny koszt transportu, 
# przy zaspokojeniu calego popytu oraz wyczerpaniu calej podazy

# okreslenie macierzy kosztow C
(C <- matrix(c(50,40,60,40,80,40,60,70,70,20,30,80), ncol=4))

# okreslenie wektora podazy 
(d <- c(700,500,800))
(znd <- rep("=",3))

# okreslenie wektora popytu 
(o <- c(400,600,500,500))
(zno <-rep("=", 4))

# Rozwiazanie zadania transportowego
(opttr <- lp.transport(C, "min", znd, d, zno, o))
names(opttr)
opttr$solution
opttr$objval

########################################################################
# Zadanie przydzialu (assignment problem)

# kazdemu wykonujacemu przydzielane jest dokladnie jedno zadanie
# kazde zadanie wykonuje wylacznie jeden wykonujacy

# Macierz kosztow wykonania przez i-tego wykonawce j-tego zadania 
# 3 wykonujacych i 3 zadania
#     Z1    Z2    Z3
# W1  2,34  5,10  3,15
# W2  4,12  5,00  1,30
# W3  4,50  2,20  9,10

# Nalezy dokonac przydzialu zadan minimalizujego laczny koszt wykonania wszystkich zajec

# okreslenie macierzy kosztow
(Cp <- matrix(c(2.34,4.12,4.5,5.1,5,2.2,3.15,1.3,9.1), ncol = 3))

# Rozwiazanie problemu przydzialu zadan
(optp <- lp.assign(kp,"min"))
names(optp)
optp$solution
optp$objval
