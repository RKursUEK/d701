# Maksymalizacja funkcji uzytecznosci Cobba-Douglasa
# log U= alpha1 * log x1 + alpha2 * log x2
# pod warunkiem ograniczenia budzetowego
# M = p1* x1+ p2* x2

M <- 100
alpha1 <- 0.3
alpha2 <- 0.7
p1 <- 5
p2 <- 7

# Maksimum warunkowe
x1 <- alpha1 / (alpha1 + alpha2) * M / p1 # funkcja popytu gosp. dom. na dobro 1
x2 <- alpha2 / (alpha1 + alpha2) * M / p2 # funkcja popytu gosp. dom. na dobro 2

# Wartosc uzytecznosci dla maksimum warunkowego
Uopt <- x1 ^ alpha1 * x2 ^ alpha2
# Rownowaznie
Uopt <- (M / (alpha1 + alpha2)) ^ (alpha1 + alpha2) * (alpha1 / p1) ^ alpha1 * (alpha2 / p2) ^ alpha2

## Krzywa obojetnosci dla U = Uopt
# x2 = Uopt^(1/alpha2) * x1^(-alpha1/alpha2)
ob <- function (x) Uopt ^ (1 / alpha2) * x ^ (-alpha1 / alpha2)
curve(ob, from = 0.25 * x1, to = 1.75 * x1, n = 1000, xlab = "x1", ylab = "x2")

# Krzywa jednakowego budzetu
# x2 = M/p2 - p1/p2 * x1
abline(M / p2, -p1 / p2)

# Punkt maksimum warunkowego
points(x1, x2, col = "red", pch = 16)

## Optymalizacja warunkowa z wykorzystaniem gotowych funkcji
# z pakietu Rsolnp
install.packages("Rsolnp")
library(Rsolnp)

# Funkcja celu
f <- function (x) -(alpha1 * log(x[1]) + alpha2 * log(x[2]))
# Warunek ograniczajacy
g <- function (x) p1 * x[1] + p2 * x[2]

# Maksymalizacja uzytecznosci przy ograniczeniu budzetowym
?solnp
solnp(rep(1, 2), f, g, M)