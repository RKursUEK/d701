methods(class = lm)

methods(summary)

# Symulacja 100 realizacji z modelu regresji: y_i = 2 x_i + 5 + eps_i, eps_i ~ N(0, 0.1) 
x <- rnorm(100)
y <- 2 * x + 5 + rnorm(100, 0, 10 ^ -1)
# Oszacowanie KMNK parametrow modelu regresji
reg <- lm(y ~ x)
attributes(reg)
rm("x", "y")

# Wektory numeryczne
a <- c(1, 7, 9, 5, 10, 14, rep(0, 3))
print(a)
length(a)
class(a)

rev(a)
sort(a)
sort(a, decreasing = TRUE)
order(a)
a[2:5]
a[-(3:4)]
summary(a)
names(a) <- letters[1:length(a)]


# Wektory tekstowe
as.character(a)
(b <- c("czerwony", "zielony", "niebieski"))
class(b)
is.character(b)
paste(b[1], b[2], sep = "-")

# Wektory czynnikowe (factor)
(d <- factor(b, ordered = "TRUE"))
levels(d)
labels(b)
ordered(d)

# Wektory logiczne
(e <- as.logical(a))

# Operatory logiczne &, |, &&, ||, ==, !=, <, <=, >, >=, any, all

a
(f <- round(runif(length(a), -1, 5)))

a > 5 & a < 10

# Macierze w R  moga powstawac z polaczenia wektorow tego samego typu
cbind(a, f)
rbind(a, f)

A <- matrix(1:25, ncol = 5)
A[2, 3]
A[4:5, ]
A[, 2:3]
A[, -(2:3)]

args(apply)
# Wyznacz wartosc funkcji (tutaj sumy) dla kazdego wiersza macierzy
apply(A, 1, sum)

# Wyznacz wartosc funkcji (tutaj sumy) dla kazdej kolumny macierzy
apply(A, 2, sum)

# Wyznacz wartosc zdefiniowanej funkcji dla kazdej kolumny macierzy
apply(A, 2, function (x) mean(x) + 5)

# Elementy algebry macierzowej

# Redefiniujemy macierz A (nastepuje nadpisanie)
A <- crossprod(matrix(round(runif(25, 1, 3)), ncol = 5))
print(A)

# Transpozycja macierzy A
t(A)
# Wyznacznik kwadratowej macierzy A
det(A)
# Dekompozycja spektralna macierzy (wartosci i wektory wlasne macierzy kwadratowej)
eigen(A)
# Wymiary macierzy
dim(A)

# Tworzenie macierzy z elementami pseudolosowymi
F <- matrix(rnorm(100, 170, 15), ncol = 5)
G <- matrix(rnorm(15, 170, 15), nrow = 5)
H <- matrix(rnorm(100), ncol = 5)
dim(F)
dim(G)
dim(H)

# Wykres rozrzutu dla macierzy A
pairs( ~ ., data = F, main = "Wykres rozrzutu")

# Mnozenie odpowiadajacych sobie elementow macierzy F i H o takich samych
# wymiarach - iloczyn Hadamarda (Schura)
(K <- F * H)
dim(K)
# F*G  : Iloczyn macierzy F oraz G o odpowiednich wymiarach
(L <- F %*% G)
dim(L)
# AxG  : Iloczyn Kroneckera macierzy A i G
(M <- A %x% G)
dim(M)
# F'H : Iloczyn skalarny macierzy F i H (iloczyn transpozycji macierzy F
# i macierzy H)
(N <- crossprod(F, H))
dim(N)
# A'*A: iloczyn skalarny
(P <- crossprod(A))
dim(P)

# Tworzy macierz diagonalna
diag(5) # jednostkowa o wymiarze 5x5
diag(a) # diagonalna o elementach przekatniowych z wektora x
# Wyciaga elementy przekatniowe z macierzy A
diag(A)
A

# Macierz odwrotna do nieosobliwej macierzy A
(Ainv <- solve(A))
A %*% Ainv
Ainv %*% A

(g <- matrix(round(runif(5, 1, 10))))
dim(g)

# Rozwiazanie rownania liniowego Ax = g
(x <- solve(A, g))

### Ramki danych grupuja wektory roznych typow w ramach kolumn
(df <- data.frame(a, b, e))
rownames(df) <- NULL
colnames(df) <- c("liczba", "tekst", "logiczny")
df
dim(df)
df[, 1]
df[, "liczba"]
df[1:3, 5]
(p <- df[order(df[, "liczba"]), ])
attach(df)
detach(df)

# Listy
l <- list(num = a, tekst = b, czynn = d)
l
names(l)
# Elementem listy mo?e byc inna lista
(ll <- list(num = a, tekst = b, lista = l))

# Otrzymujemy liste z jednym elementem num
l[1]
class(l[1])
# Otrzymujemy wektor num
l[[1]]
class(l[[1]])
# Otrzymujemy piaty element wektora num
l[[1]][5]
# Wyciagniecie elementu num z wykorzystaniem $
l$num


### Elementarna grafika
plot(a, type = "l", main = "Probny wykres", xlab = "czas [t]", ylab = "wartosc [y]")
abline(h = 5, lwd = 2, col = "red", lty = 2)
abline(v = 2)
abline(-2, 3, lwd = 3, col = "green")

# Histogram
args(hist)
hist(a, col = "red")

# Wykres pudelkowy
boxplot()
# Wykres slupkowy
barplot()
#Wykres funkcji
curve(2 * x ^ 3 + 5 * x ^ 2 + 7 * x, -100, 100, xlab = "x", ylab = expression(y == 2 * x ^ 3 + 5 * x ^ 2 + 7 * x),
  main = "Wykres funkcji")
curve(log(1 + x), 1, 100)

# Umieszczenie wielokata na wykresie
polygon()


#### petla for
x <- 7
for (i in 1:5) {
  x[i + 1] <- x[i] ^ 2 + 5
}
x

y <- c()
for (i in runif(10, 1, 15))
  y <- c(y, rnorm(1, 0, i))
y

## Grupa funkcji apply pozwalajacych zastapic petle for

# sapply - zwraca wynik w uproszczonej formie wektora (ramki danych)
args(sapply)
arg = runif(10, 1, 15)
sapply(arg, function(i) rnorm(1, 0, i))
sapply(1:10, function(i) rnorm(1, 0, i))

# Funkcja ma wiecej niz jeden argument (drugi y funkcji aryt ustalamy na poziomie 5)
sapply(1:10, aryt, y = 5)

# lapply - zwraca te same wynik co sapply, jednak w postaci listy
lapply()

## W mapply wartosc funkcji wyznaczona jest dla zmieniajacych sie kombinacji
# wartosci wiecej niz jednego argumentu
args(mapply)
mapply()

tapply()

## replicate
replicate(100, mean(rnorm(100, 5, 10)))

## odwolanie warunkowe ifelse
x <- 5
ifelse(x > 0, x ^ 1 / 3, abs(x) + 3)

# if bez instrukcji dla alternatywnego dzialania, jezeli sprawdzany warunek jest falszywy
x <- -5
if (x > 0) {
  x <- x ^ 1 / 3
}


### Petla while (wyznacza kolejny wyraz ciagu geometrycznego: x_0 = 5, x_i = x_i-1 * 10, i = 1, 2, ...
# dopoki biezacy wyraz jest mniejszy badz rowny 1000)
x<- y <- 5
while (y <= 1000) {
  y <- y * 10
  x <- c(x,y)
}


#### Instrukcja switch
x <- 4:5
dz = "rozn"
dz = "iloraz"
switch(dz, rozn = x[2] - x[1], iloraz = x[2] / x[1])


## Definiowanie wlasnej funkcji
aryt <- function(x, y) {
  if (!all(is.numeric(x), is.numeric(y), y != 0)) stop("Podaj wektor numeryczny, przy czym y != 0")
  d <- x + y
  o <- x - y
  m <- x * y
  dz <- x / y
  list(x = x, y = y, suma = d, roznica = o, iloczyn = m, iloraz = signif(dz, 2))
}

aryt(5,2)
aryt("a",3)
aryt(2,0)

m <- round(runif(100, 0, 100))
n <- round(runif(100, 0, 100))

mapply(aryt, m, n)


system.time(mapply(aryt, m, n))

##################################
pierw <- function(x) {
  a <- vars::roots(x, modulus = FALSE)
  plot(Re(a), Im(a), xlab = "Re(z)", ylab = "Im(z)")
  require(plotrix)
  draw.circle(0, 0, 1)
}

# methods

potega <- function(x) {
  y <- z <- c()
  for (i in 1:length(x)) {
    if (x[i] <= 5) {
      y[i] <- x[i] ^ 2
      z[i] <- "kwadrat"
    }
    else
    {
      y[i] <- x[i] ^ 0.5
      z[i] <- "pierwiastek"
    }
  }
  
  p <- data.frame(argument = x, wynik = signif(Re(y), 2), dzialanie = z)
  plot(p[, 1:2], col = p[, 3], pch = 19)
  p
}

################# Wykresy funkcji gestosci i dystrybuant rozkladow

#### Rozklad jednostajny
wu <- function() {
  par(mfrow = c(2, 1))
  curve(dunif(x, 1, 3), 1, 3, lwd = 2, axes = FALSE, ylab = "F(x)", xlim = c(0.5, 3.5), ylim = c(0, 0.75),
    main = "Funkcja gestosci rozkladu jednostajnego: U~(a,b)")
  
  axis(1, at = c(0.5, 1, 3, 4), labels = c("", "a", "b", ""), pos = c(0, 0))
  
  axis(2, at = c(0, 1 / 2, 2), labels = c(0, expression(frac(1, b - a)), ""), pos = c(0.5, 0), las = 2)
  
  curve(punif(x, 1, 3), 1, 3, lwd = 2, axes = FALSE, ylab = "F(x)", xlim = c(0.5, 3.5),
        ylim = c(0, 1.5), main = "Dystrybuanta rozkladu jednostajnego: U~(a,b)")
  
  axis(1, at = c(0.5, 1, 3, 4), labels = c("", "a", "b", ""), pos = c(0, 0))
  axis(2, at = c(0, 1, 2), labels = c(0, 1, ""), pos = c(0.5, 0), las = 2)
}

#### Rozklad normalny
wn <- function() {
  par(mfrow = c(2, 1))
  
  curve(dnorm(x, 0, 1), -3, 3, lwd = 2, axes = FALSE, ylab = "F(x)", xlim = c(-3.5, 3.5),
    main = "Funkcja gestosci rozkladu normalnego: N~(mu,sigma)")
  
  axis(1, at = c(-4, -1, 0, 1, 4), labels = c("", expression(mu - sigma), expression(mu), expression(mu + sigma), ""),
       pos = c(0, 0))
  
  axis(2, at = c(0, 1 / (2 * pi * exp(1)) ^ 0.5, 1 / (2 * pi) ^ 0.5, 2), 
       labels = c(0, expression(frac(1, sigma * sqrt(2 * pi * e))), expression(frac(1, sigma * sqrt(2 * pi))), ""),
       pos = c(-3.5, 0), las = 2)
  
  curve(pnorm(x, 0, 1), -3, 3, lwd = 2, axes = FALSE, ylab = "F(x)", xlim = c(-3.5, 3.5),
        main = "Dystrybuanta rozkladu normalnego: N~(mu,sigma)")
  
  axis(1, at = c(-4, -1, 0, 1, 4), labels = c("", expression(mu - sigma), expression(mu), expression(mu + sigma), ""),
       pos = c(0, 0))
  
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1), pos = c(-3.5, 0), las = 2)
}
