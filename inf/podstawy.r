# Instalowanie pakietu funkcji
?install.packages
# Ladowanie pakietu funkcji w celu ich uzycia
?library

# Masowe instalowanie pakietow z danej dziedziny np. finansow (Finance)
install.packages("ctv")
library(ctv)
install.views("Finance")

######### Prosty przyklad regresji liniowej
# Import danych poprzez schowek (dane z pliku 2_7_dod_opt_reg.xls,
# arkusz reg_lin, zakres komorek B15:D45)
?read.table
(d <- read.table(
  "clipboard",
  sep = "\t",
  dec = ",",
  header = TRUE
))
head(d)
View(d)

# Nazwy kolumn
colnames(d)

# Podsumowanie zbioru danych wg statystyk opisowych
install.packages("psych")
library(psych)
?describe
describe(d)

# Wykres rozrzutu
??"scatter plot"
plot(d)
pairs(d)

# Oszacowanie parametrow regresji liniowej - funkcja lm, pakiets stats
?lm
(l <- lm(yi ~ xi1 + xi2, data = d))
# Wypisz elementy, kt?re zwraca funkcja lm
names(l)

# Alternatywnie, jezeli wszystkie pozostale zmienne oprocz yi, uwzgledniamy
# jako objasniajace
lm(yi ~ ., data = d)

# Podsumowanie oszacowanego modelu
(ls <- summary(l))

#Wypisz elementy, ktore zwraca odsumowanie funkcj lm
names(ls)

#Wykresy - diagnostyka modelu
plot(l)

# Prognoza dotyczaca wartosci y dla ustalonych wartosci x1, x2:
# (x1,x2)=(35,40)
?predict.lm
predict(l, data.frame(t(c(xi1 = 35, xi2 = 40))))

## Tworzenie histogarmu (jako nieparametryczny estymator funkcji gestosci)

# Import danych poprzez schowek (dane z pliku 2_6_stat_opis.xls,
# arkusz stat_opis, zakres B1:C51)

y <- read.table("clipboard",
                sep = "\t",
                dec = ",",
                header = TRUE)
head(y)
View(y)

x <- y[, 1]
head(x)

?hist

# Histogram z pakietu stats
hist(
  x,
  breaks = "Sturges",
  freq = FALSE,
  density = 4,
  col = "red"
)
# domyslnie liczba klas histogramu wyznaczana wg reguly Sturgesa
# (breaks="Sturges"): k = 1 + log2 n, inne mozliwosci: regula Scotta
# (breaks="Scott") albo regula Freedmana-Diaconisa (breaks="FD")
hist(
  x,
  breaks = "Scott",
  freq = FALSE,
  density = 4,
  col = "green",
  main = "Histogram - przedzialy Scott"
)
hist(
  x,
  breaks = "FD",
  freq = FALSE,
  density = 4,
  col = "blue",
  "Histogram - przedzialy FD"
)
wyk <- recordPlot()

## Jadrowe estymatory funkcji gestosci
## (jako nieparametryczne estymatory gestosci)
library(KernSmooth)

(h.opt <- dpik(
    x,
    scalest = "minim",
    level = 2L,
    kernel = "normal",
    canonical = FALSE,
    gridsize = 401L,
    range.x = range(x),
    truncate = TRUE
  ))

kern <- bkde(x, kernel = "normal", bandwidth = h.opt)
names(kern)
plot(bkde(x, kernel = "normal", bandwidth = h.opt))

install.packages("aplpack")
library(aplpack)
# m.in. histogramy z suwakiem dla liczby klas,
# wykresy jadrowych estymatorow gestosci z suwakiem
# dla parametru wygladzania i typu jadra

slider.hist(x, col = "red")
slider.density(x)

# Sprawdz katalog roboczy
getwd()
# Zmien katalog roboczy
?setwd
# Zapisz wszystkie obiekty z przestrzeni roboczej do pliku
save(file = "moj.RData", list = ls())

#########################################################################
methods(class = lm)
methods(summary)

# Wektory numeryczne
a <- c(1, 7, 9, 5, 10, 14, rep(0, 3))
a
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
b <- c("czerwony", "zielony", "niebieski")
class(b)
is.character(b)
paste(b[1], b[2], sep = "-")

# Wektory czynnikowe
factor
d <- factor(b, ordered = "TRUE")
levels(d)
labels(b)

# Wektory logiczne
e <- as.logical(a)

# Operatory logiczne &, |, &&, ||, ==, !=, <, <=, >, >=, any, all

a
a > 5 & a < 10
which(a > 5 & a < 10)
a[a > 5 & a < 10]
f <- round(runif(length(a), -1, 5))

# Macierze w R  moga powstawac z polaczenia wektorow tego samego typu
cbind(a, f)
rbind(a, f)

A <- matrix(c(1:25), ncol = 5)
A[2, 3]
A[4:5, ]
A[, 2:3]

A[, -(2:3)]

args(apply)
# Wyznacz wartosc funkcji (tutaj sumy) dla kazdego wiersza macierzy
apply(A, 1, sum)
A

# Wyznacz wartosc funkcji (tutaj sumy) dla kazdej kolumny macierzy
apply(A, 2, sum)

# Wyznacz wartosc wlasnej funkcji dla kazdej kolumny macierzy
apply(A, 2, function (x) mean(x) + 5)

# Transpozycja macierzy A
t(A)
# Wyznacznik kwadratowej macierzy A
det(A)
# Dekompozycja spektralna (wartosci i wektory wlasne)
eigen(A)
# Wymiary macierzy
dim(A)
A <- matrix(rnorm(100, 170, 15), ncol = 5)

# Wykres rozrzutu dla macierzy A
pairs( ~ ., data = A, main = "Wykres rozrzutu")

# Elementy algebry macierzowej

# Mnozenie odpowiadajacych sobie elementow macierzy A i B o takich samych
# wymiarach - iloczyn Hadamarda
A * B
# AB  : Iloczyn macierzy A oraz B o odpowiednich wymiarach
A %*% B
# AB'	: Iloczyn wektorowy macierzy A i B, iloczyn macierzy A
#oraz transpozycji macierzy B
A %o% B
# A'B Iloczyn skalarny macierzy A i B (iloczyn transpozycji macierzy A i macierzy B)
crossprod(A, B)
crossprod(A)

# Tworzy macierz diagonalna
diag(5) # jednostkowa o wymiarze 5x5
diag(a) # diagonalna o elementach przekatniowych z wektora x
# Wyciaga elementy przekatniowe z macierzy A
diag(A)
A

# Ax=b
# Macierz odwrotna do nieosobliwej macierzy A
solve(A)
# Rozwiazanie rownania liniowego Ax=b
solve(A, b)

### Ramki danych grupuja wektory roznych typow w postaci kolumn
data.frame()
dim
D[, "nazwa"]
D[1:3, 5]