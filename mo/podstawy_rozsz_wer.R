##################################################################################################################
### Wektory ###

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

# Liczby zespolone
(z <- 5 + 6i)
class(z)
Conj(z)
Mod(z)
abs(z)
Re(z)
Im(z)
Arg(z)

# Pierwiastki wielomianu
if(!require(polynom)) install.packages("polynom")
library(polynom)
# 1 + 2*x + 3*x^2 + 4*x^3 
(wiel <- polynomial(1:4))
solve(wiel)

# Wektory licz zespolonych
(x <- rnorm(5))
(y <- rnorm(5))
# Tworzenie wektora liczb zespolonych
(z1 <- complex(real = x, imaginary = y))
as.complex(x)
plot(z1)

# Wektory tekstowe
as.character(a)
(ba <- LETTERS[a + 1])
(tx <- c("czerwony", "zielony", "niebieski"))
class(tx)
is.character(tx)
paste(tx[1], tx[2], sep = "-")

# Wektory czynnikowe (factor)

# Nieuporzadkowane kategorie
(ftx <- factor(rep(tx, 10), ordered = FALSE))
is.ordered(ftx)

# Uporzadkowane kategorie
war <- c("niski", "sredni", "wysoki")
(stan <- war[floor(runif(100, 1, 4))])
(d <- factor(stan, levels = war, ordered = TRUE))
levels(d)
unclass(d)
is.ordered(d)

# Wektory logiczne
(e <- as.logical(a))

#Operatory logiczne &, |, &&, ||, ==, !=, <, <=, >, >=, any, all

(vl <- a > 5 & a < 10)
any(vl); all(vl)
!vl

###########################################################################################################
### Macierze ###

# Tworzenie macierzy liczb
print(a)
(f <- round(runif(length(a), -1, 5)))

(raf <- rbind(a, f))
rownames(raf) <- NULL
print(raf)

(caf <- cbind(V1 = a, V2 = f)) # alternatywnie (caf<-cbind(V1=a,V2=f)); colnames(caf)<-c("V1","V2")


(B <- matrix(1:25, ncol = 5))
args(matrix)
B[2, 3]
B[4:5, ]
B[, 2:3]
B[, -(2:3)]
nrow(B)
ncol(B)
dim(B) # Wymiary macierzy B
colMeans(B)
rowMeans(B)
colSums(B)
rowSums(B)
t(B) # Transpozycja macierzy B
det(B) # Wyznacznik macierzy B

#wyznacz wartosc funkcji (tutaj sumy) dla kazdego wiersza macierzy
apply(B, 1, sum)

#wyznaczanie wartosci funkcji (tutaj sumy) dla kazdej kolumny macierzy
apply(B, 2, sum)

# wyznacz wartosc zadanej funkcji dla kazdej kolumny macierzy
apply(B, 2, function (x) mean(x) + 5)

# Tworzenie macierzy z elementami pseudolosowymi
F <- matrix(rnorm(100, 170, 15), ncol = 5)
G <- matrix(rnorm(15, 170, 15), nrow = 5)
H <- matrix(rnorm(100), ncol = 5)
dim(F)
dim(G)
dim(H)

# Iloczyn macierzy      %*%
(L <- F %*% G)
dim(L)

# Iloczyn Hadamarda      *
(K <- F * H)
dim(K)

# Iloczyn Kroneckera    %x%
(Z <- B %x% G)
dim(Z)
# alternatywnie
kronecker(B, G)

# Iloczyn skalarny      "crossprod"
(dotB <- crossprod(B)) # B'B
dim(dotB)
all.equal(dotB, t(B) %*% B)
(dotFH <- crossprod(F, H)) # F'H
all.equal(dotFH, t(F) %*% H)

# Outer product
(outB <- tcrossprod(B)) # BB'
dim(outB)
(outFH <- tcrossprod(F, H)) # FH'
dim(outFH)

# Macierz kwadratowa stopnia 5 z elementami pseudolosowymi
(M <- (matrix(round(rnorm(25, 10, 2)), ncol = 5)))
dim(M)
det(M)
(Minv <- solve(M))

# Dekompozycja QR macierzy
(QR <- qr(M))
QR$rank
qr.Q(QR) # Wydobycie macierzy Q
qr.R(QR) # Wydobycie macierzy R
qr.X(QR)

(S <- crossprod(matrix(rnorm(25), ncol = 5))) # Macierz symetryczna
# Dekompozycja Choleskiego
(L <- chol(S))

# Macierz odwrotna
(is <- solve(S))
(idec <- chol2inv(L)) # Zwraca macierz odwrotna do macierzy X=LL', przyjmujac jako argument macierz L z dekompozycji Choleskiego

identical(idec, is) #
all.equal(idec, is) # Uwzglednia blad zwiazany z zastosowaniem arytmetyki zmiennoprzecinkowej

# "Kwadratowe" uklady rownan liniowych - rozwiazanie
print(M)
(bM <- matrix(round(runif(nrow(M), 1, 9))))
det(M)
solve(M, bM)

# "Prostokatne" uklady rownan liniowych - rozwiazanie (z wykorzystaniem dekompozycji QR macierzy parametrow)
# Uklad niedookreslony
(A <- matrix(runif(12), 3))
(b <- 1:3)
dim(A)
qr(A)$rank
qr.solve(A, b)

# Uklad nadokreslony
(tA <- t(A))
(bpr <- 1:4)
dim(tA)
qr(tA)$rank
qr.solve(tA, bpr)

#### Pseudoinwersja Moore'a-Penrose'a
if(!require(MASS)) install.packages("MASS")
library(MASS)
ginv(A) #Pseudoinwersja macierzy A

# Dekompozycja wedlug wartosci osobliwych (svd) A = UDV'
(sd <- svd(A))

# Dekompozycja wedlug wartosci wlasnych (eigen) Ax = \lambda x
eigen(M)

# Uogolnione zagadnienie wlasne Ax =\lambda Bx
library(geigen)
geigen(M, S)

eigen(solve(S) %*% M)

# Wektoryzacja macierzy vec(M)
c(M)

# Tworzenie macierzy diagonalnej
# macierz jednostkowa stopnia 5
diag(5)
# macierz diagonalna o elementach diagonalnych z wektora a
is.vector(a)
diag(a) 
# Wyciaganie elementow diagonalnych z macierzy B
is.matrix(B)
diag(B)

# Pakiet matrixcalc
if(!require(matrixcalc)) install.packages("matrixcalc")
library(matrixcalc)
help(package=matrixcalc)

is.square.matrix(M) # Czy macierz jest kwadratowa
is.square.matrix(A)

is.symmetric.matrix(M) # Czy macierz jest symetryczna
is.symmetric.matrix(S)

lower.triangle(M)
upper.triangle(M)
vec(M)
vech(M)

# Potegowanie macierzy
matrix.power(M, k = 3)

# Dekompozycja LU
lu.decomposition(M)

is.diagonal.matrix(diag(rnorm(5)))
# Normy macierzowe
entrywise.norm(M, p = 1)
frobenius.norm(M)
spectral.norm(M)


###############################################################################################
### Ramki danych i listy ###

### Ramki danych grupuja wektory roznych typow w postaci kolumn
(df <- data.frame(a, ba, e))
rownames(df) <- NULL
colnames(df) <- c("liczba", "tekst", "logiczny")
print(df)
dim(df)
df[, 1]
df[, "liczba"]
df[1:3, 2]
(p <- df[order(df[, "liczba"]), ])
attach(df)
detach(df)

# Listy
(l <- list(num = a, tekst = tx, czynn = d))
names(l)
# Elementem listy moze byc inna lista
(ll <- list(num = a, tekst = b, lista = l))

# Otrzymujemy liste z jednym elementem num
l[1]
class(l[1])
#Otrzymujemy wektor num
l[[1]]
class(l[[1]])
# Otrzymujemy piaty element wektora num
l[[1]][5]
#Wycianiecie elementu num z wykorzystaniem $
l$num

############################################################################################################
### Przeplyw sterowania ###

# Petla for
xx <- 7
for (i in 1:5) {
  xx[i+1]<-xx[i]^2+5
}
print(xx)

RNGkind() # Sprawdzenie wykorzystywanego generatora liczb pseudolosoeych
set.seed(1234) # Ustawienie ziarna generatora liczb pseudolosowych

yy <- NULL
for (i in runif(10,1,15)) yy<-c(yy,rnorm(1,0,i))
print(yy)

## Grupa szybszych funkcji *apply, pozwalajacych zastapic petle for

# sapply - zwraca wynik w uproszczonej formie wektora (ramki danych) 
args(sapply)
set.seed(1234)
arg <- runif(10, 1, 15)
sapply(arg, function(i) rnorm(1, 0, i))
set.seed(5678)
sapply(1:10, function(i) rnorm(1, 0, i))

# lapply - zwraca te same wyniki co sapply, tylko w postaci listy
set.seed(5678)
lapply(1:10, function(i) rnorm(1, 0, i))

# replicate - powtarzanie wywolania funkcji (drugi argument), okreslona (przez pierwszy argument) liczbe razy
replicate(100, mean(rnorm(100, 5, 10)))

# petla while (procedura jest wykonywana dopoki sprawdzany przed jej kolejnym wykonaniem warunek jest prawdziwy)
lwar <- 7
while (lwar > 0) {
  lwar <- lwar - 5
  print(lwar)
}

# instrukcja warunkowa ifelse (wariant skrocony, zwraca wylacznie wartosci skalarne)
zz <- 5
ifelse(zz > 0, zz ^ 1 / 3, abs(zz) + 3)

zz <- -3
ifelse(zz > 0, zz ^ 1 / 3, abs(zz) + 3)

# instrukcja warunkowa if ... else ...
(war1 <- 5:10)
if (all(war1 > 0)) {
  war1 <- war1 ^ 1 / 3
}
print(war1)

# W przypadku braku czlonu else, w sytuacji gdy sprawdzany warunekj jest falszywy, nie dzieje sie nic

(war2 <- -3:5)
if (all(war2 > 0)) {
  war2 <- war2 ^ 1 / 3
}
print(war2)

# Pelna wersja if ... else ...
(war <- runif(1, -1, 1))
if (war > 0) {
  rnorm(5, 170, 3)
} else {
  runif(5, -20, -10)
}

# Instrukcja switch
w <- 4:5
dz <- "rozn"
dz <- "iloraz"
switch(dz, rozn = w[2] - w[1], iloraz = w[2] / w[1])

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

# sapply mozna stosowac dla funkcji pobierajacej wiecej niz jeden argument 
# (tylko jeden argument funkcji jest zmieniany, pozostale utrzymywane sa na tym samym poziomie /tutaj y = 5/)
sapply(1:10, aryt, y = 5)

# W przebiegu mapply mozna zmieniac wartosci wiecej niz jednego argumentu wykonywanej funkcji,
# zastosowanie mapply, daje wyniki takie same jak w przypadku zagniezdzania petli for

m <- round(runif(100, 0, 100))
n <- round(runif(100, 0, 100))

mapply(aryt, m, n)

# Pomiar czasu wykonania procedury
system.time(mapply(aryt, m, n))

#############################################################################################################
### Instalacja pakietow i importowanie danych ###

# Wykaz zaladowanych pakietow
search()

# Pomoc dotyczaca funkcji - podana nazwa musi byc identyczna z nazwa funkcji
help("lm")
?lm

# Pomoc dotyczaca szukanej frazy
help.search("linear regression")
??"linear regression"

# Przyklad wykorzystania funkcji szacujacej z pomocy KMNK parametry klasycznego modelu regresji liniowej
example(lm)

# Argumenty funkcji szacujacej paremetry klasycznej regresji liniowej
args(lm)

# Instalacja pakietu lpSolve, zawierajacego funkcje do rozwiazywania zagadnien programowania liniowego 
install.packages("lpSolve")

#Zaladowanie pakietu lpSolve
library(lpSolve)
# Pomoc dotyczaca pakietu lpSolve
help(package=lpSolve)

# Grupowa instalacja pakietow przyporzadkowanych do okreslonej dziedziny (Task View)
install.packages("ctv") # Konieczna jest instalacja i zaladowanie pakietu ctv
library("ctv")
install.views("Optimization")

# Komenda zwraca sciezke prowadzaca do wybranego pliku
choose.files()

# Pokaz katalog roboczy
getwd()

# Zmien katalog roboczy
setwd(choose.dir())

# Kopiowanie danych ze schowka (import danych)
?read.table
(dane<-read.table("clipboard",sep="\t",dec=",",header=TRUE))
View(dane)
edit(dane)

# Import danych z plikow csv
# read.csv - separator kolumn - przecinek (,), separator dziesietny - kropka (.) 
# read.csv2 - separator kolumn - srednik (;), separator dziesietny - przecinek (,)
example(read.csv)

# Pobieranie dzinnych notowan WIG 30 ze strony
# http://bossa.pl/notowania/metastock/
# notowania wszystkich instrumentow notowanych w systemie ciaglym zebrane w pliku mstcgl.zip, pobieramy WIG30.mst
temp <- tempfile()
download.file("http://bossa.pl/pub/ciagle/mstock/mstcgl.zip",temp)
unzip(temp,files="WIG30.mst")
wig30 <- read.csv(file="WIG30.mst",header=TRUE)
View(wig30)

# Konwersja ramki danych (data.frame) na obiekt klasy xts z indeksem czasowym (data)
if(!require(xts)) install.packages("xts")
library(xts)
wig30xts <- as.xts(wig30[,-(1:2)],as.Date(as.character(wig30[,2]),format="%Y%m%d"),dateFormat=datef,.RECLASS=TRUE)
colnames(wig30xts) <- c("Open","High","Low","Close","Vol")
head(wig30xts)
tail(wig30xts)
plot(diff(wig30xts$Close,arithmetic=FALSE), main="Wykres logarytmicznych stop zwtotu z notowan na zamkniecie WIG30")

# Funkcja generuje 100 losowych obserwacji z rozkladu Normalnego o parametrach (0,1),
# a wartosci zostaja przypisane do obiektu los
los<-rnorm(100)
print(los)
# Histogram dla danych z uzyskanej proby
hist(los)
# Przypisanie wykresu do obiektu pl
pl<-recordPlot()

# Kopiowanie danych do schowka (eksport danych)
write.table(los,"clipboard",sep="\t",dec=",",row.names=FALSE)

# Wypisanie wszystkich obiektow w przestrzeni roboczej
ls()

# Usuniecie wszystkich obiektow
# rm(list=ls()))

# Zapisz obiekty z przestrzeni roboczej do pliku z rozszerzeniem RData 
save(list=ls(),file="moj.RData")

# Odczytaj obiekty z pliku RData
load(file="moj.RData")

# Zaladowanie funkcji ze skryptu (Plik z rozszerzeniem R albo txt)
source(choose.files())


methods(class=lm) # Metody (funkcje), ktora mozna zastosowac dla obiektu klasy lm


# Funkcje przeladowane na przykladzie funkcji summary
methods(summary)

rx <- rnorm(100)
ry <- 2*rx+5+rnorm(100,0,10^-1)
(reg<-lm(ry~rx))
attributes(reg)
summary(reg) # Funkcja summary zastosowana dla obiektu klasy lm: summary.lm
summary(a)  # Funkcja summary dla wektora numerycznego
summary(d) # Funkcja summary zastosowana do wektora czynnikowego: summary.factor
rm("rx","ry")

############################################################################################################
### Elementarna grafika ###

### Elementarna grafika
plot(a,type="l",main="Probny wykres",xlab="czas [t]",ylab="Wartosci [y]")
abline(h=5,lwd=2,col="red",lty=2)
abline(v=2)
abline(-2,3,lwd=3,col="green")

#Histogram
args(hist)
hist(a,col="red")

# Wykres pudelkowy
boxplot(rexp(100),main="Wykres pudelkowy - dane wygenerowane z rozkladu wykladniczego")
# Wykres slupkowy
barplot(summary(d),col="blue",main="Wykres pudelkowy")
# Wykres funkcji
curve(2*x^3+5*x^2+7*x,-100,100,xlab="x",ylab=expression(y==2*x^3+5*x^2+7*x),
      main="Wykres funkcji wielomianowej")
curve(log(1+x), 1, 100)

# Umieszczenie wielokata na wykresie
polygon()

# Zwracanie rysunkow do pdf zamiast do okna wykresow konsoli
pdf("wykres1.pdf", width = 8, height = 6)
example(plot)
dev.off() # Przywraca domyslne urzadzenie do zwracania wykresow tzn. okno wykresow R
example(plot)

# Zwracanie wykresow w postaci kodu TikZ dla LaTeX do pliku
if(!require(tikzDevice)) install.packages("tikzDevice")
library(tikzDevice)
tikz("tkizwykres.tex",width=3.5,height=3.5)
plot(rnorm(100), type="l", main="Wykres wartosci pseudolosowych", xlab="indeks", ylab="Wartosc pseudolosowa")
dev.off()

# Zwrocenie kodu tikZ takze do konsoli R
tikz("tkizwykres1.tex",width=3.5,height=3.5,console=TRUE)
plot(rnorm(100), type="l", main="Wykres wartosci pseudolosowych", xlab="indeks", ylab="Wartosc pseudolosowa")
dev.off()


#############################################################################################################
### Wstepne przygotowanie danych ###

scale(M, center = TRUE, scale = TRUE)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
help(package=dplyr)

# Pobieranie pakietu "PogromcyDanych", w ktorym znajduja sie dane 
# (czesc fragmentow kodu ponizej, pochodza ze skryptu: Biecek P., "Pogromcy Danych. Przetwarzanie danych w R")
if (!require(PogromcyDanych)) install.packages("PogromcyDanych")
library(PogromcyDanych)
koty_ptaki
names(koty_ptaki)
class(koty_ptaki)

# Filtrowanie danych w oparciu o warunki logiczne
filter(koty_ptaki, predkosc > 100)

filter(koty_ptaki, predkosc > 100, druzyna == "Ptak", habitat %in% c("Polnoc", "Euroazja"))

# Porzadkowanie danych
arrange(koty_ptaki, predkosc) # Rosnace wedlug wartosci kolumny "predkosc"

arrange(koty_ptaki, desc(predkosc)) # Malejace wedlug wartosci kolumny "predkosc"

arrange(koty_ptaki, druzyna, predkosc) # Sortowanie wedlug wielu zmiennych

# Wybor wskazanych kolumn z ramki danych
select(koty_ptaki, gatunek, predkosc, waga)
select(koty_ptaki,gatunek:dlugosc, druzyna)
# Wybierz wszystkie kolumny, z wylaczeniem wskazanych poprzedzonych symbolem "-"
select(koty_ptaki,-habitat, -waga, -druzyna) 
# Wybierz kolumny, ktore maja w nazwie fraze "osc"
select(koty_ptaki, matches("osc"))

# Tworzenie nowych zmiennych, w oparciu o wartosci z kolumny (zmiennych) w zbiorze
# nazwa.zmiennej = wz?o.na.nowa.zmienna
mutate(koty_ptaki, predkosc.mph = round(predkosc * 0.621371),
       dlugosci.na.sek = round(predkosc / 3.6 / dlugosc,1))
# Nadpisywanie zmiennych
mutate(koty_ptaki, dlugosc = round(dlugosc * 100 / 2.54))

# Podsumowanie danych
summarise(koty_ptaki, najszybszy = max(predkosc),
          najwolniejszy = min(predkosc),
          najciezszy = max(waga),
          srednia.waga = mean(waga),
          najdluzsza.nazwa = max(nchar(as.character(gatunek))))

# Wykorzystanie w podsumowaniu funkcji wartosci wiecej niz jednej zmiennej
summarise(koty_ptaki, sredni.ped = mean(waga*predkosc),
          medianowy.ped = median(waga*predkosc),
          predkosc.w.dlugosciach.na.sek = max(predkosc * 1000 / dlugosc / 3600))


# Podsumowanie danych pogrupowanych wedlug kategorii zmiennej
summarise(group_by(koty_ptaki, druzyna),najszybszy = max(predkosc),
          najciezszy = max(waga),
          najzywotniejszy = max(zywotnosc),
          liczba = n())

# Pobieranie danych z eurostatu - pakiet SmarterPoland
if(!require(devtools)) install.packages(devtools)
library(devtools)
if(!require(SmarterPoland)) install_github("pbiecek/SmarterPoland")

library(SmarterPoland)
tsdtr210 <- getEurostatRCV("tsdtr210")
View(tsdtr210)

# Pobieranie danych z BDL GUS
# library(SmarterPoland)

BDLtree <- getBDLtree(2)
head(BDLtree)

DBLtransport <- getBDLsearch("transport")
head(DBLtransport)

BDLseries <- getBDLseries(metric_id = 34)
head(BDLseries)
