######################################################################################################################
# Uzyskiwanie pomocy dotyczacej funkcji lub pakietow R
######################################################################################################################

# Informacja o biezacej sesji R
sessionInfo()

# Wykaz uzywanych w biezacej sesji przestrzeni nazw
search()

# Pomoc dotyczaca funkcji - podana nazwa musi byc identyczna z nazwa funkcji R
# na przykladzie funkcji lm, implementujacej estymator KMNK (klasyczna metoda najmniejszych kwadratow) 
# parametrow klasycznego modelu normalnej regresji liniowej (KMNRL)

help("lm")
?lm

# Pomoc dotyczaca szukanej frazy
help.search("linear regression")
??"linear regression"

# Przyklad wykorzystania funkcji lm, implementujacej estymator KMNK (klasyczna metoda najmniejszych kwadratow) 
# parametrow klasycznego modelu normalnej regresji liniowej (KMNRL)
example(lm)

# Argumenty funkcji lm, wyznaczajacej wartosci estymatorow KMNK
args(lm)

#############################################################################################################
# Procedury instalacji pakietow R
#############################################################################################################

# Instalacja z serwera CRAN pakietu lpSolve, zawierajacego funkcje do rozwiazywania problemow programowania liniowego 
install.packages("lpSolve")

# Zaladowanie pakietu lpSolve
library(lpSolve)
# Pomoc dotyczaca pakietu lpSolve
help(package = lpSolve)

# Masowa instalacja pakietów z serwera CRAN przyporzadkowanych do okreslonej dziedziny (CRAN Task View)
# W celu wykonania instalacji masowej pakietow, konieczna jest instalacja i zaladowanie pakietu ctv
# CRAN Task Views: https://cran.r-project.org/web/views/
install.packages("ctv") 
library("ctv")
install.views("Optimization")

# Wœród tych kategorii pakietów mamy m.in.: statystyka wielowymiarowa (Multivariate), ekonometria (Econometrics), 
# finanse (Finance), wnioskowanie bayesowskie (Bayesian), statystyka odporna (Robust)
# wykaz rozk³adów zmiennych jedno- i wielowymiarowych (Distributions), metody optymalizacyjne (Optimization), 
# numeryczne metody rozwi¹zywania równañ ró¿niczkowych (DifferentialEquations), uczenie maszynowe (MachineLearning), 
# analiza skupieñ, chemometria (ChemPhys), nauki spo³eczne (SocialSciences) i inne

# Istnieja takze inne serwery, na ktorych zamieszczane sa pakiety R, np. serwer R-Forge: http://R-Forge.R-project.org
# instalacja pakietu CLAG (algorytm analizy skupieñ) z repozytorium Rforge, wsakzujemy repos = "http://R-Forge.R-project.org"
install.packages("CLAG", repos="http://R-Forge.R-project.org")

# Mozna zainstalowac takze tzw. wersje deweloperska pakietu R z platformy Github
# Instalacje pakietów z Github jest mozliwa za pomoca funkcji install_github z pakietu devtools
install.packages("devtools") # Instalujemy pakiet devtools
library(devtools)            # Ladujemy pakiet devtools
# Instalujemy pakiet DepthProc z Github - autorstwa zespolu: Kosiorowski, Zawadzki, Slomczynski, Bocian, Wêgrzynkiewicz
# https://github.com/zzawadz/DepthProc
install_github("zzawadz/DepthProc")

# Komenda zwraca sciezke prowadzaca do wybranego pliku
choose.files()

# Pokaz katalog roboczy
getwd()

# Zmien katalog roboczy
setwd()

# Kopiowanie danych ze schowka (import danych)
read.table("clipboard", sep="\t", dec=",", header=TRUE)

# Kopiowanie danych do schowka (eksport danych)
write.table("clipboard", sep="\t", dec=",")

Import danych z plików csv
# read.csv - separator kolumn - przecinek (,), separator dziesietny - kropka (.) 
# read.csv2 - separator kolumn - srednik (;), separator dziesietny - przecinek (,)
example(read.csv)

# Funkcja generuje 100 pseudolosowych obserwacji z rozkladu normalnego o parametrach (mu = 0, sigma = 1),
# a wartosci zostaja przypisane do obiektu a
(a <- rnorm(100))
# Histogram dla danych z uzyskanej próby
hist(a)
# Przypisanie wykresu do obiektu b
b <- recordPlot()

# Wypisanie wszystkich obiektow znajdujacych sie obecnie w przestrzeni roboczej
ls()

# Zapisz obiekty z przestrzeni roboczej do pliku z rozszerzeniem RData 
save(list = ls(), file = "moj.RData")

# Usuniecie wszystkich obiektów
rm(list = ls())

# Odczytaj obiekty z pliku RData
load(file = "moj.RData")

# Zaladowanie funkcji ze skryptu (Plik z rozszerzeniem R albo txt)
source(choose.files())

######################################################################################################################
# Opcje i ustawienia dotyczace biezacej sesji R
######################################################################################################################

sessionInfo()               # Informacje dotyczace sesji R: attached base packages (zaladowane pakiety bazowe),
# other attached packages (pozostale zaladowane pakiety)

?options
options()                   # Opcje globalne" m.in. digits - liczba wyswietlanych cyfr wyniku, 
# defaultPackages - domyslnie ladowane pakiety, scipen - preferencje dotyczace wyswietlania
# wyników w notacji naukowej, badz staloprzecinkowej (liczba calkowita: dodatnia wartosæ
# preferncja notacji staloprzecinkowej, ujemna - preferencja notacji naukowej)
getOption("scipen", default = NULL)
?par
par()                       # Opcje dotyczace grafiki

format(10^-15, scientific = FALSE)   # Wyswietlanie wyniku w postaci staloprzecinkowej
format(0.0025, scientific = TRUE)  # Wyswietlanie wyniku w postaci naukowej

signif(0.00789, 1)        # Zaokraglenie wyniku z dokladnoscia do pierwszej cyfry znaczacej
round(0.00789, 1)         # Zaokraglenie wyniku z dokladnoscia do jednege miejsca po przecinku

######################################################################################################################
# Reprezentacja liczb w R
######################################################################################################################

.Machine     # Skrótowa specyfikacja sposobu reprezentacji wartosci liczbowych w pamieci (typy jezyka C:
# liczby calkowite: typ long - 4 albo 8 bajtów (systemy 64 bitowe poza Windows),
# typ longlong - 8 bajtów; reprezantacja zmiennoprzecinkowa liczb rzeczywistych: brak typu single, 
# typ double - 8 bajtów (11 bitów - wykladnik, 53 bity - mantysa oraz znak), 
# typ long double 12 bajtów (32 bitowa wersja R) albo 16 bajtów (64-bitowa wersja R))
# typ sprawdzamy za pomoca typeof()
# 32-bitowe liczby calkowite (integers) i liczby rzeczywiste w binarnej reprezentacji zmiennoprzecinkowej
# (floating-point double precision) zgodnej z norma IEC 60559 (znana takze jako IEEE 754),
# opisujacej takze zalozenia arytmetyki zmiennoprzecinkowej (floating point arithmetics)
# 8 bajtowy wskaznik do struktury C SEXP przechowujacej obiekty R (SEXP - podtypy: 
# REALSXP - wektory numeryczne R, INTSXP - wektory liczb calkowitych, LGLSXP - wektory logiczne, 
# STRSXP - wektor znakowy, CPLXSXP - wektor liczb zespolonych, VECSXP - lista i pozostale),
# 
# Szczególy:  ?.Machine
#             https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html
#             http://adv-r.had.co.nz/C-interface.html
#             http://adv-r.had.co.nz/memory.html#object-size

?storage.mode
storage.mode(125L)  # Przechowywana jako binarnie wyra¿ona liczba calkowita
storage.mode(125)   # Przechowywana w ramach binarnej zmiennoprzecinkowej reprezentacji liczb rzeczywistych
storage.mode(35.5)  # Przechowywana w ramach binarnej zmiennoprzecinkowej reprezentacji liczb rzeczywistych
storage.mode(3+2i)  # Przechowywana jako liczba zespolona

.Machine$double.eps      # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka ¿e 1+x != 1)
.Machine$double.neg.eps  # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka ¿e 1-x != 1)

if(!require(pryr)) install.packages("pryr")
library(pryr)
(v <- c(5, 7.5, 12.6))
address(v)                # Sprawdza adres wskaznika (do SEXP reprezentujacego dany obiekt R) w pamieci RAM
sexp_type(v)
object_size(v)            # Rozmiar obiektu w bajtach



