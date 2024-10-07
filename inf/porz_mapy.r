# ###### Wykres 1

# Import danych ze schowka z pliku Woj_mierniki.xls, arkusz miern,
# zakres B144:C160

a <- read.table("clipboard", sep = "\t", header = TRUE, dec = ",")

# Wyswietlenie wartosci miernikow
print(a)

# Podzial przedzialu zmiennosci miernika syntetycznego wedlug wartosci miar pozycyjnych
br <- fivenum(a[, 2])

# Przypisanie wartosci miernika do okreslonego przedzialu
int <- findInterval(a[, 2], br)

# Prezentacja skal kolorow
example(heat.colors)

# Skala 5 kolorow
k <- rev(heat.colors(5))

# Przypisanie kolorow wojewodztwom, w oparciu o przedzial,
# do ktorego przynalezy wartosc miernika
cols <- k[int]

# Wykreslenie mapy z naniesionymi kolorami odpowiadajacymi poszczegolnym
# przedzialom miernika syntetycznego
install.packages("maptools")
library(maptools)

# Zaladowanie mapy Polski w podziale na wojew?dztwa R SpatialPolygonsDataFrame
# Kontury mapy pochodza z portalu http://www.gadm.org/country
load(url("http://wizard.uek.krakow.pl/~d701/inf/pol_adm1.rdata"))

# Pusta mapa
plot(gadm)

# Mapa z naniesionymi kolorami
plot(gadm, col = cols)

# Dodanie tytulu mapy
title("Mierniki syntetyczne rozwoju gospodarczego")

# Etykiety legendy
paste(round(br, 2), round(br[2:5], 2), sep = "-")[1:4] -> l

# Dodanie legendy
legend("bottomleft", legend = l, fill = k, title = "Wartosc miernika")

# ###### Wykres 2

# Uporzadkowanie wojewodztw malejaco wedlug wartosci miernika

ap <- a[order(a[, 2], decreasing = TRUE), ]
rownames(ap) <- NULL

# Stworzenie wykresu kolumnowego
barplot(ap[, 2], col = rainbow(16), main = "Mierniki syntetyczne", xlab = "wojewodztwo", ylab = "q")

# Dodanie legendy
legend("topright", legend = ap[, 1], cex = 0.6, bty = "n", fill = rainbow(16))

####################################################################
# Wykres animowany
# zainstaluj pakiet
install.packages("googleVis")

# Zaladuj pakiet
library(googleVis)

# pobierz dane dotyczace turystyki
load(url("http://wizard.uek.krakow.pl/~d701/inf/tm.rdata"))

# Pokaz nazwy zmiennych
names(tm)

# Wyswietl dane
View(tm)

# Wyswietl podsumowanie
summary(tm)

# Poka? dane tylko dla Polski
View(tm[tm[, 2] == "Poland", ])

# Pokaz dane dla Polski

# Sporzadz ruchomy wykres dla zbioru danych tm
M <- gvisMotionChart(tm, idvar = "country.name", timevar = "year", options = list(width = 700, height = 600))

plot(M)

# Zapisz wykres jako plik z kodem html
print(M, file = "wykres_tur.html")
