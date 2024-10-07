## Zaladuj kontury mapy Polski

load(url("http://wizard.uek.krakow.pl/~s701dok/POL_adm1.RData"))

# ###### Wykres 1

# Wypisanie obiektow w przestrzeni roboczej
ls()

# Import danych ze schowka

a <- read.table("clipboard", sep = "\t", header = TRUE, dec = ",")

# Wyswietlenie wartosci miernik?w
print(a)

# Podzial przedzialu zmiennosci miernika syntetycznego wedlug wartosci miernikow pozycyjnych
br <- fivenum(a[, 2])

# Przypisanie wartosci miernika do okreslonego przedzialu
int <- findInterval(a[, 2], br)

# Prezentacja skal kolorow
example(heat.colors)

# Skala 5 kolorow
k <- rev(heat.colors(5))

# Przypisanie kolorow wojewodztwom, w oparciu o przedzial, do ktorego przynalezy wartosc miernika
cols <- k[int]

# Wykreslenie mapy z naniesionymi kolorami odpowiadajacymi poszczegolnym
# przedzialom miernika syntetycznego

# Pusta mapa
plot(gadm)

# Mapa z naniesionymi kolorami
plot(gadm, col = cols)

# Dodanie tytulu mapy
title("Mierniki syntetyczne rozwoju gospodarczego")

# Etykiety legendy
l <- paste(round(br, 2), round(br[2:5], 2), sep = "-")[1:4]

# Dodanie legendy
legend("bottomleft", legend = l, fill = k)

# ###### Wykres 2

# Uporzadkowanie wojewodztw malejaco wedlug wartosci miernika

ap <- a[order(a[, 2], decreasing = TRUE), ]
rownames(ap) <- NULL

# Stworzenie wykresu kolumnowego
barplot(ap[, 2], col = rainbow(16), main = "Mierniki syntetyczne", xlab = "wojewodztwo", ylab = "q")

# Dodanie legendy
legend("topright", legend = ap[, 1], cex = 0.6, bty = "n", fill = rainbow(16))
