## Za³aduj kontury mapy Polski

load(url("http://wizard.uek.krakow.pl/~s701dok/POL_adm1.RData"))

# ###### Wykres 1

# Wypisanie obiektów w przestrzeni roboczej
ls()

# Import danych ze schowka

a <- read.table("clipboard", sep = "\t", header = TRUE, dec = ",")

# Wyswietlenie wartosci mierników
print(a)

# Podzia³ zmiennosci miernika syntetycznego wed³ug wartosci miar pozycyjnych
br <- fivenum(a[, 2])

# Przypisanie wartosci miernika do okreslonego przedzia³u
int <- findInterval(a[, 2], br)

# Prezentacja skal kolorów
example(heat.colors)

# Skala 5 kolorów
k <- rev(heat.colors(5))

# Przypisanie kolorów województwom, w oparciu o przedzia³, do którego przynale¿y miernik
cols <- k[int]

# Wykreslenie mapy z naniesionymi kolorami odpowiadajacymi poszczególnym
# przedzia³om miernika syntetycznego

# Pusta mapa
plot(gadm)

# Mapa z naniesionymi kolorami
plot(gadm, col = cols)

# Dodanie tytu³y mapy
title("Mierniki syntetyczne rozwoju gospodarczego")

# Etykiety legendy
l <- paste(round(br, 2), round(br[2:5], 2), sep = "-")[1:4]

# Dodanie legendy
legend("bottomleft", legend = l, fill = k)

# ###### Wykres 2

# Uporzadkowanie województw malejaco wed³ug wartosci miernika

ap <- a[order(a[, 2], decreasing = TRUE), ]
rownames(ap) <- NULL

# Stworzenie wykresu kolumnowego
barplot(ap[, 2], col = rainbow(16), main = "Mierniki syntetyczne", xlab = "województwo", ylab = "q")

# Dodanie legendy
legend("topright", legend = ap[, 1], cex = 0.6, bty = "n", fill = rainbow(16))
