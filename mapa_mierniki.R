## Za�aduj kontury mapy Polski

load(url("http://wizard.uek.krakow.pl/~s701dok/POL_adm1.RData"))

# ###### Wykres 1

# Wypisanie obiekt�w w przestrzeni roboczej
ls()

# Import danych ze schowka

a <- read.table("clipboard", sep = "\t", header = TRUE, dec = ",")

# Wyswietlenie wartosci miernik�w
print(a)

# Podzia� zmiennosci miernika syntetycznego wed�ug wartosci miar pozycyjnych
br <- fivenum(a[, 2])

# Przypisanie wartosci miernika do okreslonego przedzia�u
int <- findInterval(a[, 2], br)

# Prezentacja skal kolor�w
example(heat.colors)

# Skala 5 kolor�w
k <- rev(heat.colors(5))

# Przypisanie kolor�w wojew�dztwom, w oparciu o przedzia�, do kt�rego przynale�y miernik
cols <- k[int]

# Wykreslenie mapy z naniesionymi kolorami odpowiadajacymi poszczeg�lnym
# przedzia�om miernika syntetycznego

# Pusta mapa
plot(gadm)

# Mapa z naniesionymi kolorami
plot(gadm, col = cols)

# Dodanie tytu�y mapy
title("Mierniki syntetyczne rozwoju gospodarczego")

# Etykiety legendy
l <- paste(round(br, 2), round(br[2:5], 2), sep = "-")[1:4]

# Dodanie legendy
legend("bottomleft", legend = l, fill = k)

# ###### Wykres 2

# Uporzadkowanie wojew�dztw malejaco wed�ug wartosci miernika

ap <- a[order(a[, 2], decreasing = TRUE), ]
rownames(ap) <- NULL

# Stworzenie wykresu kolumnowego
barplot(ap[, 2], col = rainbow(16), main = "Mierniki syntetyczne", xlab = "wojew�dztwo", ylab = "q")

# Dodanie legendy
legend("topright", legend = ap[, 1], cex = 0.6, bty = "n", fill = rainbow(16))
