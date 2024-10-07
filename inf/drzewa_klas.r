# Import danych z pliku kredyt.xls
kop <- function(n = TRUE)
  read.table("clipboard",
             sep = "\t",
             header = n,
             dec = ",")
kr <- kop()
names(kr)
dim(kr)
View(kr)
summary(kr)

library(psych) # Ladowanie pakietu
describe.by(kr[, -5], kr[, 5]) # Statystylki opisowe atrybutow w podziale
# na dobrych i zlych kredytobiorcow

plot(kr[, -5], col = kr[, 5]) # Wykresy rozrzutu czarny - dobre kredyty, czerwone - zle
levels(kr[, 5])

install.packages("rpart")
library(rpart) # Ladujemy pakiet z algorytmem rekurencyjnego podzialu
# CART pozwalajacym budowac drzewa klasyfikacyjne
install.packages("rpart.plot")
library(rpart.plot) # Ladujemy pakiet pozwalajacy tworzyc wykresy drzew
# Budowa drzewa maksymalnego z wykorzystaniem kryterium Giniego
(dr <- rpart(kredyt ~ ., method = "class", data = kr))
# Wypisanie elementow zwracanych przez funkcje rpart
names(dr)

# Tabela i wykres dla procedury cost-complexity prunning
printcp(dr)
plotcp(dr)
# Przycinanie do poddrzewa optymalnego przy wspolczynniku
# zlozonosci alfa=0,05 (cp=0.025)
(pdr <- prune(dr, cp = 0.025))
# Podsumowanie zbudowanego drzewa maksymalnego
summary(dr)
# Podsumowanie zbudowanego o mininalnym cost-complexity
summary(dr, cp = 0.025)

# Sciezki prowadzace do lisci (wezlow koncowych)
(nrl <- as.numeric(rownames(pdr$frame[pdr$frame[, 1] == "<leaf>", ]))) # Numery lisci
length(nrl) # Liczba lisci
path.rpart(pdr, node = nrl)
(yl <- pdr$frame[pdr$frame[, 1] == "<leaf>", "yval"]) # Kategorie przewidywane w oparciu o liscie

# Wykresy drzewa maksymalnego
# Wykres drzewa - dla wezlow odsetek obserwacji w poszczegolnych klasach (extra=4)
prp(
  dr,
  type = 4,
  extra = 4,
  main = "Wykres drzewa maksymalnego dla kredytow",
  box.col = c("pink", "palegreen3")[dr$frame$yval]
)
# Wykres drzewa - dla wezlow odsetek obserwacji dotyczacy klasy przypisanej wezlowi (extra=8)
prp(
  dr,
  type = 4,
  extra = 8,
  main = "Wykres drzewa maksymalnego dla kredytow",
  box.col = c("pink", "palegreen3")[dr$frame$yval]
)

# Wykresy drzewa optymalnego - o minimalnym cost-complexity
# Wykres drzewa - dla wezlow odsetek obserwacji w poszczegolnych klasach (extra=4)
prp(
  pdr,
  type = 4,
  extra = 4,
  main = "Wykres drzewa optymalnego dla kredytow",
  box.col = c("pink", "palegreen3")[pdr$frame$yval]
)
# Wykres drzewa - dla wezlow odsetek obserwacji dotyczacy klasy przypisanej wezlowi (extra=8)
prp(
  pdr,
  type = 4,
  extra = 8,
  main = "Wykres drzewa optymalnego dla kredytow",
  box.col = c("pink", "palegreen3")[pdr$frame$yval]
)

# Przewidywania drzewa optymalnego dotyczace kategorii kredytu
predict(pdr, kr)
head(predict(pdr, kr, type = "matrix"))
(prog <- predict(pdr, kr, type = "class"))
(tab <- table(kr[, 5], prog))
(ptab <- prop.table(tab, 1))

# Budowa drzewa maksymalnego z wykorzystaniem kryterium entropii
(drinf <- rpart(
  kredyt ~ .,
  method = "class",
  data = kr,
  parms = list(split = "information")
))
printcp(drinf)
plotcp(drinf)
#######################################################################
# Podzial na probe uczaca i testowa
tr <- sample(1:dim(kr)[1], 700)

ktr <- kr[tr, ]
kte <- kr[-tr, ]

dim(ktr)
dim(kte)

# uczenie drzewa wylacznie w opraciu o probe uczaca
(dr1 <- rpart(kredyt ~ ., data = ktr))
printcp(dr1)
plotcp(dr1)
prp(
  dr1,
  type = 4,
  extra = 8,
  main = "Wykres drzewa optymalnego dla kredytow",
  box.col = c("pink", "palegreen3")[dr1$frame$yval]
)

predict(dr1, ktr) # Prognzowanie dla proby uczacej
(prtr <- predict(dr1, ktr, type = "class"))

(ptabtr <- prop.table(table(ktr[, 5], prtr), 1))

predict(dr1, kte) # Prognzowanie dla proby testowej
(prte <- predict(dr1, kte, type = "class"))
(ptabte <- prop.table(table(kte[, 5], prte), 1))

# Przyklad wykorzystania drzew klasyfikacyjnych w klasyfikacji zbioru danych Iris
library(rpart)
install.packages("tree")
library(tree)
library(psych)
iris
summary(iris)
describe(iris)
plot(iris, col = iris[, 5])
plot(iris[, 3:4],
     col = iris[, 5],
     pch = 19,
     main = "dlugosc platka/szerokosc platka")

ir.tr <- tree(Species ~ ., iris)
ir.tr
ir.tr1 <- snip.tree(ir.tr, nodes = c(12, 7))
summary(ir.tr1)
par(pty = "s")
plot(iris[, 3:4],
     col = iris[, 5],
     pch = 19,
     main = "dlugosc platka/szerokosc platka")
partition.tree(ir.tr1, add = TRUE, cex = 1.5)
