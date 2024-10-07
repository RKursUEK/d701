# Zaladowanie danych
load(choose.files())

# Wypisanie obiektow
ls()

# Prezentacja obiektu "wzrost"
print(wzrost)

colnames(wzrost)

summary(wzrost)

hist(wzrost[, "matka"],
     main = "Histogram dla wzrostu matki",
     xlab = "liczebnosc",
     ylab = "wzrost w cm")

boxplot(
  wzrost[, -4],
  outline = TRUE,
  main = "Wykes ramka-wasy dla zmiennych",
  xlab = "zmienna",
  ylab = "wzrost w cm"
)

# Zapamietanie wykresu jako obiektu
ram <- recordPlot()

# Oszacowanie modelu regresji liniowej: zmienna zalnezna - wzrost syna,
# wzrost matki, liczba przeczytanych ksiazek
lm(syn ~ ., data = wzrost)
summary(lm(syn ~ ., data = wzrost))

# Wyb?r podzbioru regresorow w oparciu o kryterium informacyjne
step(lm(syn ~ ., data = wzrost))

# Nanjni?sza warto?? kryterium AIC dla podzbioru zmiennych
#obja?niaj?cych wzrost ojca, wzrost matki

lm(syn ~ ojciec + matka, data = wzrost)

# Przypisanie modelu regresji liniowej do obiektu rl

rl <- lm(syn ~ ojciec + matka, data = wzrost)

# Podsumowanie modelu regresji liniowej

summary(rl)
sr <- .Last.value

# 95% przedzial ufnosci dla parametrow modelu
confint(rl)

# Graficzna ocena spelnienia zalozen modelu regresji liniowej

par(mfrow = c(2, 2))
plot(lm(syn ~ ., data = wzrost))

# Przyklad wektora numerycznego
(wn <- round(rnorm(50, 100, 30), 2))

# Przyk?ad wektora logicznego
(wl <- as.logical(rbinom(20, 1, .5)))

# Przyk?ad wektora tekstowego
(wt <- c("raz", "dwa", "trzy"))

# Przyklad wektora czynnikowego
(oc <- factor(round(runif(50, 2, 5)), labels = c("ndst", "dst", "db", "bdb")))

# Polaczyc wektor numeryczny i czynnikowy w ramke danych (data frame)
(wyniki <- data.frame(wn, oc))

# Skopiowac ramke danych wyniki poprzez schowek (clipboard) ramke danych wyniki
write.table(wyniki, "clipboard", sep = "\t", dec = ",")

# Zapisac wszystkie obiekty do pliku z rozszerzeniem RData
save(list = ls(), file = "calosc.RData")
