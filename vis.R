# Zainstaluj pakiet
install.packages("googleVis")

# Zaladuj pakiet
library(googleVis)

# Pobierz dane dotyczace turystyki
load(choose.files())

# Pokaz nazwy zmiennych
names(tm)

# Wyswietl dane
fix(tm)

# Wyswietl podsumowanie
summary(tm)

# Pokaz dane tylko dla Polski
edit(tm[tm[, 2] == "Poland", ])

# Pokaz dane dla Polski

# Sporzadz ruchomy wykres dla zbioru danych tm
M <- gvisMotionChart(
  tm,
  idvar = "country.name",
  timevar = "year",
  options = list(width = 700, height = 600)
)

plot(M)

# Zapisz wykres jako plik z kodem html
print(M, file = "wykres_tur.html")
