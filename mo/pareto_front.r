# Wyznaczyc front Pareto dla zbioru obiektow, zawierajacego wartosci dwoch
# kryteriow czastkowych f1(x), f2(x) podlegajacych maksymalizacji

install.packages("rPref")
library(rPref)
help(plot_front)

dane <- as.data.frame(matrix(c(0.5, 0.5, 1, 0.5, 2, 2, 3, 4, 4, 3), byrow = TRUE, ncol = 2))
colnames(dane) <- c("f1", "f2")
plot(dane)
pref <- high(f1) * high(f2) # preferowane sa wieksze wielkosci obu kryteriow
niezd <- psel(dane, pref) # rozwiazania niezdominowane sposrod punktow w zbiorze
plot_front(dane, pref, col = rgb(0, 0, 1)) # front Pareto
points(niezd$f1, niezd$f2, lwd = 3)
