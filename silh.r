### Ladowanie pakietow do anlizy skupien
library(cluster)
library(clusterSim)
kop <- function(n = FALSE)
read.table("clipboard", sep = "\t", header = n, dec = ",")
w <- kop() # kopiowanie danych z pliku woj_pusty.xls
rownames(tt) <- kop()[, 1] # kopiowanie nazw obiektow
ww <- data.Normalization(w, "n1") # normalizacja danych na 0-1

# Analiza skupien metoda k-srednich
# wybor pozadanej liczby skupien za pomoca metody Silhouette
si <- function(X) {
  sil = NULL
  for (k in 1:(dim(X)[1] - 2)) {
    sil[k] = mean(silhouette(kmeans(X, k + 1)$clust, dist(X))[, 3], full = TRUE)
  }
  bb <- cbind(k = 1:length(sil), sil = sil)
  kk <- bb[order(bb[, 2], decreasing = TRUE), ]
  colnames(kk) <- c("klastry", "silhouette")
  list(maxklastr = kk[1, 1],
       maxsilh = kk[1, 2],
       rank = kk)
}

os <- si(ww)$maxklastr

kmeans(ww, os)
kmeans(ww, 3)

# Aglomeracyjna metoda analizy skupien - metoda Warda
dd <- dist(ww, method = "euclidean")
de <- dist(ww, method = "manhattan")

hc <- hclust(dd, method = "ward")
# Dendrogram podzialu
plot(hc)

# Ilustracja graficzna analizy skupien metoda k-srednich

library(MASS)
m1 <- c(5, 10)
m2 <- c(5, 15)
m3 <- c(0, 12)
cov <- diag(2)
dane1 <- mvrnorm(50, m1, cov, empirical = TRUE)
dane2 <- mvrnorm(50, m2, cov, empirical = TRUE)
dane3 <- mvrnorm(50, m3, cov, empirical = TRUE)
dane <- rbind(dane1, dane2, dane3)

plot(dane, pch = 19)
as <- kmeans(dane, 3)
plot(dane,
     col = as$cluster,
     pch = 19,
     main = "Klasteryzacja metoda k-srednich")
points(as$centers,
       col = "yellow",
       pch = 19,
       cex = 3)
