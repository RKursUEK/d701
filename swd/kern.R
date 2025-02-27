x <- c(rnorm(50, 10, 2), rnorm(50, 15, 5))
# Histogram z pakietu stats
hist(x, breaks = "Sturges", freq=FALSE, density=4,col="red") 
# domyslnie liczba klas histogramu wyznaczana wedlug reguly Sturgesa 
# (breaks = "Sturges"): k = 1 + log2 n, inne mozliwoœci: regula Scotta 
# (breaks = "Scott") albo regula Freedmana-Diaconisa (breaks = "FD")
hist(x, breaks = "Scott", freq = FALSE, density = 4, col = "green") 
hist(x, breaks = "FD", freq = FALSE, density = 4, col = "blue")

# Estymator jadrowy gestosci z pakietu stats
density(x, bw = "nrd0", kernel = "gaussian")

# http://cran.r-project.org/web/packages/mosaic/vignettes/GraphicsWithMosaic.html
# histogram
# densityplot

# Estymatory jadrowe funkcji gestosci
library(KernSmooth) 

(h.opt <- dpik(x, scalest = "minim", level = 2L, kernel = "normal",   
     canonical = FALSE, gridsize = 401L, range.x = range(x), 
     truncate = TRUE))

kern <- bkde(x, kernel = "normal", bandwidth = h.opt)
names(kern)
plot(bkde(x, kernel = "normal", bandwidth = h.opt))

example(bkde2D)

dpih(x) # Optymalna szerokosc przedzialu klasowego wedlug reguly Scotta

# Inne pakiety z funkcjami zwiazanymi z estymatorami jadrowymi 
# package kernlab
# package np
# package sm

# Pakiety umozliwiajace tworzenie histogramow 
# package library(HistogramTools) # np. miary dywergencji histogramów - dywergencja Kullbacka-Leiblera
# package histogram

## package MASS
library(MASS)
hist.FD(x) # Freedman–Diaconis
hist.scott(x, prob = TRUE) # Scott 


library(aplpack) 
# m.in. histogramy z suwakiem dla liczby klas,
# wykresy jadrowych estymatorow gestosci z suwakiem 
# dla parametru wygladzania i typu jadra

slider.hist(x, col = "red")
slider.density(x)


