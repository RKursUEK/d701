x<-c(rnorm(50,10,2),rnorm(50,15,5))
#Histogram z pakietu stats
hist(x, breaks="Sturges", freq=FALSE, density=4,col="red") 
# domyœlnie liczba klas histogramu wyznaczana wg regu³y Sturgesa 
# (breaks="Sturges"): k = 1 + log2 n, inne mo¿liwoœci: regu³a Scotta 
# (breaks="Scott") albo regu³a Freedmana-Diaconisa (breaks="FD")
hist(x, breaks="Scott", freq=FALSE, density=4,col="green") 
hist(x, breaks="FD", freq=FALSE, density=4,col="blue")

#Estmator j¹drowy gêstoœci z pakietu stats
density(x,bw="nrd0",kernel=gaussian")

# http://cran.r-project.org/web/packages/mosaic/vignettes/GraphicsWithMosaic.html
# histogram
# densityplot

#Estymatory j¹drowe funkcji gêstoœci
library(KernSmooth) 

(h.opt<-dpik(x, scalest = "minim", level = 2L, kernel = "normal",   
     canonical = FALSE, gridsize = 401L, range.x = range(x), 
     truncate = TRUE))

kern<-bkde(x,kernel="normal",bandwidth=h.opt)
names(kern)
plot(bkde(x,kernel="normal",bandwidth=h.opt))

example(bkde2D)

dpih(x) #Optymalna szerokoœæ przedzia³u klasowego wg regu³y Scotta

# Inne pakiety z funkcjami zwi¹zanymi z estymatorami j¹drowymi 
# package kernlab
# package np
# package sm

# Pakiety umo¿liwiaj¹ce tworzenie histogramów 
# package library(HistogramTools) # np. miary dywergencji histogramów - dywergencja Kullbacka-Leiblera
# package histogram

## package MASS
library(MASS)
hist.FD(x) # Freedman–Diaconis
hist.scott(x, prob = TRUE) #Scott 


library(aplpack) 
# m.in. histogramy z suwakiem dla liczby klas,
# wykresy j¹drowych estymatorów gêstoœci z suwakiem 
# dla parametru wyg³adzania i typu j¹dra

slider.hist(x,col="red")
slider.density(x)


