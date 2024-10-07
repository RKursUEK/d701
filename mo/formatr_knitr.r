# Pakiet formatR
install.packages("formatR")
library(formatR)
# Porzadkowanie kodu - interfejs graficzny
tidy.gui()

getwd() # Ewentualna zmiana katalogu roboczy poprzez funkcjê setwd

# Porzadkowanie kodu i zrzut do konsoli
tidy.source("pl_geom.R")

# Porzadkowanie kodu i zapis do pliku
tidy.source("pl_geom.R", file = "output.R")

# Alternatywnie
adr <- "http://wizard.uek.krakow.pl/~d701/mo/pl_geom.r"
tidy.source(adr)
tidy.source(adr, file = "output.R")

# Pakiet knitr (Literate programming)
# Wyciagniêcie kodu z dokumentu z rozszerzeniem Rnw (pliki ³aczace kod R i LaTeX)
purl("ro.Rnw")