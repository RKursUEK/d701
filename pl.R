# Instalacja pakietu clusterSim
install.packages("clusterSim")

# Ladowanie pakietu
library(clusterSim)

# Import danych z pliku woj_pusty.xls - komorki B5:K20
a <- read.table("clipboard",sep="\t",dec=",",header=FALSE)

# Tworzenie miernikow syntetycznych z wykorzystaniem metody antywzorca
b <- data.Normalization(pattern.GDM1(a[,-1],
                                performanceVariable=c("d","s","d","s","s","s","s","s","s"),
                                scaleType="r",normalization="n1",patternType="lower")$distances,type="n4")

# Porzadkowanie obiektow
c <- as.data.frame(sort(b,decreasing=TRUE),
              as.character(a[order(b,decreasing=TRUE),1]))
names(c)<-"miernik syntetyczny"
print(c)

# Eksport wynikow do arkusza kalkulacyjnego
write.table(c,"clipboard",sep="\t",dec=",")

# Nalezy zauwazyc, ze wyniki dotyczace wartosci miernika syntetycznego
# roznia sie od tych w pliku woj_wypeln.xls, gdyz w nim do pomiaru odleglosci
# od antywzorca wykorzystano odleglosc euklidesowa, natomiast w niniejszej
# analizie wykorzystano ugolniona miare odleglosci GDM1 