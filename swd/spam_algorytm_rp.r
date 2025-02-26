u <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data")
read.table(u,header=FALSE,sep=",",dec=".")->d

nu <- url("http://wizard.uek.krakow.pl/~s701dok/swd/spam_names.txt")
n <- read.table(nu,sep="\t",dec=".",header=FALSE)
install.packages("stringr")
library(stringr)
n <- str_trim(as.character(unlist(n)))

colnames(d)<-n
apply(d,2,class)
class(d)
y <- factor(d[,58],levels=c(0,1),labels=c("nie-spam","spam"))
d[,58]<-y
head(d)
############################33
install.packages("rpart")
library(rpart) # Ladujemy pakiet z algorytmem rekurencyjnego podzialu 
# CART pozwalajacym budować drzewa klasyfikacyjne
install.packages("rpart.plot")
library(rpart.plot) # Ladujemy pakiet pozwalajacy tworzyć wykresy drzew

# Pomoc R dotyczšca funkcji pakietów rpart i rpart.plot
help(package=rpart)
help(package=rpart.plot)

# Budowa drzewa maksymalnego z wykorzystaniem kryterium Giniego
(dr <- rpart(list~.,method="class",data=d))
# Wypisanie elementów zwracanych przez funkcję rpart
names(dr)

# Tabela i wykres dla procedury cost-complexity prunning
printcp(dr)
plotcp(dr)
# Przycinanie do poddrzewa optymalnego przy współczynniku 
# złożonosci alfa=0,05 (cp=0.025)
(pdr<-prune(dr,cp=0.025))
# Podsumowanie zbudowanego drzewa maksymalnego
summary(dr)
# Podsumowanie zbudowanego o mininalnym cost-complexity
summary(dr,cp=0.025)

# Scieżki prowadzace do lisci (wezlow koncowych)
(nrl<-as.numeric(rownames(pdr$frame[pdr$frame[,1]=="<leaf>",]))) #Numery li??ci
length(nrl) #Liczba li??ci
path.rpart(pdr, node =nrl)
(yl<-pdr$frame[pdr$frame[,1]=="<leaf>","yval"]) # Kategorie przewidywane dlaw oparciu o liscie

# Wykresy drzewa maksymalnego
# Wykres drzewa - dla wezlow odsetek obserwacji w poszczegolnych klasach (extra=4)
prp(dr,type=4,extra=4,main = "Wykres drzewa maksymalnego dla kredytów" ,
    box.col=c("pink", "palegreen3")[dr$frame$yval])
# Wykres drzewa - dla węzłów odsetek obserwacji dotyczacy klasy przypisanej węzłowi (extra=8)
prp(dr,type=4,extra=8,main="Wykres drzewa maksymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[dr$frame$yval])

# Wykresy drzewa optymalnego - o minimalnym cost-complexity
# Wykres drzewa - dla wezlow odsetek obserwacji w poszczegolnych klasach (extra=4)
prp(pdr,type=4,extra=4,main="Wykres drzewa optymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])
# Wykres drzewa - dla węzłów odsetek obserwacji dotyczšcy klasy przypisanej węzłowi (extra=8)
prp(pdr,type=4,extra=8,main="Wykres drzewa optymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])

# Przewidywania drzewa optymalnego dotycz??ce kategorii listu
predict(pdr,d)
head(predict(pdr,d, type = "matrix"))
(prog<-predict(pdr,d,type="class"))
(tab<-table(d[,58],prog))
(ptab<-prop.table(tab,1))
