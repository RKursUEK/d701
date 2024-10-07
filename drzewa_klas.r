#Import danych z pliku kredyt.xls
kop<-function(n=TRUE) read.table("clipboard",sep="\t",header=n,dec=",")
kr<-kop()
names(kr)
dim(kr)
View(kr)
summary(kr)

library(psych) # �adowanie pakitu
describe.by(kr[,-5],kr[,5]) #Statystylki opisowe atrybut�w w podziale 
# na dobrych i z�ych kredytobiorc�w

plot(kr[,-5],col=kr[,5]) # Wykresy rozrzutu czarny - dobre kredyty, czerwone - z�e
levels(kr[,5])

install.packages("rpart")
library(rpart) # �adujemy pakiet z algorytmem rekurencyjnego podzia�u 
# CART pozwalaj�cym budowa� drzewa klasyfikacyjne
install.packages("rpart.plot")
library(rpart.plot) #�adujemy pakiet pozwalaj�cy tworzy� wykresy drzew
#Budowa drzewa maksymalnego z wykorzystaniem kryterium Giniego
(dr<-rpart(kredyt~.,method="class",data=kr))
# Wypisanie element�w zwracanych przez funkcj� rpart
names(dr)

# Tabela i wykres dla procedury cost-complexity prunning
printcp(dr)
plotcp(dr)
# Przycinanie do poddrzewa optymalnego przy wsp�czynniku 
# z�o�ono�ci alfa=0,05 (cp=0.025)
(pdr<-prune(dr,cp=0.025))
#Podsumowanie zbudowanego drzewa maksymalnego
summary(dr)
#Podsumowanie zbudowanego o mininalnym cost-complexity
summary(dr,cp=0.025)

# �cie�ki prowadz�ce do li�ci (w�z��w ko�cowych)
(nrl<-as.numeric(rownames(pdr$frame[pdr$frame[,1]=="<leaf>",]))) #Numery li�ci
length(nrl) #Liczba li�ci
path.rpart(pdr, node =nrl)
(yl<-pdr$frame[pdr$frame[,1]=="<leaf>","yval"]) #Kategorie przewidywane dlaw oparciu o li�cie

#Wykresy drzewa maksymalnego
#Wykres drzewa - dla w�z��w odsetek obserwacji w poszczeg�lnych klasach (extra=4)
prp(dr,type=4,extra=4,main="Wykres drzewa maksymalnego dla kredyt�w",
    box.col=c("pink", "palegreen3")[dr$frame$yval])
#Wykres drzewa - dla w�z��w odsetek obserwacji dotycz�cy klasy przypisanej w�z�owi (extra=8)
prp(dr,type=4,extra=8,main="Wykres drzewa maksymalnego dla kredyt�w",
    box.col=c("pink", "palegreen3")[dr$frame$yval])

#Wykresy drzewa optymalnego - o minimalnym cost-complexity
#Wykres drzewa - dla w�z��w odsetek obserwacji w poszczeg�lnych klasach (extra=4)
prp(pdr,type=4,extra=4,main="Wykres drzewa optymalnego dla kredyt�w",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])
#Wykres drzewa - dla w�z��w odsetek obserwacji dotycz�cy klasy przypisanej w�z�owi (extra=8)
prp(pdr,type=4,extra=8,main="Wykres drzewa optymalnego dla kredyt�w",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])

#Przewidywania drzewa optymalnego dotycz�ce kategorii kredytu
predict(pdr,kr)
head(predict(pdr,kr, type = "matrix"))
(prog<-predict(pdr,kr,type="class"))
(tab<-table(kr[,5],prog))
(ptab<-prop.table(tab,1))

# Budowa drzewa maksymalnego z wykorzystaniem kryterium entropii
(drinf<-rpart(kredyt~.,method="class",data=kr,
              parms=list(split="information")))
printcp(drinf)
plotcp(drinf)
#######################################################################
# podzia� na pr�b� ucz�c� i testow�
sample(1:dim(kr)[1],700)->tr

kr[tr,]->ktr
kr[-tr,]->kte

dim(ktr)
dim(kte)

# uczenie drzewa wy��cznie w opraciu o pr�b� ucz�c�
(rpart(kredyt~.,data=ktr)->dr1)
printcp(dr1)
plotcp(dr1)
prp(dr1,type=4,extra=8,main="Wykres drzewa optymalnego dla kredyt�w",
    box.col=c("pink", "palegreen3")[dr1$frame$yval])

predict(dr1,ktr) # prognzowanie dla pr�by ucz�cej
(prtr<-predict(dr1,ktr,type="class"))

(ptabtr<-prop.table(table(ktr[,5],prtr),1))

predict(dr1,kte) # prognzowanie dla pr�by testowej
(prte<-predict(dr1,kte,type="class"))
(ptabte<-prop.table(table(kte[,5],prte),1))

# Przyk�ad wykorzystawnia drzew klasyfikacyjnych w klasyfikacji zbioru danych Iris
library(rpart)
install.packages("tree")
library(tree)
library(psych)
iris
summary(iris)
describe(iris)
plot(iris,col=iris[,5])
plot(iris[,3:4],col=iris[,5],pch=19,main="d�ugo??� p�atka/szeroko??� p�atka")

ir.tr <- tree(Species ~., iris)
ir.tr
ir.tr1 <- snip.tree(ir.tr, nodes = c(12, 7))
summary(ir.tr1)
par(pty = "s")
plot(iris[,3:4],col=iris[,5],pch=19,main="d�ugo�??� p�atka/szerokos??� p�atka")
??
partition.tree(ir.tr1, add = TRUE, cex = 1.5)
