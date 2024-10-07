#Import danych z pliku kredyt.xls
kop<-function(n=TRUE) read.table("clipboard",sep="\t",header=n,dec=",")
kr<-kop()
names(kr)
dim(kr)
View(kr)
summary(kr)

library(psych) # ³adowanie pakitu
describe.by(kr[,-5],kr[,5]) #Statystylki opisowe atrybutów w podziale 
# na dobrych i z³ych kredytobiorców

plot(kr[,-5],col=kr[,5]) # Wykresy rozrzutu czarny - dobre kredyty, czerwone - z³e
levels(kr[,5])

install.packages("rpart")
library(rpart) # £adujemy pakiet z algorytmem rekurencyjnego podzia³u 
# CART pozwalaj¹cym budowaæ drzewa klasyfikacyjne
install.packages("rpart.plot")
library(rpart.plot) #£adujemy pakiet pozwalaj¹cy tworzyæ wykresy drzew
#Budowa drzewa maksymalnego z wykorzystaniem kryterium Giniego
(dr<-rpart(kredyt~.,method="class",data=kr))
# Wypisanie elementów zwracanych przez funkcjê rpart
names(dr)

# Tabela i wykres dla procedury cost-complexity prunning
printcp(dr)
plotcp(dr)
# Przycinanie do poddrzewa optymalnego przy wspó³czynniku 
# z³o¿onoœci alfa=0,05 (cp=0.025)
(pdr<-prune(dr,cp=0.025))
#Podsumowanie zbudowanego drzewa maksymalnego
summary(dr)
#Podsumowanie zbudowanego o mininalnym cost-complexity
summary(dr,cp=0.025)

# Œcie¿ki prowadz¹ce do liœci (wêz³ów koñcowych)
(nrl<-as.numeric(rownames(pdr$frame[pdr$frame[,1]=="<leaf>",]))) #Numery liœci
length(nrl) #Liczba liœci
path.rpart(pdr, node =nrl)
(yl<-pdr$frame[pdr$frame[,1]=="<leaf>","yval"]) #Kategorie przewidywane dlaw oparciu o liœcie

#Wykresy drzewa maksymalnego
#Wykres drzewa - dla wêz³ów odsetek obserwacji w poszczególnych klasach (extra=4)
prp(dr,type=4,extra=4,main="Wykres drzewa maksymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[dr$frame$yval])
#Wykres drzewa - dla wêz³ów odsetek obserwacji dotycz¹cy klasy przypisanej wêz³owi (extra=8)
prp(dr,type=4,extra=8,main="Wykres drzewa maksymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[dr$frame$yval])

#Wykresy drzewa optymalnego - o minimalnym cost-complexity
#Wykres drzewa - dla wêz³ów odsetek obserwacji w poszczególnych klasach (extra=4)
prp(pdr,type=4,extra=4,main="Wykres drzewa optymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])
#Wykres drzewa - dla wêz³ów odsetek obserwacji dotycz¹cy klasy przypisanej wêz³owi (extra=8)
prp(pdr,type=4,extra=8,main="Wykres drzewa optymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[pdr$frame$yval])

#Przewidywania drzewa optymalnego dotycz¹ce kategorii kredytu
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
# podzia³ na próbê ucz¹c¹ i testow¹
sample(1:dim(kr)[1],700)->tr

kr[tr,]->ktr
kr[-tr,]->kte

dim(ktr)
dim(kte)

# uczenie drzewa wy³¹cznie w opraciu o próbê ucz¹c¹
(rpart(kredyt~.,data=ktr)->dr1)
printcp(dr1)
plotcp(dr1)
prp(dr1,type=4,extra=8,main="Wykres drzewa optymalnego dla kredytów",
    box.col=c("pink", "palegreen3")[dr1$frame$yval])

predict(dr1,ktr) # prognzowanie dla próby ucz¹cej
(prtr<-predict(dr1,ktr,type="class"))

(ptabtr<-prop.table(table(ktr[,5],prtr),1))

predict(dr1,kte) # prognzowanie dla próby testowej
(prte<-predict(dr1,kte,type="class"))
(ptabte<-prop.table(table(kte[,5],prte),1))

# Przyk³ad wykorzystawnia drzew klasyfikacyjnych w klasyfikacji zbioru danych Iris
library(rpart)
install.packages("tree")
library(tree)
library(psych)
iris
summary(iris)
describe(iris)
plot(iris,col=iris[,5])
plot(iris[,3:4],col=iris[,5],pch=19,main="d³ugo??æ p³atka/szeroko??æ p³atka")

ir.tr <- tree(Species ~., iris)
ir.tr
ir.tr1 <- snip.tree(ir.tr, nodes = c(12, 7))
summary(ir.tr1)
par(pty = "s")
plot(iris[,3:4],col=iris[,5],pch=19,main="d³ugoœ??æ p³atka/szerokos??æ p³atka")
??
partition.tree(ir.tr1, add = TRUE, cex = 1.5)
