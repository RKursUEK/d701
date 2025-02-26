# Dane
adr<-"https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

d<-read.table(adr,sep="",dec=".",header=FALSE,stringsAsFactors=TRUE)
dim(d)
head(d)
ncol<-dim(d)[2]

# Wybór zmiennej zale¿nej oraz iloœciowych zmiennych objaœniaj¹cych
num <- !sapply(d[,-ncol], is.factor)
dnum<-d[,num]
dim(dnum)
head(dnum)
dnum<-dnum[,c(1,2,5)]
y<-d[,ncol]
z<--y+2
dn<-cbind(factor(z,labels=c("nie","tak")),dnum)
colnames(dn)<-c("splata","okres kredytowania","kwota kredytu","wiek aplikanta")
head(dn)

nrow<-dim(dn)[1]

#Próba ucz¹ca
m<-nrow
p<-0.8
itrain<-sort(sample(1:m, size = round(m*p), replace = FALSE))

train<-dn[itrain,]
test<-dn[-itrain,]
dim(train)
dim(test)

#Wykresy pairs
pairs(train[,-1],col=c("red","green")[as.numeric(train[,1])],
      main="Próba ucz¹ca")
pairs(test[,-1],col=c("red","green")[as.numeric(test[,1])],
      main="Próba testowa")

install.packages("psych")
library(psych)

describeBy(train[,-1],group=train[,1])
describeBy(test[,-1],group=test[,1])

### Metoda k-najbli¿szych s¹siadów (kNN) w podejmowaniu decyzji - podejœcie
### nieparametryczne
install.packages("kknn") #pakiet kknn
library(kknn)

## Kroswalidacja leave-one-out, wybór optymalnych parametrów kNN
(cv<-train.kknn(splata~.,data=train,kmax=10,distance=2,scale=TRUE,
    kernel=c("triangular", "rectangular","epanechnikov", "optimal","inv")))

names(cv)
plot(cv)
summary(cv)

#Parametry kNN z najmniejsz¹ stop¹ b³êdnych klasyfikacji (missclasification rate)
par<-cv$best.parameters
kern<-par[[1]]  # kernel="rectangular" - równa wagi niezale¿nie od odleg³oœci najbli¿szych s¹siadów przy wyznaczaniu prognozy
kopt<-par[[2]]  # k=9 - uwzglêdnianie 9 najbli¿szych s¹siadów przy wyznaczaniu prognozy 

# Trafnoœæ prognoz kNN dla CV (wiersze - obserwowane wartoœci,
# kolumny - prognozy)
prtrain<-cv$fitted.values[[order(cv$MISCLASS)[1]]]
(licz<-table(train[,1],prtrain))
(prop<-prop.table(licz,1))

# Wykres dla próby ucz¹cej wyniki CV leave-one-out:
# plusy - sp³acone kredyty, okrêgi - niesp³acone kredyty, 
# czerwony - b³êdne prognozy, czarny - trafne prognozy
pairs(train[,-1], pch = c(1,3)[as.numeric(train$splata)], 
      col = c("black", "red")[(train$splata!= prtrain)+1], 
      main="Próba ucz¹ca")

# Prognozy dla próby testowej w oparciu o metodê kNN 
# z parametrami minimalizuj¹cymi stopê b³êdnych klasyfikacji
# w procedurze leave-one-out, metryka euklidesowa (d=2),
# normalizacja (scale=TRUE)
(knn<-kknn(splata~.,train,test,k=kopt,distance=2,kernel = kern, scale=TRUE))
names(knn)
summary(knn)

#Prognozy sp³aty dotycz¹ce próby testowej
prtest<-fitted(knn)
# Trafnoœæ prognoz kNN dla próby testowej (wiersze - obserwowane wartoœci,
# kolumny - prognozy)
(liczt<-table(test[,1],prtest))
(propt<-prop.table(liczt,1))

# Wykres trafnoœci prognoz kNN dla próby testowej:
# plusy - sp³acone kredyty, okrêgi - niesp³acone kredyty, 
# czerwony - b³êdne prognozy, czarny - trafne prognozy
pairs(test[,-1], pch = c(1,3)[as.numeric(test$splata)], 
      col = c("black", "red")[(test$splata!= prtest)+1], 
      main="Próba testowa")

# Dla porównania wyniki kNN, z metryk¹ Hamminga - tasówkow¹ (d=1),
# i k=3 najbli¿szych s¹siadów, g³osowanie wa¿one odleg³oœciami,
# waga odwrotnie proporcjonalna do odleg³oœci (kernel="inv"), 
# normalizacja zmiennych (scale=TRUE)

(khi<-kknn(splata~.,train,test,k=3,distance=1,kernel = "inv", scale=TRUE))
# Prognozy powy¿szego wariantu kNN dla próby testowej
summary(khi)
#Trafnoœæ dla próby testowej
phi<-fitted(khi)
(liczhi<-table(test[,1],phi))
(prophi<-prop.table(liczhi,1))

#################################################

# Poszukiwanie k=3 punktów przestrzeni metrycznej 
# o najmniejszej odleg³oœci do danego punktu
plot(test[,2:3],col=c("red","green")[as.numeric(test[,1])],
     main="Przed standaryzacj¹")
pp<-c(52,8000)
points(pp[1],pp[2],pch=16,cex = 2)

#Standaryzacja na (0,1)
t<-test[,2:3]
ts<-scale(t,colMeans(t),apply(t,2,sd))
tse<-data.frame(splata=test[,1],ts)
pps<-scale(matrix(pp,ncol=2),colMeans(t),apply(t,2,sd))
plot(ts,col=c("red","green")[as.numeric(test[,1])],
     main="Po standaryzacji. Przestrzeñ z metryk¹ euklidesow¹")
points(pps[1],pps[2],pch=16,cex = 2)

identify(ts,plot=TRUE)
(.Last.value->nn)
locator(n = 2, type = "l")
(near<-tse[nn,1:3])
(coord<-rbind(as.numeric(pps),near[,-1]))
dist(coord,method = "euclidean", diag = TRUE, upper = TRUE)

##############################

