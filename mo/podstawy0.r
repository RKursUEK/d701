methods(class=lm)

methods(summary)

x <- rnorm(100)
y <- 2*x+5+rnorm(100,0,10^-1)
l <- lm(y~x)
attributes(l)
rm("x","y")

# Wektory numeryczne
a <- c(1,7,9,5,10,14)
a
length(a)
class(a)

rev(a)
sort(a)
sort(a,decreasing=TRUE)
order(a)
a[2:5]
a[-(3:4)]
summary(a)
names(a)<-letters[1:length(a)]



# Wektory tekstowe
as.character(a)
b <- c("czerwony","zielony","niebieski")
class(b)
is.character(b)
paste(b[1],b[2],sep="-")

# Wektory czynnikowe
factor
d <- factor(b,ordered="TRUE")
levels(d)
labels(b)
ordered()

# Wektory logiczne
as.logical

# Operatory logiczne &, |, &&, ||, ==, !=, <, <=, >, >=, any, all


# Macierze w R  moga powstawac z polaczenia wektorow tego samego typu 
cbind()
rbind()

A <- matrix(c(1:25),ncol=5)
A[2,3]
A[4:5,]
A[,2:3]


args(apply)
# Wyznacz wartosc funkcji (tutaj sumy) dla kazdego wiersza macierzy
apply(A,1,sum)

# Wyznacz wartosc funkcji (tutaj sumy) dla kazdej kolumny macierzy
apply(A,2,sum)

# Wyznacz wartosc wlasnej funkcji dla kazdej kolumny macierzy
apply(A,2,function (x) mean(x)+5)




#Transpozycja macierzy A
t(A)
# Wyznacznik kwadratowej macierzy A
det(A)
# Dekompozycja spektralna (wartosci i wektory wlasne)
eigen(A)
# Wymiary macierzy
dim(A)
# Wykres rozrzutu dla macierzy A
pairs(~.,data=A,main="Wykres rozrzutu")

# Elementy algebry macierzowej

# Mnozenie odpowiadajacych sobie elementow macierzy A i B o takich samych
# wymiarach - iloczyn Hadamarda
A*B
# AB	: Iloczyn macierzy A oraz B o odpowiednich wymiarach 
A%*%B
# AB'	: Iloczyn wektorowy macierzy A i B, iloczyn macierzy A 
# oraz transpozycji macierzy B 
A %o% B 
# A'B Iloczyn skalarny macierzy A i B (iloczyn transpozycji macierzy A i macierzy B) 
crossprod(A,B)
crossprod(A)

#Tworzy macierz diagonalna
diag(5) # jednostkowa o wymiarze 5x5
diag(x) # diagonalna o elementach przekatniowych z wektora x
# Wyci?ga elementy przekatniowe z macierzy A
diag(A)




# Macierz odwrotna do nieosobliwej macierzy A
solve(A)
# Rozwiazanie rownania liniowego Ax=b
solve(A,b)

### Ramki danych grupuja wektory roznych typow w postaci kolumn
data.frame()
dim
D[,"nazwa"]
D[1:3,5]
order
attach
detach

# List
l <- list(num=a,teskt=b,czynn=d)
l
names(l)
# Otrzymujemy liste z jednym elementem num
l[1]
class(l[1])
# Otrzymujemy wektor num
l[[1]]
class(l[[1]])
# Otrzymujemy piaty element wektora num 
l[[1]][5]
# Wyciagniecie elementu num z wykorzystaniem $
l$num


### Elementarna grafika
plot(a,type="l",main="Pr?bny wykres",xlab="czas [t]",ylab="wartosc [y]")
abline(h=5,lwd=2,col="red",lty=2)
abline(v=2)
abline(-2,3,lwd=3,col="green")

# Histogram
args(hist)
hist(a,col="red")

# Wykres pudelkowy
boxplot()
#Wykres slupkowy
barplot()
# Wykres funkcji
curve(2*x^3+5*x^2+7*x,-100,100,xlab="x",ylab=expression(y==2*x^3+5*x^2+7*x),
	main="Wykres funkcji")
curve(log(1+x), 1, 100)

# Umieszczenie wielokata na wykresie
polygon()


#### petla for
x <- 7
for (i in 1:5) {
	x[i+1]<-x[i]^2+5
	}
x

y <- c()
for (i in runif(10,1,15)) y<-c(y,rnorm(1,0,i))
y

## Grupa funkcji apply pozwalaj?cych zast?pi? p?tle for

# sapply - zwraca wynik w uproszczonej formie wektora (ramki danych) 
args(sapply)
arg=runif(10,1,15)
sapply(arg,function(i) rnorm(1,0,i))
sapply(1:10,function(i) rnorm(1,0,i))

# Funkcja wi?cej ni? jeden argument (drugi y funkcji aryt ustalamy na poziomie 5)
sapply(1:10,aryt,y=5)

# lapply - zwraca te same wynik co sapply, jednak w postaci listy
lapply()

## W mapply wartosc funkcji wyznaczona jest dla zmieniajacych sie kombinacji 
# wartosci wiekszej niz 1 liczby argumentow 
args(mapply)
mapply()


tapply()

## replicate
replicate(100,mean(rnorm(100,5,10)))

## instrukcja warunkowe ifelse
x<-5
ifelse(x>0,x^1/3,abs(x)+3)

# if bez instrukcji dla alternatywnego dzialania
x<--5
if(x>0) {
	x^1/3->x}


### Petla while
  while (signals[i,"close"] == 0) {
    signals[i,"position"] <- 1 }
}


#### Instrukcja switch
x <- 4:5
dz <- "rozn"
dz <- "iloraz"
switch(dz,rozn=x[2]-x[1],iloraz=x[2]/x[1])


## Definiowanie wlasnej funkcji
aryt<-function(x,y) {
	d=x+y
	o=x-y
	m=x*y
	dz=x/y
	list(x=x,y=y,suma=d,r??nica=o,iloczyn=m,iloraz=signif(dz,2))
	}

m <- round(runif(100,0,100))
n <- round(runif(100,0,100))

mapply(aryt,m,n)
	

system.time(mapply(aryt,m,n))

##################################
pie <- function(x) {
	vars::roots(x,modulus=FALSE)->a
	plot(Re(a),Im(a),xlab="Re(z)",ylab="Im(z)")
	require(plotrix)
	draw.circle(0,0,1)}

methods

pot <- function(x) {
	y<-z<-c()
		for (i in 1:length(x)){
			if (x[i]<=5) {
				x[i]^2->y[i]
				"kwadrat"->z[i]
			}
			else
				{x[i]^0.5->y[i]
				"pierwiastek"->z[i]
			}
				}
	
	data.frame(argument=x,wynik=signif(Re(y),2),dzialanie=z)->p
	plot(p[,1:2],col=p[,3],pch=19
)
	p}

################# Wykresy rozk?ad?w

#### jednostajny
wu <- function(){
par(mfrow=c(2,1))
 curve(dunif(x,1,3),1,3,lwd=2,axes=FALSE,ylab="F(x)",xlim=c(0.5,3.5),
	ylim=c(0,0.75),main="Funkcja gestosci rozkladu jednostajnego: U~(a,b)")
lines(c(0,1),c(0,0),lwd=2)
lines(c(3,4),c(0,0),lwd=2)

axis(1, at=c(0.5,1,3,4),labels=c("","a","b",""),pos=c(0,0))
axis(2, at=c(0,1/2,2),labels=c(0,expression(frac(1,b-a)),""),pos=c(0.5,0),
las=2)

 curve(punif(x,1,3),0,3.5,lwd=2,axes=FALSE,ylab="F(x)",xlim=c(0.5,3.5),
	ylim=c(0,1.5),main="Dystrybuanta rozkladu jednostajnego: U~(a,b)")

axis(1, at=c(0.5,1,3,4),labels=c("","a","b",""),pos=c(0,0))
axis(2, at=c(0,1,2),labels=c(0,1,""),pos=c(0.5,0),
las=2) 
}

#### normalny
wn <- function(){
par(mfrow=c(2,1))
 curve(dnorm(x,0,1),-3,3,lwd=2,axes=FALSE,ylab="F(x)",xlim=c(-3.5,3.5),
main="Funkcja gestosci rozkladu normalnego: N~(mu,sigma)")

axis(1, at=c(-4,-1,0,1,4),labels=c("",expression(mu-sigma),expression(mu),
expression(mu+sigma),""),
pos=c(0,0))
axis(2, at=c(0,1/(2*pi*exp(1))^0.5,1/(2*pi)^0.5,2),
labels=c(0,expression(frac(1,sigma*sqrt(2*pi*e))),
expression(frac(1,sigma*sqrt(2*pi))),""),pos=c(-3.5,0),
las=2) 

 curve(pnorm(x,0,1),-3,3,lwd=2,axes=FALSE,ylab="F(x)",xlim=c(-3.5,3.5),
	,main="Dystrybuanta rozkladu normalnego: N~(mu,sigma)")

axis(1, at=c(-4,-1,0,1,4),labels=c("",expression(mu-sigma),expression(mu),
expression(mu+sigma),""),
pos=c(0,0))
axis(2, at=c(0,0.5,1),
labels=c(0,0.5,1),pos=c(-3.5,0),
las=2) 
}

