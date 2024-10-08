#Import danych z tabeli ze zmiennnymi o nazwach Y (zale�na) 
#oraz X1, X2 (obja�niaj�ce)  
as.data.frame(read.table("clipboard",sep="\t",header=TRUE,dec=","))->a

#Oszacowanie modelu regresji liniowej 
lm(Y~X1+X2,data=a)->b

#Wyniki estymacji modelu
summary(b)

#Diagnostyka reszt
plot(b)

#Dob�r zmiennych do modelu metod� zst�puj�c� w oparciu 
#o kryterium informacyjne Akaike
step(b)

#W celu doboru zmiennych obja�niaj�cych do modelu 
#mo�na r�wnie� wykorzysta� funkcje pakiet�w: leaps, glmulti lub te� MuMIn