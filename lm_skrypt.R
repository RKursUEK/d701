#Import danych z tabeli ze zmiennnymi o nazwach Y (zale¿na) 
#oraz X1, X2 (objaœniaj¹ce)  
as.data.frame(read.table("clipboard",sep="\t",header=TRUE,dec=","))->a

#Oszacowanie modelu regresji liniowej 
lm(Y~X1+X2,data=a)->b

#Wyniki estymacji modelu
summary(b)

#Diagnostyka reszt
plot(b)

#Dobór zmiennych do modelu metod¹ zstêpuj¹c¹ w oparciu 
#o kryterium informacyjne Akaike
step(b)

#W celu doboru zmiennych objaœniaj¹cych do modelu 
#mo¿na równie¿ wykorzystaæ funkcje pakietów: leaps, glmulti lub te¿ MuMIn