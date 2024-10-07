# Przyklad losowego wyboru odpowiedzi na pytania
losodp <- function(pyt=15,w=4) letters[ceiling(runif(pyt,0,w))]
losodp()

# W przypadku przyjecia za podstawe odpowiedzi na pytania powyzszego schematu 
# losowych odpowiedzi, prawdopodobie?stwo zaliczenia testu mozna wyznaczyc
# w oparciu o rozklad dwumianowy

# Przyjmujac, ze test jest zaliczony, gdy prawidlowe odpowiedzi na >50% pytan
# Przy przyjetym schemacie odpowiedzi prawdopodobienstwo zaliczenia testu
# dane jest nastepujaco:
# P(X > \floor{\frac{n}{2}})=\sum_{i=\lfloor \frac{n}{2} \rfloor +1}^n \binom{n}{i}  p^i (1-p)^{n-i}
# gdzie X - zmienna losowa o rozkladzie dwumianowym , opisujaca liczbe prawidlowycvh odpowiedzi
#\floor{\frac{n}{2}}+1 - minimalna liczba poprawnych odpowiedzi gwarantujacych zaliczenie
# testu
# p=\frac{1}{w} - prawdopodobienstwo prawidlowej odpowiedzi na pojedencze pytanie 
# testu, rowne odwrotnosci liczby wariantow odpowiedzi w

pr <- matrix(mapply(function(n,p) pbinom(floor(n/2),n,p,lower.tail=FALSE),
       rep(1:40,4),rep(1/(2:5),each=40)),ncol=4)
colnames(pr) <- c(paste(2:4,"warianty",sep=" "),"5 wariant?w")

# Zapis procentowy prawdopodobienstwa zaliczenia testu z zastosowaniem losowego
# schematu odpowiedzi

ppr <- round(100*pr,2)

# Wykres
matplot(1:40,100*pr,type="l",xlab="liczba pyta?",main="Liczba pytan vs. liczba wariantow odpowiedzi 
        a prawdopodobie?stwo zdania testu"
        ,ylab="prawdopdobie?stwo zdania [%]")

legend("right",c("dwa warianty (T, F)","trzy warianty","cztery warianty",
                 "pi?? wariant?w"),col=1:4,lty=1:4,cex=0.5)
abline(h=5,lty=2)
abline(h=1,lty=2)

########### zaliczenie od >=50% prawidlowych odpowiedzi
# P(X \geq \ceil{\frac{n}{2}})=\sum_{i=\lceil \frac{n}{2} \rceil}^n \binom{n}{i} p^i (1-p)^{n-i}

prgeq <- matrix(mapply(function(n, p) 1-pbinom(ceiling(n/2)-1, n, p),
                  rep(1:40,4), rep(1/(2:5),each=40)), ncol=4)
colnames(prgeq) <- c(paste(2:4, "warianty", sep=" "), "5 wariantow")

pprgeq <- round(100*prgeq, 2)

matplot(1:40,100*prgeq, type="l", xlab="liczba pytan", main="Liczba pytan vs. liczba wariantow odpowiedzi 
        a prawdopodobienstwo zdania testu"
        , ylab="prawdopdobienstwo zdania [%]")

legend("right",c("dwa warianty (T, F)", "trzy warianty", "cztery warianty",
                 "piec wariantow"), col=1:4, lty=1:4, cex=0.5)
abline(h=5,lty=2)
abline(h=1,lty=2)


ilepytan <- function(pyt=20,geq=FALSE) switch(geq,)
