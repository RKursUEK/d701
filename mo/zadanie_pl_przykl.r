# Zadanie PL - maksymalizacja utargu

# parametry zadania
# A = 4,9  4,1
#     2,7  4,5
#     6,9  3,1

# b = 5,0
#     4,2
#     5,6

# c = 5 7


# Wyniki z dokladnoscia do pierwszego miejsca po przecinku

# Metoda geometryczna
source("http://wizard.uek.krakow.pl/~d701/mo/pl_geom.r")



A = matrix(c(4.9,2.7,6.9,4.1,4.5,3.1), ncol=2)
b=matrix(c(5,4.2,5.6))
c=c(5,7)
maxgeom(A,b,c)->optg
round(optg$rank,1) 

#ceny dualne - wyniki z dok?adno?ci? dw?ch miejsc po przecinku
c(solve(t(A)[,-3],c),0)->optgd
round(optgd,2)

#Metoda simpleks
library(lpSolve)
A = matrix(c(4.9,2.7,6.9,4.1,4.5,3.1), ncol=2)
b=c(5,4.2,5.6)
c=c(5,7)

lp("max",c,A,rep("<=",3),b,compute.sens=1)->opt
names(opt)
opt$solution->opts # 
round(opts,1)
opt$objval->optsz # 
round(optsz,1)

opt$duals->optsd
round(optsd,2)

