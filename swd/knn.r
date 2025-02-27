# Dane
adr <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

d <- read.table(adr, sep="", dec=".", header=FALSE, stringsAsFactors=TRUE)
dim(d)
head(d)
ncol <- dim(d)[2]

# Wybór zmiennej zależnej oraz ilościowych zmiennych objaśniających
num <- !sapply(d[,-ncol], is.factor)
dnum <- d[,num]
dim(dnum)
head(dnum)
dnum<-dnum[,c(1,2,5)]
y <- d[,ncol]
z <- -y + 2
dn <- cbind(factor(z, labels = c("nie", "tak")), dnum)
colnames(dn) <- c("splata", "okres kredytowania", "kwota kredytu", "wiek aplikanta")
head(dn)

nrow <- dim(dn)[1]

# Próba ucząca
m <- nrow
p <- 0.8
itrain <- sort(sample(1:m, size = round(m*p), replace = FALSE))

train <- dn[itrain,]
test <- dn[-itrain,]
dim(train)
dim(test)

# Wykresy pairs
pairs(train[,-1],col=c("red","green")[as.numeric(train[,1])], main = "Próba ucząca")
pairs(test[,-1],col=c("red","green")[as.numeric(test[,1])], main = "Próba testowa")

install.packages("psych")
library(psych)

describeBy(train[,-1], group = train[,1])
describeBy(test[,-1], group = test[,1])

### Metoda k-najbliższych sąsiadów (kNN) w podejmowaniu decyzji - podejście nieparametryczne
install.packages("kknn") #pakiet kknn
library(kknn)

## Kroswalidacja leave-one-out, wybór optymalnych parametrów kNN
(cv <- train.kknn(splata ~ ., data = train, kmax = 10, distance = 2, scale = TRUE,
    kernel = c("triangular", "rectangular", "epanechnikov", "optimal", "inv")))

names(cv)
plot(cv)
summary(cv)

# Parametry kNN z najmniejszą stopą błędnych klasyfikacji (missclasification rate)
par <- cv$best.parameters
kern <- par[[1]]  # kernel = "rectangular" - równa wagi niezależnie od odległości najbliższych sąsiadów przy wyznaczaniu prognozy
kopt <- par[[2]]  # k = 9 - uwzględnianie 9 najbliższych sąsiadów przy wyznaczaniu prognozy 

# Trafność prognoz kNN dla CV (wiersze - obserwowane wartości, kolumny - prognozy)
prtrain <- cv$fitted.values[[order(cv$MISCLASS)[1]]]
(licz <- table(train[,1],prtrain))
(prop <- prop.table(licz,1))

# Wykres dla próby uczącej wyniki CV leave-one-out: 
# plusy - spłacone kredyty, okręgi - niespłacone kredyty, 
# czerwony - błędne prognozy, czarny - trafne prognozy
pairs(train[,-1], pch = c(1,3)[as.numeric(train$splata)], 
      col = c("black", "red")[(train$splata!= prtrain) + 1], 
      main = "Próba ucząca")

# Prognozy dla próby testowej w oparciu o metodę kNN 
# z parametrami minimalizującymi stopę błędnych klasyfikacji
# w procedurze leave-one-out, metryka euklidesowa (d=2),
# normalizacja (scale=TRUE)
(knn <- kknn(splata ~ ., train, test, k = kopt, distance = 2, kernel = kern, scale = TRUE))
names(knn)
summary(knn)

#Prognozy spłaty dotyczące próby testowej
prtest<-fitted(knn)
# Trafność prognoz kNN dla próby testowej (wiersze - obserwowane wartości,
# kolumny - prognozy)
(liczt <- table(test[,1],prtest))
(propt <- prop.table(liczt,1))

# Wykres trafności prognoz kNN dla próby testowej:
# plusy - spłacone kredyty, okręgi - niespłacone kredyty, 
# czerwony - błędne prognozy, czarny - trafne prognozy
pairs(test[,-1], pch = c(1,3)[as.numeric(test$splata)], 
      col = c("black", "red")[(test$splata!= prtest) + 1], 
      main = "Próba testowa")

# Dla porównania wyniki kNN, z metryką Hamminga - tasówkową (d = 1),
# i k=3 najbliższych sąsiadów, głosowanie ważone odległościami,
# waga odwrotnie proporcjonalna do odległości (kernel = "inv"), 
# normalizacja zmiennych (scale=TRUE)

(khi <- kknn(splata ~ ., train, test, k = 3, distance = 1, kernel = "inv", scale = TRUE))
# Prognozy powyższego wariantu kNN dla próby testowej
summary(khi)
# Trafność dla próby testowej
phi <- fitted(khi)
(liczhi <- table(test[,1], phi))
(prophi <- prop.table(liczhi, 1))

#################################################

# Poszukiwanie k = 3 punktów przestrzeni metrycznej 
# o najmniejszej odległości do danego punktu
plot(test[, 2:3], col = c("red", "green")[as.numeric(test[, 1])], main = "Przed standaryzacją")
pp <- c(52,8000)
points(pp[1], pp[2], pch = 16, cex = 2)

# Standaryzacja na (0,1)
t <- test[, 2:3]
ts <- scale(t, colMeans(t), apply(t, 2, sd))
tse <- data.frame(splata = test[,1], ts)
pps <- scale(matrix(pp, ncol = 2), colMeans(t), apply(t, 2, sd))
plot(ts, col = c("red", "green")[as.numeric(test[, 1])], main = "Po standaryzacji. Przestrzeń z metryką euklidesową")
points(pps[1], pps[2], pch = 16, cex = 2)

identify(ts, plot=TRUE)
(nn <- .Last.value)
locator(n = 2, type = "l")
(near <- tse[nn,1:3])
(coord <- rbind(as.numeric(pps), near[, -1]))
dist(coord, method = "euclidean", diag = TRUE, upper = TRUE)

##############################

