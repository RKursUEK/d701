# Dane symulowane y - zmienna zalezna,
# x1, x2, x3 - zmienne objasniajace (sztuczny przypadek braku skorelowania,
# zmienne o wartosciach oczekiwanych 0 i odchyleniu standardowym 1)

bsym <- round(runif(4, 1, 10), 1)
obj <- sum(bsym[-1] ^ 2)
# Wspolczynnik determinacji R2
R2 <- 0.9
sigeps <- ((1 - R2) / R2 * obj) ^ 0.5
sta <- matrix(rnorm(160), ncol = 4)
xsym <- sta[, -4]
ysym <- bsym[1] + xsym %*% bsym[-1] + sigeps * sta[, 4]

# Oszacowanie modelu (dla symulowanych danych) z wykorzystaniem
# kalsycznej metody najmniejszych kwadratow (minimalizacja kryterium
# najmniejszych kwadratow)
X <- cbind(1, xsym)
y <- ysym

# Estymator KMNK
# b = (X'X)^(-1) * X'y

b <- solve(t(X) %*% X) %*% t(X) %*% y

# Wariancja skladnika resztowego
# se2 = 1/(n-(k+1)) * (y'y-y'X(X'X)^(-1)X'y) = 1/(n-(k+1)) * (y'y-b'(X'X)b)

se2 <- 1 / (dim(X)[1] - dim(X)[2]) * (t(y) %*% y - t(y) %*% X %*% solve(t(X) %*%
                                                                     X) %*% t(X) %*% y)

# Wektor bledow standardowych estymatorow b, ktorego elementami sa
# pierwiastki z element?w macierzy kowariancji estymatorow:
# ^Var(b) = se2*(X'X)^(-1)
# Bledy standardowe estymatorow b:
# sb = diag(se2*Vb)^0.5 = (se2*diag(Vb))^0.5

sb <- (se2 * diag(solve(t(X) %*% X))) ^ 0.5

# test t istotnosci parametrow beta
t <- b / sb

# p-value dla testu t
nu <- (dim(X)[1] - dim(X)[2])  # liczba stopni swobody dla statystyki t-Studenta
pval <- 2 * pt(-abs(t), nu)

res <- cbind(b, sb, t, pval)
colnames(res) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
rownames(res) <- paste("b", 0:3, sep ="")

###################
# Alternatywny zapis dzialan w ramach opt. kryt. NK
# X'X
xtx <- crossprod(X)
# X'y
xty <- crossprod(X, y)
# (X'X)^(-1)
xtxodw <- solve(xtx)

# b =(X'x)^(-1)X'y
b <- xtxodw %*% xty

# se2 = 1/(n-(k+1)) * (y'y-b'(X'X)b)
yty <- crossprod(y)
se2 <- 1 / (dim(X)[1] - dim(X)[2]) * (yty - t(b) %*% xtx %*% b)

# sb = diag(se2*Vb)^0.5 = (se2*diag(Vb))^0.5 = (se2*diag((X'X)^(-1)))^0.5

sb <- (se2 * diag(xtxodw)) ^ 0.5

# test t istotnosci parametrow beta
t <- b / sb

# p-value dla testu t
nu <- (dim(X)[1] - dim(X)[2])  # liczba stopni swobody dla statystyki t-Studenta
pval <- 2 * pt(-abs(t), nu)

res <- cbind(b, sb, t, pval)
colnames(res) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
rownames(res) <- paste("b", 0:3, sep ="")

################
# Wyniki wbudowanej funkcji lm dla porownania wynikow
l <- lm(y ~ X[, -1])
summary(l)
par(mfrow = c(2, 2))
plot(l)
