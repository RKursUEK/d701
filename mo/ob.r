# Zmiana jezyka komunikatow w konsoli
Sys.setenv(LANGUAGE = "en")

# Wyswietlanie elementow macierzy i innych obiektow danych w formie arkusza
# danych
pr <- matrix(rnorm(25, 5, 3), ncol = 2)
View(pr)

# Edycja danych macierzy poprzez arkusz
edit(pr)
fix(pr)

#################################################### 
# S3 - tzw. stare klasy(klasa jako atrybut obiektu)

x <- structure(1:5, class = "moja")
x

attr(x, "class")
is(x, "moja")
class(x)

# dziedziczenie w S3
y <- structure(rnorm(5), class = c("inna", "moja"))
y

(z <- structure(runif(5), class = "kolejna"))


# Metody i funkje przeciazone w S3
jaki <- function(x) UseMethod("jaki")

jaki.default <- function(x) print("Obiekt nie nalezy do zadnej stworzonej przez nas klasy")

jaki.moja <- function(x) print("Obiekt nalezy do klasy moja badz dziedziczacej z niej")

jaki.kolejna <- function(x) print("Obiekt nalezy do klasy kolejna")

u <- runif(15)

jaki(u)
jaki(x)
jaki(y)
jaki(z)

# Przyklad zastosowania funkcji dla obiektow dla klas typu S3 Tworzenie
# obiektow nalezacych do klas stworzonych przez uzytkownika - S3
a <- structure(arima.sim(list(ar = 0.7, ma = 0.2), 100), class = "szereg")
b <- structure(matrix(rnorm(100), ncol = 2), class = "rozrzut")
c <- structure(round(runif(5, 1, 25)), class = "kolumna")
d <- function(x) x^2


# Sprawdzanie klasy obiektow
sapply(list(a, b, c, d), class)

# Definiowanie metod
rysuj <- function(x) UseMethod("rysuj")

rysuj.default <- function(x) {
    cat("?adna z moich klas. Domyslny wykres")
    plot(x, main = "Domy?lny wykres R. ?adna z moich klas")
}

rysuj.szereg <- function(x) {
    stopifnot(dim(x)[2] == 1)
    plot(x, type = "l", main = "Wykres szeregu czasowego - klasa szereg", xlab = "czas", 
        ylab = "y(t)")
}

rysuj.rozrzut <- function(x) {
    stopifnot(dim(x)[2] == 2)
    plot(x, main = "Wykres rozrzutu - klasa rozrzut", xlab = "x1", ylab = "x2")
    abline(lm(x[, 1] ~ x[, 2]))
}

rysuj.kolumna <- function(x) {
    barplot(as.numeric(x), main = "Wykres rozrzutu - klasa kolumna", xlab = "nazwy", 
        ylab = "liczebno??", col = "red")
}

# Przyklad dzialania funkcji w zaleznosci od klasy obiektu
rysuj(a)
rysuj(b)
rysuj(c)
rysuj(d)

# Niewlasciwa struktura obiektu kolumn - wynik funkcji rysuj
f <- structure(matrix(rnorm(100), ncol = 5), class = "rozrzut")
rysuj(f)

########################################## 
# Klasy typu S4 - klasy formalne
setClass("formalna", list(X = "numeric", Y = "matrix", Z = "ANY"))

showClass("formalna")

ob <- new("formalna", X = rnorm(20), Y = matrix(rnorm(20), ncol = 4), Z = lm(data.frame(rnorm(100), 
    ncol = 5)))

class(ob)
typeof(ob)
mode(ob)
iss4(ob)
is.object(ob)

# Nazwy elementow obiketu klasy 'formalna'
slotNames(ob)

# Wyciaganie elementow skladowych (slots) poprzez @
ob@X
ob@Y
ob@Z

# setValidity 
