source("http://wizard.uek.krakow.pl/~d701/mo/pl_geom1.r")

# Analiza wrazliwosci -- parametry funkcji celu (c1, c2)

### max Z = c1*x1 + c2*x2 = 3x1 + 5x2 
### 5x1 + 3x2 <= 10 
### 2x1 + 4x2 <= 8 
### x1 >= 0, x2 >= 0

(A <- matrix(c(5, 3, 2, 4), byrow = TRUE, ncol = 2))
(b <- matrix(c(10, 8)))
(c <- c(3, 5))

(m <- maxgeom(A, b, c))
warstw(m, 2)

# Niech mZ = -c1/c2 -- parametr kierunkowy warstwic

# Dla mZ < -5/3 (mZ < -a11/a12) -- maksimum warunkowe lezy w punkcie W1,0
# tzn. na przeci?ciu brzeg?w polplaszczyzn: zwiazanej z pierwszym war. funkcyjnym
# oraz zwiazanej z warunkiem brzegowym na nieujemnosc x2, np.
(c10 <- c(10, 5))
(m10 <- maxgeom(A, b, c10))
warstw(m10, 2)

# Dla mz = -5/3 (mz = -a11/a12) -- niesko?czenie wiele punktow bedacych maksimami
# warunkowymi: punkty wierzcho?kowe W1,0 i W1,2 oraz ich dowolne kombinacje wypuk?e, np.
(c1012 <- c(5, 3))
(m1012 <- maxgeom(A, b, c1012))
warstw(m1012, 2)

# Dla -5/3 < mZ < -2/4 (-a11/a12 < mZ < -a21/a22) -- maksimum warunkowe lezy w punkcie W1,2
# tzn. na przeci?ciu brzeg?w polplaszczyzn: zwiazanej z pierwszym war. funkcyjnym
# oraz zwiazanej z drugim warunkiem funkcyjnym, np.
(c12 = c(3, 4))
(m12 <- maxgeom(A, b, c12))
warstw(m12, 2)

# Dla mZ = -2/4 (mZ = -a21/a22) -- niesko?czenie wiele punktow bedacych maksimami
# warunkowymi: punkty wierzcholkowe W1,2 i W0,2 oraz ich dowolne kombinacje wypuk?e, np.
(c1202 = c(2, 4))
(m1202 <- maxgeom(A, b, c1202))
warstw(m1202, 2)

# Dla mZ > -2/4 (mz > -a21/a22) -- maksimum warunkowe lezy w punkcie W0,2, 
# tzn. na przeci?ciu brzeg?w polplaszczyzn: zwiazanej z warunkiem brzegowym na nieujemnosc x1
# oraz zwiazanej z drugim war. funkcyjnym, np.
(c02 = c(1, 10))
(m02 <- maxgeom(A, b, c02))
warstw(m02, 2)

########################################################################## 

# Analiza wrazliwosci -- parametr b2

### Przyklad zadania maksymalizacji 
### max Z = 3*x1 + 4*x2 p.w. 
### 4*x1 + 2*x2 <= 7 
### 2*x1 + 4*x2 <= 6 
### x1 + 4*x2 <= 5
### x1 >= 0, x2 >= 0

(A <- matrix(c(4, 2, 1, 2, 4, 4), ncol = 2))
(b <- matrix(c(7, 6, 5)))
(c <- 3:4)
(m <- maxgeom(A, b, c))

(mp1 <- m$sympl["1,0", ])
(A2 <- A[2, ])
(b2d <- A2 %*% mp1)
bd <- b
bd[2] <- b2d

(md <- maxgeom(A, bd, c))

(mp2 <- m$pkt["1,3", ])
(b2g <- A2 %*% mp2)
bg <- b
bg[2] <- b2g

(m2 <- maxgeom(A, bg, c))

# b_2 \in [3,50; 6,29] -- zakres wartosci b2, w przypadku, kt?rych
# maksimum warunkowe lezy na przecieciu brzegow polplaszczyzn dla warunkow funkcyjnych 1 oraz 2

# Przykladowo przy b2 = 7 i pozostalych parametrach niezmienionych,
# maksimum warunkowe ulega zmianie (W1,3 nowym punktem optymalnym)
bo <- b
bo[2] <- 7
(mo <- maxgeom(A, bo, c))
