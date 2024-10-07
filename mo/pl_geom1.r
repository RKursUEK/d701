############ Maksymalizacja - zadanie PL - metoda geometryczna

maxgeom <- function(A, b, c) {
  d <- dim(A)[1]
  m <- b / A[, 2]
  sta <- m
  f <- cbind(0, m)
  rownames(f) <- paste(0, 1:d, sep = ",")
  
  n <- b / A[, 1]
  kier <- -m / n
  e <- cbind(n, 0)
  rownames(e) <- paste(1:d, 0, sep = ",")
  
  a <- r <- NULL
  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      g <- solve(A[c(i, j), ], b[c(i, j), ])
      a <- rbind(a, g)
      r <- rbind(r, paste(i, j, sep = ","))
    }
    rownames(a) <- r
  }
  
  pkt <- rbind(f, e, a)
  lpkt <- dim(pkt)[1]
  
  mi <- sapply(1:lpkt, function (i)
    min(sta + kier * pkt[i, 1]))
  
  przec <- pkt[pkt[, 1] >= 0 & pkt[, 2] >= 0 &
                 abs(pkt[, 2] - mi) < .Machine$double.eps ^ 0.5, ]
  lprzec <- dim(przec)[1]
  
  sympl <- przec[order(przec[, 1], przec[, 2]), ]
  if (sympl[1, 1] == 0) {
    sympl = rbind(rep(0, 2), sympl)
    rownames(sympl)[1] = "0,0"
  }
  colnames(sympl) <- colnames(pkt) <- c("x1", "x2")
  
  z <- przec %*% c
  rank <- cbind(przec, z)[order(z, decreasing = TRUE), ]
  colnames(rank) <- c("x1", "x2", "Z")
  
  plot(
    NULL,
    xlim = c(0, 1.1 * max(pkt[, 1])),
    ylim = c(0, 1.1 * max(pkt[, 2])),
    xlab = "x1",
    ylab = "x2",
    main = "Zagadnienie maksymalizacji"
  )
  sapply(1:d, function(i)
    abline(sta[i], kier[i]))
  
  polygon(sympl, col = "lightgrey")
  points(rank[, 1:2], bg = c("black", "red")[1 + (rank[, 3] == rank[1, 3])], pch =
           21)
  sapply(1:dim(rank)[1], function(i)
    paste(paste(
      c("W:", "x1=", "x2=", "Z="), c(rownames(rank)[i], signif(rank[i, ], 3)), sep =
        ""
    ), collapse = ", ")) -> op
  text(rank, op, pos = 4, cex = 0.7)
  text(
    x = rep(0, d),
    y = sta,
    paste("(", 1:d, ")", sep = ""),
    pos = 2,
    cex = 0.8,
    offset = 0.25
  )
  list(
    A = A,
    b = b,
    c = c,
    sta = sta,
    kier = kier,
    pkt = pkt,
    sympl = sympl,
    rank = rank
  )
}

### Wykreslanie warstwic maksymalizowanej funkcji celu w przestrzeni (x1, x2)

### Warstwice dla wybranych wartosci Z

warstw <- function(m, n = 5) {
  k <- m$rank[1, 3]
  c <- m$c
  z <- seq(0, k, length.out = n)
  sta <- z / c[2]
  kier <- -c[1] / c[2]
  xz <- z / c[1]
  mm <- 1.1 * max(m$pkt[, 1])
  for (i in 1:n) {
    abline(sta[i],
           kier,
           lty = 2,
           col = "darkgreen",
           lwd = 1.5)
    ifelse(xz[i] < mm, 0.85 * xz[i], 0.85 * mm) -> t
    text(
      x = t,
      y = sta[i] + kier * t,
      paste("Z=", signif(z[i], 3), sep = ""),
      pos = 4,
      cex = 0.8,
      col = "darkgreen"
    )
  }
  list(c = c,
       Z = z,
       pk = cbind(sta = sta, kier = kier))
}

### Warstwice Z przechodzace przez dopuszczalne punkty wierzcholkowe

warstwsim <- function(m) {
  z <- m$rank[, 3]
  c <- m$c
  sta <- z / c[2]
  kier <- -c[1] / c[2]
  xz <- z / c[1]
  mm <- 1.1 * max(m$pkt[, 1])
  for (i in 1:length(z)) {
    abline(sta[i],
           kier,
           lty = 2,
           col = "darkred",
           lwd = 1.5)
    ifelse(xz[i] < mm, 0.85 * xz[i], 0.85 * mm) -> t
    text(
      x = t,
      y = sta[i] + kier * t,
      paste("Z=", signif(z[i], 3), sep = ""),
      pos = 4,
      cex = 0.8,
      col = "darkred"
    )
  }
  list(c = c,
       Z = z,
       pk = cbind(sta = sta, kier = kier))
}

#### Trojwymiarowy wykres funkcji celu wraz z oznaczonym
#### zbiorem rozwiazan dopuszczalnych
troj <- function(m)
{
  c <- m$c
  x <- c(0, 1.1 * max(m$pkt[, 1]))
  y <- c(0, 1.1 * max(m$pkt[, 2]))
  z <- function(x = x, y = y)
    c[1] * x + c[2] * y
  my <- outer(x, y, z)
  
  res <- persp(
    z = my,
    col = "grey",
    phi = 0,
    theta = -30,
    ticktype = "detailed",
    xlab = "x1",
    ylab = "x2",
    zlab = "Z",
    main = "Wykres funkcji celu z oznaczonym ZRD"
  )
  
  sympl <- rbind(m$sympl, m$sympl[1, ])
  
  s1 <- sympl[, 1] / (1.1 * max(m$pkt[, 1]))
  s2 <- sympl[, 2] / (1.1 * max(m$pkt[, 2]))
  s3 <- z(sympl[, 1], sympl[, 2])
  
  lines(trans3d(
    x = s1,
    y = s2,
    z = s3,
    pmat = res
  ),
  col = "red",
  lwd = 2)
}


### Przyklad zadania maksymalizacji
#max Z=3*x1 + 4*x2
#p.w.
#4*x1 + 2*x2 <= 7
#2*x1 + 4*x2 <= 6
#  x1 + 4*x2 <= 5

A = matrix(c(4, 2, 1, 2, 4, 4), ncol = 2)
b = matrix(c(7, 6, 5))
c = 3:4

#### Losowe genorowanie parametrow zadania maksymalizacji PL

los <- function(par = c(2, 5), d = 2) {
  przec <- matrix(rep(0, 2))
  while (any(przec <= 0)) {
    A <- matrix(runif(4, par[1], par[2]), ncol = 2)
    par <- runif(1, 1.5, 3) * par
    b <- matrix(runif(2, par[1], par[2]))
    bb <- matrix(runif(1, par[1], par[2]))
    przec <- solve(A, b)
  }
  yn1 <- runif(1, przec[2, ], min(b / A[, 2]))
  xn1 <- runif(1, 0.1 * przec[1, ], przec[1, ])
  z1 <- c(xn1, yn1)
  z2 <- c(przec[1, ], runif(1, przec[2, ], yn1))
  AA <- solve(rbind(z1, z2), matrix(rep(bb, 2)))
  A <- round(rbind(A, t(AA)), d)
  b <- round(rbind(b, bb), d)
  list(A = A, b = b)
}

(lo <- los(d = 2))

(m <- maxgeom(lo$A, lo$b, 1:2))

#### Minimalizacja PL - metoda geometryczna - proba

mingeom <- function(A, b, c) {
  d <- dim(A)[1]
  m <- b / A[, 2]
  sta <- m
  f <- cbind(0, m)
  rownames(f) <- paste(0, 1:d, sep = ",")
  
  n <- b / A[, 1]
  kier <- -m / n
  e <- cbind(n, 0)
  rownames(e) <- paste(1:d, 0, sep = ",")
  
  a <- r <- NULL
  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      g <- solve(A[c(i, j), ], b[c(i, j), ])
      a <- rbind(a, g)
      r <- rbind(r, paste(i, j, sep = ","))
    }
    rownames(a) <- r
  }
  
  pkt <- rbind(f, e, a)
  lpkt <- dim(pkt)[1] -> lpkt
  
  mi <- sapply(1:lpkt, function (i)
    max(sta + kier * pkt[i, 1]))
  
  przec <- pkt[pkt[, 1] >= 0 & pkt[, 2] >= 0 &
                 abs(pkt[, 2] - mi) < .Machine$double.eps ^ 0.5, ]
  lprzec <- dim(przec)[1]
  
  sympl <- przec[order(przec[, 1], przec[, 2]), ]
  sympl <- rbind(c(0, 1.5 * max(pkt[, 2])),
                 sympl,
                 c(1.5 * max(pkt[, 1]), 0),
                 c(1.5 * max(pkt[, 1]), 1.5 * max(pkt[, 2])),
                 c(0, 1.5 * max(pkt[, 2])))
  
  
  #if (sympl[1,1]==0) {
  #	sympl=rbind(rep(0,2),sympl)
  #	rownames(sympl)[1]="0,0"}
  colnames(sympl) <- colnames(pkt) <- c("x1", "x2")
  
  z <- przec %*% c
  rank <- cbind(przec, z)[order(z), ]
  colnames(rank) <- c("x1", "x2", "K")
  
  plot(
    NULL,
    xlim = c(0, 1.1 * max(pkt[, 1])),
    ylim = c(0, 1.1 * max(pkt[, 2])),
    xlab = "x1",
    ylab = "x2",
    main = "Zagadnienie minimalizacji"
  )
  sapply(1:d, function(i)
    abline(sta[i], kier[i]))
  
  polygon(sympl, col = "lightgrey")
  points(rank[, 1:2], bg = c("black", "red")[1 + (rank[, 3] == rank[1, 3])], pch =
           21)
  sapply(1:dim(rank)[1], function(i)
    paste(paste(
      c("W:", "x1=", "x2=", "K="), c(rownames(rank)[i], signif(rank[i, ], 3)), sep =
        ""
    ), collapse = ", ")) -> op
  text(rank, op, pos = 4, cex = 0.7)
  text(
    x = rep(0, d),
    y = sta,
    paste("(", 1:d, ")", sep = ""),
    pos = 2,
    cex = 0.8,
    offset = 0.25
  )
  list(
    A = A,
    b = b,
    c = c,
    sta = sta,
    kier = kier,
    pkt = pkt,
    sympl = sympl,
    rank = rank
  )
}

###########

warstwsimK <- function(m) {
  z <- m$rank[, 3]
  c <- m$c
  sta <- z / c[2]
  kier <- -c[1] / c[2]
  xz <- z / c[1]
  mm <- 1.1 * max(m$pkt[, 1])
  for (i in 1:length(z)) {
    abline(sta[i],
           kier,
           lty = 2,
           col = "darkred",
           lwd = 1.5)
    ifelse(xz[i] < mm, 0.85 * xz[i], 0.85 * mm) -> t
    text(
      x = t,
      y = sta[i] + kier * t,
      paste("K=", signif(z[i], 3), sep = ""),
      pos = 4,
      cex = 0.8,
      col = "darkred"
    )
  }
  list(c = c,
       Z = z,
       pk = cbind(sta = sta, kier = kier))
}


###### Minimalizacja PL - przykladowe parametry
F <- matrix(c(4, 2, 1, 2, 4, 4), ncol = 2)
g <- matrix(c(7, 8, 5))
h <- 2:3
