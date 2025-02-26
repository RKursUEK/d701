# Uproszczona wersja funkcji zwracajaca optymalne warianty decyzji w warunkach
# niepewnosci wedlug okreslonych kryteriow (w przypadku wiecej niz jednej
# optymalnej decyzji zwracana jest ta o najnizszym indeksie)

crit <- function (rm, alpha = 0.7) {
  o <- apply(rm, 1, max)
  s <- apply(rm, 1, min)
  w <- max(s)
  wl <- match(w, s)
  op <- alpha * s + (1 - alpha) * o
  h <- max(op)
  hl <- match(h, op)
  m <- apply(rm, 2, max)
  mv <- matrix(m,
              nrow = dim(rm)[1],
              ncol = dim(rm)[2],
              byrow = TRUE)
  r <- mv - rm
  ro <- apply(r, 1, max)
  sa <- min(ro)
  sal <- match(sa, ro)
  l <- apply(rm, 1, mean)
  lm <- max(l)
  lml <- match(lm, l)
  opt <- rbind(c(wl, hl, sal, lml), c(w, h, sa, lm))
  rownames(opt) <- c("opt.dec", "crit.val")
  colnames(opt) <- c("Wald", "Hurwicz", "Savage", "Laplace")
  t <- table(opt[1, ])
  st <- sort(t, decreasing = TRUE)
  stm <- rbind(as.numeric(names(st)), st)
  colnames(stm) <- NULL
  rownames(stm) <- c("dec", "count")
  barplot(
    stm[2, ],
    names.arg = paste("dec.", stm[1, ], sep = " "),
    main = "Decision count",
    col = "blue"
  )
  list(Hurwicz.alpha = alpha, opt.dec = opt, dec.count = stm)
}

# Przyklad zastosowania (macierz wyplat 4x5 - 4 mozliwe decyzje,
# 5 mozliwych stanow natury)
mt <- matrix(c(13, 11, 8, 15, 5, 17, 11, 6, 19, 13, 13, 8, 14, 16, 17, 13, 18, 4, 20, 18),
           byrow = TRUE, ncol = 5)
crit(mt)
crit(mt, alpha = 0.2)

# Przyklad zastosowania (macierz wyplaty 5x5 o pseudolosowo
# wygenerowanych wartosciach calkowitych z rozkladu jednostajnego (a = 1, b = 20))
mw <- matrix(round(runif(25, 1, 20)), ncol=5)
crit(mw)
crit(mw, 0.4)