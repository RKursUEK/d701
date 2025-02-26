### Problem 1

source("http://wizard.uek.krakow.pl/~d701/swd/opt.r")

m <- matrix(c(90, 70, 80, 65, 10, 80, 60, 70, 45, 20, 40, 50, 20, 40, 30,
            20, 10, 40, 70, 50), ncol = 4)
print(m)
crit(m, alpha = 0.4)


### Problem 2

source("http://wizard.uek.krakow.pl/~d701/swd/std1.r")

x <- c(14, 15.5, 20.25, 10.2, 19.75, 9)
sdt(d = x, py = c(0.2,0.5,0.3), r = "dnorm", mean = c(12,17,20), sd = c(2,3,2.5))

plot(x = NULL, xlab = "x", ylab = "f_XY", xlim = c(7, 25), ylim = c(0, 0.07))
curve(0.2 * dnorm(x, 12, 2), from = 7, to = 25, col = "blue", xlab = NULL, ylab = NULL, add = TRUE)
par(new = TRUE)
curve(0.5 * dnorm(x, 17, 3), from = 7, to = 25, col = "red", xlab = NULL, ylab = NULL, add = TRUE)
par(new = TRUE)
curve(0.3 * dnorm(x, 20, 2.5), from = 7, to = 25, col = "green", xlab = NULL, ylab = NULL, add = TRUE)
abline(v = x, lty = 2)
title("Laczna gestosc")
