library(tseries)
library(zoo)

# Pobieranie notowan akcji na zamkniecie sesji 4 spolek sektora informatycznego
msft <- get.hist.quote(instrument = "msft",
                       start = "2021-01-01",
                       quote = "AdjClose")
ibm <- get.hist.quote(instrument = "ibm",
                      start = "2021-01-01",
                      quote = "AdjClose")
hpq <- get.hist.quote(instrument = "hpq",
                      start = "2021-01-01",
                      quote = "AdjClose")
aapl <- get.hist.quote(instrument = "aapl",
                       start = "2021-01-01",
                       quote = "AdjClose")

# Scalenie szeregow cen akcji na zamkniecie sesji
x <- merge(msft, ibm, hpq, aapl)
# Dzienne logarytmiczne stopy zwrotu
dx <- 100 * diff(log(x))

# Wykres dziennych szeregow cen akcji
plot(x, main = "Wykres szregow cen akcji")
# Wykres dziennych szeregow logarytmicznych stop zwrotu
plot(dx, main = "Wykres szregow logarytmicznych stop zwrotu")

# Podstawowe statystyki opisowe
library(psych)
describe(dx)

library(fPortfolio)
# Okreslenie struktur portfeli Blacka dla okreslonych oczekiwanych stop zwrotu
port <- portfolioFrontier(
  as.timeSeries(dx),
  spec = portfolioSpec(optim = list(solver = "solveRshortExact")),
  constraints = "Short",
  include.mvl = TRUE,
  title = NULL,
  description = NULL
)

slotNames(port)

port@portfolio
summary(port)

# Wykres granicy portfeli efektywnych (wybor opcji 1, 2 i 4)
plot(port)

# Wykres wag akcji w portfelu Blacka nr 10
weightsPie(port, pos = 10)

# Wykres wag akcji w portfelu Blacka nr 40
weightsPie(port, pos = 40)

# Wykres wag dla wszystkich (50) skonstruowanych portfeli Blacka
weightsPlot(port)
