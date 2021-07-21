install.packages("imputeTS")

library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)

tickers <- c("GGBR4.SA", "SUZB3.SA", "KLBN11.SA", "BRAP4.SA", "VALE3.SA","BRKM5.SA")
weights <- c(.20, .20, .20, .10, .20, .10)

#Preços (Pode ser mensal ou semanal)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2015-01-01", periodicity = "daily", auto.assign=FALSE)[,4])

benchmarkPrices <- getSymbols.yahoo("^BVSP", from="2015-01-01", periodicity = "daily", auto.assign=FALSE)[,4]
colSums(is.na(benchmarkPrices))
benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))


#Renomear colunas
colnames(portfolioPrices) <- tickers

#Soma NA por coluna
colSums(is.na(portfolioPrices))

#Plot
plot(portfolioPrices, legend = tickers)


#Calcula Retornos para DF
dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

#Calculo Portfolio Returns
portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)

#Plot Performance
chart.CumReturns(portfolioReturn)
charts.PerformanceSummary(portfolioReturn)

rets_df <- cbind(portfolioReturn, benchmarkReturns)

charts.PerformanceSummary(rets_df, main="Portfolio Mineração & Siderurgia e Papel & Celulose X Ibovespa")

#Calcula Metrics 
CAPM.beta(portfolioReturn, benchmarkReturns, .035/252)
CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/252)
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/252)

CAPM.alpha(portfolioReturn, benchmarkReturns, .035/252)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)

SharpeRatio(portfolioReturn, Rf = .035/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)

table.AnnualizedReturns(portfolioReturn, Rf=.035/252, geometric=TRUE)