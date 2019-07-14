library("CVXR")
weights_Markowitz <- function(lambda, Sigma, mu){

  w <- Variable(length(mu))
  res <- solve(Problem(Maximize(t(mu) %*% w - lambda*quad_form(w, Sigma)),
                       constraints = list(w >= 0, sum(w) == 1)))
  w_Markowitz <- as.matrix(res$getValue(w))
  rownames(w_Markowitz) <- stock_namels
  return(w_Markowitz)
}

library(xts)
library(quantmod)

begin_date <- "2015-01-01"
end_date <- "2019-07-13"
stock_namels <- c("AAPL", "IBM", "AMZN", "BAC", "MS", "FDX", "MMM", "KO", "PFE")
prices <- xts()
stock = "IBM"
for (stock in stock_namels){
  prices <- cbind(prices, Ad(getSymbols(stock, from = begin_date, to = end_date, auto.assign = FALSE)))
}

plot(prices/rep(prices[1, ], each = nrow(prices)), legend.loc = "topleft",
     main = "Normalized prices")
logret <- diff(log(prices))
colnames(logret) <- stock_namels
mu <- colMeans(logret, na.rm = TRUE)
Sigma <- cov(na.omit(logret))

#generally, \lambda is between 0 and 4. The larger \lamba the more risk-averse.

par(mfrow = c(3,2))

for (lmd in c(0.5,1,2,4,8,10)){
  barplot(t(weights_Markowitz(lmd, Sigma, mu)), main = "Portfolio allocation", xlab = paste("?? = ", lmd))
}
par(mfrow = c(1,1))
#remark: if \(\lambda\) goes to positive infinity, it will converge to GMVP(Global Minimum Variance Portfolio) situation



## Closed-form solution for most simple case

w_naive <- 1/sqrt(diag(Sigma))
w_naive <- w_naive/sum(w_naive)
w_all <-  cbind(w_Markowitz_lmd1,w_Markowitz_lmd10, w_naive)
colnames(w_all) <- c("Mkwtz_lmd1", "Mkwtz_lmd10","risk-parity" )

barplot(t(w_all), beside = TRUE, legend = colnames(w_all))

## Test for outsample performance (waited to be added)
## Sensitivity to parameters  (particularly to ??).(waited to be added)