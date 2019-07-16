#user-defined function
DFtest <- function(res){
  diff.res <- na.omit(diff(res))
  lm <- lm(diff.res~res[-length(res)])
  std.error <- summary(lm)[["coefficients"]][2,2]
  df <- lm$coefficients[2]/std.error
  
  if (df< -3.43){
    return(1)
  }
  else{
    return(0)
  }
}


#Data input
library(xts)
da <- read.csv("Original data set.csv",sep = "")
data <- xts(da[-1], order.by = as.Date(da$Date))
plot(data[,1], legend.loc = "topleft", main = "Stock log price")
n <-  ncol(data)


#AF and ADF
'''
Our choice of pair is CHEVRON and EXXON.MOBILE. The two stocks have relatively high
correlation and both come from financial sector. Companies in this sector usually have more
homogenous operations and earnings, thus we may capture trading opportunities. Also,
according to APT, to stay sector neutral means that they are more likely to be exposed to similar
risk factors.
We prefer log price due to its nice mathematical property such as additivity when calculating the
return. Additionally, our group program a code to aid in stock selection. The selection rule is
simple: the main principal is that the two stock should pass the cointegration test. Secondly, the
smaller the half time of mean reverting
1 , the better the choice. Finally, betas of CHEVRON and
for EXXON.MOBILE are similar (1 and 1.32, respectively), which shows it would be easy for us to
stay market neutral.
'''
asst1 <- data$CHEVRON
asst2 <- data$EXXON.MOBIL

library(urca)
af_CHEVRON <- ur.df(asst1,type = "drift",lags = 0)
adf_CHEVRON <- ur.df(asst1,type = "drift",lags = 4)
summary(af_CHEVRON)
summary(adf_CHEVRON)
af_EXXON.MOBIL <- ur.df(asst2,type = "drift",lags = 0)
adf_EXXON.MOBIL <- ur.df(asst2,type = "drift",lags = 4)
summary(af_EXXON.MOBIL)
summary(adf_EXXON.MOBIL)

plot(cbind(asst1, asst2), main="Stock log Prices", legend.loc ="topleft")



#linear regression model unsing the pair
ls_coeffs <- coef(lm(asst1~asst2))
mu <- ls_coeffs[1]
gamma <- ls_coeffs[2]
spread <- asst1 - mu -gamma*asst2
colnames(spread) = "spread"
plot(spread)
lines(xts(rep(0, nrow(asst1)), index(asst1)), col ="red", lwd = 2, lty = 2 )
adf_res <- ur.df(spread, type = "drift") 
summary(adf_res)

# static trading
# performance measure
longTermMean = linearMod$coefficients[1]
cointegrationRatio =  linearMod$coefficients[2]
sigma = sd(spread)
ndays = nrow(asst1)
order <- rep(NA, ndays) #record trade order(long, short, or no action)
starting <- "a"
{for (i in (1:ndays)) {
    if (starting == "a" || starting == "c"){
      if (spread[i]> 1.5*sigma & spread[i]< 1.7*sigma){
        starting = "b"
        order[i] = "Short Spread"
      }
    }
    else if(starting == "b"){
      if (spread[i]> 2.5*sigma){
        starting = "c"
        order[i] = "Stop Loss Order"
      }
      else if (starting == "b"){
        if (spread[i] < 0){
        order[i] = "Close Short Position"
        starting = "a"
        } 
      }
    }
  }
  starting <- "a"
  for (i in (1:ndays)) {
    if (starting == "a" || starting == "c"){
      if (spread[i]< -1.5*sigma & spread[i]> -1.7*sigma){
        starting = "b"
        order[i] = "Long Spread"
      }
    }
    else if(starting == "b"){
      if (spread[i]< -2.5*sigma){
        starting = "c"
        order[i] = "Stop Loss Order"
      }
      else if (starting == "b"){
        if (spread[i] > 0){
          order[i] = "Close Long Position"
          starting = "a"
        } 
      }
    }
  }
}

data <- as.data.frame(cbind(asst1, asst2, spread))
data <- cbind("Index" = (1: ndays), "Date" = as.Date(da$Date), data, order) 
pairResult <- subset(data, !is.na(data$order))

Return <- rep(NA,nrow(pairResult))
for (i in (1:(nrow(pairResult)-1))){
  if (pairResult$order[i] == "Long Spread"||pairResult$order[i] == "Short Spread"){
    if(pairResult$order[i+1] == "Stop Loss Order"){
      Return[i+1]= -abs(diff(pairResult$spread)[i])/
        (pairResult$Index[i+1]-pairResult$Index[i])*252
    }
    else{
      Return[i+1] = abs(diff(pairResult$spread)[i])/
        (pairResult$Index[i+1]-pairResult$Index[i])*252
    }
  }
}
pairResult <- cbind(pairResult, "Annulized Return"= Return)

shortspread <- subset(pairResult,pairResult$order=="Short Spread")
longspread <- subset(pairResult,pairResult$order=="Long Spread")
Close <- subset(pairResult,pairResult$order!="Short Spread")
Close <- subset(Close,Close$order!="Long Spread")

plot(x = data$Date, y = data$spread, type="l", col = "orange", xlab = "Date", ylab="Spreads")
ablineseries <- function(height){
  abline(h=height,lty=2,col="black")
}
sapply(c(1.5*sigma,-1.5*sigma,2.5*sigma,-2.5*sigma,0),ablineseries)
points(shortspread$Date,shortspread$spread,pch =25, col= "blue")
points(longspread$Date,longspread$spread,pch =24, col= "green")
points(Close$Date,Close$spread,pch =23, col= "black")
legend("bottomleft", legend=c("Close position", "Long spread", "Short spread"), col=c("black", "green", "blue"), pch=c(23,24,25))

#########################################################################
###PART 2: Dynamic analysis
U = 0
L = 0
annRetHis = 0
dataset <- subset(da, select =c("Date","CHEVRON","EXXON.MOBIL"))
cointegrationRatio = 0
intercept = 0
x = 1
newSpread = 0
for (x in (1:2653)){
  trnSet <- dataset[x:(252*5+x),]
  ##test for integration
  # af_asst1 <- ur.df(asst1,type = "drift",lags = 0)
  # adf_asst1 <- ur.df(trnSet$CHEVRON,type = "drift")  
  # adf_asst2 <- ur.df(trnSet$EXXON.MOBIL,type = "drift")  
  ##long-run relationship
  linearModule <- lm(trnSet$CHEVRON~trnSet$EXXON.MOBIL)
  cointegrationRatio[x] <- linearModule$coefficients[2]
  intercept[x] <- linearModule$coefficients[1]
  spread <- linearModule$residuals
  
  outsampleSet <- dataset[(252*5+x+1):(252*5+x+200),]
  futureSpread <- outsampleSet$CHEVRON - cointegrationRatio[x] * 
    outsampleSet$EXXON.MOBIL- intercept[x]
  newSpread[x] <- dataset$CHEVRON[252*5+x+1] - cointegrationRatio[x] * 
    dataset$EXXON.MOBIL[252*5+x+1]- intercept[x]
  
  sigma <- sd(spread)
  U[x] <- 1.5* sigma
  L[x] <-  -1.5 * sigma
  
  #calculate annualized return
  tradingDays= 60
  if (futureSpread[1] > U[x]){annRetHis[x]=(futureSpread[1]-futureSpread[tradingDays+1])*252/tradingDays
  }else if (futureSpread[1] < L[x]){annRetHis[x]=(futureSpread[tradingDays+1]-futureSpread[1])*252/tradingDays
  }else {annRetHis[x]= 0}
  
  }

#using the Garch to estimate dynamic volatility rather than historical volatility

library(fGarch)
dataset <- subset(da, select =c("Date","CHEVRON","EXXON.MOBIL"))
UGarch=0
LGarch=0
annRetGarch=0
x=1
for (x in (1:2653)){
  
  trnSet <- dataset[x:(252*5+x),]
  ##test for integration
  #af_asst1 <- ur.df(asst1,type = "drift",lags = 0)
  # adf_asst1 <- ur.df(trnSet$CHEVRON,type = "drift")  
  # adf_asst2 <- ur.df(trnSet$EXXON.MOBIL,type = "drift")  
  ##long-run relationship
  linearModule <- lm(trnSet$CHEVRON~trnSet$EXXON.MOBIL)
  cointegrationRatio[x] <- linearModule$coefficients[2]
  intercept[x] <- linearModule$coefficients[1]
  spread <- linearModule$residuals
  
  outsampleSet <- dataset[(252*5+x+1):(252*5+x+200),]
  futureSpread <- outsampleSet$CHEVRON - cointegrationRatio[x] * 
    outsampleSet$EXXON.MOBIL- intercept[x]

  modelGarch <- garchFit(~garch(1,1),data=spread)
  forecastedVolatility <- predict(modelGarch,1)$standardDeviation
  UGarch[x] <- 1.5* forecastedVolatility
  LGarch[x] <-  -1.5 * forecastedVolatility
 
  #calculate annualized return
  tradingDays= 60
  if (futureSpread[1] > UGarch[x]){annRetGarch[x]=(futureSpread[1]-futureSpread[tradingDays+1])*252/tradingDays
  }else if (futureSpread[1] <LGarch[x]){annRetGarch[x]=(futureSpread[tradingDays+1]-futureSpread[1])*252/tradingDays
  }else {annRetGarch[x]= 0}
}  


  plot(UGarch, type="l",ylim=c(-0.3,0.3),col ="blue", ylab= "Spread",xlab = "Date")  
  points(U,type ="l")
  points(newSpread,type ="l", col ="orange")
  points(-UGarch,type="l",ylim=c(-0.3,0.3),col ="blue")  
  points(-U,type ="l")
  legend("topright",
         legend = c("Historical Volatily * 1.5","Garch Volatily * 1.5","Spread"),
         col = c("black","blue","orange"),
         lty =1)
  annRetHis[is.na(annRetHis) ] <- 0
  annRetGarch[is.na(annRetGarch)] <- 0
  plot(annRetGarch,type="l", ylab="Return",xlab = "Date")
  points(annRetHis,type="l",col = "orange")
  annRetHisMean <-   mean(annRetHis[annRetHis != 0])
  annRetGarchMean <-  mean(annRetGarch[annRetGarch != 0])
  abline(h=annRetHisMean,col="orange")
  abline(h=annRetGarchMean)
  legend("topright",
         legend = c("Historical Method","Garch Method"),
         col = c("orange","black"),
         lty =1
  )
  
  
  
#######################################
#APPENDIX 1 Choosing pair using condition that half life <50 and cointegration satisfies

  #Data input
  da <- read.csv("Original data set.csv",sep = "")
  data <- exp(da[-1])
  data <- cbind(Date=as.Date(da$Date),data)
  
  
  library(egcm)
  library(urca)
  
  halfLife <- matrix(0,nrow=63,ncol=63)
  rho =1
  for (i in (2:63)){
    for (j in (i:63)){
      rho <- egcm(data[,i],data[,j])$rho
      halfLife[i,j] <- abs(log(2)/log(rho))
    }
  }
  
  
  result <-  matrix(0,nrow=63,ncol=63) #test the existence of cointegration
  
  for (i in (2:63)){
    for (j in (i:63)){
      res <- lm(data[,i]~data[,j])$residuals
      
      if(DFtest(data[,i]) == 0 & DFtest(data[,j]) == 0){result[i,j]= DFtest(res)}
      
    }
  }

  for (i in (2:63) ){
    result[i,i]=0
    halfLife[i,i]=0
  }
  pairSelect <- matrix(0,nrow = 63,ncol=63)
  for (i in (2:63)){
    for (j in (i:63)){
      if (result[i,j] == 1 & halfLife[i,j]< 50){
        pairSelect[i,j] = 1 # the pair is quite good to choose
      }
    }
  }# 
  
  write.csv(pairSelect,"pair selection result.csv")

  