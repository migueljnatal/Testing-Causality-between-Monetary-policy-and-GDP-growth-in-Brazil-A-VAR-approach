# Time Series/EconometricS II - MSc in Statistics - PPGst UFRGS

# @uthor: Miguel Jandrey Natal 

# This article was written as a partial evaluation for the Time Series/
# EconometricS II discipline

# Testing Causality between Monetary policy and GDP growth in Brazil: 
#              A Vector Autoregression approach



setwd("C:/Users/MIGUEL/OneDrive/Mestrado PPGEst/Séries Temporais")


# The websites from where the series were collected are http://ipeadata.gov.br/beta3 (Selic) and
# https://fred.stlouisfed.org/series/NAEXKP01BRQ657S (Brazilian GDP growth). 
# I downloaded the series from there and changed their names in my desktop. 

#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("vars")
#install.packages("ggthemr") 
#install.packages("aod")
#install.packages("stargazer")  
#install.packages("rlang")
#install.packages("ggplot")
#install.packages("devtools")
#install.packages("tseries")
#install.packages("quantmod") 
#install.packages('stargazer')
#install.packages("PLRModels")
#install.packages("urca") 
library(urca)
library(PLRModels)
library(stargazer)
library(quantmod)
library(tseries)
library(devtools)
library(rlang)
library(ggplot2)
library(forecast)
library(vars)
library(ggthemr)
library(aod)
library(stargazer)
ggthemr('light')

getwd()
devtools::install_github('cttobin/ggthemr')
dir()


# Reading the data

data_gdp = read.csv("gdp_tri.csv", header = T, sep = ',', dec = '.')
data_gdp = data_gdp[,2]


data_selic = read.csv("selic_over_tri.csv", header = T, sep = ',', dec = '.')
data_selic = data_selic[,6]

# Storing the data 

data_selic = ts(data_selic, start=c(2012,1), freq=4)
data_selic = data_selic[,6]


date <- seq(as.Date('2012-01-01'), as.Date('2019-12-01'),
            by='3 month')
data <- data.frame(date=date, selic=data_selic,
                   gdp=data_gdp)

# GDP and Selic in % values 

selic_pct = data_selic /100
gdp_pct = data_gdp / 100

date <- seq(as.Date('2012-01-01'), as.Date('2019-12-01'),
            by='3 month')
data_pct <- data.frame(date=date, selic=selic_pct,
                   gdp=gdp_pct)

# TS objects

selic_ts = ts(selic_pct, start=c(2012,1), freq=4)   #par(mfrow= c(1,1))
plot.ts(selic_pct)

gdp_ts = ts(gdp_pct, start=c(2012,1), freq=4)
plot.ts(gdp_pct)

adf.test(selic_ts)
adf.test(diff(selic_ts))
adf.test(diff(diff(diff(selic_ts)))) 
plot.ts(data_gdp) 

# Adding diff. until the series become stationary (getting the order of integration)

# GDP

gdp_diff_1 = diff(gdp_ts, differences=1)
gdp_diff_2 = diff(gdp_ts, differences=2) # change the differences parameters until the adf test shows stationarity 
gdp_diff_3 = diff(gdp_ts, differences=3)
adf.test(gdp_diff)


# Selic rate

selic_diff_1 = diff(selic_ts, differences=1)
selic_diff_2 = diff(selic_ts, differences=2)
selic_diff_3 = diff(selic_ts, differences=3) # m = 3 ; max order of integration 


adf.test(selic_diff_1)$p.value


# Stargazer table 

adf_level = c(adf.test(selic_ts)$statistic, adf.test(gdp_ts)$statistic)
adf_level

adf_diff_1 = c(adf.test(selic_diff_1)$statistic, adf.test(gdp_diff_1)$statistic)
adf_diff_1 

adf_diff_2 = c(adf.test(selic_diff_2)$statistic, adf.test(gdp_diff_2)$statistic)
adf_diff_2 

pv_level = c(adf.test(selic_ts)$p.value, adf.test(gdp_ts)$p.value)
pv_level

pv_diff_1 = c(adf.test(selic_diff_1)$p.value, adf.test(gdp_diff_1)$p.value)
pv_diff_1 

pv_diff_2 = c(adf.test(selic_diff_2)$p.value, adf.test(gdp_diff_2)$p.value)
pv_diff_2

adf_diff_3 = c(adf.test(selic_diff_3)$statistic, adf.test(gdp_diff_3)$statistic)
adf_diff_3 

pv_diff_3 = c(adf.test(selic_diff_3)$p.value, adf.test(gdp_diff_3)$p.value)
pv_diff_3

table =  cbind(adf_level,pv_level, adf_diff_1, pv_diff_1, adf_diff_2, pv_diff_2, adf_diff_3  , pv_diff_3) 
table

 
rownames(table) <- c('Selic', 'GDP')
colnames(table) <- c('ADF level', 'p-value', 'ADF 1st diff', 'p-value', 'ADF 2nd diff', 'p-value', 'ADF 3rd diff', 'p-value')

stargazer(table, title='Previsão da Inflação acumulada em 12 meses', digits=2, decimal.mark = '.')

# Unit root tests, acf and pacf

series <- list(selic_ts, gdp_ts)
lapply(series, adf.test)
lapply(series, acf)
lapply(series, pacf)


# Plotting 

ggplot(data, aes(x=date))+
  geom_line(aes(y=data_selic, colour='Selic rate'), size=.8)+
  geom_line(aes(y=data_gdp, colour='GDP growth'), size=.8)+
  scale_colour_manual('',
                      values=c('Selic rate'='red',
                               'GDP growth'='darkblue'))+
  theme(legend.position = 'top')+
  labs(title='GDP growth and Selic rate in Brazil (2012/Q1-2019/Q4)')+
  xlab('')+ylab('% values')


series_plot <- list("Selic"=selic_ts,
                     "GDP growth"=gdp_ts)
par(mfrow= c(2,1), mar = c(7,7,4,4))
invisible(lapply(names(series_plot), function(x) plot(series_plot[[x]], main=x, 
                                                       xlab = "", ylab = "", type = "l", bty='l',
                                                      col=c('grey1','red'), lwd = 2, grid(col='darkgrey', lwd=1))))
grid(col='darkgrey', lwd=1)

 
date <- seq(as.Date('2012-01-01'), as.Date('2019-12-01'),
            by='3 month')
data <- data.frame(date=date, selic=data_selic,
                   gdp=data_gdp)


# Selecting VAR order


data_VAR <- ts(data_pct[,-1], start=c(2012,1), freq=4)

def <- VARselect(data_VAR, lag.max = 3, type="both")
def$selection 

stargazer(def$selection, title='VAR order selection', digits=1, decimal.mark = '.')

# VAR(2)

var2 <- VAR(data_VAR, p=2, type='both')
serial.test(var2)

plot(stability(var2))

coef(var2$varresult[[1]]) 

# Teste de Wald
 

# VAR(5) (p + m)

var5 <- VAR(data_VAR, p=5, type='both')
serial.test(var5)

df = data.frame(data_VAR)
df

class(data_VAR)


# Wald Test 01: Selic rate does not granger cause GDP growth

wald.test(b=coef(var5$varresult[[1]]),  Sigma=vcov(var5$varresult[[1]]),Terms= c(2,4))


# Wald Test 02: GDP growth does not granger cause Selic rate

wald.test(b=coef(var5$varresult[[2]]), Sigma=vcov(var5$varresult[[2]]), Terms=c(1,3)) 


# Johansen Cointegration Test

jotest=ca.jo(data.frame(data_VAR), type="eigen", K=2, ecdet="none", spec="longrun")
summary(jotest)













