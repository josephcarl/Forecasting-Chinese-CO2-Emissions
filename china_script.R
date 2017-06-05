# Joseph Carl
# ECON 5305: Forecasting Project


# Clear workspace
rm(list=ls(all=TRUE))


# Required libraries and functions
library(readxl)
library(tidyverse)
library(urca)
library(dynlm)
library(stargazer)
source("Functions_ECON_5305.R")

# Read in co2 data
co2percap <- read_excel("indicator CDIAC carbon_dioxide_emissions_per_capita.xlsx")

# Collapse "Year" columns into a single column, then convert character to numeric
co2percap <- co2percap %>% 
  gather("Year", "CO2PerCap", 2:254) %>% 
  mutate_at(vars(Year), as.numeric)

# Extract just the China data
China <- co2percap %>% 
  filter(`CO2 per capita`=="China")
  
glimpse(China)

# Turn the China data into a time series
China.co2 <- China %>% filter(Year>=1960) %>% 
  select(CO2PerCap) %>% ts(., start=c(1960, 1), frequency=1)

ts.plot(China.co2)

# Calculate growth rate of CO2 emissions (log-diff)
g.China <- diff(log(China.co2))

ts.plot(g.China)

# Plot the ACF/PACF functions
acf(g.China)
pacf(g.China)
acf.pacf(g.China)
  # looks like possibly an ARMA (1,1) process 

## Other datasets - GDP per capita growth
GDPperCap <- read_excel("indicatorwdigdp_percapita_growth.xlsx")
GDPperCap <- GDPperCap %>% 
  gather("Year", "GDPperCap", 2:53) %>% 
  mutate_at(vars(Year), as.numeric) %>% 
  rename(Country = `GDP per capita growth (annual %)`)
ChinaGDPgrowth <- GDPperCap %>% 
  filter(Country == "China") %>% 
  na.omit %>% 
  select(GDPperCap) %>% 
  ts(., start=c(1961,1), frequency=1)

ts.plot(ChinaGDPgrowth)

acf(ChinaGDPgrowth)
pacf(ChinaGDPgrowth)
acf.pacf(ChinaGDPgrowth)
  # Could be an ARMA(2,1) process

## Electricity consumption per capita
ElecCons <- read_excel("Indicator_Electricity consumption per capita.xlsx")
ElecCons <- ElecCons %>% 
  gather("Year", "Elec_Cons_per_Cap", 2:53) %>% 
  mutate_at(vars(Year), as.numeric) %>% 
  rename(Country = `Electricity consumption, per capita (kWh)`)
ChinaElec <- ElecCons %>% 
  filter(Country=="China") %>% 
  na.omit %>% 
  select(Elec_Cons_per_Cap) %>% 
  ts(., start = c(1971,1), frequency=1)

ts.plot(ChinaElec)

g.elec <- diff(log(ChinaElec))
ts.plot(g.elec)

acf(g.elec)
pacf(g.elec)
acf.pacf(g.elec)

## Industry as % of GDP
Industry <- read_excel("Industry (p of GDP).xlsx")
Industry <- Industry %>% 
  gather("Year", "IndustryPerc", 2:53) %>% 
  mutate_at(vars(Year), as.numeric) %>% 
  rename(Country = `Industry, value added (% of GDP)`)
ChinaInd <- Industry %>% 
  filter(Country == "China") %>% 
  na.omit %>% 
  select(IndustryPerc) %>% 
  ts(., start = c(1960,1), frequency=1)

ts.plot(ChinaInd)

g.ind <- diff(log(ChinaInd))

ts.plot(g.ind)

acf(g.ind)
pacf(g.ind)
acf.pacf(g.ind)
  # also hard to tell
  # Maybe ARMA(6,6)?
  # From plotting the original series, it looks like there could be a roughly 6-year cycle

## Coal consumption per capita
Coal <- read_excel("Coal Consumption per capita.xls.xlsx")
Coal <- Coal %>% 
  gather("Year", "CoalCons", 2:48) %>% 
  mutate_at(vars(Year), as.numeric) %>%
  rename(Country = `Coal Consumption per person (tonnes oil equivalent)`)
ChinaCoal <- Coal %>% 
  filter(Country=="China") %>% 
  na.omit %>% 
  select(CoalCons) %>% 
  ts(., start=c(1965,1), frequency=1)

ts.plot(ChinaCoal)

g.coal <- diff(log(ChinaCoal))

ts.plot(g.coal)

acf(g.coal)
pacf(g.coal)
acf.pacf(g.coal)
  # Maybe an ARMA(1,2)

# % of Population Employed
empdat <- read_excel("nationaldata.xls")
tot.emp <- empdat %>% filter(Commodity_Desc == "Total employment") %>% 
  arrange(Year_Desc) %>% 
  mutate(Pop = Amount*10000/1000000) %>% 
  select(Pop) %>% ts(., start=c(1952,1), frequency=1)
tot.pop <- empdat %>% filter(Commodity_Desc == "Population") %>% 
  arrange(Year_Desc) %>% 
  mutate(Pop = Amount*10000/1000000) %>% 
  select(Pop) %>% ts(., start=c(1952,1), frequency=1)
perc.employed <- tot.emp/tot.pop

ts.plot(perc.employed)

g.perc.employed <- diff(log(perc.employed))

ts.plot(g.perc.employed)

acf(g.perc.employed)
pacf(g.perc.employed)
acf.pacf(g.perc.employed)

perc.unemp <- 1-perc.employed
ts.plot(perc.unemp)

g.perc.unemp <- diff(log(perc.unemp))
acf(g.perc.unemp)
pacf(g.perc.unemp)
acf.pacf(g.perc.unemp)

# Exchange rate
ex.rate <- empdat %>% filter(Commodity_Desc == "Exchange rate") %>% 
  arrange(Year_Desc) %>% select(Amount) %>% 
  ts(., start=c(1960,1), frequency=1)

ts.plot(ex.rate)

g.ex.rate <- diff(log(ex.rate))

ts.plot(g.ex.rate)



##### Unit Root Tests #####
adf.results(China.co2, max.lags = 1)
adf.results(g.China, max.lags = 1)
  # Chinese CO2 emissions are nonstationary
  # but the CO2 growth rate is stationary

adf.results(ChinaGDPgrowth, max.lags = 1)
  # the China GDP growth rate is already stationary

adf.results(ChinaElec, max.lags = 1)
  # China Electricity consumption per capita is nonstationary
adf.results(g.elec, max.lags = 1)
  # Even the growth rates of electricity consumption per capita are nonstationary
g.elec.test.drift <- ur.df(g.elec, type = "drift", lags = 1)
summary(g.elec.test.drift)
g.elec.test.trend <- ur.df(g.elec, type = "trend", lags = 1)
summary(g.elec.test.trend)
# 1st-diff the series and try again
g.g.elec <- diff(g.elec)
adf.results(g.g.elec, max.lags = 1)
  # Now the series is stationary

acf(g.g.elec)
pacf(g.g.elec)
acf.pacf(g.g.elec)
  # this one is harder to tell
  # ARMA(1,2), ARMA(1,6), AR(6)?

adf.results(ChinaInd, max.lags = 1)
  # % of GDP in Industry is nonstationary
adf.results(g.ind, max.lags = 1)
  # growth rate of % of GDP in industry is stationary

adf.results(ChinaCoal, max.lags = 1)
  # coal consumption per capita is nonstationary
adf.results(g.coal, max.lags = 1)
  # growth rate of coal consumption per capita is stationary

adf.results(perc.employed, max.lags = 1)
adf.results(g.perc.employed, max.lags = 1)

adf.results(perc.unemp, max.lags = 1)
adf.results(g.perc.unemp, max.lags = 1)

adf.results(ex.rate, max.lags = 1)
adf.results(g.ex.rate, max.lags = 1)


##### VAR Model ######

# 3 variables: g.China (CO2), ChinaGDPgrowth, g.elec

z <- cbind(g.China, ChinaGDPgrowth, g.elec) %>% na.omit
cov.matrix <- var(z) # Reduced form covariance matrix

# Estimate the models
  # Will use 1 lag because VARselect() said minimum AIC occurs when using 1 lag
  # and theoretically it makes sense that what happened 2+ years ago has little explanatory power
  # for what happens today

eq1 <- dynlm(g.China ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.elec, 1), data = z)
eq2 <- dynlm(ChinaGDPgrowth ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.elec, 1), data = z)
eq3 <- dynlm(g.elec ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.elec, 1), data = z)

# Nice output (Stargazer) of 3 equations
stargazer(eq1, eq2, eq3, type = "text", covariate.labels = c("lag CO2", "lag GDP", "lag Electricity"),
          dep.var.labels = c("CO2", "GDP", "Electricity"))


# Extract coefficients to a matrix
coef.mat <- local({
  out <- rbind(coef(eq1), coef(eq2), coef(eq3))
  rownames(out) <- c("eq.CO2", "eq.GDP", "eq.elec")
  out
})
Dhat.0 <- coef.mat[,1, drop=FALSE]        # matrix of constants
Dhat.1 <- coef.mat[,2:4, drop=FALSE]      # matrix of coefficeints on Y_{t-1} and X_{t-1}

# Get fitted residuals
ehat.CO2 <- resid(eq1)
ehat.GDP <- resid(eq2)
ehat.elec <- resid(eq3)
ehat <- rbind(ehat.CO2, ehat.GDP, ehat.elec)

sigma.ehat <- var(t(ehat))

# Compare covariance matrix with computed matrix
cov.matrix
sigma.ehat

# Cholesky decomposition
B <- chol(sigma.ehat)

# Recovered structural parameters
solve(B)  # B-inverse
A0 <- (-1)*(solve(B) - diag(3))   # A0 matrix
Gamma.0 <- solve(B) %*% Dhat.0 
Gamma.1 <- solve(B) %*% Dhat.1

# View the recovered structural parameters
Gamma.0; Gamma.1

# Compare to textbook commands
var.mod <- vars::VAR(z, ic = c("AIC"), lag.max=6)
summary(var.mod)

var.1lag <- vars::VAR(z, p=1)
summary(var.1lag)

logLik(var.1lag)
var.2lag <- vars::VAR(z, p=2)
summary(var.2lag)
logLik(var.2lag)

LRtest.stat <- 2*(logLik(var.2lag)-logLik(var.1lag))
pchisq(LRtest.stat, df = 12, lower.tail = F)
  # the likelihood ratio test says that model with 2 lags is better fit than 1 lag

# Select max number of lags
vars::VARselect(z, lag.max = 8, type = "trend")
  # This method agrees that 1 lag is the best choice for the VAR model

test.mod <- dynlm(g.China ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.coal, 1) + L(g.g.elec) + L(g.ind,1))
summary(test.mod)



###### IRF #######
# Impulse response function for a 1-std dev shock

# Dr. Bejan's IRF function
irf.fun <- function(A.mat, C.mat, struct.shock, n.ahead){
  #---------------------------------------------------------------#
  #-- Inputs:                                                   --#
  #--     A.mat: 2-by-2 matrix of 1st lag coefficients          --#
  #--     C.mat: Choleski decomposition matrix                  --#
  #--     n.ahead: length of the IRFs (periods after the shock) --#
  #--     struct.shock: (0,1) vector of structural shocks       --#
  #---------------------------------------------------------------#
  shocks <- matrix(struct.shock, ncol=1)                        
  df <- matrix(0, ncol=n.ahead, nrow=nrow(A.mat))
  for (i in 1:n.ahead){
    if (i == 1){
      df[,i] <- C.mat %*% shocks
    } else {
      df[,i] <- A.mat %*%  df[,i-1]
    }
  }  
  rownames(df) <- rownames(A.mat)
  colnames(df) <- paste("irf.",0:(n.ahead-1),sep="")
  out <- t(df)  # transpose (time: by row, variable: by column)
}
co2.shock.irf <- irf.fun(A.mat = Dhat.1, C.mat = B, struct.shock = c(1,0,0), n.ahead = 12)
gdp.shock.irf <- irf.fun(A.mat = Dhat.1, C.mat = B, struct.shock = c(0,1,0), n.ahead = 12)
elec.shock.irf <- irf.fun(A.mat = Dhat.1, C.mat = B, struct.shock = c(0,0,1), n.ahead = 12)

local({
  par(mar = rep(2, 4))
  par(mfrow=c(3,3))    # place 4 plots on 1 page (2-by-2)
  ts.plot(co2.shock.irf[,"eq.CO2"], ylab ="CO2", main="CO2 --> CO2")
  ts.plot(co2.shock.irf[,"eq.GDP"], ylab ="GDP", main="CO2 --> GDP")
  ts.plot(co2.shock.irf[,"eq.elec"], ylab ="Electricity", main="CO2 --> Elec")
  
  ts.plot(gdp.shock.irf[,"eq.CO2"], ylab ="CO2", main="GDP --> CO2")
  ts.plot(gdp.shock.irf[,"eq.GDP"], ylab ="GDP", main="GDP --> GDP")
  ts.plot(gdp.shock.irf[,"eq.elec"], ylab ="Elec", main="GDP --> Elec")
  
  ts.plot(elec.shock.irf[,"eq.CO2"], ylab ="CO2", main="Elec --> CO2")
  ts.plot(elec.shock.irf[,"eq.GDP"], ylab ="GDP", main="Elec --> GDP")
  ts.plot(elec.shock.irf[,"eq.elec"], ylab ="x", main="Elec --> Elec")
  par(mfrow=c(1,1))    # place 1 plot per page
})

var1.reorder <- vars::VAR(z[,c("g.elec", "ChinaGDPgrowth", "g.China")], p = 1)
irf.noconf <- vars::irf(var1.reorder, n.ahead=12, boot=F)
plot(irf.noconf)

