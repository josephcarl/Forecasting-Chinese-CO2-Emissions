# Joseph Carl
# ECON 5305: Forecasting Project


# Clear workspace
rm(list=ls(all=TRUE))


# Required libraries and functions
library(readxl)
library(tidyverse)
library(urca)
library(dynlm)
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


##### VAR Model ######

# 3 variables: g.China (CO2), ChinaGDPgrowth, g.coal

z <- cbind(g.China, ChinaGDPgrowth, g.coal) %>% na.omit
cov.matrix <- var(z) # Reduced form covariance matrix

# Estimate the models
  # Will use 1 lag for all models because each variable exhibited AR(1) behavior

eq1 <- dynlm(g.China ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.coal, 1) + 
               L(g.China, 2) + L(ChinaGDPgrowth, 2) + L(g.coal, 2), data = z)
eq2 <- dynlm(ChinaGDPgrowth ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.coal, 1) + 
               L(g.China, 2) + L(ChinaGDPgrowth, 2) + L(g.coal, 2), data = z)
eq3 <- dynlm(g.coal ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.coal, 1) + 
               L(g.China, 2) + L(ChinaGDPgrowth, 2) + L(g.coal, 2), data = z)

# Extract coefficients to a matrix
coef.mat <- local({
  out <- rbind(coef(eq1), coef(eq2), coef(eq3))
  rownames(out) <- c("eq.CO2", "eq.GDP", "eq.COAL")
  out
})
Dhat.0 <- coef.mat[,1, drop=FALSE]        # matrix of constants
Dhat.1 <- coef.mat[,2:4, drop=FALSE]      # matrix of coefficeints on Y_{t-1} and X_{t-1}
Dhat.2 <- coef.mat[,5:7, drop=FALSE]      # matrix of coefficeints on Y_{t-2} and X_{t-2}

# Get fitted residuals
ehat.CO2 <- resid(eq1)
ehat.GDP <- resid(eq2)
ehat.coal <- resid(eq3)
ehat <- rbind(ehat.CO2, ehat.GDP, ehat.coal)

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
Gamma.2 <- solve(B) %*% Dhat.2

# View the recovered structural parameters
Gamma.0; Gamma.1; Gamma.2

# Compare to textbook commands
var.mod <- vars::VAR(z, ic = c("AIC"), lag.max=4)
summary(var.mod)

var.1lag <- vars::VAR(z, p=1)
logLik(var.1lag)
var.2lag <- vars::VAR(z, p=2)
summary(var.2lag)
logLik(var.2lag)

LRtest.stat <- 2*(logLik(var.2lag)-logLik(var.1lag))
pchisq(LRtest.stat, df = 12, lower.tail = F)
  # the likelihood ratio test says that model with 2 lags is better fit than 1 lag

# Select max number of lags
vars::VARselect(z, lag.max = 8, type = "const")
  # This method agrees that 1 lag is the best choice for the VAR model

test.mod <- dynlm(g.China ~ L(g.China, 1) + L(ChinaGDPgrowth, 1) + L(g.coal, 1) + L(g.g.elec) + L(g.ind,1))
summary(test.mod)
