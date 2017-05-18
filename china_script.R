# Joseph Carl
# ECON 5305: Forecasting Project


# Clear workspace
rm(list=ls(all=TRUE))


# Required libraries
library(readxl)
library(tidyverse)

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

