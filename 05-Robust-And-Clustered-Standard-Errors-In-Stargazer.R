library(wbstats) # to retrieve World Bank development data 
library(dplyr)
library(data.table)
library(plm)
library(stargazer) # this is what we are working with
library(sandwich) # to calculate robust and clustered standard errors
library(pander)


# DOWNLOADING AND MANIULATING DATA

data <- wb(indicator = c("NY.GDP.MKTP.PP.KD", # WB code for PPP GDP per capita in 2011 $
                         "IP.PAT.RESD", # WB code for patent applications by residents
                         "SP.POP.TOTL"), # WB code for population
           startdate = 2000, 
           enddate = 2015)

countries <- wbcountries() 

data <- merge(data, countries[c("iso2c", "region")], 
              by = "iso2c", all.x = TRUE)
data <- subset(subset(data, region != "Aggregates"))

data$indicatorID[data$indicatorID == "NY.GDP.MKTP.PP.KD"] <- "GDP"
data$indicatorID[data$indicatorID == "IP.PAT.RESD"] <- "patent_applications"
data$indicatorID[data$indicatorID == "SP.POP.TOTL"] <- "population"

data <- dcast(data, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

names(data)[names(data) == "date"] <- "year"
data$year <- as.numeric(data$year)
data <- data %>%
  select(-iso2c, -region)

data$population <- data$population / 10^6
data$GDP <- data$GDP / 10^9

data <- data[complete.cases(data),]


# ROBUST STANDARD ERRORS

linear_regression <- lm(formula = patent_applications ~ 
                          GDP + population,
                        data = data %>% filter(year == 2011))

robust_standard_errors <- vcov(linear_regression, sandwich)
robust_standard_errors <- sqrt(diag(robust_standard_errors))

df = data.frame(robust_standard_errors)
pander(df)

stargazer(linear_regression, title = "Linear regression", 
          se = list(robust_standard_errors),
          type = "html", out = "linear regression.html")


# CLUSTERED STANDARD ERRORS

p_data = pdata.frame(data) # we need to create a 'panel data frame' for panels regressions

fe_regression <- plm(patent_applications ~ GDP + population, # fe model
                     data = p_data,
                     model = "within",
                     effect = "twoways")

fd_regression <- plm(diff(patent_applications) ~ diff(GDP, lag = 2) + # fd model
                       diff(diff(GDP), lag = c(0:1)) +
                       diff(population, lag = 2) + diff(diff(population), lag = c(0:1)),
                     data = p_data,
                     model = 'pooling')


clustered_standard_errors_fe <- vcovHC(fe_regression, type = "HC0", cluster = "group")
clustered_standard_errors_fe <- sqrt(diag(clustered_standard_errors_fe))

clustered_standard_errors_fd <- vcovHC(fd_regression, type = "HC0", cluster = "group")
clustered_standard_errors_fd <- sqrt(diag(clustered_standard_errors_fd))


stargazer(fe_regression, fd_regression, title = "Panel regressions", 
          se = list(clustered_standard_errors_fe, clustered_standard_errors_fd),
          type = "html", out = "panel regression.html")