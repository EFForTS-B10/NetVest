



library(fable)
library(Refforts)
library(tidyverse)
library(tsibble)
library(tsibbledata)
library(lubridate)

## read price data:
p_rubber <- Refforts::get.abm.main.default(crop="rubber")$prices
p_rubber <- as.numeric(strsplit(substr(p_rubber, 2, (nchar(p_rubber) - 1)), split=" ")[[1]])

p_oilpalm <- Refforts::get.abm.main.default(crop="oilpalm")$prices
p_oilpalm <- as.numeric(strsplit(substr(p_oilpalm, 2, (nchar(p_oilpalm) - 1)), split=" ")[[1]])

prices <- tibble(time = 1960:2011, oilpalm = p_oilpalm, rubber = p_rubber) %>% 
  tidyr::pivot_longer(cols=c(oilpalm, rubber))


prices <- tsibble::as_tsibble(prices, index=time, key=name)



## Ofrecast model:
prices %>%
  model(
    ets = ETS(box_cox(value, 0.9)),
    arima = ARIMA(log(value)),
    snaive = SNAIVE(value)
  ) %>%
  forecast(h = "20 years") %>% 
  autoplot(filter(prices, time > 2000), level = NULL)






aus_retail %>%
  filter(
    State %in% c("New South Wales", "Victoria"),
    Industry == "Department stores"
  ) %>% 
  model(
    ets = ETS(box_cox(Turnover, 0.3)),
    arima = ARIMA(log(Turnover)),
    snaive = SNAIVE(Turnover)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(aus_retail, year(Month) > 2010), level = NULL)





#####

##PRSim:
p_rubber <- Refforts::get.abm.main.default(crop="rubber")$prices
p_rubber <- as.numeric(strsplit(substr(p_rubber, 2, (nchar(p_rubber) - 1)), split=" ")[[1]])

p_oilpalm <- Refforts::get.abm.main.default(crop="oilpalm")$prices
p_oilpalm <- as.numeric(strsplit(substr(p_oilpalm, 2, (nchar(p_oilpalm) - 1)), split=" ")[[1]])

prices <- tibble(time = 1960:2011, oilpalm = p_oilpalm, rubber = p_rubber) #%>% 
  #tidyr::pivot_longer(cols=c(oilpalm, rubber))

library(PRSim)
prsim(data=prices$rubber)



### prophet:
library(prophet)

p_rubber <- Refforts::get.abm.main.default(crop="rubber")$prices
p_rubber <- as.numeric(strsplit(substr(p_rubber, 2, (nchar(p_rubber) - 1)), split=" ")[[1]])
p_rubber <- tibble(ds = seq(as.Date("1960/01/01"), by = "year", length.out = 52), y = p_rubber)




p_oilpalm <- Refforts::get.abm.main.default(crop="oilpalm")$prices
p_oilpalm <- as.numeric(strsplit(substr(p_oilpalm, 2, (nchar(p_oilpalm) - 1)), split=" ")[[1]])
p_oilpalm <- tibble(ds = seq(as.Date("1960/01/01"), by = "year", length.out = 52), y = p_oilpalm)


# build model:
m <- prophet(p_oilpalm, weekly.seasonality = FALSE, daily.seasonality = FALSE)
# forecast:
future <- make_future_dataframe(m, periods = 52, freq="year")
f <- predict(m, future)

ggplot(f, aes(x = ds, y=trend)) +
  geom_line()

plot(m, f)

prophet_plot_components(m, forecastprophet)









###########   READ NEW DATA:

prices <- read.csv("03_Analyses/04_testing_Refforts/wb_prices.csv", sep=",", header=TRUE)
# time series start at 1 january 1960:
prices$time <- seq(as.Date("1960/01/01"), by = "month", length.out = nrow(prices))

op <- prices %>% dplyr::select(time, oilpalm) %>% dplyr::rename(ds=time, y=oilpalm)

# build model:
m <- prophet(op, weekly.seasonality = FALSE, daily.seasonality = FALSE, mcmc.samples = 100)
# forecast:
future <- make_future_dataframe(m, periods = 50*12, freq="month")
f <- predict(m, future)


plot(m,f)

#prophet_plot_components(m, forecastprophet)


###########   BOXCOX and prohpet:
prices <- read.csv("03_Analyses/04_testing_Refforts/wb_prices.csv", sep=",", header=TRUE)
prices$time <- seq(as.Date("1960/01/01"), by = "month", length.out = nrow(prices))
op <- prices %>% dplyr::select(time, oilpalm) %>% dplyr::rename(ds=time, y=oilpalm)

plot(op, typ="l")


# The BoxCox.lambda() function will choose a value of lambda
library(forecast)
lam = BoxCox.lambda(op$y, method = "loglik")
op$y = BoxCox(op$y, lam)

plot(op, typ="l")



###################### MONTECARLO:

f_stock_return <- function(stock_price, n, stock_mu, stock_sigma){
  delta_t <- 1/n # one period
  for (i in seq(n)){
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    # calculate stock price (using quantile function of normal distribution)
    stock_price <- stock_price * (1 + qnorm(epsilon, 
                                            stock_mu * delta_t, 
                                            stock_sigma* sqrt(delta_t)))
  }
  return(stock_price)
}

f_stock_return(100, 20, .1, .2)


# parameters
simulations <- 10000 # number of MC simulations
n <- 20 # trading days
stock_price <- 100
stock_mu <- .1 # drift 10%
stock_sigma <- .2 # volatility 20%

# Monte Carlo simulations
set.seed(42) # for reproducibility
stock_prices <- c()
for (i in seq(simulations)){
  stock_prices <- c(stock_prices,
                    f_stock_return(stock_price=stock_price, 
                                   n=n, 
                                   stock_mu=stock_mu, 
                                   stock_sigma=stock_sigma))
}


################  BROWNIAN MOTION:

library(sde)

prices <- read.csv("03_Analyses/04_testing_Refforts/wb_prices.csv", sep=",", header=TRUE)
prices$time <- seq(as.Date("1960/01/01"), by = "month", length.out = nrow(prices))

stock_mu <- mean(prices$oilpalm)
stock_sd <- sd(prices$oilpalm)
N <- 50*12 # 50 years, each with 12 months
t <- (1:N)/12

blupp <- sde::GBM(x=stock_mu, r=0, sigma=stock_sd, T=t, N=N)


#####

library(somebm)

prices <- read.csv("03_Analyses/04_testing_Refforts/wb_prices.csv", sep=",", header=TRUE)
prices$time <- seq(as.Date("1960/01/01"), by = "month", length.out = nrow(prices))
prices$timestep <- seq(1:nrow(prices))

stock_mu <- mean(prices$oilpalm)
stock_sd <- sd(prices$oilpalm)
stock_start <- prices$oilpalm[[1]]
time_start <- 1 #first(prices$timestep)
time_end <- 2 #last(prices$timestep)
steps <- nrow(prices) - 1

n.ts <- 5

sim <- purrr::map_dfr(1:n.ts, function(x){
  result <- somebm::gbm(x0=stock_start, mu=stock_mu, sigma=stock_sd, t0=time_start, t=time_end, n=steps)  
  result.tib <- tibble(id=x, time=prices$time, simprice=result)
  return(result.tib)
})


ggplot(sim, aes(x=time, y=simprice, color=factor(id))) +
  geom_line()




b <- gbm(x0=50, mu=1, sigma=1, t0=1, t=2, n=100)
plot(b)
