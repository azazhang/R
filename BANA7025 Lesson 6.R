library(tidyverse)
transactions <- completejourney::transactions_sample
set.seed(10)
x <- runif(1)
x <- c(8, 3, -2, -5)
for (i in x)
{
if (i < 0) {
  paste("x is less than zero: ", i)
} else {
  print("x is not less than zero")
}
}

x <- 0

if (x < 0) {
  print("x is a negative number")
} else if (x > 0) {
  print("x is a positive number")
} else {
  print("x is zero")
}

month <- 13

if ((month >=0) & (month<= 9)) {
  paste0("data/", "Month-0", month, ".csv")
} else if ((month >=10) & (month <= 12)) {
  paste0("data/", "Month-", month, ".csv")
} else {
  print("invalid month")
}

(x <- c(runif(5), NA))
x
if_else(x > 0.5, "greater than", "less_than",, missing = "not available")

set.seed(123)
x <- c(runif(10), NA, Inf, 1.25)
x
dplyr::case_when(
  x < 0.3 ~ "low",
  x < 0.7 ~ "medium",
  x < 0.9 ~ "medium high",
  x < 1.0 ~ "high",
  is.na(x) ~ "missing",
  TRUE ~ "out of bond"
)

set.seed(123)
(x <- c(runif(10), NA, Inf, 1.25))
##  [1] 0.287578 0.788305 0.408977 0.883017 0.940467 0.045556 0.528105
##  [8] 0.892419 0.551435 0.456615       NA      Inf 1.250000

dplyr::case_when(
  x < .3   ~ "low",
  x < .7   ~ "medium",
  x < .9   ~ "medium high",
  x <=1.0  ~ "high",
  is.na(x) ~ "missing",
  TRUE     ~ "out of bounds"
)

month <- 1:13

dplyr::case_when(
  month %in% 1:9 ~ paste0("data/", "Month-0", month, ".csv"),
  month %in% 10:12 ~ paste0("data/", "Month-", month, ".csv"),
  TRUE ~ print("Invalid month")
)

transactions %>%
  select(household_id, basket_id, quantity, sales_value) %>%
  mutate(
    power_rating = case_when(
      sales_value < quantile(transactions$sales_value, 0.25) ~ 1,
      sales_value < quantile(transactions$sales_value, 0.50) ~ 2,
      sales_value < quantile(transactions$sales_value, 0.75) ~ 3,
      T ~ 4
    )
  )
data <- transactions %>%
  select(household_id, basket_id, retail_disc, coupon_disc, coupon_match_disc) %>%
  mutate(total_disc = retail_disc + coupon_disc + coupon_match_disc) %>%
  mutate(disc_rating = case_when(
    total_disc == 0 ~ 'none',
    total_disc < quantile(total_disc, 0.25) ~ 'low',
    total_disc < quantile(total_disc, 0.75) ~ 'medium',
    total_disc >= quantile(total_disc, 0.75) ~ 'high',
    T ~ 'other'
  ))

x <- c(8, 3, -2, 5)
if (x<0) {
  print("x contains a negative number")
}
library(here)
monthly_data_files <- here("data/monthly_data")
list.files(monthly_data_files)
for (data_file in list.files(monthly_data_files)){
  df <- readr::read_csv(here(monthly_data_files, data_file))
  new_name <- stringr::str_sub(data_file, end = -5)
  assign(new_name, df)
}
(data_files <- list.files(monthly_data_files))
as.numeric(stringr::str_extract(data_files, "\\d+"))
for(data_file in data_files) {
  # steps to import each data set
  if(as.numeric(stringr::str_extract(data_file, "\\d+"))>7)
    break
  df <- readr::read_csv(paste0("data/monthly_data/", data_file))
  new_name <- stringr::str_sub(data_file, end = -5)
  assign(new_name, df)
  rm(df)
}

coin <- c("heads", "tails")

# set number of tries to zero
n_tries <- 0

# this will be used to imitate a flip of 10 coins
flip <- NULL

while(length(unique(flip)) != 1) {
  # flip coin 10x
  flip <- sample(coin, 10, replace = TRUE)
  
  # add to the number of tries
  n_tries <- n_tries + 1
}

n_tries

coin <- c("heads", "tails")
n_tries <- 0
repeat {
  # flip coin 10x
  flip <- sample(coin, 10, replace = TRUE)
  
  # add to the number of tries
  n_tries <- n_tries + 1
  
  # if current flip contains all heads or tails break
  if (length(unique(flip)) == 1) {
    print(n_tries)
    break
  }
}

value <- 0
step <- 0

repeat {
  # randomly add or subtract 1
  random_step <- sample(c(-1, 1), 1)
  value <- value + random_step
  
  # count step
  step <- step + 1
  
  # break once our walk exceeds 100
  if (value == 100 | step> 10000) {
    print(step)
    break
  }
}
diamonds %>% 
  split(.$cut) %>% 
  map(~lm(price ~ carat, data = .)) %>%
  map(summary) %>% 
  map_dbl(~.$r.squared)

airquality %>% map_int(~ length(unique(.)))
airquality %>% map_int(~ sum(is.na(.)))
airquality %>% map_dbl(~ (sd(., na.rm = T)))
combined <- data_file %>%
  map_dfr(~readr::read_csv(here(monthly_data_files, data_file)))
combined %>%
  map_df(~class(.))
combined %>%
  map_int(~n_distinct(.))

transactions <- completejourney::transactions_sample
sales <- function(store, week) {
  
  # aggregation function     
  store_sales <- aggregate(
    transactions$sales_value,
    by = list(
      store_id = transactions$store_id,
      week = transactions$week), FUN = "sum"
  )
  
  # get result for relevant store & week
  valid_store <- store_sales$store_id == store
  valid_week <- store_sales$week == week
  result <- store_sales[valid_store & valid_week, 'x']
  
  # round and return result
  round(result, digits = 2)
}
body(sales)
formals(sales)
environment(sales)

ratio <- function(x, y, digits = 2){
  warning("you dare call me?")
  round(x/y, digits = digits)
  message("you'd better not call me next time")
}
ratio(2,3, 5)

rescale <- function(x, digits = 2, na.rm = TRUE){
  if(!is.numeric(x)) {
    stop("don't give me none numeric value")
  }
  if(na.rm == F & sum(is.na(x))>0) {
    warning("NA detected!")
  }
  rng <- range(x, na.rm = na.rm)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
}

set.seed(123)
test_vector <- c(runif(20, min = 25, max = 40), NA)
rescale(test_vector, na.rm = F)

f1 <- function(x, n, verbose = TRUE) {
  status <- NULL
  iterate <- 1:n
  
  # create progress bar
  if(verbose) pb <- txtProgressBar(min = 0, max = length(n), style = 3)
  
  # some iteration in your loop
  for(i in seq_along(iterate)) {
    
    # some functionality
    Sys.sleep(x)
    
    # update progress bar
    if(verbose) setTxtProgressBar(pb, i/n)
  }
}

f1(x = .2, n = 10)
