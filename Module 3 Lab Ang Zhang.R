# Module 3 lab
# author: Ang Zhang
library(tidyverse)
library(completejourney)
transactions <- transactions_sample
transactions

products
transactions <- transactions %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc)/quantity, 
         loyalty_price = (sales_value +coupon_match_disc)/quantity,
         coupon_price = (sales_value - coupon_disc)/quantity
         ) %>%
  select(regular_price, loyalty_price, coupon_price, product_id, everything())
transactions %>%
  slice_max(order_by = loyalty_price, n = 5)
transactions %>%
  filter(quantity > 0) %>%
  slice_max(order_by = loyalty_price, n = 5)
  .[1, "household_id"]
product_1 <- transactions %>%
  filter(quantity > 0) %>%
  slice_max(order_by = loyalty_price, n = 5) %>%
  .[1, "product_id"]
products %>%
  filter(product_id == as.vector(product_1))

transactions %>%
  filter(regular_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

transactions %>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

transactions %>%
  filter(coupon_price <=1 ) %>%
  select(product_id) %>%
  n_distinct()
transactions
by_basket <- group_by(transactions, basket_id)
basket_sales <- summarize(by_basket, basket_sales = sum(sales_value))
sales_over_10 <- filter(basket_sales, basket_sales > 10)
sales_over_20 <- filter(basket_sales, basket_sales > 20)
count(sales_over_20) / count(basket_sales)
count(sales_over_10) / count(basket_sales)

transactions %>%
  group_by(store_id) %>%
  summarize(total_sales_value = sum(sales_value, rm.na = TRUE)) %>%
  arrange(desc(total_sales_value))

transactions %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize(avg_pct_loyalty_disc = mean(pct_loyalty_disc, na.rm = TRUE)) %>%
  arrange(desc(avg_pct_loyalty_disc))

library(readxl)
excel_sheets(path = "mbta.xlsx")
mbta <- read_excel(path = "mbta.xlsx", skip = 1, na = 'NA')
head(mbta, 6)
#summarize(mbta,sum(is.na()))
colSums(is.na(mbta))

mbta <- mbta %>%
  slice(-c(1, 7, 11)) %>%
  select(-c(1)) 
dim(mbta)

mbta <- mbta %>%
  pivot_longer(cols = -c("mode"), names_to = 'date', values_to = 'thou_riders')
dim(mbta)

mbta <- mbta %>%
  separate(col = date, into = c("year", "month"), sep = "-")

filter(mbta, thou_riders == "40", mode =="Boat")
mbta <- mbta %>%
  mutate(thou_riders = replace(thou_riders, mode =="Boat" & thou_riders == "40", 4))

mbta %>%
  group_by(mode) %>%
  summarize(avg_ridership = mean(thou_riders))
mbta %>%
  filter(month == "01") %>%
  group_by(mode) %>%
  summarize(avg_ridership = format(mean(thou_riders), nsmall = 2))
mbta %>%
  filter(mode == "Boat") %>%
  group_by(year) %>%
  summarize(total_rider = sum(thou_riders)) %>%
  arrange(desc(total_rider))

mbta %>%
  filter(mode =="Heavy Rail") %>%
  group_by(month) %>%
  summarize(avg_rider = mean(thou_riders)) %>%
  arrange(desc(avg_rider))
