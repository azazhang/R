transactions_sample %>%
  left_join(products, by = "product_id") %>%
  filter(department == 'MEAT') %>%
  group_by(product_category) %>%
  summarize(total_spend = sum(`sales_value`)) %>%
  arrange(desc(total_spend))

transactions_sample %>%
  left_join(demographics, by = "household_id") %>%
  group_by(age) %>%
  summarize(sales_by_age = sum(`sales_value`)) %>%
  arrange(desc(sales_by_age))

joined <- transactions_sample %>%
  right_join(coupons) %>%
  right_join(coupon_redemptions)
joined

transactions_sample %>%
  left_join(demographics, by = 'household_id') %>%
  group_by(income) %>%
  summarize(quantity_by_income = sum(quantity)) %>%
  arrange(desc(quantity_by_income))

transactions_sample %>%
  group_by(household_id) %>%
  summarize(sales_by_household = sum(sales_value)) %>%
  filter(sales_by_household > 100) %>%
  left_join(demographics, by = "household_id") %>%
  summarise(n())

transactions_sample %>%
  group_by(household_id) %>%
  summarize(sales_by_household = sum(sales_value)) %>%
  filter(sales_by_household > 100) %>%
  anti_join(demographics, by = "household_id") %>%
  summarise(n())

transactions_sample %>%
  group_by(product_id) %>%
  summarize(sales_by_product = sum(sales_value)) %>%
  semi_join(promotions_sample, by = "product_id") %>%
  filter(display_location)
  
  summarise(n())
  
library(dplyr)
library(stringr)
products
head(products$product_category)
str_to_lower(products$product_category) %>% head()
str_to_upper(products$product_category) 
products %>% mutate(product_category = str_to_lower(product_category))
products$product_category
str_count(products$product_category)
products %>%
  mutate(category_length = str_count(product_category)) %>%
  select(product_category, category_length)
products %>%
  filter(str_count(product_category) > mean(str_count(product_category), na.rm = T))

products %>%
  mutate(product_type_length = str_count(product_type)) %>%
  select(product_type, product_type_length, everything()) %>%
  arrange(product_type_length) %>%
  arrange(desc(product_type_length))

products %>%
  mutate(product_id_start = str_sub(product_id, start = 1, end = 3)) %>%
  filter(product_id_start == "222")
str_detect(products$product_category, "FRUIT")

products %>% filter(str_detect(product_category, "FRUIT"))

str_count(products$product_category, "FRZN") %>% print()
str_extract(products$product_category, "FRZN") %>% head()
class(str_locate(products$product_category, "FRZN"))
str_locate(products$product_category, "FRZN")[1,1]
str_replace_all(products$product_category, "FRZN", "Frozen")

products %>%
  filter(str_detect(product_category, regex("summer|fall", ignore_case = T)))

products %>%
  filter(str_detect(product_category, regex("^fruit", ignore_case = T)))
products %>%
  filter(str_detect(product_category, regex("fruit$", ignore_case = T)))
products %>%
  filter(str_detect(product_category, regex("^fruit|fruit$", ignore_case = T)))

products %>% 
  filter(str_detect(product_category, regex(".", ignore_case = TRUE)))
products %>% 
  filter(str_detect(product_category, regex("\\.", ignore_case = TRUE)))

products %>% 
  select(product_type, package_size) %>%
  filter(str_detect(package_size, regex("\\.[0-9] oz", ignore_case = TRUE)))

products %>% 
  select(product_type, package_size) %>%
  filter(str_detect(package_size, regex("\\.[0-9] (oz|lb)", ignore_case = TRUE)))

products %>% 
  select(product_type, package_size) %>%
  filter(str_detect(package_size, regex("\\.[0-4] (oz|lb)", ignore_case = TRUE)))

products %>%
  filter(str_detect(package_size, regex("^\\d")))

products %>%
  filter(str_detect(package_size, regex("^\\D")))
products %>%
  filter(str_detect(package_size, regex("[[:punct:]]")))

products %>%
  select(product_category, package_size) %>%
  filter(str_detect(package_size, regex("\\d{3,}")))

products %>%
  filter(str_detect(package_size, regex("^\\d{2,}(\\.)?.*lb", ignore_case = TRUE))) %>%
  inner_join(transactions_sample, by = "product_id") %>%
  group_by(product_type) %>%
  summarize(total_sales = sum(sales_value)) %>%
  slice_max(total_sales, n = 5)

products %>%
  filter(str_detect(product_type, regex("bulk", ignore_case = T))) %>%
  print(width = Inf)

products %>%
  filter(str_detect(package_size, regex("[^[:punct:]]")))

products %>%
  filter(str_detect(product_type, regex("pizza", ignore_case = T)))

transactions_sample %>%
  right_join(., filter(products, str_detect(product_type, regex("pizz", ignore_case = T))), by = "product_id") %>%
  group_by(product_id) %>%
  summarize(sales_by_product = sum(sales_value)) %>%
  arrange(desc(sales_by_product))

product_filter <- products %>%
  filter(str_detect(product_category, regex("pizza", ignore_case = T))
         & str_detect(product_type, regex("snake|appetizer", ignore_case = T))
  ) 
product_filter
transactions_sample %>%
  right_join(product_filter, by = "product_id") %>%
  group_by(product_id) %>%
  summarize(quantity_by_product = sum(quantity)) %>%
  arrange(desc(quantity_by_product))

products %>%
  filter(str_detect(package_size, regex("\\d"), negate = T)) %>%
  print()

library(tidyverse)
library(lubridate)
library(completejourney)
glimpse(transactions_sample)
Sys.time()
Sys.timezone()
Sys.Date()
today()
now()
timestamp()

non_dates <- transactions_sample %>%
  mutate(date = as.character(transaction_timestamp))
glimpse(non_dates)
non_dates %>%
  mutate(date = ymd_hms(date)) %>%
  glimpse()
library(nycflights13)
flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year = year, month = month, day = day,
                                   hour = hour, min = minute))
  
transactions_sample %>%
  filter(month(transaction_timestamp) == 12) %>%
  count(day(transaction_timestamp))
wday("1989-02-28", label = T)
transactions_sample %>%
  mutate(wday = wday(transaction_timestamp)) %>%
  group_by(wday) %>%
  summarize(sales_wday = sum(sales_value)) %>%
  arrange(desc(sales_wday))

x <- today()

y <- ymd("2023-08-08")

x>y
x-y
class(x)
class(y)
diff <- x-y
class(diff)
print(diff)
x <- ymd_hm("2018-06-01 12:10")
y <- ymd_hm("2016-03-21 13:54")
z <- ymd_hm("2018-05-21 13:54")
diff <- x - y
class(diff)
as.duration(x - y)
as.duration(x - z)
transactions_sample %>%
  group_by(household_id) %>%
  summarize(start = min(transaction_timestamp), end = max(transaction_timestamp)) %>%
  mutate(diff = as.duration(end - start)) %>%
  arrange(desc(diff))
dseconds(55)
dminutes(55)
transactions_sample %>%
  group_by(household_id) %>%
  summarize(first = min(transaction_timestamp), last = max(transaction_timestamp)) %>%
  mutate(diff = as.duration(last - first)) %>%
  filter(diff >= dweeks(25)) %>%
  print()
one_am <- ymd_hms("2018-03-11 01:00:00", tz = "America/New_York")
one_am
one_am + dhours(1)-1
one_am + hours(1)

last_transaction_date <- max(transactions_sample$transaction_timestamp)
last_transaction_date
transactions_sample %>%
  group_by(household_id) %>%
  summarize(last_transaction = max(transaction_timestamp)) %>%
  print() %>%
  filter(last_transaction < last_transaction_date - dmonths(3))
transactions_sample %>%
  filter(as.Date(transaction_timestamp) == as.Date("2017-11-23")) %>%
  summarize(format(sum(sales_value), nsmall = 2))

transactions_sample %>%
  mutate(time_of_day = hms::as_hms(transaction_timestamp)) %>%
  summarize(average_time = hms::as_hms(mean(as.numeric(time_of_day)))) %>%
  arrange(time_of_day)

transactions_sample %>%
  mutate(time_of_day = hms::as_hms(transaction_timestamp)) %>%
  arrange(time_of_day) %>%
  select(household_id, time_of_day, everything()) %>%
  print() %>%
  arrange(desc(time_of_day)) %>%
  print()

            