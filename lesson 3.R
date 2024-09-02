df %>% filter(
  store_id =="309",
  household_id == '1167',
  quantity > 4 | sales_value > 10
)

filter(df, store_id =="309" | store_id == "400")
filter(df, store_id %in% c('309', '400'))

df %>% distinct() 
df %>% sample_frac(size = 0.5, replace = FALSE)
df %>% sample_n(size = 10, replace = TRUE)
df %>% slice(3:5)
df %>% top_n(n = 5, wt = sales_value)

df %>%
  filter(store_id == '309', quantity > 1) %>%
  group_by(household_id) %>%
  summarize(quantity = sum(quantity)) %>%
  arrange(desc(quantity))

mutate(df, spend_per_item = sales_value / quantity)

mutate(df, total_disc = coupon_disc + retail_disc + coupon_match_disc) %>%
  mutate(df, disc_to_sales = total_disc / sales_value) %>%
  mutate(bins = ntile(disc_to_sales, 10)) %>%
  arrange(desc(bins)) %>%
  select(bins, everything())

df %>%
  group_by(store_id, product_id) %>%
  summarize(total = sum(sales_value)) %>%
  mutate(rank = min_rank(desc(total))) %>%
  filter(rank <= 5) %>%
  arrange(desc(total))

df %>%
  group_by(store_id, week) %>%
  summarize(sum_quantity = sum(quantity)) %>%
  mutate(lag = lag(sum_quantity)) %>%
  arrange(desc(lag))

df %>%
  filter(quantity > 1) %>%
  group_by(store_id) %>%
  summarise(avg_sales = mean(sales_value)) %>%
  arrange(desc(avg_sales))

df %>%
  filter(store_id == "367") %>%
  lm(sales_value ~ week + retail_disc, data = .) %>%
  summary()

library(ggplot2)
df %>%
  filter(store_id == "367", week <= 10) %>%
  ggplot(aes(x = factor(week), y = sales_value)) +
  geom_jitter(width = .05, alpha = .4) +
  geom_boxplot(alpha = .1)
