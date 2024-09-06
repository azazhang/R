library(ggplot2)
library(completejourney)
library(dplyr)
df <- transactions_sample %>%
  inner_join(products) %>%
  inner_join(demographics) 
glimpse(df)

ggplot(df)
ggplot(df, aes(x = quantity, y = sales_value))
ggplot(df, aes(x = quantity, y =sales_value)) + geom_point()
df %>%
  filter(quantity > 100) %>%  
  ggplot(aes(x = quantity, y = sales_value)) + 
  geom_point()
df %>%
  filter(department == "PRODUCE") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point()

df %>%
  filter(department == "MEAT") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point()
df %>%
  filter(department == "FUEL") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point()
df %>%
  ggplot(aes(x = quantity, y = sales_value, color = household_size)) + 
  geom_point()
ggplot(df, aes(x = quantity, y = sales_value)) + geom_point(color = "blue")

df %>%
  filter(department == "GROCERY") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point()
df %>%
  filter(department == "GROCERY") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point(color = "red")
df %>%
  filter(department == "GROCERY") %>%
  ggplot(aes(x = quantity, y = sales_value, color = household_size)) + geom_point()
df %>%
  filter(department == "GROCERY") %>%
  ggplot(aes(x = quantity, y = sales_value, color = household_size)) + geom_point(size = 0.5)
df %>%
  filter(department == "GROCERY") %>%
  ggplot(aes(x = quantity, y = sales_value, size = household_size)) + geom_point()
ggplot(df, aes(x = quantity, y = sales_value)) + geom_point()
ggplot(df, aes(x = quantity, y = sales_value)) + geom_smooth()
ggplot(df, aes(x = household_size)) + geom_bar()
ggplot(df, aes(x = sales_value)) + geom_histogram()
ggplot(df, aes(x = quantity, y = sales_value)) + geom_point(color = "blue") + geom_smooth(color = "red")
ggplot(df, aes(x = quantity, y = sales_value, color = household_size)) + geom_point() + geom_smooth(se = F)
ggplot(df, aes(x = quantity, y = sales_value)) + geom_point(aes(color = household_size)) + geom_smooth(se = F)
df %>%
  filter(department == "PRODUCE") %>%
  ggplot(aes(x = household_comp, y = sales_value)) + geom_boxplot()
df %>%
  filter(department == "PRODUCE") %>%
  ggplot(aes(x = sales_value, y = quantity, color = household_comp)) + geom_point()
df %>%
  ggplot(aes(x = retail_disc)) + geom_histogram()
ggplot(df, aes(x = household_size)) + geom_bar()
household_count <- count(df, household_size)
household_count
ggplot(household_count, aes(x = household_size, y = n)) + geom_bar(stat = "identity")
df %>%
  group_by(income, basket_id) %>%
  summarize(total_sales = sum(sales_value)) %>%
  ggplot(aes(income, total_sales)) + 
  geom_point(color = "blue") +
  stat_summary(fun = "median", geom = "point", color = "red", size = 2)
df %>%
  filter(department == "MEAT") %>%
  ggplot(aes(x = product_category))+ geom_bar()
product_category_count <- df %>%
  filter(department == "MEAT") %>%
  count(product_category)
ggplot(product_category_count, aes(x = product_category, y = n)) + geom_count(aes(size = n))  
df %>%
  ggplot(aes(x = sales_value, y = retail_disc)) + geom_point()
df %>%
  ggplot(aes(x = sales_value, y = retail_disc)) + 
  stat_summary_bin(fun = "mean", geom = "bar", orientation = 'y')
ggplot(df, aes(x = household_size, fill = brand)) + geom_bar()
ggplot(df, aes(x = household_size, fill = brand)) + geom_bar(position = "dodge")
ggplot(df, aes(x = household_size, fill = brand)) + geom_bar(position = "dodge")
ggplot(df, aes(x = household_size, fill = brand)) + geom_bar(position = "fill")
ggplot(df, aes(x = week)) + geom_bar()
ggplot(df, aes(x = week, fill = brand)) + geom_bar(position = "fill")
ggplot(df, aes(x = quantity, y= sales_value, color = household_size)) + geom_point()
# same as above, with explicit scales
ggplot(df, aes(x = quantity, y = sales_value, color = household_size)) +
  geom_point()
scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()
ggplot(df, aes(x = household_size, fill = brand)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, by = .2), labels = scales::percent)
ggplot(df, aes(x = quantity, y = sales_value, color = household_size)) +
  geom_point() +
  scale_color_brewer()
ggplot(df, aes(x = quantity, y = sales_value, color = household_size)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr")
df %>%
  ggplot(aes(x = sales_value, y = quantity)) + geom_point() + 
  scale_x_log10()
