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
df %>%
  ggplot(aes(x = sales_value, y =retail_disc)) + geom_point() + 
  scale_x_log10(labels = scales::dollar) + 
  scale_y_log10(labels = scales::dollar)
ggplot(df, aes(x = quantity, y = sales_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 50))
ggplot(df, aes(x = household_size)) +
  geom_bar() +
  coord_flip()                                                                              
ggplot(df, aes(x = department)) + geom_bar() + coord_flip()
ggplot(df, aes(x = quantity, y = sales_value)) +
  geom_point() +
  facet_grid(~ marital_status)
ggplot(df, aes(x = quantity, y = sales_value)) +
  geom_point() +
  facet_grid(household_size ~ marital_status)
df %>%
  filter(department == "MEAT") %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point() +
  facet_grid(product_category ~ brand)
hshld_by_basket <- df %>%
  group_by(basket_id, household_size) %>%
  summarize(quantity = sum(quantity), sales_value = sum(sales_value))
ggplot(hshld_by_basket, aes(x = quantity, y = sales_value, color = household_size)) +
  geom_point() +
  labs(title = "Customer Net Spend By Basket",
       subtitle = "Household transaction data covering 2016-2017.",
       x = "Quantity of items in each basket",
       y = "Total net spend per basket ($)",
       color = "Household size")
top_baskets <- hshld_by_basket %>%
  ungroup() %>%
  slice_max(sales_value, n = 10)
top_baskets
ggplot(hshld_by_basket, aes(x = quantity, y = sales_value)) +
  geom_point(aes(color = household_size)) +
  geom_label(data = top_baskets, aes(label = basket_id))

library(ggrepel)

ggplot(hshld_by_basket, aes(x = quantity, y = sales_value)) + 
  geom_point(aes(color = household_size)) +
  geom_label_repel(data = top_baskets, aes(label = basket_id))

# exercise data
hshld_df <- df %>%
  group_by(household_id) %>%
  summarize(
    total_sales = sum(sales_value),
    total_quantity = sum(quantity)
  ) %>%
  mutate(dollar_per_item = total_sales / total_quantity)
hshld_df
top_10 <- hshld_df %>%
  slice_max(dollar_per_item, n =10)
top_10
ggplot(top_10, aes(x = total_sales)) + 
  geom_bar()
ggplot(hshld_df, aes(x = total_quantity, y = total_sales)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "total quantity vs total sales", x = "log of total quantity", y = "log of total sales")

library(stringr)
df %>%
  filter(str_detect(product_type, regex("PIZZA", ignore_case = T))) %>%
  ggplot(aes(x = marital_status)) + geom_bar()
df %>%
  filter(str_detect(product_type, regex("PIZZA", ignore_case = T))) %>%
  ggplot(aes(x = quantity, y = sales_value)) + geom_point() + facet_grid(~product_category)

library(tidyverse)
library(completejourney)
m1 <- c("Aug", "Oct", "Jan", "Mar")
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
month_levels
(my_months <- factor(m1, levels = month_levels))
my_months
sort(my_months)
promotions_sample %>%
  count(display_location)
ggplot(promotions_sample, aes(display_location)) +
  geom_bar()
promotions_sample %>%
  count(display_location) %>%
  filter(n > 5000) %>%
  ggplot(aes(display_location, n)) +
  geom_col()
promotions_sample %>%
  count(display_location)
promotions_sample %>%
  count(display_location) %>%
  filter(n > 5000) %>%
  ggplot(aes(display_location, n)) +
  geom_col() +
  scale_x_discrete(drop = FALSE)
demographics
class(demographics$age)
colors <- c('blue', 'green', 'blue', 'yellow', 'blue', 'green')
factor(colors)
options <- c('blue', 'red', 'yellow', 'green')
factor(colors, levels = options)
sizes <- c('small', 'large', 'large', 'medium', 'small', 'large')
factor(sizes)
options <- c('small', 'medium', 'large')
factor(sizes, levels = options, ordered = TRUE)
options <- c('small', 'medium', 'large')
factor(sizes, levels = options, ordered = TRUE)
class(demographics$home_ownership)
levels(demographics$home_ownership)
demographics %>%
  count(home_ownership)
demographics %>%
  ggplot(aes(x = home_ownership)) + geom_bar()
p1 <- ggplot(demographics, aes(home_ownership)) +
  geom_bar() +
  ggtitle('Original order') +
  coord_flip()
new_levels <- c("Unknown", "Probable Renter", "Probable Homeowner", "Renter", "Homeowner")
p2 <- demographics %>%
  mutate(home_ownership = fct_relevel(home_ownership, new_levels)) %>%
  ggplot(aes(home_ownership)) +
  geom_bar() +
  ggtitle('New order') +
  coord_flip()
gridExtra::grid.arrange(p1, p2, nrow = 1)
spend_by_dept <- transactions_sample %>%
  inner_join(products, by = "product_id") %>%
  group_by(department) %>%
  summarize(total_spend = sum(sales_value))
ggplot(spend_by_dept, aes(total_spend, department)) +
  geom_point()
ggplot(spend_by_dept, aes(total_spend, fct_reorder(department, total_spend))) +
  geom_point()
ggplot(demographics, aes(x = age)) + geom_bar()
demographics %>%
  group_by(age) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = fct_reorder(age, count), y = count)) + geom_bar(stat = "identity")
age_counts <- demographics %>%
  count(age)
spend_by_dept <- mutate(spend_by_dept, department = factor(department))
spend_by_dept %>%
  mutate(department = fct_recode(
    department,
    "STORE SUPPLIES" = "CNTRL/STORE SUP",
    "OVER COUNTER PHARMA" = "DRUG GM"
  )) %>%
  ggplot(aes(total_spend, fct_reorder(department, total_spend))) +
  geom_point()
spend_by_dept %>%
  mutate(department = fct_recode(
    department,
    "MEAT & SEAFOOD" = "MEAT",
    "MEAT & SEAFOOD" = "SEAFOOD"
  ))  
spend_by_dept %>%
  mutate(department = fct_collapse(
    department,
    "MEAT & SEAFOOD" = c("MEAT", "MEAT-PCKGD", "SEAFOOD", "SEAFOOD-PCKGD")
  ))
transactions_sample %>%
  inner_join(products, by = "product_id") %>%
  mutate(department = fct_lump(department, n = 10)) %>%
  group_by(department) %>%
  summarize(total_spend = sum(sales_value)) %>%
  ggplot(aes(total_spend, fct_reorder(department, total_spend))) +
  geom_point()
demographics %>%
  ggplot(aes(x = income)) + geom_bar() 
class(demographics$income)
demographics %>%
  mutate(income = fct_collapse(income, "low income" = c("Under 15K", "15-24K", "25-34K", "35-49K"), "middle income" = c("50-74K", "75-99K", "100-124K"), "upper income" = c("125-149K", "150-174K", "175-199K", "200-249K", "250K+"))) %>%
  ggplot(aes(x = income)) + geom_bar()
products %>%
  filter(product_category == "BREAD") %>%
  mutate(product_type = fct_lump(product_type, n = 10)) %>%
  count(product_type) %>%
  arrange(product_type) %>%
  ggplot(aes(product_type, n)) + geom_bar(stat = "identity") + coord_flip()

library(tidyverse)
library(AmesHousing)
ames <- AmesHousing::make_ames()
ames
pkgs <- c("AmesHousing", "ggridges", "GGally", 
          "glue","gridExtra", "hexbin", "naniar", 
          "treemap", "viridis")
install.packages(pkgs)
glue::glue(
  'row:{dim(ames)[1]}|\n',
  'columns: {dim(ames)[2]}'
)
plot1 = ggplot(ames, aes(x= Sale_Price)) + geom_histogram()
plot1 + scale_x_continuous(labels = scales::dollar)
# Histograms with various bin widths

# Too crude (i.e., under fitting)
p1 <- ggplot(ames, aes(Sale_Price)) +
  geom_histogram(binwidth = 100000) +
  ggtitle("Bin width = $100,000")  

# Less crude
p2 <- ggplot(ames, aes(Sale_Price)) +
  geom_histogram(binwidth = 50000) +
  ggtitle("Bin width = $50,000")

# Just right?
p3 <- ggplot(ames, aes(Sale_Price)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Bin width = $5,000")

# Too flexible (i.e., over fitting)
p4 <- ggplot(ames, aes(Sale_Price)) +
  geom_histogram(binwidth = 1000) +
  ggtitle("Bin width = $1,000")  

# Display plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)
which.max(table(ames$Sale_Price))
ames %>%
  count(cut_width(Sale_Price, width = 5000)) %>%
  arrange(desc(n))
ggplot(ames, aes(Sale_Price)) + 
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::dollar) +
  labs(x= "sale price", title = "histogram of sale price")
p1 <- ggplot(ames, aes(sample = Sale_Price)) + stat_qq()
p2 <- ggplot(ames, aes(sample = log(Sale_Price))) + stat_qq() 
grid.arrange(p1, p2, ncol = 2)       

p1 <- ggplot(ames, aes("var", Sale_Price)) +
  geom_boxplot(outlier.alpha = .25) +
  scale_y_log10(
    labels = scales::dollar,
    breaks = quantile(ames$Sale_Price)
  )

p2 <- ggplot(ames, aes("var", Sale_Price)) +
  geom_point() +
  geom_violin() +
  scale_y_log10(
    labels = scales::dollar,
    breaks = quantile(ames$Sale_Price)
  )

gridExtra::grid.arrange(p1, p2, ncol = 2)

p3 <- ggplot(ames, aes("var", Sale_Price)) + 
  geom_point()
p3 <- p3 + geom_violin()
p3 <- p3 + scale_y_log10()
outliers <- outliers::scores(log(ames$Sale_Price), type = "iqr", lim = 1.5)
stem(ames$Sale_Price[outliers])
sort(ames$Sale_Price[outliers])

p1 <- ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(aes(y = ..density..), size = 1.5) + 
  geom_density(color = "purple2") +
  scale_x_log10(labels = scales::dollar) +
  ggtitle("Illustrates a skewed distribution")

p2 <- ggplot(ames, aes(x = "", y = Sale_Price)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10(labels = scales::dollar) +
  ggtitle("Illustrates a outliers")

p3 <- ggplot(ames, aes(sample = Sale_Price)) +
  geom_qq(alpha = 0.5) +
  ggtitle("Illustrates whether we can assume normality")

ames$Index <- seq_len(nrow(ames))  # add row number as a new column
p4 <- ggplot(ames, aes(x = Index, y = Sale_Price)) +
  geom_line(alpha = 0.5) +
  ggtitle("Illustrates pattern based on index")

grid.arrange(p1, p2, p3, p4, ncol = 2)
ggplot(ames, aes(MS_Zoning)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1))
p1 <- ames %>% 
  count(MS_Zoning) %>%
  ggplot(aes(reorder(MS_Zoning, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = "MS_Zoning", y = "Frequency") +
  ggtitle("Total count")

# Bar chart of proportions
p2 <- ames %>% 
  count(MS_Zoning) %>%
  mutate(pct = n / sum(n)) %>%  # convert to proportions
  ggplot(aes(reorder(MS_Zoning, pct), pct)) +
  geom_col() +
  coord_flip() +  # now x becomes y
  labs(x = "MS_Zoning", y = "Relative frequency") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Percent of whole")

# Dispay both plots side by side
grid.arrange(p1, p2, ncol = 2)
ames %>% 
  count(MS_Zoning) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(pct))
ames %>%
  mutate(MS_Zoning = fct_lump(MS_Zoning, n = 3)) %>%
  count(MS_Zoning) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(MS_Zoning, pct), pct)) + geom_col() + coord_flip()
ames %>%
  mutate(Kitchen_Qual = fct_relevel(Kitchen_Qual, "Poor", "Fair", "Typical", "Good", "Excellent")) %>%
  ggplot(aes(Kitchen_Qual)) + geom_bar()
p1 <- ggplot(ames, aes(Mo_Sold)) + 
  geom_bar() + 
  xlab("Month sold (numeric)")

# Converting Mo_Sold to a factor
p2 <- ggplot(ames, aes(as.factor(Mo_Sold))) + 
  geom_bar() + 
  xlab("Month sold (factor)")

# Display both plots side by side
grid.arrange(p1, p2, nrow = 2)
p1 <- ames %>%  
  count(Neighborhood) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(Neighborhood, pct))) +
  geom_point() +
  labs(x = "Relative frequency", y = "Neighborhood")
p2 <- p1 + geom_segment(aes(x = 0, xend = pct, y = Neighborhood, yend = Neighborhood), size = 0.15)
grid.arrange(p1, p2)
ames %>%
  count(Neighborhood) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(pct)
ames %>%  
  mutate(Neighborhood = fct_lump_prop(Neighborhood, 0.01)) %>%
  count(Neighborhood) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(Neighborhood, pct))) +
  geom_point() +
  labs(x = "Relative frequency", y = "Neighborhood") +
  geom_segment(
    aes(x = 0, xend = pct, y = Neighborhood, yend = Neighborhood),
    size = 0.15
  )
ames %>%
  ggplot(aes(sample = Gr_Liv_Area)) + geom_qq()
ames %>%
  ggplot(aes(x = Gr_Liv_Area)) + geom_boxplot() + coord_flip()
ames %>%
  ggplot(aes(x = Gr_Liv_Area)) + geom_histogram()

ames %>%
  ggplot(aes(sample = Year_Built)) + geom_qq()
ames %>%
  ggplot(aes(x = Year_Built)) + geom_boxplot() + coord_flip()

ames %>%
  ggplot(aes(x = Year_Built)) + geom_histogram()

ames %>%  
  count(Overall_Qual) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(Overall_Qual, pct))) +
  geom_point() +
  labs(x = "Relative frequency", y = "Neighborhood")  
ames %>%  
  count(Overall_Qual) %>%
  mutate(pct = n / sum(n))

ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::dollar)
set1 <- RColorBrewer::brewer.pal(n = 9, name = "Set1")
p1 <- ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = set1[1]) +
  geom_smooth(method = "auto", se = FALSE, color = set1[2])

# Scatter plot with trend lines (log10 scale)
p2 <- ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = set1[1]) +
  geom_smooth(method = "auto", se = FALSE, color = set1[2]) +
  scale_x_log10() +
  scale_y_log10()

# Display plots side by side
grid.arrange(p1, p2, ncol = 2)
# Scatter plot
p1 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.2)

# Scatter plot with 2-D KDE
p2 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.1) +
  stat_density2d(aes(fill = stat(level)), geom = "polygon") +
  viridis::scale_fill_viridis(option = "A") +
  theme(legend.position = "none")

# heat map of hexagonal bin counts
p3 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_hex(bins = 50, show.legend = FALSE) +
  viridis::scale_fill_viridis(option = "D")  # "D" is the default

# Display plots side by side
grid.arrange(p1, p2, p3, ncol = 3)

ames$Bedroom_AbvGr <- as.ordered(ames$Bedroom_AbvGr)
p1 <- ggplot(ames, aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_point(alpha = 0.1) +
  ggtitle("Scatter plot")

# Strip plot (with jittering)
p2 <- ggplot(ames, aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_jitter(alpha = 0.1, width = 0.2) +
  ggtitle("Strip plot")

# Box plots
p3 <- ggplot(ames, aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_boxplot() +
  ggtitle("Box plot")

# Violin plots
p4 <- ggplot(ames, aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_violin() +
  ggtitle("Violin plot")

# Display plots side by side
grid.arrange(p1, p2, p3, p4, nrow = 2)

p1 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual)) + geom_freqpoly()
p2 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual, fill = Overall_Qual)) + geom_density(alpha = 0.15)
grid.arrange(p1, p2, nrow = 2)
ggplot(ames, aes(x = Sale_Price, y = Overall_Qual)) + geom_density_ridges() + scale_x_continuous(labels = scales::dollar)

ames %>%
  mutate(
    Group = ifelse(
      Sale_Price > mean(Sale_Price),
      yes = "Sold above avg.",
      no = "Sold below avg."),
    Kitchen_Qual = fct_relevel(
      Kitchen_Qual, "Poor", "Fair", "Typical", "Good", "Excellent")
    ) %>%
  ggplot(aes(Kitchen_Qual)) + geom_bar() + facet_wrap(~Group)
ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price, color = Central_Air, shape = Central_Air)) +
  geom_point(alpha = 0.3) +
  theme(legend.position = "top") +
  facet_wrap(~Central_Air)

ames %>%
  mutate(Central_Air = fct_recode(
    Central_Air, 
    "With A/C" = "Y",
    "Without A/C" = "N"
  )) %>%
  ggplot(aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_hex(bins = 50, show.legend = FALSE) +
  facet_wrap(~Central_Air)  +
  viridis::scale_fill_viridis(option = "D")

ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) + 
  facet_wrap(~House_Style, nrow = 2) +
  theme_bw()
ggplot(ames, aes(Gr_Liv_Area, Sale_Price, 
                 color = Central_Air, shape = Central_Air)) +
  geom_point(alpha = 0.2) +
  geom_density2d(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~ House_Style, nrow = 2) +
  ggtitle("Sale Price vs. Above Ground Sq.Ft",
          subtitle = "How does central air and house style influence this relationship?") +
  theme_bw()

variables <- c("Sale_Price", "Year_Built", "Year_Remod_Add", "Overall_Qual")

# Parallel coordinate plot
ames %>%
  select(variables) %>%
  GGally::ggparcoord(alpha = 0.05, scale = "center")
ames %>%
  select(variables) %>%
  mutate(Above_Avg = Sale_Price > mean(Sale_Price)) %>%
  GGally::ggparcoord(
    alpha = 0.05,
    scale = "center",
    columns = 1:4,
    groupColumn = "Above_Avg"
  )
ames2 <- ames %>%
  mutate(
    Above_Avg = Sale_Price > mean(Sale_Price),
    Garage_Type = abbreviate(Garage_Type),
    Garage_Qual = abbreviate(Garage_Qual)
  )

par(mfrow = c(1, 2))
mosaicplot(Above_Avg ~ Garage_Type, data = ames2, las = 1)
mosaicplot(Above_Avg ~ Garage_Type + Garage_Cars, data = ames2, las = 1)
ames %>% 
  mutate(Above_Below = ifelse(Sale_Price > mean(Sale_Price), "Above Avg", "Below Avg")) %>%
  count(Garage_Type, Garage_Cars, Above_Below) %>%
  treemap::treemap(
    index = c("Above_Below", "Garage_Type", "Garage_Cars"),
    vSize = "n"
  )
ames %>%
  select_if(is.numeric) %>%          # select all the numeric columns
  cor() %>%                          # compute the correlation matrix
  heatmap(                           
    symm = TRUE,                     # since correlation matrices are symmetric!
    col = viridis::inferno(nrow(ames))
  )  
ames %>%
  select(Sale_Price, contains("sf")) %>%  # select column names containing "sf"
  GGally::ggpairs()

variables <- c("Overall_Qual", "Kitchen_Qual", "Bsmt_Qual", "Extra_Qual")

# Parallel coordinate plot
ames %>%
  select(variables) %>%
  GGally::ggparcoord(alpha = 0.05, scale = "center")

ames_raw <- AmesHousing::ames_raw
sum(is.na(ames_raw))
library(naniar)
gg_miss_var(ames_raw)
vis_miss(ames_raw)
ames_raw %>% 
  filter(is.na(`Garage Type`)) %>% 
  select(contains("garage"))
ames_raw %>%
  gg_miss_upset(nsets = 10)
flights <- nycflights13::flights
flights
gg_miss_var(flights)
sum(is.ns(flights))
colSums(is.na(flights))
gg_miss_upset(flights)
library(completejourney)
joined <- transactions_sample %>%
  inner_join(products) %>%
  inner_join(demographics)
joined
sum(is.na(joined))
p1 <- ggplot(joined, aes(sample = sales_value)) + geom_qq()
p2 <- ggplot(joined, aes(sample = quantity)) + geom_qq()
p3 <- ggplot(joined, aes(sample = retail_disc)) + geom_qq()
p4 <- ggplot(joined, aes(sample = coupon_disc)) + geom_qq()
grid.arrange(p1, p2, p3, p4)

p1 <- ggplot(joined, aes(x = sales_value)) + geom_histogram()
p2 <- ggplot(joined, aes(x = quantity)) + geom_histogram()
p3 <- ggplot(joined, aes(x = retail_disc)) + geom_histogram()
p4 <- ggplot(joined, aes(x = coupon_disc)) + geom_histogram()
grid.arrange(p1, p2, p3, p4)
joined %>%
  count(department) %>%
  ggplot(aes(reorder(department, n), n)) + geom_col() + coord_flip()
joined %>%
  select(c("sales_value", "quantity", "department", "brand")) %>%
  GGally::ggparcoord(
    alpha = 0.2,
    columns = 1:3,
    groupColumn = "brand"
  )
joined %>%
  select(c("sales_value", "quantity", "department", "brand")) %>%
  GGally::ggpairs()
joined %>%
  ggplot(aes(x = sales_value, y = department)) + geom_boxplot()
