library(tidyverse)
library(ggplot2)
library(janitor)
library(plotly)
library(ggthemes)
library(patchwork) 

#load data
df <- read_csv("heart_2020_cleaned.csv")

#clean data
df <- df %>%
  clean_names()
df <- tibble(df)

#check missing value 
sum(is.na(data))

#delete missing value 
data <- na.omit(data)

glimpse(df)

nrow(df)
ncol(df)

#change character to factor
df$heart_disease <- as.factor(df$heart_disease)
df$smoking <- as.factor(df$smoking)
df$alcohol_drinking <- as.factor(df$alcohol_drinking)
df$stroke <- as.factor(df$stroke)
df$diff_walking <- as.factor(df$diff_walking)
df$sex <- as.factor(df$sex)
df$diabetic <- as.factor(df$diabetic)
df$physical_activity <- as.factor(df$physical_activity)
df$gen_health <- as.factor(df$gen_health)
df$asthma <- as.factor(df$asthma)
df$kidney_disease <- as.factor(df$kidney_disease)
df$skin_cancer <- as.factor(df$skin_cancer)
df$race <- as.factor(df$race)

d_count <- df %>%
  count(race) %>%
  arrange(desc(n))

#histogram 
hist(df$bmi)
mean(df$bmi)
median(df$bmi)

#barplot
barplot(table(df$sleep_time))

#boxplot
boxplot(df$sleep_time)
fivenum(df$sleep_time)

#five number summary
min(df$sleep_time)
max(df$sleep_time)
quantile(df$sleep_time, probs = c(.25, .5, .75))

#whiske
q3 <- quantile(df$sleep_time, probs = .75)
q1 <- quantile(df$sleep_time, probs = .25)
IQR_sleep_time <- q3 - q1


q3 + 1.5*IQR_sleep_time
q1 - 1.5*IQR_sleep_time

boxplot.stats(df$sleep_time, coef = 1.5)

#outliers
df_no_outliers <- df %>%
  filter(sleep_time <= 10 & sleep_time >= 3)

boxplot(df_no_outliers$sleep_time)

#boxplot 2 variables
boxplot(bmi ~ age_category, data = df)

#ggplot
#scatter plot
car <- read_csv("car.csv")
sum(is.na(car))
car <- na.omit(car)

car$transmission <- as.factor(car$transmission)
car$fuel <- as.factor(car$fuel)
car$owner <- as.factor(car$owner)
car$seller_type <- as.factor(car$seller_type)
car$mileage_kmpl <- as.numeric(car$mileage_kmpl)

plot(car$max_power_bhp, car$engine_CC, 
     pch = 16, col = "salmon")

cor(car$max_power_bhp, car$engine_CC)
lm(engine_CC ~ max_power_bhp, data = car)

ggplot(data = car, mapping = aes(x = max_power_bhp,
                                 y = engine_CC)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = car, mapping = aes(x = max_power_bhp,
                                 y = engine_CC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_rug()

#scatter plot 

ggplot(car, aes(x = max_power_bhp,
                y = engine_CC)) +
  geom_point(size = 2, 
             col = "salmon",
             alpha = 0.4)

ggplot(car, aes(x = mileage_kmpl,
                y = engine_CC)) +
  geom_point(size = 2, 
             col = "red",
             alpha = 0.4)

# histogram
ggplot(car, aes(max_power_bhp)) +
  geom_histogram(bins = 10,
                 fill = "blue",
                 alpha = 0.5) 

car %>%
  count(max_power_bhp) %>%
  arrange(desc(max_power_bhp))

#bar plot
ggplot(car, aes((owner))) +
  geom_bar(fill = "blue",
           alpha = 0.5)

ggplot(car, aes((seller_type))) +
  geom_bar(fill = "blue",
           alpha = 0.5)

#mapping/setting
#mapping fill barplot
ggplot(car, aes(owner, fill = owner)) +
  geom_bar()

#fill
ggplot(car, aes(owner, fill = transmission)) +
  geom_bar()

ggplot(car, aes(fuel, fill = transmission)) +
  geom_bar(position = "dodge")

ggplot(car, aes(fuel, fill = transmission)) +
  geom_bar(position = "fill")

#setting
ggplot(car, aes(max_power_bhp, engine_CC)) +
  geom_point(color = "blue4",
             alpha = 0.4, 
             size = 2)

ggplot(car, aes(max_power_bhp, engine_CC)) +
  geom_point(color = "red",
             alpha = 0.6, 
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "black")

#mapping and setting 
ggplot(car, aes(max_power_bhp, engine_CC, 
                color = fuel)) +
  geom_point(alpha = 0.4, size = 2)

###dplyr
car %>%
  count(fuel) %>%
  ggplot(., aes(reorder(fuel, -n), n)) +
  geom_col()

car %>%
  group_by(owner) %>%
  summarise(avg_price = mean(selling_price)) %>%
  ggplot(., aes(reorder(owner, -avg_price), avg_price)) +
  geom_col(fill = "salmon", color = "black") +
  theme_minimal()

#plotly 
df <- ggplot(small_car, aes(max_power_bhp, engine_CC)) +
  geom_jitter() +
  geom_smooth(method = "lm", 
              se = F) +
  theme_minimal()

ggplotly(df)

#sample data
set.seed(42)
small_car <- car %>%
  sample_n(5000)
ggplot(small_car, aes(max_power_bhp, engine_CC)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_fivethirtyeight()

#label
set.seed(42)
small_car <- sample_n(car, 5000)
ggplot(small_car, aes(max_power_bhp, engine_CC, 
                      col = fuel, 
                      shape = transmission)) +
  geom_point(size = 3, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Relationship power and engine by color",
       x = "Power", 
       y = "Engine",
       caption = "kaggle.com")

#patchwork 
p1 <- qplot(x = year, data = car, geom = "histogram", bins = 20)
p2 <- qplot(x = engine_CC, data = car, geom = "density")
p3 <- qplot(max_power_bhp, engine_CC, data = car, geom = "point")
p4 <- qplot(max_power_bhp, engine_CC, data = car, geom = "smooth")

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

#facet
set.seed(42)
df <- sample_n(car, 5000)
ggplot(df, aes(mileage_kmpl, engine_CC)) +
  geom_point() +
  facet_wrap(~ fuel)

set.seed(42)
df <- sample_n(car, 5000)
ggplot(df, aes(max_power_bhp, selling_price)) +
  geom_point() +
  facet_wrap(~ seller_type,
             nrow = 2)

set.seed(42)
df <- sample_n(car, 5000)
ggplot(df, aes(year, km_driven,
               col = owner)) +
  geom_point(size = 2,
             alpha = 0.4) +
  facet_wrap(~ seller_type) +
  theme_minimal()


















