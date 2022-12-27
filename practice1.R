a <- 0
if (a != 0) {
  print(1 / a)
} else {
 print('No reciprocal for 0.')
}

library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000

ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5) {
  print(murders$state[ind])
} else {
  print('No state has murder rate that low')
}

a <- 0
ifelse(a > 0, 1 / a, NA)

a <- c(0, 1, 2, -4, 5)
result <- ifelse(a > 0, 1 / a, NA)

tmp <- data.frame(a = a, is_a_positive = a > 0, answer1 = 1/a, answer2 = NA, result = result)
knitr::kable(tmp)

data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

avg <- function(x) {
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
identical(mean(x), avg(x))

s <- 3
avg(1:10)
s

avg <- function(x, arithmetic = TRUE) {
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}

m <- 25
s_n <- vector(length = m)
for(n in 1:m) {
  s_n[n] <- compute_s_n(n)
}

n <- 1:m
plot(n, s_n)

n <- 1:25
compute_s_n(n)

x <- 1:10
sapply(x, sqrt)

n <- 1:25
s_n <- sapply(n, compute_s_n)

library(tidyverse)

murders <- mutate(murders, rate = total / population * 100000)
head(murders)

filter(murders, rate <= 0.71)

new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

print(filter(murders, rank(rate) > length(rate) - 5))

murders %>% select(state, region, rate) %>%
  filter(rate <= 0.71)

16 %>% sqrt()
16 %>% sqrt() %>% log2()
16 %>% sqrt() %>% log(base = 2)

data(heights)

s <- heights %>%
  filter(sex == 'Female') %>%
  summarize(average = mean(height),
            stanard_deviation = sd(height))
s

heights %>%
  filter(sex == 'Female') %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))

murders %>% mutate(rate = total/population*100000) %>%
  summarize(avg_rate = mean(rate))

us_murder_rate <- murders %>%
  summarize(avg_rate = sum(total) / sum(population) * 100000)
us_murder_rate

class(us_murder_rate)

us_murder_rate %>% pull(avg_rate)

heights %>% group_by(sex) %>% head(6)

heights %>%
  group_by(sex) %>%
  summarize(average = mean(height),
            standard_deviation = sd(height))

murders %>%
  group_by(region) %>%
  summarize(median_rate = median(rate))

murders %>%
  arrange(population) %>%
  head()

murders %>%
  arrange(rate) %>%
  head()

murders %>%
  arrange(desc(rate)) %>%
  head()

murders %>%
  arrange(region, rate) %>%
  head()

murders %>% top_n(5, rate)

murders %>% group_by(region) %>% head(5)

murders %>% group_by(region) %>% class()

class(murders[,4])

class(as_tibble(murders[,4]))

tibble(id = c(1, 2, 3), func = c(mean, median, sd))

grades <- tibble(names = c('John', 'Juan', 'Jean', 'Yeo'),
                 exam_1 = c(95, 80, 90, 85),
                 exam_2 = c(95, 85, 85, 90))

grades <- data.frame(names = c('John', 'Juan', 'Jean', 'Yeo'),
                 exam_1 = c(95, 80, 90, 85),
                 exam_2 = c(95, 85, 85, 90), stringsAsFactors = TRUE)

as_tibble(grades) %>% class()

filter(murders, region == 'South') %>%
  mutate(rate = total / population * 10^5) %>%
  summarize(median = median(rate)) %>%
  pull(median)

rates <- filter(murders, region == 'South') %>%
  mutate(rate = total / population * 10^5) %>%
  .$rate
median(rates)

compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}
n <- 1:25
s_n <- sapply(n, compute_s_n)

library(purrr)
s_n <- map(n, compute_s_n)
class(s_n)

s_n <- map_dbl(n, compute_s_n)
class(s_n)

s_n <- map_df(n, compute_s_n)

compute_s_n <- function(n) {
  x <- 1:n
  tibble(sum = sum(x))
}
s_n <- map_df(n, compute_s_n) 

x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ 'Negative', x > 0 ~ 'Positive', TRUE ~ 'Zero')

murders %>%
  mutate(group = case_when(
    abb %in% c('ME', 'NH', 'VT', 'MA', 'RI', 'CT') ~ 'New England',
    abb %in% c('WA', 'OR', 'CA') ~ 'West Coast',
    region == 'South' ~ 'South',
    TRUE ~ 'Other')) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5)

x >= 0 & x <= 1

between(x, 0, 1)

filename <- 'murders.csv'
dir <- system.file('extdata', package = 'dslabs')
fullpath <- file.path(dir, filename)
file.copy(fullpath, 'murders.csv')

dat <- read_csv(filename)

system.file(package = 'dslabs')

dir <- system.file(package = 'dslabs')
list.files(path = dir)

dir <- system.file(package = 'dslabs')
filename %in% list.files(file.path(dir, 'extdata'))

dir <- system.file('extdata', package = 'dslabs')
fullpath <- file.path(dir, filename)

file.copy(fullpath, 'murders.csv')

list.files()

filename <- 'murders.csv'
dir <- system.file('extdata', package = 'dslabs')
fullpath <- file.path(dir, filename)
file.copy(fullpath, 'murders.csv')

library(readr)

read_lines('murders.csv', n_max = 3)

dat <- read_csv(filename)

dat <- read_csv(fullpath)

library(readxl)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/"
dat <- read_csv(url)

download.file(url, 'murders_2.csv')

dat2 <- read.csv(filename)

class(dat2$abb)
class(dat2$region)

dat <- read.csv('murders.csv', stringsAsFactors = TRUE)
class(dat$state)

path <- system.file('extdata', package = 'dslabs')
filename <- 'murders.csv'
x <- scan(file.path(path, filename), sep = ',', what = 'c')
x[1:10]

library(ggplot2)

ggplot(murders, aes(x = population/10^6, y = total)) +
  geom_point()

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total)) 

p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

p_test <- p + geom_text(aes(population/10^6, total, label = abb))

p_test <- p + geom_text(aes(population/10^6, total), label = abb) # error

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = 'Hello there!'))

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')

p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() + scale_y_log10() +
  xlab('Populations in millions (log scale)') +
  ylab('Total number of murders (log scale)') +
  ggtitle('US Gun Murders in 2010')
p + geom_point(size = 3, color = 'blue')

p + geom_point(aes(col = region), size = 3)

r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))

library(ggthemes)
library(ggrepel)

r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r), lty = 2, color = 'darkgrey') +
  geom_text_repel() +
  scale_x_log10() + scale_y_log10() +
  xlab('Populations in millions (log scale)') +
  ylab('Total number of murders (log scale)') +
  ggtitle('US Gun Murders in 2010') +
  scale_color_discrete(name = 'Region') +
  theme_economist()

x <- log10(murders$population)
y <- murders$total
qplot(x, y)

library(gridExtra)
p1 <- qplot(x)
p2 <- qplot(x, y)
grid.arrange(p1, p2, ncol = 2)

murders %>% group_by(region) %>%
  summarize(n = n()) %>%
  mutate(Proportion = n / sum(n),
         region = reorder(region, Proportion)) %>%
  ggplot(aes(x = region, y = Proportion, fill = region)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  xlab('')

ds_theme_set()
heights %>% filter(sex == 'Male') %>%
  ggplot(aes(height)) + stat_ecdf() +
  ylab('F(a)') + xlab('a')

heights %>% 
  filter(sex == 'Male') %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, color = 'black')

heights %>%
  filter(sex == 'Male') %>%
  ggplot(aes(height)) +
  geom_density(alpha = .2, fill = '#00BFC4', color = 0) +
  geom_line(stat = 'density')

p1 <- heights %>%
  filter(sex == 'Male') %>% ggplot(aes(height)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5) +
  geom_line(stat = 'density', adjust = 0.5)

p2 <- heights %>%
  filter(sex == 'Male') %>% ggplot(aes(height)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5) +
  geom_line(stat = 'density', adjust = 2)

grid.arrange(p1, p2, ncol = 2)

d <- with(heights, density(height[sex == 'Male']))
tmp <- data.frame(height = d$x, density = d$y)
tmp %>% ggplot(aes(height, density)) + geom_line() +
  geom_area(aes(x = height, y = density),
            data = filter(tmp, between(height, 65, 68)),
            alpha = 0.2, fill = '#00BFC4')

heights %>%
  ggplot(aes(height, fill = sex)) +
  geom_density(alpha = 0.2, color = 0) +
  geom_line(stat = 'density')

m <- sum(x) / length(x)

s <- sqrt(sum((x - m) ^ 2) / length(x))

index <- heights$sex == 'Male'
x <- heights$height[index]

m <- mean(x)
s <- sd(x)
c(average = m, sd = s)

z <- scale(x)
mean(abs(z) < 2)

pnorm(-1.96)

qnorm(0.975)

qnorm(0.975, mean = 5, sd = 2)

mean(x <= 69.5)

p <- seq(0.05, 0.95, 0.05)
sample_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x),
                               sd = sd(x))

qplot(theoretical_quantiles, sample_quantiles) +
  geom_abline()

sample_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
qplot(theoretical_quantiles, sample_quantiles) +
  geom_abline()

heights %>% filter(sex == 'Male') %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

heights %>% ggplot(aes(x = sex, y = height, fill = sex)) +
  geom_boxplot()

p1 <- heights %>% filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_density(fill = '#F8766D')
p2 <- heights %>% filter(sex == 'Female') %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() + geom_abline() + ylab('Standard Units')
grid.arrange(p1, p2, ncol = 2)

murders %>% ggplot(aes(region)) + geom_bar()

tab <- murders %>%
  count(region) %>%
  mutate(proportion = n / sum(n))
tab

tab %>% ggplot(aes(region, proportion)) + 
  geom_bar(stat = 'identity')

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_histogram()

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1)

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = 'blue',
                 col = 'black') +
  xlab('Male heights in inches') +
  ggtitle('Histogram')

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_density()

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_density(fill = 'blue')

heights %>%
  filter(sex == 'Female') %>%
  ggplot(aes(height)) +
  geom_density(fill = 'blue', adjust = 2)

heights %>% filter(sex == 'Male') %>%
  ggplot(aes(sample = height)) +
  geom_qq()

params <- heights %>% filter(sex == 'Male') %>%
  summarize(mean = mean(height), sd = sd(height))

heights %>% filter(sex == 'Male') %>%
  ggplot(aes(sample = height)) +
  geom_qq(dparams = params) +
  geom_abline()

heights %>% 
  filter(sex == 'Male') %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

x <- expand.grid(x = 1:12, y = 1:10) %>%
  mutate(z = 1:120)

x %>% ggplot(aes(x, y, fill = z)) + geom_raster()

x %>%
  ggplot(aes(x, y, fill = z)) + 
  geom_raster() +
  scale_fill_gradientn(colors = terrain.colors(10))

x <- heights %>% filter(sex == 'Male') %>% pull(height)

qplot(x)

heights %>% qplot(sex, height, data = .)

heights %>% qplot(sex, height, data = ., geom = 'boxplot')

qplot(x, geom = 'density')

qplot(x, bins = 15, color = I('black'), xlab = 'Population')

library(tidyverse)
library(dslabs)
data(gapminder)
gapminder %>% as_tibble()

gapminder %>%
  filter(year == 2015 & country %in% c('Sri Lanka', 'Turkey')) %>%
  select(country, infant_mortality)

filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c('Europe', 'Asia')
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~ year)

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(. ~ year, scales = 'free')

gapminder %>%
  filter(country == 'United States') %>%
  ggplot(aes(year, fertility)) +
  geom_point()

gapminder %>%
  filter(country == 'United States') %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c('South Korea', 'Germany')
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c('South Korea', 'Germany')
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

countries <- c('South Korea', 'Germany')
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

labels <- data.frame(country = countries, x = c(1975, 1985), y = c(60, 72))
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = 'none')

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = 'black')

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = 'black')

gapminder %>%
  filter(year == past_year) %>%
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5, color = 'black')

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = 'black') +
  scale_x_continuous(trans = 'log2')

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans = 'log2')

gapminder <- gapminder %>% 
  mutate(group = case_when( region %in% c("Western Europe", "Northern Europe",
                                          "Southern Europe", "Northern America",
                                          "Australia and New Zealand") ~ "West",
                                           region %in% c("Eastern Asia", "South-Eastern Asia") 
                                            ~ "East Asia", 
                                          region %in% c("Caribbean", "Central America", "South America") 
                                            ~ "Latin America",
                                         continent == "Africa" & region != "Northern Africa"
                                          ~ "Sub-Saharan",
                                         TRUE ~ "Others"))
gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America", 
                                          "East Asia", "Sub-Saharan", "West")))

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = 'log2') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

p + geom_point(alpha = 0.5)

library(ggridges)
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = 'log2')
p + geom_density_ridges()

p + geom_density_ridges(jittered_points = TRUE)

p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3,
                        point_alpha = 1, alpha = 0.7)

past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder %>%
  filter(year %in% years & !is.na(gdp)) %>%
  mutate(west = ifelse(group == 'West', 'West', 'Developing')) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = 'black') +
  scale_x_continuous(trans = 'log2') +
  facet_grid(year ~ west)

country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  pull(country)

country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>%
  pull(country)

country_list <- intersect(country_list_1, country_list_2)

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = 'log2') +
  xlab('') + facet_grid(. ~ year)

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(group, dollars_per_day, fill = year)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = 'log2') + xlab('')

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = 'grey') +
  scale_x_continuous(trans = 'log2') +
  facet_grid(. ~ year)

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(group = ifelse(group == 'West', 'West', 'Developing')) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = 'log2') +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)

p <- gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(group = ifelse(group == 'West', 'West', 'Developing')) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = 'log2', limit = c(0.125, 300))

p + geom_density(alpha = 0.2) +
  facet_grid(year ~ .)

p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

gapminder %>% 
  filter(year %in% years & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = 'log2') +
  geom_density_ridges() +
  facet_grid(. ~ year)

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population / sum(population) * 2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = 'log2', limit = c(0.125, 300)) +
  geom_density(alpha = 0.2, bw = 0.75, position = 'stack') +
  facet_grid(year ~ .)

data(murders)
murders %>% mutate(murder_rate = total / population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab('')

heights %>%
  ggplot(aes(sex, height)) +
  geom_point()

heights %>%
  ggplot(aes(sex, height)) +
  geom_jitter(width = 0.1, alpha = 0.2)

heights %>%
  ggplot(aes(height, ..density..)) +
  geom_histogram(binwidth = 1, color = 'black') +
  facet_grid(sex ~ .)

heights %>%
  ggplot(aes(sex, height)) +
  geom_boxplot(coef = 3) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  ylab('Height in inches')

gapminder %>%
  filter(year %in% c(1970, 2010) & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp / population / 365, year = factor(year)) %>%
  ggplot(aes(continent, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = 'log2') +
  ylab('Income in dollars per day')

library(ggrepel)
gapminder %>%
  mutate(year = paste0('life_expectancy_', year)) %>%
  select(country, year, life_expectancy) %>%
  spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010) / 2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab('Average of 2010 and 2015') +
  ylab('Difference between 2015 and 2010')

library(tidyverse)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
names(us_contagious_diseases)

the_disease <- 'Measles'
dat <- us_contagious_diseases %>%
  filter(!state %in% c('Hawaii', 'Alaska') & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% filter(state == 'California' & !is.na(rate)) %>%
  ggplot(aes(year, rate)) + geom_line() +
  ylab('Cases per 10,000') +
  geom_vline(xintercept = 1963, col = 'blue')

dat %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = 'grey50') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), trans = 'sqrt') +
  geom_vline(xintercept = 1963, col = 'blue') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size = 8)) +
  ggtitle(the_disease) +
  ylab('') + xlab('')

avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>%
  group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE) /
              sum(population, na.rm = TRUE) * 10000)

dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = 'grey50',
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1) +
  scale_y_continuous(trans = 'sqrt', breaks = c(5, 25, 125, 300)) +
  ggtitle('Cases per 10,000 by state') +
  xlab('') + ylab('') +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = 'US average'),
            color = 'black') +
  geom_vline(xintercept = 1963, col = 'blue')

B <- 10000
beads <- rep(c('red', 'blue'), times = c(2, 3))
events <- replicate(B, sample(beads, 1))

tab <- table(events)
tab

prop.table(tab)

set.seed(1986)

sample(beads, 5)
sample(beads, 5)
sample(beads, 5)

sample(beads, 6) # error

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

x <- sample(beads, 5)
x[1]
x[2:5]

number <- 'Three'
suit <- 'Hearts'
paste(number, suit)

paste(letters[1:5], as.character(1:5))

expand.grid(pants = c('blue', 'black'), shirt = c('white', 'grey', 'plaid'))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six",
             "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

kings <- paste('King', suits)
mean(deck %in% kings)

library(gtools)
hands <- permutations(52, 2, v = deck)
head(hands)

permutations(3, 2)

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index, ]

hands <- permutations(52, 2, v = deck)

first_card <- hands[,1]
second_card <- hands[,2]

kings <- paste('King', suits)
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) /
  sum(first_card %in% kings)

mean(first_card %in% kings & second_card %in% kings) /
  mean

combinations(3, 2)

aces <- paste('Ace', suits)
facecard <- c('King', 'Queen', 'Jack', 'Ten')
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
       (hands[,2] %in% aces & hands[,1] %in% facecard))

hand <- sample(deck, 2)
hand

(hands[1] %in% aces & hands[2] %in% facecard) |
  (hands[2] %in% aces & hands[1] %in% facecard)

blackjack <- function() {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
}

blackjack()

B <- 10000
results <- replicate(B, blackjack())
mean(results)

B <- 10000
monty_hall <- function(strategy) {
  doors <- as.character(1:3)
  prize <- sample(c('car', 'goat', 'goat'))
  prize_door <- doors[prize == 'car']
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
  switch <- doors[!doors %in% c(my_pick, show)]
  choice <- ifelse(strategy == 'stick', stick, switch)
  choice == prize_door
}

stick <- replicate(B, monty_hall('stick'))
mean(stick)

switch <- replicate(B, monty_hall('switch'))
mean(switch)

n <- 50
bdays <- sample(1:365, n, replace = TRUE)

duplicated(c(1, 2, 3, 1, 4 ,3, 5))
any(duplicated(c(1, 2, 3, 1, 4, 3, 5)))

B <- 10000
same_birthday <- function(n) {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
}

results <- replicate(B, same_birthday(23))
mean(results)

compute_prob <- function(n, B = 10000) {
  results <- replicate(B, same_birthday(n))
  mean(results)
}

n <- seq(1, 60)
prob <- sapply(n, compute_prob)

qplot(n, prob)

exact_prob <- function(n) {
  prob_unique <- seq(365, 365 - n + 1) / 365
  1 - prod(prob_unique)
}

eprob <- sapply(n, exact_prob)
qplot(n, prob) + geom_line(aes(n, eprob), col = 'red')

B <- 10 ^ seq(1, 5, len = 100)
compute_prob <- function(B, n = 25) {
  same_day <- replicate(B, same_birthday(n))
  mean(same_day)
}

prob <- sapply(B, compute_prob)
qplot(log10(B), prob, geom = 'line')

x <- heights %>% filter(sex == 'Male') %>% pull(height)

F <- function(a) mean(x <= a)

1 - F(70.5)

m <- mean(x)
s <- sd(x)
1 - pnorm(70.5, m, s)

mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

pnorm(68.5, m, s) - pnorm(67.5, m, s)
pnorm(69.5, m, s) - pnorm(68.5, m, s)
pnorm(70.5, m, s) - pnorm(69.5, m, s)

mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, m, s) - pnorm(70.1, m, s)

1 - pnorm(76, m, s)

n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heigths <- rnorm(n, m, s)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, m, s)
  max(simulated_data)
})

mean(tallest >= 7 * 12)

data.frame(tallest = tallest) %>% ggplot(aes(tallest)) +
  geom_histogram(color = 'black', binwidth = 1)

x <- seq(-4, 4, length.out = 100)
qplot(x, f, geom = 'line',
      data = data.frame(x, f = dnorm(x)))
