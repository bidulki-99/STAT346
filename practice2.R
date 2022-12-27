library(tidyverse)
library(dslabs)
take_poll(25)

p <- 0.51
sqrt(p*(1-p))/sqrt(1000)

x_hat <- 0.48
se <- sqrt(x_hat*(1-x_hat)/25)
se

pnorm(0.01/se) - pnorm(-0.01/se)
1.96*se

pnorm(1.96)-pnorm(-1.96)

B <- 10000
N <- 1000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE,
              prob = c(1-p, p))
  mean(x)
})

p <- 0.45; N <- 1000
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

B <- 10000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

mean(x_hat)
sd(x_hat)

library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat=x_hat) %>% ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color='black')
p2 <- data.frame(x_hat=x_hat) %>% ggplot(aes(sample=x_hat)) +
  stat_qq(dparams = list(mean=mean(x_hat), sd=sd(x_hat))) +
  geom_abline() +
  ylab('x_hat') +
  xlab('Theoretical normal')
grid.arrange(p1, p2, nrow=1)

p <- 0.45; N <- 1000
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)

qnorm(0.975)
z <- qnorm(0.995)
z

N <- 1000
B <- 10000
inside <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE,
              prob = c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  between(p, x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
mean(inside)

N <- 25
x_hat <- 0.48
(2*x_hat-1)+c(-1.96,1.96)*2*sqrt(x_hat*(1-x_hat)/N)

N <- 100
z <- sqrt(N)*0.02/0.5
1 - (pnorm(z) - pnorm(-z))

library(Lahman)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, BB_per_game = BB / G) %>%
  ggplot(aes(HR_per_game, BB_per_game)) +
  geom_point(alpha = 0.5)

p <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
p

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(z_HR = round((HR - mean(HR))/sd(HR)),
         R_per_game = R/G) %>%
  filter(z_HR %in% -2:3) %>%
  ggplot() + stat_qq(aes(sample=R_per_game)) + 
  facet_wrap(~z_HR)

summary_stats <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  summarize(avg_HR = mean(HR_per_game),
            s_HR = sd(HR_per_game),
            avg_R = mean(R_per_game),
            s_R = sd(R_per_game),
            r = cor(HR_per_game, R_per_game))
summary_stats

reg_line <- summary_stats %>%
  summarize(slope = r*s_R/s_HR,
            intercept = avg_R - slope*avg_HR)
p + geom_abline(intercept = reg_line$intercept,
                slope = reg_line$slope)

p + geom_smooth(method = 'lm')

get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

bb_slope <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope

singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G,
         R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))
singles_slope

Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G,
         HR = HR/G) %>%
  summarize(cor(BB,HR), cor(Singles,HR), cor(BB,Singles))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

dat %>%
  group_by(HR_strata) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))

dat %>% mutate(BB_strata = round(BB/G, 1),
               HR_per_game = HR/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9) %>%
  group_by(BB_strata) %>%
  summarize(slope = get_slope(HR_per_game, R_per_game))

hr_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(HR_per_game, R_per_game))
hr_slope

library(HistData)
data('GaltonFamilies')
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == 'male') %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data) {
  resid <- galton_heights$son -
    (beta0 + beta1 * galton_heights$father)
  return (sum(resid^2))
}

fit <- lm(son ~ father, data = galton_heights)
fit$coef

summary(fit)

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>% .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

galton_heights %>% ggplot(aes(son, father)) +
  geom_point() + 
  geom_smooth(method = 'lm')

fit <- galton_heights %>% lm(son ~ father, data = .)
y_hat <- predict(fit, se.fit = TRUE)
names(y_hat)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4& HR <= 1.2)

get_slope <- function(x, y) cor(X, y) * sd(y) / sd(x)
dat %>% group_by(HR) %>%
  summarize(slope = get_slope(BB, R))

dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>% .$coef

dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

get_lse <- function(data) {
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}
dat %>% group_by(HR) %>% do(get_lse(.))

library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

tidy(fit, conf.int = TRUE)

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == 'BB') %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() + geom_point()

glance(fit)

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  lm(R ~ BB + singles + doubles + triples + HR,
     data = .)

coefs <- tidy(fit, conf.int = TRUE)
coefs

Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()

pa_per_game <- Batting %>%
  filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

players <- Batting %>%
  filter(yearID %in% 1997:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G, singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, triples = sum(X3B)/G,
            HR = sum(HR)/G, AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 1000) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

qplot(R_hat, data = players, binwidth = 0.5,
      color = I('black'))

players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by = 'playerID')

position_names <-
  paste0('G_', c('p', 'c', '1b', '2b', '3b', 'ss', 'lf', 'cf',
                 'rf', 'dh'))
tmp <- Appearances %>%
  filter(yearID == 2002) %>%
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()
pos <- tmp %>%
  select(position_names) %>%
  apply(., 1, which.max)

players <- tibble(playerID = tmp$playerID,
                  POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, 'G_'))) %>%
  filter(POS != 'P') %>%
  right_join(players, by='playerID') %>%
  filter(!is.na(POS) & !is.na(salary))

players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by='playerID')

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>% top_n(10)

players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() + 
  scale_x_log10()

N <- 25
g <- 100000
sim_data <- tibble(group = rep(1:g, each = N),
                   x = rnorm(N * g),
                   y = rnorm(N * g))

library(tidyverse)
res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(x, y)) %>% arrange(desc(r))

sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) + geom_point() + geom_smooth(method = 'lm')

res %>% ggplot(aes(x = r)) + geom_histogram(bins = 15)

sim_data_scor <- sim_data %>%
  filter(group == res$group[which.max(res$r)])

summary(lm(y ~ x, sim_data_scor))$coef

set.seed(1985)
x <- rnorm(100, 100, 1)
y <- rnorm(100, 100, 1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

qplot(x, y, alpha = 0.5)

cor(x, y)
cor(x[-23], y[-23])

qplot(rank(x), rank(y))

cor(rank(x), rank(y))
cor(x, y, method = 'spearman')

admissions <- as.data.frame(UCBAdmissions)
xtabs(Freq ~ Admit + Gender, admissions) %>% prop.table(2)

ggplot(admissions, aes(x=Gender, fill=Admit)) +
  geom_bar(aes(y=Freq), position='fill', stat='identity')

t1 <- admissions %>% filter(Admit == 'Admitted')
t2 <- admissions %>% group_by(Dept, Gender) %>% summarize(Sum = sum(Freq))
admissions2 <- full_join(t1, t2, by = c('Dept', 'Gender')) %>%
  mutate(Admit_Rate = Freq/Sum) %>%
  select(Dept, Gender, Admit_Rate) %>%
  spread(Gender, Admit_Rate)
admissions2

admissions %>% group_by(Dept) %>%
  summarize(Major_selectivity = sum((Admit == 'Admitted') * Freq) / sum(Freq),
            Percent_female_applicants = sum(Freq * (Gender == 'Female')) /
              sum(Freq) * 100) %>%
  ggplot(aes(Major_selectivity, Percent_female_applicants, label = Dept)) +
  geom_text()

ggplot(admissions, aes(x=Gender, fill=Admit)) +
  geom_bar(aes(y=Freq), stat='identity') +
  scale_y_continuous(labels=scales::percent) + facet_wrap(~Dept)

admissions %>%
  mutate(Percent_admitted = (Admit == 'Admitted') * Freq / sum(Freq)) %>%
  ggplot(aes(Gender, y = Percent_admitted, fill = Dept)) +
  geom_bar(stat = 'identity', position = 'stack')

admissions3 <- admissions %>% group_by(Dept, Gender) %>%
  mutate(Applicants = sum(Freq), Admit_rate = Freq/Applicants) %>%
  filter(Admit == 'Admitted') %>% select(-Admit, -Freq)
ggplot(admissions3, aes(Dept, Admit_rate, col = Gender, size = Applicants)) +
  geom_point()

admissions3 %>% group_by(Gender) %>%
  summarize(Average = mean(Admit_rate))

library(tidyverse)
library(dslabs)
path <- system.file('extdata', package='dslabs')
filename <- file.path(path, 'fertility-two-countries-example.csv')
wide_data <- read_csv(filename)

head(wide_data)

new_tidy_data <- pivot_longer(wide_data, `1960`:`2015`,
                              names_to = 'year',
                              values_to = 'fertility')

new_tidy_data <- wide_data %>%
  pivot_longer(`1960`:`2015`, 'year', values_to = 'fertility')

head(new_tidy_data)

new_tidy_data <- wide_data %>%
  pivot_longer(-country, names_to = 'year', values_to = 'fertility')

data('gapminder')
tidy_data <- gapminder %>%
  filter(country %in% c('South Korea', 'Germany') & !is.na(fertility)) %>%
  select(country, year, fertility)

class(tidy_data$year)
class(new_tidy_data$year)

new_tidy_data <- wide_data %>%
  pivot_longer(-country, names_to = 'year', values_to = 'fertility',
               names_transform = list(year = as.integer))
class(new_tidy_data$year)

new_tidy_data %>% ggplot(aes(year, fertility, color = country)) +
  geom_point()

new_wide_data <- new_tidy_data %>%
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)

path <- system.file('extdata', package = 'dslabs')

filename <- 'life-expectancy-and-fertility-two-countries-example.csv'
filename <- file.path(path, filename)

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% pivot_longer(-country)
head(dat)

dat$name[1:5]

dat %>% separate(name, c('year', 'variable_name'), '_')

dat %>% separate(name, c('year', 'variable_name'))

var_names <- c('year', 'first_variable_name', 'second_variable_name')
dat %>% separate(name, var_names, fill = 'right')

dat %>% separate(name, c('year', 'variable_name'), extra = 'merge')

dat %>%
  separate(name, c('year', 'variable_name'), extra = 'merge') %>%
  pivot_wider(names_from = 'variable_name', values_from = 'value')

dat %>% separate(name, var_names, fill = 'right') %>%
  unite(variable_name, first_variable_name, second_variable_name) %>%
  pivot_wider(names_from = 'variable_name', values_from = 'value') %>%
  rename(fertility = fertility_NA)

library(tidyverse)
library(dslabs)
data(murders)
head(murders)

data("polls_us_election_2016")
head(results_us_election_2016)

identical(results_us_election_2016$state, murders$state)

tab <- left_join(murders, results_us_election_2016, by = 'state') %>%
  select(-others) %>% rename(ev = electoral_votes)
head(tab)

library(ggrepel)
tab %>% ggplot(aes(population/10^6, ev, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  geom_smooth(method = 'lm', se = FALSE)

tab_1 <- slice(murders, 1:6) %>% select(state, population)
tab_1

tab_2 <- results_us_election_2016 %>%
  filter(state %in% c('Alabama', 'Alaska', 'Arizona',
                      'California', 'Connecticut', 'Delaware')) %>%
  select(state, electoral_votes) %>% rename(ev = electoral_votes)
tab_2

left_join(tab_1, tab_2, by = 'state')

tab_1 %>% left_join(tab_2, by = 'state')

tab_1 %>% right_join(tab_2, by = 'state')

inner_join(tab_1, tab_2, by = 'state')

full_join(tab_1, tab_2, by = 'state')

semi_join(tab_1, tab_2, by = 'state')

anti_join(tab_1, tab_2, by = 'state')

bind_cols(a = 1:3, b = 4:6)

tab_1 <- tab[, 1:3]
tab_2 <- tab[, 4:6]
tab_3 <- tab[, 7:8]
new_tab <- bind_cols(tab_1, tab_2, tab_3)
head(new_tab)

tab_1 <- tab[1:2, ]
tab_2 <- tab[3:4, ]
bind_rows(tab_1, tab_2)

intersect(1:10, 6:15)
intersect(c('a', 'b', 'c'), c('b', 'c', 'd'))

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::intersect(tab_1, tab_2)

union(1:10, 6:15)
union(c('a', 'b', 'c'), c('b', 'c', 'd'))

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::union(tab_1, tab_2)

setdiff(1:10, 6:15)
setdiff(6:15, 1:10)

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::setdiff(tab_1, tab_2)

setequal(1:5, 1:6)

setequal(1:5, 5:1)

dplyr::setequal(tab_1, tab_2)

library(tidyverse)
library(dslabs)
data('polls_us_election_2016')
polls_us_election_2016$startdate %>% head

class(polls_us_election_2016$startdate)

as.numeric(polls_us_election_2016$startdate) %>% head

as.Date('1970-01-01') %>% as.numeric

polls_us_election_2016 %>%
  filter(pollster == 'Ipsos' & state == 'U.S.') %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

library(lubridate)

set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

tibble(date = dates,
       month = month(dates),
       day = day(dates),
       year = year(dates))

month(dates, label = TRUE)

x <- c(20090101, '2009-01-02', '2009 01 03', '2009-1-4',
       '2009-1 5', 'Created on 2009 1 6', '200901 !!! 07')
ymd(x)

x <- '09/01/02'

ymd(x)

mdy(x)

ydm(x)
myd(x)
dmy(x)
dym(x)

now()
now('GMT')

now() %>% hour()
now() %>% minute()
now() %>% second()

x <- c('12:34:56')
hms(x)
x <- 'Nov/1/2021 12:34:56'
mdy_hms(x)

make_date(2021, 11, 01)

make_date(1980:1989)

polls_us_election_2016 %>%
  mutate(week = round_date(startdate, 'week')) %>%
  group_by(week) %>%
  summarize(margin = mean(rawpoll_clinton - rawpoll_trump)) %>%
  qplot(week, margin, data = .)

library(tidyverse)
library(dslabs)
data('polls_2008')
qplot(day, margin, data = polls_2008)

span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin,
                    kernel = 'box', bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = 'grey') +
  geom_line(aes(day, smooth), color = 'red')

span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin,
                    kernel = 'normal', bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = 'grey') +
  geom_line(aes(day, smooth), color = 'red')

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1,
             span = span, data = polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = 'grey') +
  geom_line(aes(day, smooth), color = 'red')

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree = 1,
               span = span, data = polls_2008)
fit_2 <- loess(margin ~ day,
               span = span, data = polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted,
                      smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = 'grey') +
  geom_line(aes(day, smooth_1), color = 'red', lty = 2) +
  geom_line(aes(day, smooth_2), color = 'orange', lty = 1)

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(span = 0.15)

library(tidyverse)
library(dslabs)
data('mnist_27')
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

library(caret)
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1,
                       mnist_27$train, type = 'class')
confusionMatrix(y_hat_knn_1,
                mnist_27$train$y)$overall[['Accuracy']]

y_hat_knn_1 <- predict(knn_fit_1,
                       mnist_27$test, type = 'class')
confusionMatrix(y_hat_knn_1,
                mnist_27$test$y)$overall['Accuracy']

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401,
                         mnist_27$test, type = 'class')
confusionMatrix(y_hat_knn_401,
                mnist_27$test$y)$overall['Accuracy']

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k) {
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = 'class')
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall['Accuracy']
  
  y_hat <- predict(fit, mnist_27$test, type = 'class')
  cm_train <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_train$overall['Accuracy']

  tibble(train = train_error, test = test_error)  
})

ks[which.max(accuracy$test)]
max(accuracy$test)

set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I('black'))

(m <- median(income))

N <- 100
X <- sample(income, N)
median(X)

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I('black'))
p2 <- qplot(sample = scale(M), xlab = 'theoretical',
            ylab = 'sample') + geom_abline()
grid.arrange(p1, p2, ncol = 2)

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

quantile(M, c(0.025, 0.975))

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

quantile(M_star, c(0.025, 0.975))
