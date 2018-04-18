library(tidyverse)

library("ggplot2")
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, alpha = class), 
             size = 3) +
  facet_wrap(~ class, nrow = 3, ncol = 3) #+
  # facet_grid(drv ~ cyl)

#params: size, alpha, shape
?mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "lm")

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "auto")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))

#Гистограммы
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "fill"
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "dodge"
  )

#добавляет небольшой шуи
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )
#Смена осей
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + #
  geom_abline() + 
  coord_fixed()  #

#Трансформирование данных при помощи dplyr
install.packages("nycflights13")
library(nycflights13)
?flights
ggplot(data = flights[flights$dep_delay < 250 & month < 7,], aes(x = dep_delay)) +
  geom_bar() +
  facet_grid(~month)

library("dplyr")
filter(flights, month == 1, day == 1)
nov_dec <- filter(flights, month %in% c(11,12))

NA == NA #Пример к годами

arrange(flights, dep_delay)
arrange(flights, desc(dep_delay))

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day), -origin)
rename(nov_dec, месяц = month, год = year, день = day)

select(flights, day, month, everything())
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
cumsum(c(1,2,3,4,5))
cummean(c(1,2,3,4,5))
cummin(c(1,2,3,4,0))
cummax(c(1,2))
cumprod(c(1,2,3,4,5))
min_rank(c(1,3,2))
min_rank(desc(c(1,3,2)))
percent_rank(c(1,2,4,3))
?cume_dist
?dense_rank

summarize(flights, delay = mean(dep_delay, na.rm = TRUE), var_delay = max(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_month <- group_by(flights, year, month)
summarize(by_month, delay = mean(dep_delay, na.rm = TRUE))

#Pipe
#Традиционная конструкция
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE, method = "auto")

#x %>% f(y) трансформируется в f(x, y)
delay <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")


not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/30)

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)


install.packages("Lahman")
library("Lahman")
batting <- as_tibble(Lahman::Batting)
ggplot2::diamonds

batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE)

#Полезные функции summary
x <- c(1,2,3,4,4)
mean(x)
median(x)
sd(x) #стандартное отклонение
IQR(x) #межквартильный размах
mad(x) #median absolute deviation
min(x);
max(x);
first(c(1,2,3))
last(c(1,2,3))
n_distinct(c(1,2,1,2,1)) # число уникальных значений

not_cancelled %>%
  count(dest)

#Группировка по множественным переменным
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))

#Grouped Mutates (and Filters)
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)

popular_dests <- flights %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

library(tidyverse)

#EDA
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

smaller <- diamonds %>%
  filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.15)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)

diamonds[diamonds$carat == 1,]

#Пропущенные значения
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = filter(diamonds2, x != 0, y != 0, z != 0), mapping = aes(x = x, y = z)) +
  geom_point() + 
  geom_smooth(method = "lm")


nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))
  
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price, color = cut), size = 1)

ggplot(data = diamonds) +
  geom_point(
    mapping = aes(x = carat, y = price),
    alpha = 1 / 500
  )

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

#install.packages("hexbin")
library("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid)) %>%
  ggplot() +
    geom_point(mapping = aes(x = carat, y = resid))

diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
  geom_tile()


#Получение и установка дирректорий
getwd()
#setwd(Дирректория) - установка дирректории
ggsave("diamonds.pdf") #сохранить текущий график в pdf

#tibbles
library(tidyverse)
data <- as_tibble(iris)
tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

nycflights13::flights %>%
  print(n = 10, width = Inf)

#индексирование
x <- nycflights13::flights;
x[1]
x[[1]]
x["year"]
x[["year"]]

#Чтение данных
library("readr")

read_csv("The first line of metadata
          The second line of metadata
          x,y,z
          1,2,3", skip = 2)

read_csv("# A comment I want to skip
         x,y,z
         1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)

read_csv("a,b,c\n1,2,.", na = ".")

#Парсинг
parse_logical(c("TRUE", "FALSE", "NA"))
parse_integer(c("1", "2", "3"))
parse_date(c("2010-01-01", "1979-10-14"))
parse_integer(c("1", "231", ".", "456"), na = ".")
parse_double(c("1311,234"), locale = locale(decimal_mark = ","))

# и т д
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

charToRaw("Hello")

Sys.timezone()


par(mai = c(1,1,1,1), omi = c(0.5, 0.5, 0.5, 0.5))


install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(mtcars$wt,mtcars$disp,mtcars$mpg,
              pch=16, highlight.3d=TRUE, angle=20,
              xlab="Weight",ylab="Displacement",zlab="Fuel Economy (mpg)",
              type="h",
              main="Relationships between car specifications")
