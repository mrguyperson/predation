# load packages
library (dplyr)
library(hflights)

# check data
data(hflights)
head(hflights)

# convert to 'local dataframe'

flights <- as_tibble(hflights)

# print

flights

# specify number of rows to print

print(flights, n = 20)

# convert to normal dataframe to see all columns

data.frame(head(flights))

# base R approach to show all flights on Jan 1

flights[flights$Month==1 & flights$DayofMonth==1,]

# dplyr approach

filter(flights, Month==1, DayofMonth==1)

# use pipe for OR conditions

filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

# use can also use %in% operator

filter(flights, UniqueCarrier %in% c("AA","UA"))

## SELECT to pick columns by name

# base R approach to select DepTime, ArrTime,and FlightNum columns

flights[,c("DepTime", "ArrTime", "FlightNum")]

# dplyr approach

select(flights, DepTime, ArrTime, FlightNum)

# use colon to select range of columns and contains

select(flights, Year:DayofMonth, contains('Taxi'), contains("Delay"))


## "Chaining" or "Pipelining"

# nested method to select UniqueCarrier and DepDelay columns and filter for delays > 60 mins

filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# chaining method

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter (DepDelay > 60)

# %>% can be used outside of dplyr

# create two vectors and calculate the Euclidean distance between them

x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))

# chaining method

(x1-x2)^2 %>% sum() %>% sqrt()


## Reorder rows

# select UniqueCarrier and DepDelay and sort by DepDelay
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

# dplyr approach

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

# use desc for descending

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

## mutate: add new variables

# base R approach to create a new variable 'speed' (in mph)

flights$Speed <- flights$Distance / flights$AirTime * 60
flights[, c("Distance", "AirTime", "Speed")]

# dplyr approach

flights %>%
  select(Distance,AirTime) %>%
  mutate(Speed = Distance / AirTime * 60)

# store the new variable

flights <- flights %>% mutate(Speed = Distance / AirTime * 60)

## Summarise: reduce variables to values

# base R approach to calculate the average arrival delay to each destination

head(with(flights, tapply(ArrDelay, Dest, mean, na.rm=TRUE)))
head(aggregate(ArrDelay ~ Dest, flights, mean))

# dplyr approach

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

## across

# for each carrier, calculate percentage of flights cancelled or diverted

flights %>%
  group_by(UniqueCarrier) %>%
  summarise(across(c(Cancelled,Diverted), ~ mean(.x, na.rm=TRUE)))

# for each carrier, calculate the min and max arrival and departure delays

flights %>%
  group_by(UniqueCarrier) %>%
  summarise(across(matches("Delay"), list(min = min, max = max), na.rm=TRUE))

# for each day of the year, calculate the total number of flights and then sort them in desc order

flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# rewrite more simply with the tally function

flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of flights and the number of distinct planes that flew there

flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

# grouping can sometimes be useful without summarising

# for each destination, show the number of cancelled and not cancelled flights

flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()


## Window Functions - ranking, ordering, offsetting, cum. aggregate functions

# for each carrier, calculate which two days of the year had their longest departure delays
# note that the smallest, not the largest, value is ranked as 1, so you have to use desc order

flights %>% 
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewrite more simply with top_n function

flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# for each month, calculate the number of flights and the change from the previous month

flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

# rewrite more simply with tally function

flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))

# randomly sample a fixed number of rows (without replacement by default)

flights %>% sample_n(5)

# sample a fraction of rows with replacement

flights %>% sample_frac(0.25, replace=TRUE)

# base R approach to look at structure

str(flights)

# dplyr approach

glimpse(flights)

### Remove flights to switch to nycflights13

rm(flights)

# load nycflights13

library(nycflights13)
flights

# besides just using select to pick columns....

flights %>% select(carrier, flight)

# ... you can use the minus sign to hide columns

flights %>% select(-month, -day)

# hide a range of columns

flights %>% select(-(dep_time:arr_delay))

# hide any column with a matching name

flights %>% select(-contains("time"))

# pick columns using a character vector

cols <- c("carrier", "flight", "tailnum")
flights %>% select(one_of(cols))

# select() can be used to rename columns though  all columns not mentioned are dropped

flights %>% select(tail = tailnum)

# rename() can be used to rename columns,  all columns not mentioned are kept

flights %>% rename(tail = tailnum)

## Choosing rows: filter, between, slice, sample_n, top_n, distinct

# filter() supports the use of multiple conditions

flights %>% filter(dep_time >= 600, dep_time <= 605)

# between() is a concise alternative for determining if numeric values fall in a range

flights %>% filter(between(dep_time, 600, 605))

# side note: is.na() can also be useful when filtering

flights %>% filter(!is.na(dep_time))

# slice() filters rows by position

flights %>% slice(1000:1005)

# keep the first three rows within each group

flights %>% group_by(month, day) %>% slice(1:3)

# sample three rows from every group 

flights %>% group_by(month, day) %>% sample_n(3)


# keep three rows from each group with the top dep_delay

flights %>% group_by(month, day) %>% top_n(3, dep_delay)

# also sort by dep_delay in each group

flights %>% group_by(month, day) %>% top_n(3, dep_delay) %>% arrange(desc(dep_delay))

# unique rows can be identified by using unique() from base R

flights %>% select(origin, dest) %>% unique()

# dplyr provides an alternative that is more efficient

flights %>% select(origin, dest) %>% distinct()

# side note: when chaining, parentheses are not necessary if not passing arguments

flights %>% select(origin, dest) %>% distinct

## Adding new variables: mutate, transmute, add_rownames

# mutate creates a new variable and keeps all exisitng variables

flights %>% mutate(speed = distance/air_time*60)

# transmute keeps only the new variable

flights %>% transmute(speed = distance/air_time*60)

# example head with row names

mtcars %>% head

# rownames_to_column() turns row names into explicit variables
# important, needs tibble!

library(tibble)

mtcars %>% rownames_to_column(var="model") %>% head

# side note: dplyr no longer prints row names (ever) for local data frames

mtcars %>% tbl_df()

## Grouping and counting: summarise, tally, count, group_size, n_groups, ungroup

# summarise can be used to count the number of rows in each group

flights %>% group_by(month) %>% summarise(cnt = n())

# tally and count can do this more concisely

flights %>% group_by(month) %>% tally
flights %>% count(month)

# you can sort by the count

flights %>% group_by(month) %>% summarise(cnt = n()) %>% arrange(desc(cnt))

# tally and count have a sort parameter for this purpose

flights %>% group_by(month) %>% tally(sort=TRUE)
flights %>% count(month, sort=TRUE)

# you can sum over a specific variable instead of simply counting rows

flights %>% group_by(month) %>% summarise(dist = sum(distance))

# tally and count have a parameter for this

flights %>% group_by(month) %>% tally(wt = distance)
flights %>% count(month, wt = distance)

# group_size returns the counts as a vector

flights %>% group_by(month) %>% group_size

# n_groups simply reports the number of groups

flights %>% group_by(month) %>% n_groups

# group by two variables, summarize, and arrange (output is possibly confusing)

flights %>% group_by(month, day) %>% summarise(cnt = n()) %>% arrange(desc(cnt)) %>% print(n = 40)

# use ungroup before arranging to arrange across all groups

flights %>% group_by(month, day) %>% summarise(cnt = n()) %>% ungroup %>% arrange(desc(cnt))

## Creating data frames using data_frame: better than data.frame for creating data frames
## IMPORTANT data_frame is now tibble()

# data_frame example

tibble(a = 1:6, b = a*2, c = 'string', 'd+e' = 1) %>% glimpse

# data.frame example 
data.frame(a = 1:6, c = 'string', 'd+e' = 1) %>% glimpse

## Joining (merging) tables: left_join, right_join, inner_join, full_join, semi_join, anti_join

# create two simple data frames

(a <- tibble(color = c('green', 'yellow', 'red'), num = 1:3))
(b <- tibble(color = c('green', 'yellow', 'pink'), size = c("S", "M", "L")))

# only include observations found in both a and b (automatically joins on variables that appear in both tables)

inner_join(a,b)

# include observations found in either

full_join(a, b)

# include all observations found in a

left_join(a, b)

# include all observations found in b

right_join(a, b)

# right_join(a, b) is identical to left_join(b, a) except for column ordering

left_join(b, a)

# filter a to only show observations that appear in b

semi_join(a, b)

# filter a to show only observations that do not appear in b

anti_join(a, b)

# sometimes matching variables don't have identical names

b <- b %>% rename(col = color)

# specify that the join should occur by matching "color" in "a" to "col" in "b"

inner_join(a, b, by=c("color" = "col"))

## Viewing more output: print, view

# specify that you want to see more rows

flights %>% print(n=15)

# specify that you want to see all rows (DON'T RUN THIS!!!!)

flights %>% print(n=Inf)

# specify that you want to see all columns

flights %>% print(width=Inf)

# show up to 1000 rows and all columns

flights %>% View()

# set option to see all columns and fewer rows

options(dplyr.width = Inf, dplyr.print_min = 6)

# reset options (or just close R)

options(dplyr.width = NULL, dplyr.print_min = 10)
