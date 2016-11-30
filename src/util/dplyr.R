# dplyr databases

# use src_database to connect to different databases
# E.g. src_postgresql, src_bigquery, src_sqlite (supported in R, no installation required)

library(dplyr)
library(RSQLite)
library(nycflights13)

# set up connection to sql lite database
my_db <- src_sqlite("my_db.sqlite3", create = T)
# this copies the data frame to the database and returns a table object in the remote source
flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))
# then can work using standard dplyr operations
# (under the hood, operations are translated to SQL and executed on the database)
select(flights_sqlite, year:day, dep_delay, arr_delay)

# dplyr is lazy, it delays any work until necessary (collect together everything and send in one step)
c1 <- filter(flights_sqlite, year == 2013, month == 1, day == 1)
c1$query
c2 <- select(c1, year, month, day, carrier, dep_delay, air_time, distance)
c2$query
c3 <- mutate(c2, speed = distance / air_time * 60)
c3$query
c4 <- arrange(c3, year, month, day, carrier)
# Notice how c4$query is a collection of all previous steps
c4$query
# this pulls down the result (notice that now result is a local data frame; c4 is a sqlite source)
result <- collect(c4)

# Forcing computation (pulling result from database)
# collect: execute query and return result to R => use if want to get result into R
# compute: execute query and store in temporary table => immediate processing step on the database
# collapse: turns query into a table expression?

