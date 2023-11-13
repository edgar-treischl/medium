


library(DBI)

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(drv = RSQLite::SQLite(), 
                 dbname = ":memory:")

#dbListTables(con)
dbWriteTable(conn = con, 
             name = "mtcars", 
             value = mtcars)


## --Example SQL Code

## SELECT var FROM data;


## SELECT AVG(var) FROM data;


#Run SQL code from R
library(sqldf)
sqldf('SELECT AVG(mpg) FROM mtcars;')

#Establish a connection
library(DBI)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "sakila",
  host = "relational.fit.cvut.cz",
  port = 3306,
  username = "guest",
  password = "relational"
)

## #List all available tables/data
## dbListTables(con)
## #  [1] "actor"         "address"       "category"
## #  [4] "city"          "country"       "customer"
## #  [7] "film"          "film_actor"    "film_category"
## # [10] "film_text"     "inventory"     "language"
## # [13] "payment"       "rental"        "staff"
## # [16] "store"

#Disconnect after have finished the job
dbDisconnect(con)

# Create in-memory RSQLite database
con <- dbConnect(drv = RSQLite::SQLite(),
                 dbname = ":memory:")

#Write a table into the data base
dbWriteTable(conn = con,
             name = "mtcars",
             value = mtcars)

#Send queries to the local database
result_DB <- dbSendQuery(con, "SELECT AVG(mpg) FROM mtcars;")
dbFetch(result_DB)
dbClearResult(result_DB)


#Create a table from the database
library(dplyr)
library(dbplyr)

mtcars_db <- tbl(con, "mtcars")

#Get mean with dplyr (locally)
mtcars |> 
  summarise(mean_mpg = mean(mpg))

#Get mean with dbplyr
summary <- mtcars_db |> 
  summarise(mean_mpg = mean(mpg))

#Collect (execute and retrieve) the result from the db
summary |>  collect()

#Inspect SQL query
summary |>  show_query()

#tolower returns strings in lower case
tolower("HeLLo WoRld")

#Translate with different SQL engines
translate_sql(tolower("HeLLo WoRld"), con = simulate_sqlite())
translate_sql(tolower("HeLLo WoRld"), con = simulate_access())

## --Example SQL Code

## SELECT AVG(mpg) FROM mtcars;

