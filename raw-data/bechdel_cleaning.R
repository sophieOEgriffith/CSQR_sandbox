require(RJSONIO)
require(tidyverse)

####API Pull from BechdelTest.com

Bechdel_pull <- "http://bechdeltest.com/api/v1/getAllMovies"

 raw_data <- fromJSON(Bechdel_pull)
 View(raw_data)

 ##extract title to vector

title <- c()

for (i in 1:length(raw_data)) {
  x <- raw_data[[i]]$title
  title <- c(title, x)
  i <- i + 1
}

View(title)

##extract Bechdel Rating to vector

bechdel <- c()

for (i in 1:length(raw_data)) {
  x <- raw_data[[i]]$rating
  bechdel <- c(bechdel, x)
  i <- i + 1
}

View(bechdel)
summary(bechdel)

##extract year to vector

year <- c()

for (i in 1:length(raw_data)) {
  x <- raw_data[[i]]$year
  year <- c(year, x)
  i <- i + 1
}

View(year)


##merge to dataframe

bechdel_data <- tibble(title, bechdel, year)

bechdel_data %>%
  class()  

bechdel_data %>%
  head()

bechdel_data %>%
  summary()

View(bechdel_data)

##write to csv

bechdel_data %>%
  write_csv("data/bechdel.csv")
