##read and subset data
data <- read.csv("netflix_titles.csv")

summary(data)
data$type <- as.factor(data$type)
data$country <- as.factor(data$country)

#create data subsets for US TV and Movies, respectively
US <- subset(data,data$country == "United States")
USmovies <- subset(US, US$type == "Movie")
UStv <- subset(US, US$type == "TV Show")

summary(USmovies)

#drop country and type vars
USmovies <- USmovies[,c(1,3:5,7:12)]
UStv <- UStv[,c(1,3:5,7:12)]

head(USmovies)
summary(USmovies)

##visualize movie ratings

USmovies$rating <- as.factor(USmovies$rating)
summary(USmovies)

USmov_rat <-subset(USmovies, USmovies$rating != "TV-MA")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-14")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-PG")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-G")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-Y")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-Y7")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "NR")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "TV-Y7-FV")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "UR")
USmov_rat <-subset(USmov_rat, USmov_rat$rating != "")

summary(USmov_rat)

tab1 <- table(USmov_rat, USmov_rat$release_year)
