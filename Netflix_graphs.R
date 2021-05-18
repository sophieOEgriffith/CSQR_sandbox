require(tidyverse)

##read and subset data
data <- read_csv("netflix_titles.csv")

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

#subset to include only G, PG, PG-13, R
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

#table of duration vs year

tab1 <- table(USmov_rat$duration, USmov_rat$release_year)
tab1

summary(USmov_rat$duration)

#scatterplot of duration over year
####need to rewrite duration as class numeric - requires dropping "min"

duration <- str_split_fixed(USmov_rat$duration, " ", 2)
head(duration)

USmov_rat <- cbind(USmov_rat, duration)
summary(USmov_rat)


USmov_rat <- USmov_rat[,c(1:7,11,9,10)]
names(USmov_rat)[names(USmov_rat)=="1"] <- "duration"
USmov_rat$duration <- as.numeric(USmov_rat$duration)

ggplot(data = USmov_rat, mapping = aes(x = release_year, y = duration, color = rating, shape = rating)) +
  geom_point(alpha = .75) +
  theme(panel.background = element_rect(), plot.background = element_blank()) +
  scale_y_continuous(name ="Film Duration", n.breaks = 12) +
  scale_x_continuous(name = "Release Year", n.breaks = 8) +
  scale_color_discrete(name = "Film Rating") +
  scale_shape_discrete(name = "Film Rating") +
  ggtitle("Duration of U.S. Netflix Films by Rating (1955 - 2020)", subtitle = "1955 - 2020")
  