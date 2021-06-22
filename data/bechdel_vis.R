require(tidyverse)
require(ggplot2)

####read bechdel data

bechdel_data <- read_csv("data/bechdel.csv")

bechdel_data %>%
  summary()


####plain bar

plot1 <- ggplot(data = bechdel_data, mapping = aes(y = bechdel)) +
  geom_bar() +
  theme(panel.background = element_rect(), plot.background = element_blank()) +
  scale_y_continuous(name ="Bechdel Rating") +
  ggtitle("Bechdel Ratings", subtitle = "1874 - 2021")

plot1

####scatter plot - hot mess

plot2 <- ggplot(data = bechdel_data, mapping = aes(x = year, y = bechdel)) +
  geom_point() +
  theme(panel.background = element_rect(), plot.background = element_blank()) +
  scale_y_continuous(name ="Bechdel Rating") +
  scale_x_continuous(name ="Film Release Year") +
  ggtitle("Bechdel Ratings", subtitle = "1874 - 2021")

plot2

####average by year

##calculate yearly average

bechdel_data %>%
  group_by(year)  %>%
  mutate(avg = mean(bechdel)) %>%
  select(-title, -bechdel) %>%
  unique() %>%
  ggplot(mapping = aes(x = year, y = avg)) +
  geom_line(color = "#56B4E9") +
  geom_point(size = .4, color = "#0072B2") +
  geom_hline(yintercept = 0, show.legend = T, color = "#009E73", linetype = "dotted") +
  geom_hline(yintercept = 1, show.legend = T, color = "#009E73") +
  geom_hline(yintercept = 2, show.legend = T, color = "#009E73") +
  geom_hline(yintercept = 3, show.legend = T, color = "#009E73") +
  theme(panel.background = element_rect(), plot.background = element_blank()) +
  scale_y_continuous(name ="Average Bechdel Rating") +
  scale_x_continuous(name ="Film Release Year", n.breaks = 14) +
  ggtitle("Average Bechdel Ratings by Year", subtitle = "1874 - 2021") -> plot3

plot3  
  
###plot 4 - bar graph, glaad "Where We Are on TV" annual report - trans representation data

require(readxl)
require(ggthemes)


glaad <- read_excel("raw-data/glaad_trans_data.xlsx", na = "na", col_names = T)

##problem - can't do a stacked bar graph with fill by transgender identity, bc it's three variables, not one grouping variable
##o fix, generate simulation based on counts of each var

#View relevant vars
glaad %>%
  select(total.trans, total.trans.f, total.trans.m, total.trans.nb, year)

#generate representative data for year and gender identity, to match aggregated counts in original
year <- c(rep(2015, 7), rep(2016, 16), rep(2017, 17), rep(2018, 26), rep(2019, 38), rep(2020, 29))
identity <- c(rep("f", 6), rep("m", 1), rep("f", 12), rep("m", 4), rep("f", 9), rep("m", 4), rep("nb", 4), rep("f", 17), rep("m", 5), rep("nb", 4), rep("f", 21), rep("m", 12), rep("nb", 5), rep("f", 15), rep("m",12), rep("nb", 2))

#create a tibble and use this to create stacked bar graph
glaadsim <- as_tibble(cbind(year, identity))
summary(glaadsim)

glaadsim %>%
  mutate(year = as.factor(year), identity = as.factor(identity)) %>%
  group_by(year) %>%
  ggplot(data = glaadsim , mapping = aes(x = year, fill = identity)) +
  geom_bar(width = 0.7) +
  theme_wsj(base_size = 8) +
  theme(legend.key.size = unit(0.5, "cm"), legend.position = "bottom", legend.box = "vertical") +
  scale_x_discrete(name ="Year") +
  scale_fill_manual(name = "Gender Identity", labels = c("Female-Identifying", "Male-Identifying", "Non-Binary"), values = c("aquamarine", "gold", "coral")) +
  annotate("text", x = "2015", y = 8, label = "7" ) +
  annotate("text", x = "2016", y = 17, label = "16") +
  annotate("text", x = "2017", y = 18, label = "17") +
  annotate("text", x = "2018", y = 27, label = "26") +
  annotate("text", x = "2019", y = 39, label = "38") +
  annotate("text", x = "2020", y = 31, label = "29") +
  ggtitle("Total Trans Characters on TV in U.S. (2015 - 2020)", subtitle = "Broadcast, Cable, and Streaming")
