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
  

