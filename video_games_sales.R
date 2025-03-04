install.packages("tidyverse")
install.packages("broom")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("gridGraphics")
install.packages("hrbrthemes")
install.packages("ggtext")
install.packages("gganimate")
install.packages("gifski")
install.packages("corrplot")

library(tidyverse)
library(tidyr)
library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(ggpubr)
library(gridGraphics)
library(hrbrthemes)
library(ggtext)
library(gganimate)
library(gifski)
library(corrplot)

video_games <- read.csv(file = 'data/Video_Games_Sales.csv')

# genre sales
video_games_copy <- video_games

video_games_copy <- video_games %>%
  group_by(Genre) %>%
  summarise(n = n())

video_games_copy %>%
  ggplot(aes(n, reorder(Genre, -n))) +
  geom_col() +
  labs(y = "Platform", x = "Amount") +
  ggtitle('<span style="color:brown"> Video Games Genres </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## genre sales

# genre sales by regions
video_games_copy <- video_games %>%
  filter(Genre != "") %>%
  group_by(Genre) %>%
  summarise(
    n = n(),
    sumNA = sum(NA_Sales, na.rm = TRUE),
    sumEU = sum(EU_Sales, na.rm = TRUE),
    sumJP = sum(JP_Sales, na.rm = TRUE),
    sumOther = sum(Other_Sales, na.rm = TRUE)
  ) %>%
  ungroup()

sales_data <- video_games_copy %>%
  pivot_longer(cols = starts_with("sum"),
               names_to = "condition",
               values_to = "regionValue")

ggplot(sales_data, aes(fill = condition, x = Genre, y = regionValue)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Genre", y = "Region", fill = "Global Sales") +
  ggtitle('<span style="color:brown"> Genre Sales By Regions </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## genre sales by regions

# histogram of games sales
video_games_copy <- video_games
video_games_copy <- video_games_copy %>%
  mutate(Gr = ifelse(Global_Sales > 10, 'more than 10M', 'o')) %>%
  mutate(Gr = ifelse(Global_Sales > 1 & Global_Sales <= 10, '1M to 10M', Gr)) %>%
  mutate(Gr = ifelse(Global_Sales > 0.1 & Global_Sales <= 1, '0.1M to 1M', Gr)) %>%
  mutate(Gr = ifelse(Global_Sales > 0.01 & Global_Sales <= 0.1, '0.01M to 0.1M', Gr)) %>%
  mutate(Gr = ifelse(Global_Sales <= 0.01, '0.01M or less', Gr))

video_games_copy %>%
  ggplot(aes(x = factor(Gr), fill = Gr)) +
  labs(x = 'Group', y = 'Amount', fill = "Group") +
  geom_histogram(stat = "count") +
  ggtitle('<span style="color:brown"> Groups Global Sales Histogram </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## histogram of games sales

# histogram of platform sales
nintendo_platforms <- video_games %>%
  filter(Publisher == "Nintendo") %>%
  select(Platform) %>%
  unique()
nintendo_platforms <- nintendo_platforms[[1]]

video_games_copy <- video_games %>%
  group_by(Platform) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(Nintendo = Platform %in% nintendo_platforms)
video_games_copy %>%
  ggplot(aes(x=n, y=reorder(Platform, n), fill=!Nintendo)) +
  geom_col() +
  labs(x = "Amount", y = "Platform") +
  ggtitle('<span style="color:brown"> Platform Sales Histogram </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5)) +
  theme(legend.position = 'none')
## histogram of platform sales

# dynamic of top-10 platforms game sales
video_games_copy <- video_games %>%
  filter(Year_of_Release != 'N/A' & !(Platform == 'DS' & Year_of_Release == 1985)) %>%
  mutate(Year_of_Release = as.integer(Year_of_Release))

top10_platforms <- video_games_copy %>%
  group_by(Platform) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10)

top10_platforms <- (top10_platforms %>% select(Platform))[[1]]

p <- video_games_copy %>%
  filter(Platform %in% top10_platforms & Year_of_Release <= 2016) %>%
  group_by(Platform, Year_of_Release) %>%
  summarise(n = sum(Global_Sales)) %>%
  ggplot( aes(x=Year_of_Release, y=n, group=Platform, color=Platform)) +
  geom_line(size=2) +
  labs(x = "Year", y = "Sales") +
  ggtitle('<span style="color:brown"> The Dynamic Of Video Games Sales By Platforms </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5)) +
  transition_reveal(Year_of_Release)
animate(p, duration = 10, height = 600, width = 2000, renderer = gifski_renderer(), end_pause=1)
## dynamic of top-10 platforms game sales

# top-20 global sales of games on one and on all platforms
jp_publishers <- c("Namco Bandai Games", "5pb", "Konami Digital Entertainment",
                   "Nintendo", "Banpresto", "Hudson Soft", "Sega",
                   "Sony Computer Entertainment", "Capcom", "Tecmo Koei",
                   "Square Enix", "Nippon Ichi Software", "Idea Factory",
                   "D3Publisher")

top20_sales_games <- video_games %>%
  arrange(desc(Global_Sales)) %>%
  slice(1:20) %>%
  mutate(Nintendo = Publisher %in% jp_publishers) %>%
  ggplot(aes(Global_Sales, reorder(Name, Global_Sales), fill=!Nintendo)) +
  geom_col() +
  ggtitle('<span style="color:brown"> Single </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain')) +
  labs(x = "Sales", y = "Name") +
  theme(legend.position = 'none')

video_games_copy <- video_games %>%
  group_by(Name, Publisher) %>%
  summarise(s = sum(Global_Sales)) %>%
  filter(Name != "") %>%
  mutate(Nintendo = Publisher %in% jp_publishers)

video_games_copy <- video_games_copy %>% arrange(desc(s))
video_games_copy <- video_games_copy[1:20,]

top20_sales_filter <- ggplot(video_games_copy, aes(x=s, y=reorder(Name, s))) +
  geom_col(aes(fill=!Nintendo)) +
  ggtitle('<span style="color:brown"> In Total </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain')) +
  labs(x = "Sales", y = "Name") +
  theme(legend.position = 'none')

ggarrange(top20_sales_games, top20_sales_filter) +
  ggtitle('<span style="color:brown"> Games Global Sales </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain'))
## top-20 global sales of games on one and on all platforms

# dynamic region sales
sales_data <- video_games %>%
  filter(Year_of_Release <= 2016) %>%
  group_by(Year_of_Release) %>%
  summarise(sNA = sum(NA_Sales),
            sEU = sum(EU_Sales),
            sJP = sum(JP_Sales),
            sOther = sum(Other_Sales))

sales_data <- sales_data %>%
  mutate(Year_of_Release = as.integer(Year_of_Release))

ggplot() +
  geom_line(data=sales_data, aes(x=Year_of_Release,y=sNA),color='red', size=2, group=1) + 
  geom_line(data=sales_data, aes(x=Year_of_Release,y=sEU),color='green', size=2, group=1) +
  geom_line(data=sales_data, aes(x=Year_of_Release,y=sJP),color='blue', size=2, group=1) +
  geom_line(data=sales_data, aes(x=Year_of_Release,y=sOther),color='black', size=2, group=1) +
  geom_textbox(
    data = sales_data,
    aes(x=1980, y=0, label = '<span style="color:red"> NA </span><br><span style="color:green"><b> EU </b></span><br><span style="color:blue"><b> JP </b></span><br><span style="color:black"><b> Other </b></span>'),
    width = grid::unit(0.1, "npc"),
    hjust = -0, vjust = -1
  ) +
  labs(x = "Year", y = "Sales") +
  ggtitle('<span style="color:brown"> The Dynamic Of Video Games Sales By Regions </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## dynamic region sales

# comparison of number of games and number of sales of jp and non-jp publishers
video_games_copy <- video_games
video_games_copy1 <- video_games_copy %>%
  filter(Publisher != "Unknown") %>%
  group_by(Publisher) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  slice(1:20)

video_games_copy2 <- video_games_copy %>%
  filter(Publisher != "Unknown") %>%
  group_by(Publisher) %>%
  summarise(n = sum(Global_Sales)) %>%
  arrange(-n) %>%
  slice(1:20)

video_games_copy1 <- video_games_copy1 %>%
  mutate(IsJP = Publisher %in% jp_publishers)

video_games_copy2 <- video_games_copy2 %>%
  mutate(IsJP = Publisher %in% jp_publishers)

count_games <- video_games_copy1 %>%
  ggplot(aes(n, reorder(Publisher, n), fill=IsJP, color="red")) +
  geom_col() +
  labs(x = "Amount", y = "Publisher") +
  scale_fill_manual(values = c("grey", "orange")) +
  theme_ipsum() +
  theme(legend.position = 'none') +
  ggtitle('<span style="color:brown"> Count Games </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))

count_sales <- video_games_copy2 %>%
  ggplot(aes(n, reorder(Publisher, n), fill=IsJP, color="red")) +
  geom_col() +
  labs(x = "Amount", y = "Publisher") +
  scale_fill_manual(values = c("grey", "orange")) +
  theme_ipsum() +
  theme(legend.position = 'none') +
  ggtitle('<span style="color:brown"> Count Sales </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))

ggarrange(count_games, count_sales) +
  ggtitle('<span style="color:brown"> JP Companies On Video Games Market </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## comparison of number of games and number of sales of jp and non-jp publishers

# dynamic of top-5 publishers game sales
video_games_copy <- video_games %>%
  filter(Year_of_Release != 'N/A') %>%
  mutate(Year_of_Release = as.integer(Year_of_Release))

top5_publishers <- video_games_copy %>%
  group_by(Publisher) %>%
  summarise(n = sum(Global_Sales)) %>%
  arrange(desc(n)) %>%
  slice(1:5)

top5_publishers <- (top5_publishers %>% select(Publisher))[[1]]

p <- video_games_copy %>%
  filter(Publisher %in% top5_publishers & Year_of_Release <= 2016) %>%
  group_by(Publisher, Year_of_Release) %>%
  summarise(n = sum(Global_Sales)) %>%
  ggplot( aes(x=Year_of_Release, y=n, group=Publisher, color=Publisher)) +
  geom_line(size=2) +
  labs(x = "Year", y = "Sales") +
  ggtitle('<span style="color:brown"> The Dynamic Of Video Games Sales By Publishers </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5)) +
  transition_reveal(Year_of_Release)
animate(p, duration = 10, height = 600, width = 2000, renderer = gifski_renderer(), end_pause=1)
## dynamic of top-5 publishers game sales

# user and critic score histogram
filtered_video_games <- video_games %>%
  filter(User_Count > 20, Critic_Count > 5)

g1 <- filtered_video_games %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(color='red', bins = 25) +
  labs(x = 'Score', y = 'Amount') +
  ggtitle('<span style="color:brown"> Critics </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))

g2 <- filtered_video_games %>%
  ggplot(aes(User_Score)) +
  geom_histogram(color='red', bins = 25) +
  labs(x = 'Score', y = 'Amount') +
  ggtitle('<span style="color:brown"> Users </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))

ggarrange(g1, g2) +
  ggtitle('<span style="color:brown"> Critics and Users Histograms </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## user and critic score histogram

# correlation user and critic scores
video_games_copy <- video_games
video_games_copy <- video_games_copy %>%
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>%
  filter(Critic_Count > 5 & User_Count > 20) %>%
  mutate(Result = ifelse(Critic_Score > as.integer(User_Score * 10) + 20,  "no", "yes")) %>%
  mutate(Result = ifelse(Critic_Score < as.integer(User_Score * 10) - 20, "no", Result))

ggplot(video_games_copy, aes(x=Critic_Score, y=User_Score, color=Result)) + 
  geom_point(size=2) +
  theme_ipsum() +
  theme(legend.position = 'none') +
  ggtitle('<span style="color:brown"> User And Critic Scores Correlation </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## correlation user and critic scores

# user and critic score of top 100 games
video_games_copy <- video_games
video_games_copy <- video_games_copy %>%
  mutate(Key=paste(Name, Platform, sep=""))

top_100_keys <- video_games_copy %>%
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>%
  filter(Critic_Count > 5 & User_Count > 20) %>%
  arrange(desc(Global_Sales))
top_100_keys <- top_100_keys[1:100,]$Key

video_games_copy <- video_games_copy %>%
  filter(Key %in% top_100_keys)

ggplot(video_games_copy, aes(x=Critic_Score, y=User_Score)) + 
  geom_point(size=2, color='blue') +
  theme_ipsum() +
  theme(legend.position = 'none') +
  ggtitle('<span style="color:brown"> User And Critic Scores Correlation Top 100 Games </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## user and critic score of top 100 games

# comparison of user and critic score of top 100 games with other games
video_games_copy <- video_games
video_games_copy <- video_games_copy %>%
  mutate(Key=paste(Name, Platform, sep=""))

top_100_keys <- video_games_copy %>%
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>%
  filter(Critic_Count > 5 & User_Count > 20) %>%
  arrange(desc(Global_Sales)) %>% 
  slice(1:100)
top_100_keys <- top_100_keys$Key


video_games_copy <- video_games_copy %>%
  mutate(Top100 = Key %in% top_100_keys) %>%
  arrange(Top100)

ggplot(video_games_copy, aes(x=Critic_Score, y=User_Score)) + 
  geom_point(aes(color=Top100)) +
  theme_ipsum() +
  theme(legend.position = 'none') +
  ggtitle('<span style="color:brown"> User And Critic Scores Correlation Top 100 Games </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## comparison of user and critic score of top 100 games with other games

# correlogram
video_games_copy <- video_games %>%
  filter(Year_of_Release != "N/A", Name != "", Genre != "", !is.na(Critic_Count),
         !is.na(Critic_Score), !is.na(User_Count), !is.na(User_Score),
         Developer != "", Rating != "", User_Count > 20, Critic_Count > 5) %>%
  select(Year_of_Release, User_Score, Critic_Score, Global_Sales)

video_games_copy <- video_games_copy %>%
  mutate(Year_of_Release = as.numeric(Year_of_Release)) %>%
  mutate(Global_Sales = as.numeric(Global_Sales)) %>%
  mutate(User_Score = as.numeric(User_Score)) %>%
  mutate(Critic_Score = as.numeric(Critic_Score))

corrplot(cor(video_games_copy), method = "color")
## correlogram

# video games rating count
video_games_rating <- video_games %>%
  group_by(Rating) %>%
  summarise(n = n())

video_games_rating %>%
  ggplot(aes(n, reorder(Rating, -n))) +
  geom_col() +
  labs(x = "Amount", y = "Rating") +
  ggtitle('<span style="color:brown"> Video Games Ratings </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## video games rating count

# pie chart of comparison of global video games rating with nintendo
plot_pie = function(content, fill_color, title){
  
  content_copy <- content
  
  content_copy <- content_copy %>%
    filter(Rating != "") %>%
    mutate(Rating = ifelse(Rating == "E" | Rating == "T" | Rating == "E10+" | Rating == "M", Rating, "Other"))
  
  content_copy <- content_copy %>%
    group_by(Rating) %>%
    summarise(n = n())
  
  content_copy <- content_copy %>%
    arrange(desc(n)) %>%
    mutate(NPercent = n / sum(n) * 100) %>%
    mutate(NPercentString = ifelse(NPercent < 5, "", sprintf("%1.1f%%", NPercent)))
  
  
  data <- data.frame(
    Rating=content_copy$Rating,
    N=content_copy$n,
    perc=content_copy$NPercentString
  )
  
  data <- data %>% arrange(N)
  
  data <- data %>% 
    mutate(prop = N / sum(data$N) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  return (ggplot(data, aes(x="", y=prop, fill=factor(-ypos, labels=rev(Rating)), color="red", label=N)) +
            labs(fill="Rating") +
            scale_fill_manual(values=fill_color) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) +
            theme_void() +
            ggtitle(title) +
            theme(plot.title = element_text(color = "blue")) +
            geom_text(aes(y = ypos, label = perc), color = "brown", size=6, vjust = "outward") +
            ggtitle(paste('<span style="color:brown">', title, '</span>')) +
            theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5)))
}

colors <- c("beige", "pink", "green", "#F69173", "dark red")
colors_nintendo <- c("beige", "pink", "#F69173", "green")

nintendo_data <- video_games %>%
  filter(Publisher == "Nintendo" & Rating != "")

nintendo_pie <- plot_pie(nintendo_data, colors_nintendo, "Nintendo")
video_games_pie <- plot_pie(video_games, colors, "All")

ggarrange(video_games_pie, nintendo_pie) +
  ggtitle('<span style="color:brown"> Video Games Ratings Structure </span>') +
  theme(plot.title = element_markdown(size = 18, face = 'plain', hjust = 0.5))
## pie chart of comparison of global video games rating with nintendo