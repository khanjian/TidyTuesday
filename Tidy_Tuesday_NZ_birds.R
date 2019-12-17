# Tidy Tuesday 11/19/2019
# NZ Bird of the Year
# Roupen Khanjian

library(tidyverse) 
library(lubridate)
library(ggthemes)
library(patchwork)
library(wesanderson)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

nz_bird <- nz_bird %>% 
  dplyr::filter(complete.cases(bird_breed) == TRUE) %>% # Remove missing votes
  mutate(Rank = fct_infreq(vote_rank, ordered = TRUE)) %>% # Convert vote_rank into a factor
  mutate(Rank = str_replace(Rank, "vote_", "")) %>% # Change vote column strings. 
  dplyr::select(-vote_rank) %>% 
  unite("time", date:hour, remove = FALSE) %>% # Combine date and hour to a new column named time
  mutate(time = ymd_h(time)) %>% # Convert time to date-times type
  mutate(weekday =  wday(date, label = TRUE, abbr = FALSE)) # Add the weekday 

# Get top 5 birds
nz_bird_top5 <- nz_bird %>% 
  count(bird_breed, sort = TRUE) %>% 
  slice(1:5) 

# Get top 5 birds for my data frame
nz_bird_join <- nz_bird %>% 
  inner_join(nz_bird_top5, by = "bird_breed") %>% 
  arrange(desc(n))

# Color palette
my_colors <-  wes_palette(name = "Darjeeling1", n = 5, type = "discrete")

p1 <- nz_bird_join %>% 
  group_by(time, bird_breed) %>% 
  count(time) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = time, y = n, fill = bird_breed)) +
  geom_col(alpha = 0.95) + 
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  labs(x = "",y = "Frequency", title = "Hourly Voting") +
  theme_bw() +
  scale_fill_manual(values = my_colors)  +
  theme(title = element_text(face="bold", size=11),
        axis.text.y = element_text(face="bold", size=11, angle = 10),
        axis.text.x =  element_text(face="bold", size=11, angle = 10),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "grey", colour = "black"),
        legend.position = "none")

p4 <- nz_bird_join %>% 
  group_by(weekday, hour, bird_breed) %>% 
  count(hour) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = hour, y = n, fill = bird_breed)) +
  geom_col() + 
  facet_grid(weekday ~ .) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = seq(0, 750, by = 250)) +
  labs(x = "Time of day", y = "Frequency") +
  scale_fill_manual(values = my_colors)  +
  theme_bw() +
  theme(axis.text.y = element_text(face="bold", size=11, angle = 10),
        axis.text.x =  element_text(face="bold", size=11, angle = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "grey",colour = "black"),
        strip.text = element_text(face="bold", size=9),
        strip.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", face = "bold", size = 7.75),
        legend.position = c(0,1),
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.key.size = unit(0.255, units = "cm"),
        legend.background = element_rect(fill = "grey"))

p3 <- nz_bird_join %>% 
  group_by(hour, bird_breed) %>% 
  count(hour) %>% 
  ggplot(aes(x = hour, y = n, fill = bird_breed)) +
  geom_col(alpha = 0.95) + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16000, by = 1000)) +
  labs(x = "", y = "Frequency", title = "Hour of Day Voting Occured") +
  theme_bw() +
  scale_fill_manual(name = "Bird Breed", values = my_colors) +
  theme(title = element_text(face="bold", size=11),
        axis.text.y = element_text(face="bold", size=11, angle = 10),
        axis.text.x = element_text(face="bold", size=11, angle = 10),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "grey",colour = "black"),
        legend.position = "none")

p2 <- nz_bird_join %>% 
  group_by(weekday, bird_breed) %>% 
  count(weekday) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = weekday, y = n, fill = bird_breed)) +
  geom_col(alpha = 0.95) + 
  scale_y_continuous(breaks = seq(0, 40000, by = 2500)) +
  labs(x = "",y = "Frequency", title = "Day of Week Voting Occured") +
  theme_bw() +
  scale_fill_manual(values = my_colors) +
  theme(title = element_text(face="bold", size=11),
        axis.text.y = element_text(face="bold", size=11, angle = 10),
        axis.text.x =  element_text(face="bold", size=10.5, angle = 13),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = "grey", colour = "black"))

((p1 / p2 / p3) | p4) +
  plot_annotation(
    title = "Tidy Tuesday: NZ Bird of the Year",
    subtitle = "When did people vote for the top 5 bird breeds?",
    caption = "By Roupen Khanjian",
    theme = theme(plot.title = element_text(face = "bold", size = 16),
                  plot.subtitle = element_text( size = 12),
                  plot.caption = element_text(face = "italic", size = 11))) 

