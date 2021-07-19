# Librerías ====
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Lectura de datos ====
data <- read_csv("data/mushrooms_clean.csv") %>% janitor::clean_names()

# Limpieza de datos ====
data_clean <- data %>% select(-c(x24, x25, x26, x27)) %>% 
  filter(!is.na(class))
# Reescribor datos limpios
# Modificaciones
data_clean %>% mutate(cap_color = replace(cap_color, cap_color == 'cinammon', 'cinnamon')) %>% 
  write_csv("data/mushrooms_clean2.csv")


# Visualizaciones ====
data_clean %>% group_by(class, odor) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(class, count, fill = odor)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle("Mushrooms by odor") +
  xlab("Class") + ylab("Count") + scale_y_comma() +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum() +
  ggsave("odor.png")


data_clean %>% group_by(class, stalk_root) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(class, count, fill = stalk_root)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle("Mushrooms by Stalk Root") +
  xlab("Class") + ylab("Count of mushrooms") +
  scale_y_comma() +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum() +
  ggsave("stalk_root.png")

data_clean %>% group_by(class, stalk_surface_below_ring)%>% 
  summarise(count = n()) %>% 
  ggplot(aes(class, count, color = stalk_surface_below_ring)) +
  geom_point() +
  ggtitle("Mushrooms by Stalk Root") +
  xlab("Class") + ylab("Count of mushrooms") +
  scale_y_comma() +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum()

data_clean %>% group_by(class, stalk_surface_below_ring)%>% 
  summarise(count = n()) %>% 
  mutate(class_stalk = str_c(class, stalk_surface_below_ring, sep = "-")) %>% 
  select(class_stalk, count, stalk_surface_below_ring) %>% 
  ggplot(aes(class_stalk, count)) +
  geom_segment(aes(x = class_stalk ,xend = class_stalk, y = 0, yend = count), color="black") +
  geom_point(aes(color = stalk_surface_below_ring), size = 4) +
  coord_flip() +
  ylab("Number of mushrooms") + xlab("") +
  ggtitle("Mushrooms with Stalk surface below ring") +
  theme_ipsum() + theme(legend.position = "none") +
  ggsave("saltk_surface.png")





