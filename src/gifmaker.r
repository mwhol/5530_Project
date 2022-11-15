library(maps)
library(lubridate)
library(tidyverse)
library(magick)


df <- read.csv("../data_clean/Accidents_Sai.csv", stringsAsFactors = TRUE)

df$month <- month(df$Start_Time)

g <- df %>%
  filter(!is.na(State)) %>% 
  group_by(State, month) %>%
  summarise(Percentage = n()) %>%
  group_by(State) %>%
  mutate(Percentage=Percentage/sum(Percentage)*100)

df1 = data.frame(month = c(1 : 12),
                 State = "HI",
                 Percentage = 0)

g <- rbind(g, df1)
rm(df)

## create a directory to which the images will be written
dir_out <- file.path(tempdir(), "wrecksbymonth")
dir.create(dir_out, recursive = TRUE)

## get a sorted list of unique years in the TX housing dataset
months <- 
  g %>%
  pull(month) %>%
  unique(.) %>%
  sort(.)


## loop through years ...
## subset data ...
## create barplot of sales by month for each year ...
## write plot to file
states_map <- map_data("state")
states_map$region = state.abb[match(str_to_title(states_map$region),state.name)]

for (m in months) {
  
  p <-
    g %>%
    filter(month == m) %>%
    select(-month)
  
  new_map <- merge(p, states_map, by.x = "State", by.y = "region")
  new_map <- arrange(new_map, group, order) # to sort polygons in right order
  
  pl <- ggplot(new_map, aes(x = long, y = lat, group = group, fill = Percentage)) +
    geom_polygon(color = "black") + 
    coord_map("polyconic") + labs(x = "", y = "") +
    # scale_fill("heat", limits=c(0,50)) + 
    scale_fill_viridis_c(option = "plasma", limits=c(0,50), direction=-1) +
    ggtitle(paste("Month:", m)) +
    theme(legend.position="bottom", legend.key.size = unit(0.5, 'cm'))

  m2 = str_pad(m, 2, pad = "0")
  fp <- file.path(dir_out, paste0(m2, ".png"))
  
  ggsave(plot = pl, 
         filename = fp, 
         device = "png",
         scale=2)
    
}


##############

images <- list.files(dir_out, full.names = TRUE)
image_list <- lapply(images, image_read)

images <- image_join(image_list)
gif <- image_animate(images, fps = 2)

fp <- file.path("../results/", paste0("map_by_month", ".gif"))

## save to disk
image_write(image = gif, path = fp)




