library(maps)
library(lubridate)
library(tidyverse)
library(magick)


df <- US_Accidents_Dec21_updated

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

imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

fp <- file.path(dir_out, paste0(1, ".gif"))

## save to disk
image_write(image = img_animated,
            path = fp)




