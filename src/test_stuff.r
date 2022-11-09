

## Count instances by state by month


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
  tx_sales %>%
  pull(month) %>%
  unique(.) %>%
  sort(.)


## loop through years ...
## subset data ...
## create barplot of sales by month for each year ...
## write plot to file
for (m in months) {
  
  p <-
    g %>%
    filter(month == 1) %>%
    select(-month)
    
  pl <- plot_geo() %>%
  add_trace(
    z = p$Percentage, text = state.name, span = I(0),
    locations = state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = q, title='test')
  
  fp <- file.path(dir_out, paste0(y, ".png"))
  
  ggsave(plot = pl, 
         filename = fp, 
         device = "png")==
  
}

####################

## df <- data.frame( state = c("california","texas","nevada","north dakota", rep("NA", 47)),
##                   freq = c(14717, 6842, 1090, 52, rep(0, 47)),
##                   stringsAsFactors = =FALSE )

states <- 
  g %>%
  pull(State) %>%
  unique(.) %>%
  sort(.)

##
library(maps)
library(ggthemes)
states_map <- map_data("State", state = states)
new_map <- merge(g, states_map, by.x = "State", by.y = "region")
new_map <- arrange(new_map, group, order) # to sort polygons in right order

ggplot(new_map, aes(x = long, y = lat, group = group, fill = Percentage)) +
  geom_polygon(color = "black") + 
  coord_map("polyconic") + labs(x = "", y = "") +
  scale_fill_viridis_c(option = "B") +
  ggtitle("1")

#################




states_map$region = state.abb[match(str_to_title(states_map$region),state.name)]







##################


## https://plotly-r.com/maps.html
q <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  title = 'Month 1'
  ## lakecolor = toRGB('white')
)

plot_geo() %>%
  add_trace(
    z = ~g, text = state.name, span = I(0),
    locations = state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = q)






