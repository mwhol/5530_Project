library(lubridate)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggrepel)
library(usmap)
library(tidyverse)


df1 =read.csv(file="../data_clean/Accidents_Sai.csv")
df1 <- df1 %>% arrange(desc(Severity))
names(df1)
str(df1)
#stacked bar chart to show accident count during Day and night
ggplot(data=df1, aes(Severity,..count..,fill=Sunrise_Sunset))+ 
  geom_bar(stat="count")+
  labs(y="No of Accidents")+ 
  scale_y_continuous(trans='sqrt')+
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5))
ggsave("../results/Stacked_bar_chart.jpg")
#summarise 
k= df1 %>%
  group_by(Severity,Sunrise_Sunset) %>%
  summarise(n=n())
#Dodged bar chart to show accident count during Day and night
ggplot(k, aes(x = Severity, y = n, fill=Sunrise_Sunset)) +
  geom_col(position = position_dodge())+
  labs(y="No of Accidents")+ 
  scale_y_continuous(trans='sqrt')+
  geom_text(aes(label = n), vjust =0)
ggsave("../results/dodged_bar_chart.jpg")
rm(k)
####
k= df1 %>%
  group_by(State) %>%
  summarise(n=n())
ggplot(k, aes(x = State,y = n,fill=State)) +
  geom_col(position = position_dodge())+
  labs(y="No of Accidents")+
  scale_y_continuous(trans='sqrt')
ggsave("../results/statewise_bar_chart.jpg")
rm(k)
k<- df1 %>%
  group_by(City) %>%
  summarise(n=n())
k<-head(k[order(-k$n),],10)

ggplot(data=k, aes(City,n,fill=City))+
  geom_col(position = position_dodge())+
  labs(y="No of Accidents")+
  scale_y_continuous(trans='sqrt')
ggsave("../results/Top10_cities_bar_chart_2016-21.jpg")
rm(k)
k<- df1 %>%
  group_by(Severity) %>%
  summarise(n=n())
k$Severity <-as.character(k$Severity)

k<-k %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

ggplot(k, aes(x = "" , y = n, fill = fct_inorder(Severity))) +
  geom_col(width = 6, color = 3) +
  geom_text(aes(label = Severity),
            position = position_stack(vjust = 0.5))+ 
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = k,
                   aes(y = pos, label = paste0(n)),
                   size = 3, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Severity")) +
  theme_void()
ggsave("../results/Severity_Pie_chart.jpg")

rm(k)
k<-df1 %>% 
  group_by(Timezone)%>%
  summarise(n=n())

ggplot(k, aes(x = Timezone,y = n,fill=Timezone)) +
  geom_col(position = position_dodge())+
  labs(y="No of Accidents")+
  scale_y_continuous(trans='sqrt')
ggsave("../results/Timezone_bar_chart.jpg")
rm(k)

 df1 %>% 
   select(month,Year)%>%
   group_by(month,Year) %>% 
   filter(Year == 2021)%>%
   summarise(total.count=n())%>%
   ggplot( aes(x=month, y=total.count)) +
   geom_line( color="Red") +
   geom_point(shape=21, color="black", fill="#69b3a2") +
   xlim(1,12)+
   ggtitle("Accidents in 2021")
 ggsave("../results/Accidents_2021.jpg") 
 ########
test1<-df1 %>% 
   select(Year,month)%>%
   group_by(Year,month)%>%
   summarise(total.count=n())
 gg <- ggplot(test1)
 gg <- gg + geom_bar(aes(x = month, y = total.count,fill=Year), position = "stack", stat = "identity")
 gg <- gg + scale_x_continuous(breaks=c(1:12))
 gg <- gg + theme()
 gg <- gg + scale_y_continuous(trans='sqrt')
 gg <- gg + facet_wrap(~Year)
 gg
 ggsave("../results/Accidents_Year_month.jpg")

 gg <- ggplot(test1)
 gg <- gg + geom_bar(aes(x = month, y = total.count,fill=Year), position = "stack", stat = "identity")
 gg <- gg + scale_x_continuous(breaks=c(1:12))
 gg <- gg + theme()
 gg <- gg + scale_y_continuous(trans='sqrt')
 gg <- gg + facet_grid(~Year)
 gg
 ggsave("../results/Accidents_Year_month2.jpg")
 ##########
 
 df1 %>% 
   group_by(Year,month,day) %>% 
   filter(month == 11 & Year == 2021)%>%
   summarise(total.count=n())%>%
  ggplot(aes(x=day, y=total.count)) +
  geom_line( color="Red") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  ggtitle("Accidents in Nov 2021")
 ggsave("../results/Accidents_Nov_2021.jpg")

colour_palette = c("red","blue","green","yellow")
df1%>% 
  group_by(Traffic_Signal,Severity)%>%
  summarise(total.count=n())%>%
  ggplot(aes(x = Traffic_Signal, y = total.count, fill=factor(Severity))) +
  scale_fill_manual(values = colour_palette)+
  geom_col(position = position_dodge())+
  labs(y="No of Accidents")+
  scale_y_continuous(trans='sqrt')
ggsave("../results/Accidents_Traffic_Sev.jpg") 
################
k1<- df1 %>%
  group_by(Year,City) %>%
  summarise(n=n())
k1<- k1[order(k1$Year,-k1$n),]
#Extracting Top 10 cities with highest accident number from each year
k1 <-Reduce(rbind,by(k1,k1["Year"],head,n=10))
gg <- ggplot(k1)
gg <- gg + geom_bar(aes(x =Year, y = n,fill=City), position="Dodge", stat = "identity")
gg <- gg + scale_x_continuous(breaks=c(2016:2021))
gg <- gg + scale_y_continuous(trans='sqrt')
gg <- gg + facet_grid(~Year)
gg
ggsave("../results/Top10_cities_2016-2021.jpg")



k1<- df1 %>%
  group_by(Severity, Junction) %>%
  summarise(n=n())

# Stacked + percent
ggplot(k1, aes(fill=Severity, y=n, x=Junction)) + 
  geom_bar(position="fill", stat="identity")
ggsave("../results/2.jpg")


#top_wcs = names(sort(summary(as.factor(df1$Weather_Condition), decreasing=T)[1:20]))

k1<- df1 %>%
  group_by(Severity, Weather_Condition) %>%
  summarise(n=n()) 

# Stacked + percent
ggplot(k1, aes(fill=Severity, y=n, x=Weather_Condition)) + 
  geom_bar(position="fill", stat="identity")
ggsave("../results/3.jpg")
