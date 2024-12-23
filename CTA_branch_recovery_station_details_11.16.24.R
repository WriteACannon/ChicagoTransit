library(tidyverse)
library(ggrepel)
library(extrafont)


#Bring in train ridership data via API Pull
ctatrainrides <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")

#CTA Station info from my KEY file.
ctastopkey <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata//sources/ctastopkey.csv")
ctastopkeymixed <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata//sources/ctastopkeymixed.csv")

#Creating this variable so I don't keep pulling from the API.
ctastationrides <- ctatrainrides
# Coding Randolph-Wabash to become Washington-Wabash
ctastationrides$station_id <- replace(ctastationrides$station_id,ctastationrides$station_id == "40200", 41700)
ctastationrides <- ctastationrides %>% group_by(station_id, month_beginning) %>% summarise(monthtotal=sum(monthtotal))
#Clean up Date column
ctastationrides$date <-  as.Date(ctastationrides$month_beginning, format="%Y-%m-%d")
#Pull the year out and filter to only 2017 and onward
ctastationrides$year <- format(as.Date(ctastationrides$date, format="%Y-%m-%d"),"%Y")
ctastationrides$month <- format(as.Date(ctastationrides$date, format="%Y-%m-%d"),"%m")
ctastationrides <- filter(ctastationrides,year>2016)

#Pull the date and name of the most recent month for later
thisdate <- max(ctastationrides$date)
thismonth <- format(as.Date(thisdate,format="%Y-%m-%d"),"%b")
thisyear <- format(as.Date(thisdate,format="%Y-%m-%d"),"%Y")
#Pull the initial date and year later
firstdate <- min(ctastationrides$date)
firstyear <- format(as.Date(firstdate,format="%Y-%m-%d"),"%Y")


#Pare this down to the bare minimum for now
ctastationrides <- ctastationrides %>% select(station_id,monthtotal,year,month)



#Getting the pre-covid average ridership for each station. This is the 2017-2019 avg for each month.

#Pivot Wider so every year is its own column. Since not every station has data for every year UNCHOP to avoid NULL values.
stationswider <- pivot_wider(ctastationrides,names_from=year,values_from=monthtotal) %>% unchop(everything())
#Calculate the 2017-2019 avg
stationswider$preavg <- ((stationswider$"2017" + stationswider$"2018" + stationswider$"2019") / 3)
#Go back to a long format
ctastationrides <- stationswider %>% pivot_longer(cols=firstyear:thisyear,names_to="year",values_to="rides")
#Turn NAs into zeroes
ctastationrides <- ctastationrides %>% replace(is.na(.), 0)
#Recreate our date column from the month and year
ctastationrides$date <- paste(ctastationrides$year,"-",ctastationrides$month,"-","01",sep="")
ctastationrides$date <-  as.Date(ctastationrides$date, format="%Y-%m-%d")
#Filter out months we don't have data for
ctastationrides <- ctastationrides %>% filter(date<=thisdate)


#Calculate branch rides for each month compared to 2017-2019 average.
ctabranchridesmixed <- merge(ctastopkeymixed,ctastationrides, by= "station_id")
ctabranchridesmixed$weightrides<- ctabranchridesmixed$rides * ctabranchridesmixed$weight
ctabranchridesmixed$weightpreavg<- ctabranchridesmixed$preavg * ctabranchridesmixed$weight
#Create a new dataset where we consolidate by branch.
ctabranchridesmixed <- ctabranchridesmixed %>% group_by(date,weight_Branch,weight_Hex,year,month) %>% summarise(rides=sum(weightrides),preavg=sum(weightpreavg))
ctabranchridesmixed$nowshare <- ctabranchridesmixed$rides / ctabranchridesmixed$preavg

#CTA stop key data for each line, with mix
ctastationridesmixed<- merge(ctastopkeymixed,ctastationrides, by= "station_id")
#calculate rides for each month compared to the 2017-2019 average.
ctastationridesmixed$nowshare <- ctastationridesmixed$rides / ctastationridesmixed$preavg
ctastationridesmixed <- ctastationridesmixed %>% filter(rides != 0)



#####################
#####################
# Its Plotting Time #
#####################
#####################

###########################
#Bar Graph
###########################


##########
#going to dig into some branches





brownstops <- ggplot(ctastationridesmixed %>% filter(Brown =="Yes") %>% filter(date == thisdate )%>% filter(Branch != "Loop"))+
  geom_col(aes(x=preavg,y=reorder(NAME,-rides),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(NAME,-rides),fill=Hex), position="dodge")+
  geom_text(aes(x=0,y=reorder(NAME,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3)+
  geom_text(aes(x=rides,y=reorder(NAME,-rides),label=scales::comma(rides)),hjust=-0.05,size=3.5)+
  scale_fill_identity(guide="none")+
    labs(x="Rides",
     title="CTA Ridership Recovery - Brown Line",
     subtitle=paste(thismonth,thisyear,"recovery to Pre-Covid", thismonth,"ridership"),
     y="Station",
     caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


brownstops

ggsave(plot=brownstops, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/brownstops.png",height=8,width=8)

#################################

pinkstops <- ggplot(ctastationridesmixed %>% filter(Pink =="Yes") %>% filter(date == thisdate )%>% filter(Branch != "Loop"))+
  geom_col(aes(x=preavg,y=reorder(NAME,-rides),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(NAME,-rides),fill=Hex), position="dodge")+
  geom_text(aes(x=0,y=reorder(NAME,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3)+
  geom_text(aes(x=rides,y=reorder(NAME,-rides),label=scales::comma(rides)),hjust=-0.05,size=3.5)+
  scale_fill_identity(guide="none")+
  labs(x="Rides",
       title="CTA Ridership Recovery - Pink Line",
       subtitle=paste(thismonth,thisyear,"recovery to Pre-Covid", thismonth,"ridership"),
       y="Station",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


pinkstops

ggsave(plot=pinkstops, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkstops.png",height=8,width=8)


#################################

Orangestops <- ggplot(ctastationridesmixed %>% filter(Orange =="Yes") %>% filter(date == thisdate )%>% filter(Branch != "Loop"))+
  geom_col(aes(x=preavg,y=reorder(NAME,-rides),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(NAME,-rides),fill=Hex), position="dodge")+
  geom_text(aes(x=0,y=reorder(NAME,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3)+
  geom_text(aes(x=rides,y=reorder(NAME,-rides),label=scales::comma(rides)),hjust=-0.05,size=3.5)+
  scale_fill_identity(guide="none")+
  labs(x="Rides",
       title="CTA Ridership Recovery - Orange Line",
       subtitle=paste(thismonth,thisyear,"recovery to Pre-Covid", thismonth,"ridership"),
       y="Station",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


Orangestops

ggsave(plot=Orangestops, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/Orangestops.png",height=8,width=8)



#################################

Loopstops <- ggplot(ctastationridesmixed %>% filter(date == thisdate )%>% filter(Branch == "Loop"))+
  geom_col(aes(x=preavg,y=reorder(NAME,-rides),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(NAME,-rides),fill=Hex), position="dodge")+
  geom_text(aes(x=0,y=reorder(NAME,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3)+
  geom_text(aes(x=rides,y=reorder(NAME,-rides),label=scales::comma(rides)),hjust=-0.05,size=3.5)+
  scale_fill_identity(guide="none")+
  labs(x="Rides",
       title="CTA Ridership Recovery - Loop",
       subtitle=paste(thismonth,thisyear,"recovery to Pre-Covid", thismonth,"ridership"),
       y="Station",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


Loopstops

ggsave(plot=Loopstops, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/Loopstops.png",height=8,width=8)













ctatopstops <- ggplot(ctastationridesmixed %>% filter(date == thisdate) %>% filter(rides > 5) )+
  geom_col(aes(x=preavg,y=reorder(STATION,-rides),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(STATION,-rides),fill=Hex), position="dodge")+
  geom_text(aes(x=rides,y=reorder(STATION,-rides),label=scales::comma(rides)),hjust=-0.1,size=2.5)+
  geom_text(aes(x=0,y=reorder(STATION,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=2.5)+
  scale_fill_identity(guide="none")+
  labs(x="Rides",
       title="CTA Station Ridership Recovery",
       subtitle=paste(thismonth,thisyear,"Station recovery to Pre-Covid", thismonth,"ridership"),
       y="Station",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


ctatopstops

ggsave(plot=ctatopstops, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/ctatopstops2.png",height=20,width=8)


#############################################

ctatopstopshead <- ggplot(ctastationridesmixed %>% filter(date == thisdate) %>% arrange(-nowshare) %>% head(20) )+
  geom_col(aes(x=preavg,y=reorder(STATION,-nowshare),fill=Hex),alpha=0.2, position="dodge")+
  geom_col(aes(x=rides,y=reorder(STATION,-nowshare),fill=Hex), position="dodge")+
  geom_text(aes(x=rides,y=reorder(STATION,-nowshare),label=scales::comma(rides)),hjust=-0.1,size=2.5)+
  geom_text(aes(x=0,y=reorder(STATION,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=2.5)+
  scale_fill_identity(guide="none")+
  labs(x="Rides",
       title="CTA Station Ridership Recovery - Top Recoveries",
       subtitle=paste(thismonth,thisyear,"Station recovery to Pre-Covid", thismonth,"ridership"),
       y="Station",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


ctatopstopshead

ggsave(plot=ctatopstopshead, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/ctatopstopshead.png",height=8,width=8)
