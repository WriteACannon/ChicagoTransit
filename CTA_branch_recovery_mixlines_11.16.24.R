library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)


#Bring in train ridership data via API Pull
ctatrainrides <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")

#CTA Station info from my KEY file. This provides station details, ridership allocations for multi-line stations, line colors, etc.
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
thismonthnum <- format(as.Date(thisdate,format="%Y-%m-%d"),"%m")
thisyear <- format(as.Date(thisdate,format="%Y-%m-%d"),"%Y")
#Pull the initial date and year later
firstdate <- min(ctastationrides$date)
firstyear <- format(as.Date(firstdate,format="%Y-%m-%d"),"%Y")


#Pare ctastationrides this down to the bare minimum for now
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


#CTA stop key data for each line
ctastationridesmixed<- merge(ctastopkeymixed,ctastationrides, by= "station_id")


#Calculate branch rides for each month compared to 2017-2019 average.
ctabranchridesmixed <- ctastationridesmixed
ctabranchridesmixed$weightrides<- ctabranchridesmixed$rides * ctabranchridesmixed$weight
ctabranchridesmixed$weightpreavg<- ctabranchridesmixed$preavg * ctabranchridesmixed$weight
#Create a new dataset where we consolidate by branch.
ctabranchridesmixed <- ctabranchridesmixed %>% group_by(date,weight_Branch,weight_Hex,year,month) %>% summarise(rides=sum(weightrides),preavg=sum(weightpreavg))
ctabranchridesmixed$nowshare <- ctabranchridesmixed$rides / ctabranchridesmixed$preavg


#calculate rides for each month compared to the 2017-2019 average.
ctastationridesmixed$nowshare <- ctastationridesmixed$rides / ctastationridesmixed$preavg



#####################
#####################
# Its Plotting Time #
#####################
#####################

#Plotting Total Ridership Numbers
ctabranchlineridesmixed <- ggplot(ctabranchridesmixed %>% group_by(weight_Branch))+
  geom_point(data=filter(ctabranchridesmixed, month==thismonthnum),aes(x=date,y=rides,color=weight_Branch),position="identity",shape=1,show.legend = FALSE)+
  geom_point(data=filter(ctabranchridesmixed, date==thisdate),aes(x=date,y=rides,color=weight_Branch),position="identity",shape=19,show.legend = FALSE)+
  geom_line(aes(x=date,y=rides,color=weight_Branch),linetype=1,linewidth=0.5)+
  geom_text_repel(data=ctabranchridesmixed %>% group_by(weight_Branch) %>% filter(date == thisdate),aes(x=date,y=rides,color=weight_Branch,label=paste(weight_Branch,scales::comma(rides))),
                  force             = 0.3,
                  force_pull        = 2,
                  size              = 2.8,
                  nudge_x           = 30,
                  direction         = "y",
                  hjust             = -0.2,
                  segment.size      = 0.2,
                  segment.curvature = 0.1)+
  labs(x="",y="Rides","Ridership",
       title="Chicago Transit Recovery",
       subtitle=paste("CTA Rail Branches - Monthly Ridership through",thismonth,thisyear),
       linetype=2,
       color="Mode:",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",,expand=c(0,0))+
  scale_color_manual(values=ctabranchridesmixed$weight_Hex, guide="none")+
  expand_limits(x = as.Date(c("2017-01-01", "2026-08-01")))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


ctabranchlineridesmixed

ggsave(plot=ctabranchlineridesmixed, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/ctabranchlineridesmixed.png",height=8,width=8)

####

ctabranchlinerecoverymixed <- ggplot(ctabranchridesmixed)+
  geom_point(data=filter(ctabranchridesmixed, month==thismonthnum),aes(x=date,y=nowshare,color=weight_Branch),position="identity",shape=1,show.legend = FALSE)+
  geom_point(data=filter(ctabranchridesmixed, date==thisdate),aes(x=date,y=nowshare,color=weight_Branch),position="identity",shape=19,show.legend = FALSE)+
  geom_line(aes(x=date,y=nowshare,color=weight_Branch),linetype=1,linewidth=0.5)+
  geom_text_repel(data=ctabranchridesmixed %>% group_by(weight_Branch) %>% filter(date == thisdate),aes(x=date,y=nowshare,color=weight_Branch,label=paste(weight_Branch,scales::percent(round(nowshare,2)))),
                  force             = 0.1,
                  force_pull        = 2,
                  size              = 2.5,
                  nudge_x           = 30,
                  direction         = "y",
                  hjust             = -0.3,
                  segment.size      = 0.1,
                  segment.curvature = 0.2)+
  labs(x="",y="Ridership",
       title="Chicago Transit Recovery",
       subtitle=paste("Rail Branch Recovery to Pre-Covid Ridership through",thismonth,thisyear),
       linetype=2,
       color="Mode:",
       caption="Plot: acannon.bsky.social\nData: data.cityofchicago.org")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::percent)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",,expand=c(0,0))+
  scale_color_manual(values=ctabranchridesmixed$weight_Hex, guide="none")+
  expand_limits(x = as.Date(c("2017-01-01", "2026-10-01")))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


ctabranchlinerecoverymixed

ggsave(plot=ctabranchlinerecoverymixed, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/ctabranchlinerecoverymixed.png",height=8,width=8)

###########################
#Bar Graph
###########################

branchrecoverycolmix <- ggplot(ctabranchridesmixed %>% group_by(weight_Branch) %>% filter(date == thisdate))+
  geom_col(aes(x=preavg,y=reorder(weight_Branch,-rides),fill=weight_Branch),alpha=0.2)+
  geom_col(aes(x=rides,y=reorder(weight_Branch,-rides),fill=weight_Branch))+
  geom_text(aes(x=rides,y=reorder(weight_Branch,-rides),label=scales::comma(rides)),hjust=-0.1,size=3.5)+
  geom_text(aes(x=0,y=reorder(weight_Branch,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3.5)+
  scale_fill_manual(values=ctabranchridesmixed$weight_Hex, guide="none")+labs(x="Rides",
       title="CTA Ridership Recovery",
       subtitle=paste(thismonth,thisyear,"Branch recovery to Pre-Covid", thismonth,"ridership"),
       y="Branch",
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

branchrecoverycolmix 

ggsave(plot=branchrecoverycolmix, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/branchrecoverycolmix.png",height=6,width=8)





