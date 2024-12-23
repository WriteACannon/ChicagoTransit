library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)

#Bringing in Metra Data. Latest available here:
#https://rtams.org/media/resources/metra-monthly-ridership-line 
metrarides <- read.csv("Metra_Monthly_Ridership_by_Line_2002_2024.csv")

#bringing in color data
metracolors <- read.csv("metracolors.csv")

#dates
metrarides$date <- paste(metrarides$YEAR,"-",metrarides$MONTH,"-","01",sep="")
metrarides$date <-  as.Date(metrarides$date, format="%Y-%m-%d")
names(metrarides)[names(metrarides)== 'YEAR'] <- "year"
names(metrarides)[names(metrarides)== 'MONTH'] <- "month"
#filter to 2017 and beyond
metrarides <- filter(metrarides,year>2016)

#Pull the date and name of the most recent month for later
thisdate <- max(metrarides$date)
thismonth <- format(as.Date(thisdate,format="%Y-%m-%d"),"%b")
thismonthnum <- format(as.Date(thisdate,format="%Y-%m-%d"),"%m")
thisyear <- format(as.Date(thisdate,format="%Y-%m-%d"),"%Y")
#Pull the initial date and year later
firstdate <- min(metrarides$date)
firstyear <- format(as.Date(firstdate,format="%Y-%m-%d"),"%Y")



#Remove date for widening purposes
metrarides <- metrarides %>% group_by(month, year,LONGNAME) %>% summarise(rides=sum(RIDES))


#Pivot Wider so every year is its own column. Since not every station has data for every year UNCHOP to avoid NULL values.
metrawider <- pivot_wider(metrarides,names_from=year,values_from=rides) %>% unchop(everything())
#Calculate the 2017-2019 avg
metrawider$preavg <- ((metrawider$"2017" + metrawider$"2018" + metrawider$"2019") / 3)
#Go back to a long format
metrarides <- metrawider %>% pivot_longer(cols=firstyear:thisyear,names_to="year",values_to="rides")
#Turn NAs into zeroes
metrarides <- metrarides %>% replace(is.na(.), 0)
#Recreate our date column from the month and year
metrarides$date <- paste(metrarides$year,"-",metrarides$month,"-","01",sep="")
metrarides$date <-  as.Date(metrarides$date, format="%Y-%m-%d")
#Filter out months we don't have data for
metrarides <- metrarides %>% filter(date<=thisdate)

metrarides$nowshare <- metrarides$rides / metrarides$preavg
metrarides <- right_join(metrarides,metracolors,by="LONGNAME")

#some summary data
metratotal <- metrarides %>% filter(date == thisdate)
metratotal <- metratotal %>% summarise(rides=sum(rides),preavg=sum(preavg))
metratotal$nowshare <- metratotal$rides / metratotal$preavg



#Plot
metracols <- ggplot(metrarides %>% group_by(shortname) %>% filter(date == thisdate))+
  geom_col(aes(x=preavg,y=reorder(shortname,-rides),fill=Hex),alpha=0.2)+
  geom_col(aes(x=rides,y=reorder(shortname,-rides),fill=Hex))+
  geom_text(aes(x=0,y=reorder(shortname,-rides),label=scales::percent(nowshare,accuracy=1)),hjust=1,size=3)+
  geom_text(aes(x=rides,y=reorder(shortname,-rides),label=scales::comma(rides)),hjust=-.1,size=3)+
  scale_fill_identity(guide="none")+
      labs(x="Rides",
      title="Metra Ridership Recovery",
      subtitle=paste("share of pre-covid rides as of",thismonth,thisyear,"\n",scales::comma(metratotal$rides),"rides,",scales::percent(metratotal$nowshare),"recovered"),
      y="Line",
      caption="Plot: acannon.bsky.social\nData: rtams.org/ridership/metra/lines")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 8))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))

metracols

ggsave(plot=metracols, "metracols.png",height=6,width=8)




metralines <- ggplot(metrarides %>% group_by(shortname))+
  geom_point(data=filter(metrarides, month==thismonthnum),aes(x=date,y=rides,color=Hex),position="identity",shape=1,show.legend = FALSE)+
  geom_point(data=filter(metrarides, date==thisdate),aes(x=date,y=rides,color=Hex),position="identity",shape=19,show.legend = FALSE)+
  geom_line(aes(x=date,y=rides,color=Hex),linetype=1,linewidth=0.5)+
  geom_text_repel(data=metrarides %>% group_by(shortname) %>% filter(date == thisdate),aes(x=date,y=rides,color=Hex,label=paste(shortname,scales::comma(rides))),
                  force             = 0.3,
                  force_pull        = 2,
                  size              = 2.8,
                  nudge_x           = 30,
                  direction         = "y",
                  hjust             = -0.2,
                  segment.size      = 0.2,
                  segment.curvature = 0.1)+
  labs(x="Month",y="Rides","Ridership",
       title="Metra Ridership Recovery",
       subtitle=paste(thismonth,thisyear,"\n", scales::comma(metratotal$rides),"rides,",scales::percent(metratotal$nowshare),"recovered"),
       linetype=2,
       color="Mode:",
       caption="Plot: acannon.bsky.social\nData: rtams.org/ridership/metra/lines")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  scale_color_identity(guide="none")+
  expand_limits(x = as.Date(c("2017-01-01", "2027-01-01")))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))


metralines

ggsave(plot=metralines, "metralines.png",height=6,width=8)




metralinesrecovery <- ggplot(metrarides %>% group_by(shortname))+
  geom_point(data=filter(metrarides, month==thismonthnum),aes(x=date,y=nowshare,color=Hex),position="identity",shape=1,show.legend = FALSE)+
  geom_point(data=filter(metrarides, date==thisdate),aes(x=date,y=nowshare,color=Hex),position="identity",shape=19,show.legend = FALSE)+
  geom_line(aes(x=date,y=nowshare,color=Hex),linetype=1,linewidth=0.5)+
  geom_text_repel(data=metrarides %>% group_by(shortname) %>% filter(date == thisdate),aes(x=date,y=nowshare,color=Hex,label=paste(shortname,scales::percent(nowshare))),
                  force             = 0.3,
                  force_pull        = 2,
                  size              = 2.8,
                  nudge_x           = 30,
                  direction         = "y",
                  hjust             = -0.2,
                  segment.size      = 0.2,
                  segment.curvature = 0.1)+
  labs(x="Month",y="Ridership Percentage",
       title="Metra Ridership Recovery",
       subtitle=paste("Share of pre-Covid rides as of",thismonth,thisyear, "\n", scales::comma(metratotal$rides),"rides,",scales::percent(metratotal$nowshare),"recovered"),
       linetype=2,
       color="Mode:",
       caption="Plot: acannon.bsky.social\nData: rtams.org/ridership/metra/lines")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::percent)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  scale_color_identity(guide="none")+
  expand_limits(x = as.Date(c("2017-01-01", "2027-01-01")))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))

metralinesrecovery

ggsave(plot=metralinesrecovery, "metralinesrecovery.png",height=8,width=8)
