library(tidyverse)
library(ggrepel)
library(extrafont)


#Upload and summarise train data (API Pull)
ctatrain <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")
ctatrain$date <-  as.Date(ctatrain$month_beginning, format="%Y-%m-%d")
#Filter to 2017 onward
ctatrain <- filter(ctatrain,date > as.Date(17140))
#Format to only month and ridership
ctatrainmonthly <- ctatrain %>% group_by(date) %>% summarise(ctatrain=sum(monthtotal))



#Upload and summarise bus data (API Pull)
ctabus <- read.csv("https://data.cityofchicago.org/resource/bynn-gwxy.csv?$limit=450000")
ctabus$date <-  as.Date(ctabus$month_beginning, format="%Y-%m-%d")
#Filter to 2017 onwards
ctabus <- filter(ctabus,date > as.Date(17140))
#Format to only month and ridership
ctabusmonthly <- ctabus %>% group_by(date) %>% summarise(ctabus=sum(monthtotal))

#formats data to plot total monthly rides
ctarides <- right_join(ctatrainmonthly, ctabusmonthly, by = "date")
ctarides <- ctarides %>% pivot_longer(!date,names_to = "mode",values_to="rides")

#Add Month and Year Columns
ctarides$month <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%m")
ctarides$year <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%Y")


#Bringing in Metra Data. Latest available here:
#https://rtams.org/media/resources/metra-monthly-ridership-line 

metrarides <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/Metra_Monthly_Ridership_by_Line_2002_2024.csv")
metrarides$date <- paste(metrarides$YEAR,"-",metrarides$MONTH,"-","01",sep="")
metrarides$date <-  as.Date(metrarides$date, format="%Y-%m-%d")
metrarides <- metrarides %>% group_by(date, MONTH, YEAR) %>% summarise(rides=sum(RIDES))
names(metrarides)[names(metrarides)== 'YEAR'] <- "year"
names(metrarides)[names(metrarides)== 'MONTH'] <- "month"
metrarides$mode <- "metra"


#Combining Metra and CTA
transitrides <- rbind(ctarides,metrarides)
transitrides <- filter(transitrides,year>2016)

#Pull the date and name of the most recent month for later
thisdate <- max(transitrides$date)
thismonth <- format(as.Date(thisdate,format="%Y-%m-%d"),"%b")
thismonthnum <- format(as.Date(thisdate,format="%Y-%m-%d"),"%m")
thisyear <- format(as.Date(thisdate,format="%Y-%m-%d"),"%Y")
#Pull the initial date and year later
firstdate <- min(transitrides$date)
firstyear <- format(as.Date(firstdate,format="%Y-%m-%d"),"%Y")


#Summary table of our most recent months
ridercount <- transitrides %>% group_by(mode) %>%filter(date == max(date))
ridercount$ridesmil <- round(ridercount$rides / 1000000,2)
ridercount$monthname <- format(as.Date(ridercount$date, format="%Y-%m-%d"),"%b")

#########################################################################
#Rides compared to 2017-2019 monthly avg

transitwider <- pivot_wider(as_tibble(transitrides[,-1]),names_from=year,values_from=rides)
transitwider$preavg <- ((transitwider$"2017" + transitwider$"2018" + transitwider$"2019") / 3)
transitavg <- transitwider %>% pivot_longer(cols=firstyear:thisyear,names_to="year",values_to="rides")
transitavg$rideshare <- transitavg$rides / transitavg$preavg

# Add Dates back in
transitavg$date <- paste(transitavg$year,"-",transitavg$month,"-","01",sep="")
transitavg$date <-  as.Date(transitavg$date, format="%Y-%m-%d")
transitavg$monthname <- format(as.Date(transitavg$date, format="%Y-%m-%d"),"%b")

#take max rides from any category
maxrides <- max(transitrides$rides)
maxavg <- max(transitavg$rideshare, na.rm=TRUE)

#Isolate the month of the most recent data for each mode separately
modemonth <- transitavg %>% group_by(mode) %>% filter(rides>1) %>% filter(date==max(date))
modemonth <- select(modemonth, mode, month)
modemonth <- rename(modemonth, nowmonth =  month)
modemonth <- left_join(transitavg, modemonth)
modemonth <- filter(modemonth, month == nowmonth)


#Plotting rides with SMOOTH
allrecover<- ggplot(transitrides)+
  geom_point(data=modemonth,aes(x=date,y=rides,color=mode),position="identity",size=2,shape=1, show.legend = FALSE)+
  geom_point(data=transitavg %>% group_by(mode) %>% drop_na(rideshare) %>%filter(date == max(date)),aes(x=date,y=rides,color=mode),size=2,show.legend = FALSE)+
  geom_line(aes(x=date,y=rides,color=mode),linetype=5,linewidth=0.5)+
  geom_smooth(data=filter(transitrides,year>2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0,linewidth=0.5)+
  geom_smooth(data=filter(transitrides,year<2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0,linewidth=0.5)+
  geom_text(data=ridercount,aes(x=date,y=rides,label=paste(monthname," ",year,"\n"," ",ridesmil,"m",sep=""),color=mode),size=3,hjust=-.15,vjust=.5,show.legend = FALSE)+
  scale_color_manual(values=c("#41B6E6","#E4002B","navy","#f4a324"),
                     label=c("CTA Bus",
                             "CTA Train",
                             "Metra",
                             "Pace Paratransit"))+
  scale_fill_manual(values=c("#41B6E6","#E4002B","navy","#f4a324"))+
  guides(fill="none")+
  labs(x="",y="Rides",
       "Ridership",
       title="Chicagoland Transit Recovery",
       subtitle="Monthly Ridership",
       color="",
       caption="Vis: acannon.bsky.social\n Data: rtams.org/ridership")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  expand_limits(x = as.Date(c("2017-02-01", "2025-08-01")))+
  theme_classic()+
  guides(col = guide_legend(label.position = "top",nrow=1))+
  theme(plot.title = element_text(hjust = 0.01,face="bold"), plot.subtitle = element_text(hjust = 0.01),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position=c(0.65,1.07), legend.key.spacing.x = unit(0.2,"cm"), legend.key.size = unit(.01,"cm"),
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        legend.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))

allrecover

ggsave(plot=allrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/allrecover.png",height=6,width=8)




# Plot Recovery ratios
allrecoverpercent <- ggplot(transitavg)+
  geom_line(aes(x=date,y=rideshare,color=mode),linetype=1,linewidth=0.5)+
  geom_point(data=modemonth,aes(x=date,y=rideshare,color=mode),position="identity",size=2,shape=1, show.legend = FALSE)+
  geom_point(data=transitavg %>% group_by(mode) %>% drop_na(rideshare) %>%filter(date == max(date)),aes(x=date,y=rideshare,color=mode),size=2,show.legend = FALSE)+
  geom_text(data=transitavg %>% group_by(mode) %>% drop_na(rideshare) %>%filter(date == max(date)),aes(x=date,y=rideshare,label=paste(monthname,year,"\n  ",scales::percent(rideshare)),color=mode),size=3,hjust=-.2,vjust=.5,show.legend = FALSE)+
  scale_color_manual(values=c("#41B6E6","#E4002B","navy","#f4a324"),label=c("CTA Bus","CTA Train", "Metra","Pace Paratransit"))+
  labs(x="",
       y="Ridership",
       title="Chicagoland Transit Recovery",
       color="",
       subtitle="Share of Pre-Covid Average Ridership",
       caption="Vis: acannon.bsky.social\n Data: rtams.org/ridership")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::percent)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  expand_limits(x = as.Date(c("2017-01-01", "2025-08-01")))+
  theme_classic()+
  guides(col = guide_legend(label.position = "top",nrow=1))+
  theme(plot.title = element_text(hjust = 0.01,face="bold"), plot.subtitle = element_text(hjust = 0.01),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position=c(0.65,1.07), legend.key.spacing.x = unit(0.2,"cm"), legend.key.size = unit(.01,"cm"),
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9"),
        legend.background = element_rect(fill = "#f9f9f9"),
        panel.grid.major.x = element_line(color="#f0f0f0"),panel.grid.major.y = element_line(color="#f0f0f0"))+
  theme(text=element_text(family="Arial"))

allrecoverpercent

ggsave(plot=allrecoverpercent, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/allrecoverpercent.png",height=6,width=8)




