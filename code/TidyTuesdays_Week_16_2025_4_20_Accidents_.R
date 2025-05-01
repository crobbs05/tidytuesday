#tidytuesday
#dataset:4/20
#year: 2025
#week: 16
#by: Chauncey Robbs







#add necessary packages
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(showtext)
library(scales)
library(ggtext)
library(ggforce)
library(gt)
library(sf)
library(showtext)
library(packcircles)
library(patchwork)
library(janitor)
library(glue)
library(skimr)
library(ggtext)
library(tigris)
library(waffle)
library(MetBrewer)
library(voronoiTreemap)
library(treemap)
library(treemapify)
library(zoo)



#import data####
daily_accidents <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents.csv') 

daily_accidents_420 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents_420.csv')








#review the datasets####
skim(daily_accidents)
skim(daily_accidents_420)









#add year, month,day of week(beginning on sunday), and season to each dataset####
                                                                                                                    daily_accidents <- daily_accidents |> 
                                                                                                                    mutate(month = month(date,label = TRUE,abbr = FALSE),
                                                                                                                    day = day(date),   
                                                                                                                    day_of_week = wday(date,label = TRUE,week_start = 1,abbr = FALSE),
                                                                                                                    year = year(date),
                                                                                                                    quarter = quarter(x = date,type = "year.quarter",fiscal_start = 1),
                                                                                                                    season = case_when(
                                                                                                                    month %in% c("December","January","February") ~ "Winter", 
                                                                                                                    month %in% c("March","April","May") ~ "Spring",
                                                                                                                    month %in% c("June","July","August") ~ "Summer",
                                                                                                                                                .default = "Fall"))






daily_accidents_420 <- daily_accidents_420 |> mutate(date = ymd(date),
                                                                                                                    month = month(date,label = TRUE,abbr = FALSE), 
                                                                                                                    day = day(date),
                                                                                                                    day_of_week = wday(date,label = TRUE,week_start = 1,abbr = FALSE),
                                                                                                                    year = year(date),
                                                                                                                    quarter = quarter(date,type = "quarter",fiscal_start = 1),
                                                                                                                                                                                                                             season = case_when(
                                                                                                                    month %in% c("December","January","February") ~ "Winter", 
                                                                                                                    month %in% c("March","April","May") ~ "Spring",
                                                                                                                    month %in% c("June","July","August") ~ "Summer",
                                                 .default = "Fall"))








total_420_accidents<- daily_accidents |> filter(month == "April" & day == 20) |> 
    
    group_by(date,year,month,day,day_of_week) |>
    
    summarise(total_fatality_count = sum(fatalities_count),.groups = "drop")


   



prime_hours_420_daily_accidents <- daily_accidents_420 |> 
    
    filter(month == "April" & day == 20 & e420 == TRUE) |> group_by(date,e420,year,month,day,day_of_week) |> 
    summarise(fatalities_count = sum(fatalities_count),.groups = "drop") 




final_dataset <- prime_hours_420_daily_accidents |> 
    
    inner_join(y = select(.data = total_420_accidents,date,total_fatality_count),by = "date") |> 
    
    mutate(difference = c(total_fatality_count-fatalities_count), 
           pct_420_accidents = c(fatalities_count/total_fatality_count)) |> ungroup()


    max_year <- final_dataset |> filter(pct_420_accidents == max(final_dataset$pct_420_accidents))
    
    
    min_year <- final_dataset |> filter(pct_420_accidents == min(final_dataset$pct_420_accidents))





#visualize data set####


#add google fonts####
    font_add_google(name = "Red Hat Text", family = "Red Hat Text")
    #ENABLES TEXT YOU SELECT FROM GOOGLE FONTS
    showtext_auto()



#create chart with ggplot
    final_dataset|> ggplot(mapping = aes(x =date, y = pct_420_accidents))+
    geom_line(color = "#840032",linewidth  = .65, alpha =.85)+
    geom_line(color = "#840032", linewidth  = 2.0, alpha =.45)+
    geom_vline(mapping = aes(xintercept = 2000),alpha =.35,linetype = 2)+
    geom_point(data = final_dataset |> filter(year == 2007),
               mapping = aes(x = date,y =pct_420_accidents ),color = "#840032",  
               size = 2,shape = 21,stroke = .85,fill="#F0F0F0")+
    geom_point(data = final_dataset |> filter(year == 2010),
                   mapping = aes(x = date,y =pct_420_accidents ),color = "#840032",  
                   size = 2,shape = 21,stroke = .85,fill="#F0F0F0")+
    geom_point(data = max_year,mapping = aes(x = date,y =pct_420_accidents), color = "#840032",  
               size = 2,shape = 21,stroke = .85,fill="#F0F0F0")+
    geom_point(data = min_year,mapping = aes(x = date,y =pct_420_accidents ),color = "#840032",  
               size = 2,shape = 21,stroke = .85,fill="#F0F0F0")+
    scale_x_date(breaks = seq(from = min(final_dataset$date),
                        to = max(final_dataset$date),by ="3 years"),
                        date_labels = "%b %Y")+
   scale_y_continuous(labels = label_percent(),limits = c(.20,.65))+
   theme_fivethirtyeight()+
   #theme(text = element_text(family = "Red Hat Text",size = 12.5))+
   theme(axis.text = element_text(family = "Red Hat Text",size = 20.5))+
   theme(panel.grid.major = element_line(colour = "#e5e5e5"))+
    
    
#add annotations to chart####
    
    
    #label for greatest percentage of fatal accidents in 2009#
    geom_richtext(data = max_year,mapping = aes(x = date,y = pct_420_accidents),
    label = str_wrap(
    "In 2009, **64%** of fatal accidents occurred<br>during the 4/20 celebration.Yet, the total <br>          number of fatal accidents was 98, the<br>second lowest in this 24-year period.",
    width = 40,whitespace_only = FALSE),size =8.75, hjust =0,
                  family = "Red Hat Text",fill="#F0F0F0",label.size = NA,
                  lineheight =.37,
                  label.padding = unit(0,"lines"),
                  nudge_x =195, nudge_y = -.025)+
 
    
    
    
    #label sharing the difference between 2009 and 2010#
    geom_richtext(data = max_year,mapping = aes(x = date,y = .52),
    label = str_wrap("From 2009 to 2010, fatal accidents<br>decreased by **26 percentage points**.<br>       Representing the largest decline in the<br> observed 24-year period.",
    width =25,whitespace_only = FALSE),size =8.75, hjust =0,
                  family = "Red Hat Text",fill="#F0F0F0",label.size = NA,
                  lineheight =.37,
                  label.padding = unit(0,"lines"),
                  nudge_x =335, nudge_y = .015)+
    
    
    
        
    #label for fatal accidents in 2007#
    geom_richtext(data = final_dataset |> filter(year == 1999),
                  mapping = aes(x = date,y = pct_420_accidents),
    label = str_wrap("2007 had the largest number of accidents<br> during the 4/20 celebration at 88,                         accounting<br> for **42%** of fatal accidents.",
                width =25,whitespace_only = FALSE),size =8.75, hjust =0,
                  family = "Red Hat Text",fill="#F0F0F0",label.size = NA,
                  lineheight =.37,
                  label.padding = unit(0,"lines"),
                  nudge_x =155, nudge_y = .10)+

    
    
    #add arrow annotations for avg time to HIRE####
    annotate(geom = "curve",x = as.Date("2004-04-20")+200,y = .497,
             xend = as.Date("2006-04-20")+300,yend = .431, 
         curvature =-0.30,linewidth =.25, color ="#282828",
         arrow = arrow(length = unit(x = .10,units = 'cm')))+
    
    

    
    #label for smallest percentage of fatal accidents in 2002#
    geom_richtext(data = min_year,mapping = aes(x = date,y = pct_420_accidents),
    label = c("In 2002, **27%** of fatal accidents happened during the 4/20 celebration.<br> 
    This represents the lowest percentage of fatal accidents in this<br>24 year period."),size =8.75,        hjust = 0,
    family = "Red Hat Text",fill="#F0F0F0",label.size = NA,
    lineheight =.37,
    label.padding = unit(0,"lines"),
    nudge_x =155, nudge_y = -.020)+
        
        
    
        
        
    #add title, subtitle and caption to chart#
    labs(title = "Percent of Annual Fatal Accidents Between 4:20pm and 11:59pm on 4/20.",
    subtitle = "Fatal accidents increased by **9.4%** from 1992 to 2016, but further analysis is                  necessary to determine if this increase is linked to the celebration of 4/20.<br> 
             Specifically, data on the number of registered drivers during this 24-year period.",
    caption = "#TidyTuesday 2025 | Week 16 | Source: 420 (data-raw) OSF | By: @ETTS12.BSKY.SOCIAL")+
    theme(plot.title = element_text(family = "Red Hat Text",size = 40))+
    theme(plot.subtitle = element_markdown(family = "Red Hat Text",size = 23,lineheight = .45))+
    theme(plot.caption = element_text(family = "Red Hat Text",size =15.5,hjust = 0,vjust = -5))+
    coord_cartesian(expand = TRUE, clip = "off")


    
#export plot####
ggsave(filename = "Fatal_Accidents_420.png",plot = last_plot(),
       width =7.5,height = 5,units = "in",dpi = 300)










    
    #geom_line(data = daily_accidents_420,mapping = aes(x = date,y = fatalities_count), color="red", size  = .15)
#timeseries_daily_accidents <- zoo(x = daily_accidents,order.by = daily_accidents$date,frequency = 12)
#summarise(avg_daily_fatalities = mean(fatalities_count,na.rm = TRUE),days = n()) 

#aily_accidents <- daily_accidents |> arrange(date) |>  mutate(sma_7days = rollmean(fatalities_count,k = 14,fill = NA,align = "right"),.keep = "all")