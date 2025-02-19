#tidytuesday
#dataset: FBI Crime Data Agency Level
#year: 2025
#week: 7
#by: Chauncey Robbs



#add necessary packages
library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(ggforce)
library(gt)
library(sf)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(tigris)
library(waffle)










#add tidytuesday data####
agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')








#filter agency level data to the state of maryland####
#filter agency type to county, city and college police departments#
maryland_agencies <- agencies |> filter(state == "Maryland" & !is.na(agency_type)) 









#cleaning county location data####
maryland_agencies <- mutate(maryland_agencies,county = if_else(agency_name %in% c("Morgan State University",
    "University of Baltimore","Coppin State University","University of Maryland: Baltimore City",
    "Baltimore City Sheriff's Office","General Services: Baltimore City","State Police: Baltimore City"),
    "BALTIMORE CITY",county))









#filter to city and county agencies only#####
city_agencies <- maryland_agencies |> filter(agency_type %in% c("City","County")) |> 
    mutate(is_nibrs = factor(x = is_nibrs,levels = c("TRUE","FALSE")))







#aggregate data by agency type to see frequency of reporting####
reporting_status <- city_agencies |> group_by(agency_type,is_nibrs) |> summarise(total = n(),.groups = "drop")









#add google fonts####
font_add_google(name = "Mitr", family = "Mitr")
#ENABLES TEXT YOU SELECT FROM GOOGLE FONTS
showtext_auto()









#create plot####
reporting_status |> ggplot(mapping = aes(values = total,fill = is_nibrs))+
geom_waffle(n_rows = 7,flip = TRUE,make_proportional = FALSE,size = .05, width =.85, height =.85,color = NA,
            alpha =.75,radius = unit(4, "pt"),show.legend = FALSE)+
   facet_wrap(~agency_type,nrow =1)+
   theme_void()+
    
    scale_fill_manual(values =c("TRUE"="#E6A600","FALSE"="#972136"))+
    
    labs(title = "Participation varies by Maryland law enforcement agencies when reporting crime data to the FBI.")+
    labs(subtitle = ("Counties report at <span style ='color:#E6A600;'>**93%**</span> to the FBI's National Incident-Based Reporting System (NIBRS) compared to <span style ='color:#E6A600;'>**82%**</span>  by city agencies."))+
    labs(caption =  "#TidyTuesday 2025 - Week 07 | Source: FBI | By: @ETTS12.BSKY.SOCIAL")+
    
  
    
    theme(plot.background = element_rect(fill = "#292929", color = NA))+
    theme(strip.text.x = element_text(vjust = 0.5,margin = ))+
    theme(strip.text = element_text(color = "#DCDCDC",size = 28,family = "Mitr",face ="bold",
         margin = margin(t =25,b=-20)))+
    theme(plot.title = element_markdown(color = "#DCDCDC", size =28, hjust = .5,
         family = "Mitr", face = "bold",margin = margin(t=15)))+
    theme(plot.subtitle = element_markdown(color = "#DCDCDC", size =22,  hjust = .5,
         family = "Mitr"))+
    theme(plot.caption.position = "plot",plot.caption = element_text(color = "#DCDCDC",
         size = 12.5, family ="Mitr", hjust = -.55,margin = margin(b = 5)))+
    annotate(geom = "text",x = 5,y = .25,hjust =.75,color = "#DCDCDC", size =5,family ="Mitr",
             label = "1 Box = 1 Law Enforcement Agency")+
   coord_equal(expand = TRUE,clip = "off",ylim = c(0,13))
   








#export plot####
ggsave(filename = "Agency_Reporting_Status.png",plot = last_plot(),
       width =7.5,height = 5,units = "in",dpi = 300)
  





