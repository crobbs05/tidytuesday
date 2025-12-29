

#NEEDED LIBRARIES
library(tidyverse)
library(camcorder)
library(tidycensus)
library(tigris)
library(sf)
library(ggthemes)
library(tigris)



getwd()
setwd(dir = "~")


#read in data####
roundabouts_clean <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')






#use vector to filter out those regions you do not want in the final dataset####
non_conus_or_territories <- c("PR", "VI", "GU", "AS", "MP")





#removes non conus territories, blank spaces, and counties listed incorrectly as a state### 
us_traffic_calming <- roundabouts_clean |> filter(country == "United States" & !is.na(state_region) & 
!state_region %in% non_conus_or_territories & 
!state_region %in% c("Brown Co.","Burnett Co.","California","Co.","Kenosha Co.","Marinette Co.","Washington Co."))





#create file with categories for us regions####
df_states <- data.frame(state_name = state.name, state_region = state.abb,  area = state.area, region = state.region)

#us_roundabouts <- left_join(x = us_traffic_calming,y = df_states,by = "state_region")



#get population estimates for us states using tidycensus####
pop_acs_2022 <- get_acs(
    geography = "state",
    geometry = TRUE,
    variables = "B01003_001", # Total Population
    year = 2022,
    survey = "acs5",
    keep_geo_vars = TRUE) |> shift_geometry(position = "below",preserve_area = FALSE,) |> #add Alaska and Hawaii
    left_join(df_states,by = join_by("STUSPS" == "state_region"))




pop_final <-  pop_acs_2022 |> select(GEOID,NAME.x,STUSPS, region,estimate,variable,geometry) |> janitor::clean_names()



us_roundabouts_population_join <- left_join(x = us_traffic_calming,y = pop_final,
                                  by = join_by("state_region"== "stusps"),keep = FALSE)


us_roundabouts_final <- us_roundabouts_population_join|> 
    group_by(name_x,state_region,region,variable,estimate,geometry) |> 
    summarise(total_roundabouts = n(),.groups = "drop") |> 
    select(statename = name_x,state_region,region,variable,estimate,total_roundabouts,geometry) |> 
    mutate(roundabouts_per_people = (total_roundabouts/estimate)*100000) |> st_as_sf()


plot(us_roundabouts_final$geometry)






#creates a vector with the data breaks#####
roundabout_breaks <- classInt::classIntervals(var = us_roundabouts_final$roundabouts_per_people,n = 4,style = "jenks",dataPrecision = 2)$brks






#cuts your data into categories with labels using the roundabout_breaks as the breaks for the categories####
us_roundabouts_final$breaks <- cut(us_roundabouts_final$roundabouts_per_people,
                                   breaks = roundabout_breaks,include.lowest = TRUE,dig.lab = 2,
                                   labels = c("Less Than 3", "3 to 5", "5 to 8", "More than 8")) 






#create map with ggplot####
ggplot(data = us_roundabouts_final,mapping =  aes(fill = breaks))+
   geom_sf(alpha = .55,color = "#282828", size =.035)+
    scale_fill_manual(values = c("Less Than 3" = "#30638e",
                                 "3 to 5" = "#00798c",
                                 "5 to 8" = "#edae49",
                                 "More than 8" = "#d1495b"),
                      
                      name = "Roundabouts Per 100K People",
                      labels = c("Less Than 3","3 to 5","5 to 8","More than 8"))+
    theme_fivethirtyeight()+
    theme(legend.position = "inside")+
    theme(legend.direction = "vertical")+
    
    theme(legend.position.inside = c(0.70, 0.13))+
    theme(legend.title = element_text(family = "Rockwell",face = "bold", color = "#494949", size=rel(.375)))+
    theme(legend.text = element_text(family = "Rockwell",face = "bold", color = "#494949",size=rel(.375)))+
    theme(legend.key.size = unit(3.5, "pt") )+
    theme(legend.key.spacing.y = unit(.05,"cm"))+
    guides(fill = guide_legend(byrow = TRUE))+
    
    theme(legend.margin = margin(b = 0))+
    theme(plot.margin = margin(t = 1))+
    theme(plot.background = element_rect(fill = "#EBEBEB"))+
    theme(panel.background = element_rect(fill = "#EBEBEB"))+
    theme(legend.background = element_blank())+
    theme(panel.grid.major =  element_blank())+
    theme(axis.text = element_blank())+
    labs(title = str_wrap("Roundabouts in the United States: How Many Exist per 100,000 Residents?",
                          width = 100))+
    theme(plot.title = element_text(family = "Rockwell",size = rel(.75),face = "bold",color = "#494949",
    margin = margin(l = 25,b = 03,t = 10)))+
    labs(subtitle = str_wrap("Nebraska leads with 11 roundabouts per 100,000 people, while Oklahoma ranks last with just 1            roundabout per 100,000 residents.",width = 104))+
    theme(plot.subtitle = element_text(family = "Rockwell",size = rel(.50),face = "bold",color = "#494949",
    margin = margin(l = 25) ))+
    theme(plot.caption = element_text(family = "Rockwell",size = rel(.25),face = "bold",color = "#494949",
                                      hjust =.10,vjust = 5))+
    labs(caption = "Source: Kittelson & Associates, Inc | U.S. Census Bureau |  Map By: @ETTS12.BSKY.SOCIAL")+
    coord_sf(crs = 9311,expand = TRUE,xlim = c(-2450000, 2450000), ylim = c(-2500000, 800000)) 
    #converted the xlim and ylim limits tometers because of the coordinate system



ggsave(filename = "Roundabouts_Per_100K_People.png",plot = last_plot(),width = 5 ,height = 4 ,units = "in",
dpi = 500,bg ="#EBEBEB")
             