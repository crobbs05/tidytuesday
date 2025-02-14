#TidyTuesdays 
#Week 20
#Year 2023
#Topic:tornadoes
#by:Chauncey Robbs

library(tidyverse)
library(sf)
library(maps)
library(tigris)
library(sf)
library(classInt)
library(ggthemes)
library(tidycensus)
library(scales)
library(ggtext)
library(ggrepel)


#READ IN DATA####
tornadoes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')


state_regions <- read.csv("mississippi_regions.csv") |> 
      separate_longer_delim(cols = "counties",delim = ",") 


state_regions$counties <- str_trim(state_regions$counties,side = "both")








   
      






#CONVERT LAT AND LONG COORDINATES TO SF OBJECTS####
tornadoes_sf <- tornadoes |> 
   select(yr,st,loss, slat, slon) |> 
   st_as_sf(coords = c("slon", "slat"),remove = FALSE, crs = 4326) |> 
   st_transform(crs = 2163)
   








#PULL MISSISSPPI COUNTY DATA FOR JOIN WITH TORNADO DATA####
mississippi_counties <- tigris::counties(state ="MS") |> st_transform(crs = 2163)







#JOIN TORNADO AND MISSISSIPPI COUNTY BOUNDARY DATA TOGETHER####
mississippi_tornadoes <- st_join(mississippi_counties, tornadoes_sf, left = TRUE)







#FIND TOTAL POPULATION VARIABALE####
variables <- tidycensus::load_variables(year = 2023,dataset = "acs5/subject")





#CREATE MISSISSIPI POPULATION VARIABLE####
mississippi_variables <- c(total_pop="S0101_C01_001")






#PULL MISSISSIPPI COUNTY POPUALTION DATA####
mississippi_population <- get_acs(geography = "county", state = "MS",
variables = mississippi_variables,year = 2023,geometry =FALSE )







#JOIN POPULATION DATAT WITH TORNADO DATA####
mississippi_pop_tornados_join <- left_join(x = mississippi_population, 
                                           y = mississippi_tornadoes,by ="GEOID")



#CREATE FINAL DATASET WITH TOTAL LOSS AND TOTAL LOSS PER CAPITA####
#THROUGHT WAS INCORRECT. NEED POPULATION FOR EACH IN THE DATASET BEGINNING IN 1950#
 final_dataset <- mississippi_pop_tornados_join |> st_drop_geometry() |>  #DROPPING THE GEOMETRY COLUMN
   select(GEOID,NAME.y,loss,estimate) |> 
   group_by(GEOID,NAME.y,estimate) |> 
   summarise(total_tornados = n(),
             total_loss = sum(loss,na.rm = TRUE),.groups = "drop")|> 
   mutate(total_loss = (total_loss),total_loss_per_capita = (total_loss/estimate))








#ADD MISSISSIPPI STATE REGIONS TO STATE FOR FUTURE ANALYSIS BY REGION####
final_dataset <- final_dataset |> 
 left_join(y = state_regions,by = join_by(NAME.y == counties)) |> print(n = 90)



rm(final_datset)


      
 
 
#CREATE CASE WHEN FUNCTION TO SET COLORS FOR OUTLIERS AND REDUCE OPACITY OF OTHER POINTS ON CHART
final_dataset <- final_dataset |> mutate(point_colors = 
   case_when(
   NAME.y == "Leake" ~ "#780116", #most total damages
   NAME.y == "Stone" ~ "#f2bb05", # lowest total damages
   NAME.y == "Hinds" ~ "#e56b6f", #most tornado
   TRUE ~ "#023047"), # all other counties
   fill = colorspace::lighten(point_colors,amount = 0.65)) 





     



   #USE FINAL DATASET TO CREATE PLOT 
   final_dataset |> ggplot(mapping = aes(x= total_tornados,y = total_loss,size = total_loss/1000))+
   
   
   #ADD HORIZONTAL LINE TO SHOW THE MEDIAN TORNADO LOSS
   geom_hline(yintercept = median(final_dataset$total_loss,na.rm = T), 
              color = "#282828",alpha = .75,linetype =3)+
      
      
      
      
      
  
   
   
   #ADD POINT DATA TO PLOT
    geom_point(color = final_dataset$point_colors,fill = final_dataset$fill,
               pch=21,show.legend = F)+
      
      

   
   
   #ADD ANNOTATION FOR LEAKE COUNTY
   annotate(geom = "label",x = 57.5,y = 672822000,label = toupper("Leake County"),
               size =3.25,
               family = "sans",
               hjust = "left",
               vjust = "center",
               fill = "#f0f0f0",
               color = "#282828",
               label.size = 0)+
      
      
      
      
      
      
      
      
      #MANUAL SCALE FOR CHART#
      #DID NOT ADD TO GRAPHIC#
      #annotate(geom = "point",x = c(110,110,110),y = c(672822000,520000000,440000000), pch=1,
            #fill = "#f0f0f0",color = "#3A3A3A",size =c(6.5,4.5,2.5))+
      
      
      
      
      
      
      
      
      
      #MANUAL SCALE FOR CHART#
      #DID NOT ADD TO GRAPHIC#
      #annotate(geom = "text",x = c(115,115,115),y = c(682822000,520000000,420000000),
               #label = c("500M","300M","100M"),color = "#3A3A3A",size =1.75)+
      
      
      
      
      
      
      
      #ADD ANNOTATION FOR LEAKE COUNTY DAMAGES####
      annotate(geom = "label",x = 60.75,y = 503622000,label = "$672M in Damages",
               size =2.05,
               family = "sans",
               hjust = "left",
               vjust = "center",
               fill = "#f0f0f0",
               color = "#282828",
               label.size = 0)+
  
   
   
      #ADD ANNOTATION FOR STONE COUNTY#### 
      annotate(geom = "label",x = 19.5,y = 1005000,label = toupper("Stone County"),
            size =3.25,
            family = "sans",
            hjust = "left",
            vjust = "center",
            fill = "#f0f0f0",
            color = "#282828",
            label.padding = unit(.005,"lines"),
            label.size = 0)+
      
      
      
      
      
      
      
      
      
     #ADD ANNOTATION FOR STONE COUNTY DAMAGES#### 
     annotate(geom = "label",x = 22.5,y =770000,label = "$1.05M in Damages",
           size =2.05,
           family = "sans",
           hjust = "left",
           vjust = "center",
           fill = "#f0f0f0",
           color = "#282828",
           label.padding = unit(.005,"lines"),
           label.size = 0)+
      
      
      
      
      
      
      
      
      
      
      #ADD ANNOTATION FOR HINDS COUNTY#### 
      annotate(geom = "label",x = 106,y = 147937000,label = toupper("HINDS County"),
               size =3.25,
               family = "sans",
               hjust = "left",
               vjust = "center",
               fill = "#f0f0f0",
               color = "#282828",
               label.padding = unit(.005,"lines"),
               label.size = 0)+
      
      
      
      
      
      
      
      
      
      #ADD ANNOTATION FOR HINDS COUNTY Damages#### 
      annotate(geom = "label",x = 108,y = 115937000,label = "$147M in Damages",
           size =2.05,
           family = "sans",
           hjust = "left",
           vjust = "center",
           fill = "#f0f0f0",
           color = "#282828",
           label.padding = unit(.005,"lines"),
           label.size = 0)+
      
     
   
   
      
      
      
      
      
      
   #ADD ANNOTATION FOR OTHER MISSISSIPPI COUNTIES
           annotate(geom = "label",x = 70.5,y = 2355000,
                      label = toupper("Other Counties\nin Mississippi"),
                      fill = "#f0f0f0",
                      color = "#282828",
                      family = "sans",
                      fontface ="plain",
                      alpha =.55,
                      size =2.00,
                      hjust = "center",
                      vjust = "center",
                      label.padding = unit(.005,"lines"),
                      label.size = 0)+
      
      
      
      
      
      
      
      
      
    #add arrow annotations for avg time to HIRE####
      annotate(geom = "curve",x = 69.25,y = 2910000,xend = 73.5,yend = 6450000, 
               curvature =-0.30,linewidth =.25, color ="#282828",
               arrow = arrow(length = unit(x = .10,units = 'cm')))+
      
      
      
      
      
      
      

  
   #ADD ANNOTATION FOR TOTAL DAMAGE FROM TORNADOS
   annotate(geom = "label",x = 90,y = 15999999,label = toupper("median\neconomic damages\n$10.5m"),
            size =1.99,
            fill = "#f0f0f0",
            color = "#282828",
            fontface ="plain",
            label.size = NA)+#removes boarder around label
   
      
      
      
      
      
      
      
      
     #ADD PLOT THEME####
     theme_fivethirtyeight()+
      
      
      
      
      
      
      
      
      
     #SET X AND Y SCALE FOR THE PLOT####
     scale_y_continuous(labels = label_currency(scale_cut = cut_short_scale(1e-9)), trans = "log10",
                        limits = c(700000,1000000000))+
     scale_x_continuous(breaks = seq(0,120,20),limits = c(0,120))+
     
         
     
      
      
      
      
      
      
     #ADD TITLE, LABELS, AND CAPTION TO THE CHART####
     ylab(label = "Total Economic Loss (In U.S. Dollars)")+
     xlab(label = "Number of Tornadoes")+
     labs(title = "Tornadoes Leave No County Untouched",
          subtitle = str_to_title("Economic damages for counties in Mississippi from 1950 to 2022."),
     caption = "#TidyTuesday 2023 - Week 20 | Source: NOAA | By: @ETTS12.BSKY.SOCIAL")+
     
    
     
     
      
      
      
      
      
     #ADJUST THE THEME OF THE LABELS
     theme(plot.title = element_text(hjust = .45))+
     
     theme(plot.subtitle = element_text(hjust = .45))+
     
     theme(axis.title.y = element_text(face = "bold",
                                       size = 7,
                                       color = "#282828",
                                       hjust = .5,
                                       margin = margin(r = 8, l = 1)
                                       ))+
     theme(axis.title.x = element_text(face = "bold",
                                       size = 7,
                                       color = "#282828",
                                       hjust = .5,
                                       margin = margin(t =5, b = -5)))+
     theme(plot.caption = element_text(size = 5,
                                       hjust = 0,
                                       vjust = -1.00,
                                       family = "sans",
                                       colour = "#282828"))+
   
     
     
      
      
      
     
      
      
     #TURN OFF CLIP. SET LIMITS FOR THE Y-AXIS. ADJUST LIMITS OF THE SCALE TO SEE CHANGES ON PLOT####
     coord_cartesian(clip = "off",ylim =c(800000,1000000000))
  
  
 
  
  
  
  
  
  
    
#PLOT CHART AND SAVE TO LOCALLY####
ggsave(filename = "tornadoes.png",
       device = "png",width = 7.5,height = 5 ,units = "in",dpi = 400,plot = last_plot())

 






  