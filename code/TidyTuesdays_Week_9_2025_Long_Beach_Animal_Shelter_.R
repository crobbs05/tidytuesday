#tidytuesday
#dataset:Long Beach Animal Shelter
#year: 2025
#week: 9
#by: Chauncey Robbs






#analysis goal what is the outcome by animal type?#
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
library(patchwork)
library(janitor)
library(glue)
library(skimr)
library(ggtext)
library(tigris)
library(waffle)


#analysis goal: what percent of cats and dogs are adopted over time?


#get data####
longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')









#take a look at the data####
skim(longbeach) #view the data
    

#view data####
glimpse(longbeach) # see the structure of the data









#select columns you want into included in working data set####
animal_outcomes <- longbeach |> select(animal_type,primary_color,outcome_type,intake_date,outcome_date,intake_type,intake_subtype)










#add date columns for month and year to animal outcomes####
animal_outcomes <- animal_outcomes |> mutate(
    outcome_month = month(outcome_date),
    outcome_year = year(outcome_date)) 









#total cats not adopted by year####
cats_not_adopted <- animal_outcomes|> filter(animal_type == "cat",outcome_type != "adoption") |> 
    count(animal_type,outcome_year,name = "total_cats_not_adopted") |> ungroup()




total_animal_adoptions <- animal_outcomes|> filter(!is.na(outcome_type)) |> 
    count(outcome_year,name = "total_adoptions") |> ungroup() 




#total cats adopted by year####
cats_adopted <- animal_outcomes |> filter(animal_type =="cat", outcome_type == "adoption") |>  
     group_by(animal_type,outcome_type,outcome_year) |>  
     summarize(total_cats_adopted = n(),.groups = "drop")







#join cat data sets together and create final clean dataset####
cats_clean <- cats_adopted |> select(-outcome_type) |> left_join(total_animal_adoptions |> 
select(outcome_year,total_adoptions),by = "outcome_year") |> 
mutate(pct = (total_cats_adopted/total_adoptions)*100)
            








#total cats not adopted by year####
dogs_not_adopted <- animal_outcomes|> filter(animal_type == "dog",outcome_type != "adoption") |> 
    count(animal_type,outcome_year,name = "total_dogs_not_adopted") |> ungroup()









#total cats adopted by year####
dogs_adopted <- animal_outcomes |> filter(animal_type =="dog", outcome_type == "adoption") |>  
    count(animal_type,outcome_type,outcome_year,name = "total_dogs_adopted") 







#join dog data sets together and create final clean dataset####
dogs_clean <- dogs_adopted |> select(-outcome_type) |> 
    left_join(total_animal_adoptions |> 
    select(outcome_year,total_adoptions),by = "outcome_year") |> 
    mutate(pct = (total_dogs_adopted/total_adoptions)*100)









#calculate adoptions outcomes for animals that are not dogs and cats####
adoption_not_cats_dogs <- animal_outcomes |> filter(!animal_type %in% c("cat","dog"),outcome_type == "adoption") |> 
    group_by(outcome_type,outcome_year) |> count(name = "total_not_cat_dog_adoptions") |> ungroup() 
    








#join dog data sets together and create final clean dataset####
adoptions_clean <- adoption_not_cats_dogs |> select(-outcome_type) |> 
    left_join(total_animal_adoptions |> 
    select(outcome_year,total_adoptions),by = "outcome_year") |> 
    mutate(pct = (total_not_cat_dog_adoptions/total_adoptions)*100)




#add google fonts####
font_add_google(name = "Noto Sans", family = "Noto Sans")
#ENABLES TEXT YOU SELECT FROM GOOGLE FONTS
showtext_auto()









#labels for cats#
cat_first_point_label <- cats_clean |> slice(which.min(outcome_year))
cat_last_point_label <- cats_clean |> slice(which.max(outcome_year))
cat_2020_label <- cats_clean |> slice(4)




#labels for dogs#
dog_first_point_label <- dogs_clean |> slice(which.min(outcome_year))
dog_last_point_label <- dogs_clean |> slice(which.max(outcome_year))
dog_2020_label <- dogs_clean |> slice(4)





#labels for all other animals#
adoption_first_point_label <- adoptions_clean |> slice(which.min(outcome_year)) 
adoption_last_point_label <- adoptions_clean |> slice(which.max(outcome_year)) 
adoption_2020_label <- adoptions_clean |> slice(4)








#Plot line Graph####

    ggplot(mapping = aes())+
    geom_line(data = dogs_clean,mapping = aes(x = outcome_year,y = pct, color = animal_type),
        linewidth = 1.25,show.legend = FALSE)+
    geom_line(data = dogs_clean,mapping = aes(x = outcome_year,y = pct),color = "#F0F0F0",
                  linewidth = .85,show.legend = FALSE,alpha =.35)+
    geom_line(cats_clean,mapping = aes(x = outcome_year,y = pct, color = animal_type),
              linewidth = 1.25,show.legend = FALSE)+
    geom_line(data = cats_clean,mapping = aes(x = outcome_year,y = pct),color = "#F0F0F0",
                  linewidth = .85,show.legend = FALSE,alpha =.35)+
        
    geom_line(data = adoptions_clean, aes(x = outcome_year,y = pct,),color = "#840032",
                  linewidth = 1.25,show.legend = FALSE)+
        
    geom_line(data = adoptions_clean, aes(x = outcome_year,y = pct,),color ="#F0F0F0",
                  linewidth = .85,show.legend = FALSE,alpha =.35)+
    #dog points#
    geom_point(data = dogs_clean,mapping = aes(x = outcome_year,y = pct, color = animal_type),
               size = 1.5,shape = 21,stroke = .5,fill="#F0F0F0",show.legend = FALSE)+
    #cat points#
    geom_point(data = cats_clean,mapping = aes(x = outcome_year,y = pct, color = animal_type),
               size = 1.5,shape = 21,stroke = .5,fill="#F0F0F0",show.legend = FALSE)+
    #all other animals points#
    geom_point(data = adoptions_clean, aes(x = outcome_year,y = pct), color = "#840032",
                   size = 1.5,shape = 21,stroke = .5,fill="#F0F0F0",show.legend = FALSE)+
    
    #adjust the scale#
    scale_color_manual(values = c("#002642","#e59500","#840032"))+
    scale_x_continuous(breaks = seq(2017,2024, by =1))+
    scale_y_continuous(breaks = seq(0,20,by = 5), limits = c(-.5,21),
                       labels = function(pct_label) paste(pct_label,"%"))+
    
    
    #annotations and labels##
    annotate(geom = "text",x = 2024.5,y = 1.16, label = "Other Animals", color = "#840032", 
            size =8, fontface ="bold" )+
    annotate(geom = "text",x = 2024.25,y =17.45, label = "Cats", color = "#002642", size =8, 
             fontface ="bold")+
    annotate(geom = "text",x = 2024.25,y =14.16, label = "Dogs", color = "#e59500", size =8, 
             fontface ="bold")+
    
    
    
    #on plot annotations#
    annotate(geom = "label",x = 2022.75,y =10, 
    label = str_wrap("Dog Adoptions Increased 128% Between 2020 and 2024.",
    width = 30,indent = 1,whitespace_only = FALSE), size =5.5, 
    family = "Noto Sans",fill="#F0F0F0",label.size = NA,lineheight =.5, alpha = .5)+
    
    
    
    annotate(geom = "label",x = 2020.75,y =15.75, 
    label = str_wrap("Cat Adoptions Increased 39% Between 2020 and 2024.",
    width = 30,indent = 1,whitespace_only = FALSE), size =5.5, 
             family = "Noto Sans",fill="#F0F0F0",label.size = NA,lineheight =.5)+
    
    
    
    #cat 2107 point label##
    geom_label(data = cat_first_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#002642", fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    #cat 2020 point label#
    geom_label(data = cat_2020_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#002642", fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    
    #cat 2024 point label##
    geom_label(data = cat_last_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#002642", fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    
    #dog 2017 point label##
    geom_label(data = dog_first_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#e59500",fill="#F0F0F0",label.size =  NA,nudge_y = -.80)+
    
    
    #dog 2020 point label##
    geom_label(data = dog_2020_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#e59500",fill="#F0F0F0",label.size =  NA,nudge_y = -.80)+
    
    
    
    #dog 2024 point label##
    geom_label(data = dog_last_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#e59500",fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    
    #2017 adoption label other than dogs and cats#
    geom_label(data = adoption_first_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#840032",fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    #2020 adoption label other than dogs and cats#
    geom_label(data = adoption_2020_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#840032",fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    
    #2024 adoption label other than dogs and cats#
    geom_label(data = adoption_last_point_label,mapping = aes(x = outcome_year,y = pct,
    label = paste0(round(pct,2),sep = "%"),family = "Noto Sans"),size = 6,fontface = "bold",
    color = "#840032",fill="#F0F0F0",label.size =  NA,nudge_y = .80)+
    
    #adjust theme elements#
    theme_fivethirtyeight()+
    theme(text = element_text(family = "Noto Sans"))+
    theme(axis.text = element_text(family = "Noto Sans", size = 15.25))+
    theme(panel.grid = element_line(color ="#e5e5e5",linewidth = .05))+
    
    
    #title and subtitle labels####
    labs(title = "Long Beach Animal Shelter Adoption Outcomes")+
    labs(subtitle = "Adoptions of <span style ='color:#002642;'>**Cats**</span> and <span style ='color      :#e59500;'>**Dogs**</span> Differ by **3** Percentage Points in 2024. 
    Whereas <span style ='color:#840032;'>**Other Animals**</span> are Adopted at a<br> Much Lower Percentage.")+
    labs(caption = "#TidyTuesday 2025 - Week 09|Source: City of Long Beach Animal Care Services|By:@ETTS12.BSKY.SOCIAL")+
    theme(plot.title = element_markdown(size = 50,family = "Noto Sans",margin = margin(b = 2.5)))+
    theme(plot.subtitle =  element_markdown(size = 30, family = "Noto Sans", margin = margin(b = 0),
    lineheight = .40))+
    theme(plot.caption.position = "plot",plot.caption = element_text(size = 12.5,hjust = .05,
    family ="Noto Sans", margin = margin(t=10,b = 0)))+
    


    coord_cartesian(clip = "off",expand = TRUE,ylim = c(NA,20))
    





    #export plot####
    ggsave(filename = "LongBeach_Animalshelter_Adoptions.png",plot = last_plot(),
           width =7.5,height = 5,units = "in",dpi = 300)
    
    

