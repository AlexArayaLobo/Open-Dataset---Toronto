#Visualizing Toronto's Open Dataset - Fires

#Libraries
library(rio)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(rlang)
library(ggpubr)
library(tidyverse)
  library(lubridate)

#Data
FireIncidents<-read.csv("Fire incidents.csv", 
                        dec = ".", 
                        sep = ";", 
                        header = TRUE)

#Check new data
View(FireIncidents)
summary(FireIncidents)
head(FireIncidents)
names(FireIncidents)
str(FireIncidents)
glimpse(FireIncidents)
attach(FireIncidents)

###Visualizing the data

#Change these string variables according to the plot you want.
#The next variables are predetermined to plot "Area of origin"
#Note: Undetermined levels were removed

column_string = "Material_First_Ignited" #Name of the variable in the dataset
column_value1 = "99 - Undetermined (formerly 98)" #Level 1 - removed
column_value2 = "97 - Other" #Level 2 - removed
column_name = sym(column_string) #Creates a symbol from a string

#Custom Function to avoid repetitive tidyverse verbs
custom_function <- function(df, 
                            column, 
                            old_col, new_col) {
    df %>%
    count({{column}}) %>%
    top_n(10,n) %>%
    arrange(desc(n)) %>%
    filter(UQ(column_name) != UQ(column_value1) & 
           UQ(column_name) != UQ(column_value2)) %>%
    mutate(!!new_col := fct_reorder(!!old_col, n))
}


#Area of origin
Area_of_origin <-
  custom_function(df = FireIncidents, 
                  column = Area_of_Origin,
                  old_col = quo(Area_of_Origin), 
                  new_col = quo(Area_of_Origin2)) %>%
  ggplot(aes(x=Area_of_Origin2, y = n)) + 
    geom_col(fill = "#f46d43", alpha = 0.8) + 
    coord_flip() +
    labs(x = "Area of Origin") +
    theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c("Process Manufacturing","Roof",
                              "Washroom/Bathroom","Laundry Area", 
                              "Garage", "Living Area", 
                              "Rubbish Storage", "Sleeping Area", 
                              "Balcony", "Cooking Area"))

#Extent of fire
Extent_Fire <- 
  custom_function(df = FireIncidents, 
                  column = Extent_Of_Fire,
                  old_col = quo(Extent_Of_Fire), 
                  new_col = quo(Extent_Of_Fire2)) %>%
  ggplot(aes(x=Extent_Of_Fire2, y = n)) + 
    geom_col(fill = "#f46d43") + 
    coord_flip() +
    labs(x = "Extent of Fire") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c("Spread beyond building of origin",
                              "Entire structure",
                              "Resulted in exposure fire",
                              "Spread to other floors", 
                              "Confined to roof", 
                              "Spread beyond room of origin",  
                              "Spread to entire room of origin", 
                              "Confined to part of room", 
                              "Confined to object of origin"))

#Ignition source
Ignition_Source <- 
  custom_function(df = FireIncidents, 
                  column = Ignition_Source,
                  old_col = quo(Ignition_Source), 
                  new_col = quo(Ignition_Source2)) %>%
  ggplot(aes(x=Ignition_Source2, y = n)) + 
    geom_col(fill = "#f46d43") + 
    coord_flip() +
    labs(x = "Ignition Source") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c("Cutting/Welding Equipment","Blow Torch", 
                              "Bunsen Burner","Electrical Articles",
                              "Candle", "Circuit Wiring/Copper", 
                              "Clothes Dryer",  "Oven", 
                              "Smoker?s Articles", "Stove"))

#Material First Ignited
Material_First_Ignited <-
  custom_function(df = FireIncidents, 
                  column = Material_First_Ignited,
                  old_col = quo(Material_First_Ignited), 
                  new_col = quo(Material_First_Ignited2)) %>%
  ggplot(aes(x=Material_First_Ignited2, y = n)) + 
  geom_col(fill = "#f46d43") + 
  coord_flip() +
  labs(x = "Material First Ignited") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Cabinetry","Interior Wall/Ceiling",
                            "Paper/Cardboard","Plastic", 
                            "Wood", "Electrical Wiring Insulation", 
                            "Rubbish/Trash/Waste", 
                            "Cooking Oil/Grease"))

ggarrange(Area_Origin, 
          Extent_Fire,
          Ignition_Source, 
          Material_First_Ignited +
            rremove("x.text"), 
            labels = c("A","B","C","D"),
            ncol = 2, nrow = 2)


#People hurt by Extent of Fire
FireIncidents %>%
  group_by(Extent_Of_Fire) %>%
  summarize(Civilian_Casualties = sum(Civilian_Casualties),
            TFS_Firefighter_Casualties = sum(TFS_Firefighter_Casualties)) %>%
  top_n(n = 10) %>%
  arrange(desc(TFS_Firefighter_Casualties,
               Civilian_Casualties,
               Count_of_Persons_Rescued)) %>% 
  ungroup() %>%
  gather(key = "Casualities", 
         value = "n", 
         Civilian_Casualties:TFS_Firefighter_Casualties) %>%
  ggplot(aes(x = Extent_Of_Fire, y = n, fill = Casualities)) + 
    geom_col(show.legend = FALSE, fill = "#fdae61") + 
    facet_wrap(~ Casualities, scales = "free_y", 
               labeller = labeller(Casualities = c(Civilian_Casualties = "Civilian Casualties", 
                                                   TFS_Firefighter_Casualties = "Firefighter Casualties"))) +
    coord_flip() + 
    theme_bw() + 
    theme(strip.background = element_rect(fill="#fdae61"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c("Confined to object of origin",
                              "Spread beyond building",
                              "Resulted in Exposures Fires",
                              "Confined to Part of Room", 
                              "Spread to entire room", 
                              "Spread beyond room",  
                              "Spread to separate suites", 
                              "Spread to other floors", 
                              "Entire structure", 
                              "Confined to Roof")) +
    labs(x = "Extent of fire")

#People hurt by Smoke Spread
FireIncidents %>%
  group_by(Smoke_Spread) %>%
  filter(Smoke_Spread != "99 - Undetermined") %>%
  summarize(Civilian_Casualties = sum(Civilian_Casualties),
            TFS_Firefighter_Casualties = sum(TFS_Firefighter_Casualties)) %>%
  top_n(n = 10) %>%
  arrange(desc(Civilian_Casualties)) %>%
  ungroup() %>%
  gather(key = "Casualities", 
         value = "n", 
         Civilian_Casualties:TFS_Firefighter_Casualties) %>%
  ggplot(aes(x = Smoke_Spread, y = n)) + 
    geom_col(show.legend = FALSE, fill = "#fdae61") + 
    facet_wrap(~Casualities, scales = "free_y", 
               labeller = labeller(Casualities = c(Civilian_Casualties = "Civilian Casualties", 
                                                   TFS_Firefighter_Casualties = "Firefighter Casualties"))) + 
    coord_flip() + 
    theme_bw() +
    theme(strip.background = element_rect(fill="#fdae61"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c("Spread beyond building",
                              "Confined to part of room",
                              "Spread to entire room",
                              "Spread beyond room", 
                              "Spread beyond suite", 
                              "Spread to separate suites",  
                              "Spread to other floors", 
                              "Entire structure", 
                              "Confined to Roof")) +
    labs(x = "Smoke Spread")

#Months
Months <- 
  FireIncidents %>%
  mutate(month=month(TFS_Alarm_Time, 
                     label = TRUE, 
                     locale="English_United States")) %>%
  ggplot(aes(x=month)) + 
    geom_bar(fill = "#fee08b") + 
    labs(x = "Month", y = "Count") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) 
#Days
Week_Days <- 
  FireIncidents%>%
  mutate(wday=wday(TFS_Alarm_Time,
                   label = TRUE,
                   locale="English_United States")) %>%
  ggplot(aes(x=wday)) + 
    geom_bar(fill = "#fee08b") +
    labs(x = "Week day", y ="Count") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) 

#Hours
Hours <- 
  FireIncidents%>%
  mutate(hour = hour(ymd_hms(TFS_Alarm_Time))) %>%
  ggplot(aes(x=hour)) + 
  geom_bar(fill = "#fee08b") +
  labs(x = "Hours", y = "Count") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 

#Mixed Plot
ggarrange(Hours,
          ggarrange(Week_Days,Months, ncol = 2, labels = c("B","C")),
          nrow = 2, labels = "A")


#Leaflet Map: Estimated Dollar Loss per Intersection

summary(log(Estimated_Dollar_Loss))

#Estimated Dollar Loss Distribution
ggplot(FireIncidents,
       aes(Estimated_Dollar_Loss)) +
  geom_histogram()

ggplot(FireIncidents,
       aes(log(Estimated_Dollar_Loss))) +
  geom_histogram()


pal <- colorNumeric(palette = "Reds", domain = c(1:16.5),
                    reverse = FALSE)

FireIncidents %>%
  transmute(Intersection, Latitude, Longitude, log(Estimated_Dollar_Loss)) %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(label = ~Intersection, radius = 2,
                   color = ~pal(log(Estimated_Dollar_Loss))) %>%
  addLegend(title = "Estimated Dollar Loss", pal = pal, values = c(1:16.5), 
            position = "bottomright")


#Ggplot Map: Time Response
Duration_map<- FireIncidents %>% 
  mutate(
    TFS_Duration_Time=difftime(ymd_hms(TFS_Arrival_Time),
                               ymd_hms(TFS_Alarm_Time),units = "mins")) %>%
  mutate(
    Duration = case_when(TFS_Duration_Time <= 5 ~ "Quick response (<= 5min)",
                         TRUE ~ "Slow response (> 5min)")) %>% 
  select(Intersection,Longitude, Latitude, Duration,TFS_Duration_Time)

Duration_map %>%
  ggplot(aes(Longitude, Latitude, color = Duration)) +
  geom_point(alpha = 0.4) +
  labs(color = NULL)

#Leaflet: Time Response
map <- 
  Duration_map %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addSearchOSM() %>%
  addReverseSearchOSM() %>%
  addResetMapButton()

Quick_Response <- 
  Duration_map %>%
  filter(Duration == "Quick response (<= 5min)")

Slow_Response <-
  Duration_map %>%
  filter(Duration == "Slow response (> 5min)")

pal <- colorFactor(palette = c("green", "red"),
                   levels = c("Quick response (<= 5min)", "Slow response (> 5min)"))

map %>%
  addCircleMarkers(
    data = Quick_Response,
    radius = 1,
    label = ~Intersection,
    color = ~pal(Duration),
    group = "Quick response (<= 5min)") %>%
  addCircleMarkers(
    data = Slow_Response,
    radius = 1,
    label = ~Intersection,
    color = ~pal(Duration),
    group = "Slow response (> 5min)") %>%
  addLayersControl(
    overlayGroups = c("Quick response (<= 5min)",
                      "Slow response (> 5min)"))



#Heatmap Avg Time Controling Fire
p2 <-
  FireIncidents%>%
  mutate(year=year(TFS_Alarm_Time),
         TFS_Duration_Time_Controling=difftime(ymd_hms(Fire_Under_Control_Time),
                                               ymd_hms(TFS_Arrival_Time),units = "mins")) %>%
  select(year,TFS_Duration_Time_Controling,Incident_Station_Area) %>%
  group_by(year,Incident_Station_Area) %>%
  summarise(Avg_Controling_mins = mean(TFS_Duration_Time_Controling)) %>%
  arrange(desc(year)) %>% 
  ungroup() %>%
  ggplot(aes(x=as.factor(year),
             y=as.factor(Incident_Station_Area),
             fill=as.numeric(Avg_Controling_mins)))+
  geom_tile() + labs(x = "Year", 
                     y = "Station", 
                     fill = "Average Controlling Time (min)") +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1) + 
  theme_grey(base_size=10) +
  theme(legend.title = element_text(size = 9, vjust = 6, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.6,"cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.key.height=grid::unit(0.8,"cm"),
        axis.title.x = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 6),
        plot.background=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_line(size=0.7))
p2

















#Area of origin
'
FireIncidents %>%
  count(Area_of_Origin) %>%
  top_n(10,n) %>%
  arrange(desc(n)) %>%
  filter(Area_of_Origin != "97 - Other - unclassified" 
         & Area_of_Origin != "99 - Undetermined  (formerly 98)") %>%
  mutate(Area_of_Origin2 = fct_reorder(Area_of_Origin, n))'

#Extent of fire
'FireIncidents %>%
  count(Extent_Of_Fire) %>%
  top_n(10,n) %>%
  arrange(desc(n)) %>%
  filter(Extent_Of_Fire != "99 - Undetermined") %>%
  mutate(Extent_Of_Fire2 = fct_reorder(Extent_Of_Fire, n))'

#Ignition source
'FireIncidents %>%
  count(Ignition_Source) %>%
  top_n(10,n) %>%
  arrange(desc(n)) %>%
  filter(Ignition_Source != "999 - Undetermined" 
         & Ignition_Source != "98 - Other") %>%
  mutate(Ignition_Source2 = fct_reorder(Ignition_Source, n))'

#Material First Ignited
'FireIncidents %>%
  count(Material_First_Ignited) %>%
  top_n(10,n) %>%
  arrange(desc(n)) %>%
  filter(Material_First_Ignited != "99 - Undetermined (formerly 98)"
         & Material_First_Ignited != "97 - Other") %>%
  mutate(Material_First_Ignited2 = fct_reorder(Material_First_Ignited, n))'

