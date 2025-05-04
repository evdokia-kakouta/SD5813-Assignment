# SD5813 Assignment: Portfolio

# Install packages

install.packages("janitor")
install.packages("stringr")
install.packages("tidyverse")
install.packages("viridis")
install.packages("waffle")
install.packages("showtext")
install.packages("extrafont")
install.packages("ggtext")
install.packages("ggspatial")

# Load packages

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library (ggplot2)
library(tidyverse)
library(visdat)
library(viridis)
library(waffle)
library(showtext)
library(extrafont)
library(ggtext)
library(ggspatial)


# 1. Import data

forest <- read_csv ("Data/P_Data_Extract_From_Environment_Social_and_Governance_(ESG)_Data.csv")

# 2. Inspect and Clean the data

dim(forest) # Observations = 244, Variables = 68

head(forest, 10)
tail(forest,10)

  # Keep only valid rows and
  # keep only the valid columns (to remove years with NA values, keep only years 2000-2021 -> remove columns 5:34 = years 1960:1989)

  dim(forest) # total columns = 68
  names(forest) # includes years 1960:2023
  
  forest_clean <- forest[1:239, c(1:4 , 45:66)] 
  
  dim(forest_clean) # total columns = 26
  names(forest_clean) # includes years 1990:2021
  
  # Replace spaces in variable names with _
  
  forest_clean <- forest_clean %>%
    janitor::clean_names()

  # remove countries with too many NA values
      # (imputing the data would have been too unreliable)
  
  forest_clean <- forest_clean %>%
    filter(country_name != "Montenegro" &
             country_name !="Serbia" & 
             country_name !="South Sudan")
  
  dim(forest_clean)
  
  # Check that there are no NA values
  
  sum(is.na(forest_clean)) # 0 (success!)
  
  # Reshape the data to long format
  
  forest_long <- forest_clean %>%
    pivot_longer(cols = contains("_yr"),
                 values_to = "values",
                 names_to = "year")
  
  # Check structure of variables
  
  str(forest_clean$values) # numeric (= OK)
  str(forest_clean$year) # character (not suitable for year -> create a new column)
  
  # Extract only numeric values from year column
    # and convert the new  period column to numeric
  
  forest_long <- forest_long %>%
    mutate (period = str_extract (year, "[0-9]+")) %>%
    mutate (year = as.numeric (period))
  
  # Create a dictionary for the country code 
  
  country_key <- forest_long %>%
    select(country_name, country_code) %>%
    reframe(country_code = unique(country_code),
            country_name = unique(country_name))
  
  
  # Create a data dictionary with a description for the unique series code
  
  data_key <- forest_clean %>%
    select(series_name, series_code) %>%
    reframe(series_code = unique(series_code),
            series_name = unique(series_name)) 
  data_key <- data_key %>%
    mutate(series_code = str_to_lower (series_code)) %>%
    mutate(series_code = str_replace_all(series_code, "\\.", "_"))
  

  # Create a new dataset 
  
  forest_new <- forest_long %>%
    select(country_name, country_code, year, values) %>%
    rename(country = country_name,
           forest_cover = values) %>%
    mutate(forest_cover = round(forest_cover,2)) # to round all values to 2 decimal places
  
  dim(forest_new)
  
  # Save R objects to a file
  saveRDS(forest_new, "Data/forest_data.rds")
  
  # Save workspace
  save.image(file = "Data/forest_data.rdata")
  

# 3. Create a static graph/figure showing trends over time across countries.

#     Select the top ten countries with the largest forest area (2020) - based on FAO data
  
  forest_top <- forest_new %>%
    filter(country == "Russian Federation" |
             country == "Brazil" |
             country == "Canada" |
             country == "United States" |
             country == "China" |
             country == "Australia" |
             country == "Congo, Dem. Rep." |
             country == "Indonesia" |
             country == "Peru" |
             country == "India")

    forest_top <- forest_top %>%
    mutate(country = case_when(country == "Congo, Dem. Rep." ~ "DR Congo",
      TRUE ~ country)) # as the name will be used later as a label on the yaxis
  
  # Create a ggplot layer (heatmap)
  
  # Heatmap does not show a clear and meaningful temporal trend
  #  (could be because of short timeframe - yearly changes in forest cover are small)
    
  # Calculate the % change in forest cover relative to the baseline of 2000
  
  forest_2000 <- forest_top %>%
    filter(year == 2000) %>%
    rename(forest_cover_2000 = forest_cover) %>%
    select("country", "forest_cover_2000")
  
  forest_top_perc_change <- forest_top %>%
    left_join (forest_2000, by = "country") %>%
    mutate(perc_change = (forest_cover - forest_cover_2000)/forest_cover_2000*100) %>%
    mutate(perc_change = round(perc_change, 2))
  
  # Create a new variable to use it to re-order countries on the y-axis
  
  perc_change_2021 <- forest_top_perc_change %>%
    filter(year == 2021) %>%
    rename(perc_change_2021 = perc_change) %>%
    select("country", "perc_change_2021")
  
  forest_top_perc_change <- forest_top_perc_change %>%
    left_join (perc_change_2021, by = "country") 
    
  
  # Create a heatmap to show % change since 2000
  
  #   Create a ggplot() layer and add a geom_tile layer
  #   Map years to the x-axis and country names to the y-axis 
  #   Fill in the bars as such as to show the percentage change in forest cover since 2000
  
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = country,
                   fill = perc_change))
  
  #   Re-order the y-axis based on forest cover percentage change from 2000 (with the largest on top)
  
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change))
  
  #   Add white borders to make the tiles and the values they represent clearer to distinguish
  
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change),
               colour = "white",
               size = 0.05)
  
  #   Change the colour scheme of the tiles to one that uses green and yellow to visually refer to forest cover
   
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change),
               colour = "white",
               size = 0.05 ) +
    scale_fill_viridis_c()
  
  #   Adjust the direction, position and size of the scale bar
  
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change),
               colour = "white",
               size = 0.05 ) +
    scale_fill_viridis_c( guide = guide_colorbar(barwidth = unit(11, "cm"))) +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal")
  
  #   Add legend title and adjust position
  
  forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change),
               colour = "white",
               size = 0.05 ) +
    scale_fill_viridis_c( guide = guide_colorbar(barwidth = unit(11, "cm"),
                          title.position = "top")) +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.title = element_text(hjust = 0.5)) +
    labs(y = "", x = "", 
         fill = "% change in forest cover since 2000")
  
  #   Adjust the direction of the colour scale to show decrease in yello and increase in blue
  #   Set the breaks and limits for the scale bar
  
    summary(forest_top_perc_change$perc_change) # to decide the breaks and limits for scale_fill_viridis_c
  
    forest_top_perc_change %>%
      ggplot() +
      geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                     fill = perc_change),
                 colour = "white",
                 size = 0.05 ) +
      scale_fill_viridis_c(direction = -1,
                           guide = guide_colorbar( barwidth = unit(11, "cm"),
                                                  title.position = "top"),
                           breaks = c(seq(-15, 30, 5)),
                           limits = c(-15, 30)) +
      theme(legend.position = "bottom", 
            legend.direction = "horizontal",
            legend.title = element_text(hjust = 0.5)) +
      labs(y = "", x = "", 
           fill = "% change in forest cover since 2000")
  
   #  Add title and subtitle and adjust positions
   #  Change the theme to minimal
   #  Change the fonts style and size
   #  Make final adjustments
    
    font_add_google("Alegreya SC", 
                    family = "alegreya_sc")
    showtext_auto()
    
    
    forest_top_perc_change %>%
    ggplot() +
    geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                   fill = perc_change),
    colour = "white",
    size = 0.1 ) +
    scale_fill_viridis_c(begin = 0, end = 1,
                         direction = -1,
                         guide = guide_colorbar(barwidth = unit(11, "cm"),
                                                title.position = "top"),
                         breaks = c(seq(-15, 30, 5)),
                         limits = c(-15, 30),
                         values = scales::rescale(c(-15, 10, 30))) +  # to shift the position of 0 on the legendbar
      theme_minimal() +
      theme(legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "wqy-microhei"),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "wqy-microhei"),
          plot.subtitle = element_text(hjust = 0.5, size = 14, family = "wqy-microhei"),
          axis.text = element_text (size = 14, family = "wqy-microhei"),
          axis.ticks.x = element_line(color = "grey"),
          axis.ticks.y = element_line(color = "grey"))+
    labs(y = "", x = "", 
         fill = "Percentage change in forest cover (%) since 2000",
         title = "China: a Glimpse of Hope Amidst Global Forest Loss",
         subtitle = "The top ten countries with the largest forest area in the world show great variation in 
forest cover change (%) since 2000.")
    
    # assign the plot to an object
    
    forest_heatmap <- forest_top_perc_change %>%
      ggplot() +
      geom_tile (aes(x = year, y = reorder(country, perc_change_2021, increasing = TRUE),
                     fill = perc_change),
                 colour = "white",
                 size = 0.1 ) +
      scale_fill_viridis_c(begin = 0, end = 1,
                           direction = -1,
                           guide = guide_colorbar(barwidth = unit(11, "cm"),
                                                  title.position = "top"),
                           breaks = c(seq(-15, 30, 5)),
                           limits = c(-15, 30),
                           values = scales::rescale(c(-15, 10, 30))) +  # to shift the position of 0 on the legendbar
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.direction = "horizontal",
            legend.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "wqy-microhei"),
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "wqy-microhei"),
            plot.subtitle = element_text(hjust = 0.5, size = 14, family = "wqy-microhei"),
            axis.text = element_text (size = 14, family = "wqy-microhei"),
            axis.ticks.x = element_line(color = "grey"),
            axis.ticks.y = element_line(color = "grey"))+
      labs(y = "", x = "", 
           fill = "Percentage change in forest cover (%) since 2000",
           title = "China: a Glimpse of Hope Amidst Global Forest Loss",
           subtitle = "The top ten countries with the largest forest area in the world show great variation in 
forest cover change (%) since 2000.")
    
    
  # Save the plot
    
  ggsave(file = "Images/forest-cover_heatmap.png",
         plot = forest_heatmap,
         dpi = 300,
         height = 8,
         width = 10)  
  
  ggsave(file = "Images/forest-cover_heatmap.jpeg",
         dpi = 300,
         height = 8,
         width = 10)

  
  
# 4. Create a map showing spatial variation in the data.
  
  # Install and load packages
  
  install.packages("sf")
  library(sf)
  
  # Load the shapefile

  map_world <- st_read("Data/WB_countries_Admin0_10m.shp")
  
  # Inspect the columns
  
  dim(map_world)
  tail(map_world, 10)
  names(map_world)
  
  # Check the coordinate reference system
  
  st_crs(map_world) # "WGS 84"
  
  # Create a basic map using ggplot and change its projection
  
  ggplot() +
    geom_sf(data = map_world)+
    coord_sf(crs=3857)
  
  # Clean the data:
  #   Select the relevant variables
  #   Identify a common variable between the two datasets (= ISO_A3)
  
  map_world_clean <- map_world %>%
    select("INCOME_GRP", "CONTINENT","REGION_WB", "WB_NAME", "ISO_A3", "geometry" ) %>%
    rename(country_code = ISO_A3,
           country_name_map = WB_NAME,
           continent = CONTINENT,
           region = REGION_WB,
           income = INCOME_GRP)
  
  length(map_world_clean$country_name_map) # 251
  length(map_world_clean$country_code) # 251
  
  # Remove invalid observations from country_code
  
  map_world_clean <- map_world_clean %>%
    filter(country_code != -99)
  
  #   Create a dictionary for country codes
  
  country_key_2 <- map_world_clean %>%
    select(country_name_map, country_code) 
  
  #   Check for and remove any NA values
  
  sum(is.na(map_world_clean)) # 0
  
  # Prepare the forest data before joining them to the shp
  
  #   Re-clean and re-structure the original dataset so that it also includes the three countries that were excluded from the first graph
  
  # Keep only valid rows and
  # keep only the valid columns (to remove years with NA values, keep only years 2000-2021 -> remove columns 5:34 = years 1960:1989)
  
  dim(forest) # total columns = 68
  names(forest) # includes years 1960:2023
  
  forest_map_dta <- forest[1:239, c(1:4 , 45:66)] 
  
  dim(forest_map_dta) # total columns = 26
  names(forest_map_dta) # includes years 1990:2021
  
  # Replace spaces in variable names with _
  
  forest_map_dta <- forest_map_dta %>%
    janitor::clean_names()
  
  # Check no. of NA values and visualise no. of countries with NA values (sould be shown as grey on the map)
  
  sum(is.na(forest_map_dta))
  vis_miss(forest_map_dta) # 3 countries with NA values
  
  # Reshape the data to long format
  
  forest_map_dta_long <- forest_map_dta %>%
    pivot_longer(cols = contains("_yr"),
                 values_to = "values",
                 names_to = "year")
  
  # Check structure of variables
  
  str(forest_map_dta_long$values) # numeric (= OK)
  str(forest_map_dta_long$year) # character (not suitable for year -> create a new column)
  
  # Extract only numeric values from year column
  # and convert the new  period column to numeric
  
  forest_map_dta_long <- forest_map_dta_long %>%
    mutate (period = str_extract (year, "[0-9]+")) %>%
    mutate (year = as.numeric (period))
  
  # Rename variables and round values 
  
  forest_map_dta_long <- forest_map_dta_long %>%
    select(country_name, country_code, year, values) %>%
    rename(country_name_forest = country_name,
           forest_cover = values) %>%
    mutate(forest_cover = round(forest_cover,2)) # to round all values to 2 decimal places
  
  #   Convert back to wide format (need to convert years to character values and rename them as they will the names of the variables)
  
  forest_map_dta__wide <- forest_map_dta_long %>%
    mutate(year = as.character(year)) %>%
    mutate(year_ch = paste0("yr_", year)) %>%
    select(country_name_forest, country_code, forest_cover, year_ch) %>%
    pivot_wider(names_from = year_ch,
                values_from = forest_cover) 
    

  # Join forest_map_data to the shapefile
  
  length(map_world_clean$country_code) #244
  length(forest_map_dta__wide$country_code) #239 (-> there will be some NA values in the joined dataset)
  
  # Compare country codes between the two datasets
  
  country_key_3 <- country_key_2 %>%
    left_join(country_key, by = "country_code") %>%
    rename(country_name_forest = country_name)
  
  vis_miss(country_key_3)
  sum(is.na(country_key_3$country_name_forest)) # 52 countries/states are not present in the forest dataset (will be shown as NA on the map)
  
  # Join the data
  
  map_world_forest <- map_world_clean %>% 
    left_join(forest_map_dta__wide, by = "country_code")
  
  vis_miss(map_world_forest)
  sum(is.na(map_world_forest$yr_2001)) # 52
  
  dim(map_world_forest) # observations = 244 (same no. as in map_world dataset)
  
  # Convert the dataset to a shapefile
  
  map_world_forest <- st_as_sf(map_world_forest)
  
  class(map_world_forest) # to check that it has been converted to a shapefile
  
  # Plot
  
  plot(st_geometry(map_world_forest))
  
  ggplot() +
    geom_sf(data = map_world_forest, color = "black", linewidth = 0.05, fill = "grey")
  
  # Testing: create a map showing forest cover (%) in 2021
  
  ggplot() +
    geom_sf(data = map_world_forest, aes(fill = yr_2021), color = "black", linewidth = 0.05)
  

  # Create a map showing the difference in forest cover over time
  #   Calculate the difference between the mean forest cover of the first 3 and last 3 years of the dataset
  #   Calculating the mean of three years instead of  just taking the first year for more reliable results
  
  map_world_forest <- map_world_forest %>%
    mutate(forest_cover_chg = rowMeans(across(c(yr_2019, yr_2020, yr_2021))) - rowMeans(across(c(yr_2000, yr_2001, yr_2002)))) %>%
    mutate(forest_cover_chg = round(forest_cover_chg, 2))
  
  ggplot() +
    geom_sf(data = map_world_forest, aes(fill = forest_cover_chg), color = "black", linewidth = 0.05)
  
  # Edit the map:
  #   Change the colour scale (use the same as before for uniformity), change the name and position of the scale bar
  #   Add a key for NA values
  #   Add a title and subtitle
  #   Apply a theme
  #   Change the fonts
  #   Add labels for the countries menioned int the subtitle

  ggplot() +
    geom_sf(data = map_world_forest, 
            aes(fill = forest_cover_chg), 
            color = "black", 
            linewidth = 0.05) +
    scale_fill_viridis_c(begin = 0, end = 1,
                         direction = -1,
                         guide = guide_colorbar(barwidth = unit(11, "cm"),
                                                barheight = unit(0.5, "cm"),
                                                title.position = "top"),
                         breaks = c(seq(-10, 8, 2)),
                         limits = c(-10.5, 8.5),
                         oob = scales::squish, # to make sure that values beyond the limits or clipped onto the nearest colour
                         values = scales::rescale(c(-10.5, 1.5, 8.5))) +  # to shift the position of 0 on the legendbar
    labs( title = "Forest Cover in the 21st Century: Gains and Losses",
          subtitle = "Hope for Forest Restoration from Europe and East Asia \n Concerns for Forest Decline from South America and Central Africa",
          fill = "Change in Forest Cover (%) between 2000 and 2021") +
    annotation_north_arrow(location = "bl", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = ggspatial::north_arrow_minimal(fill = c("grey40"), line_col = "grey20")) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.title = element_text(hjust = 0.5, size = 14),                       
          plot.title = element_text(hjust = 0.5, size = 18,  face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.text = element_text(size = 12, colour = "grey30"))
    
  # Assign the plot to an object
  
  forest_map <- ggplot() +
    geom_sf(data = map_world_forest, 
            aes(fill = forest_cover_chg), 
            color = "black", 
            linewidth = 0.05) +
    scale_fill_viridis_c(begin = 0, end = 1,
                         direction = -1,
                         guide = guide_colorbar(barwidth = unit(11, "cm"),
                                                barheight = unit(0.5, "cm"),
                                                title.position = "top"),
                         breaks = c(seq(-10, 8, 2)),
                         limits = c(-10.5, 8.5),
                         oob = scales::squish, # to make sure that values beyond the limits or clipped onto the nearest colour
                         values = scales::rescale(c(-10.5, 1.5, 8.5))) +  # to shift the position of 0 on the legendbar
    labs( title = "Forest Cover in the 21st Century: Gains and Losses",
          subtitle = "\nHope for Forest Restoration from Europe and East Asia \n Concerns for Forest Decline from South America and Central Africa",
          fill = "Change in Forest Cover (%) between 2000 and 2021") +
    annotation_north_arrow(location = "bl", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = ggspatial::north_arrow_minimal(fill = c("grey40"), line_col = "grey20")) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.title = element_text(hjust = 0.5, size = 29, face = "bold"),                       
          plot.title = element_text(hjust = 0.5, size = 40,  face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 35),
          legend.text = element_text(size = 29))
  
  summary(map_world_forest$forest_cover_chg) # to check max and min values to set the limits for the scale
  
  # Save plot
  
  ggsave(plot = forest_map,
    file = "Images/forest-cover_map.png",
         dpi = 300,
         height = 5,
         width = 6)  
  
  ggsave(plot = forest_map,
    file = "Images/forest-cover_map.jpeg",
         dpi = 300,
         height = 5,
         width = 6)
  
  
  # Save workspace
  save.image(file = "data/forest_map.rdata")
  
  
# 5. Create an interactive figure (either using leaflet or plotly)
  
install.packages("plotly")

library(plotly)

# Convert the ggplot map (forest)map) to an interactive map using ggplotly

# Create a new variable for the text that will appear when hovering over the plot

map_world_forest <- map_world_forest %>%
  mutate(text = paste0("Country: ", country_name_forest,
                       "\nChange in forest cover (%) between 2000 and 2021: ", forest_cover_chg, "%",
                       "\nAdditional information:",
                       "\nForest cover in 2021: ", yr_2021, "%",
                       "\nIncome: ", income))

# Check top and bottom countries to be mentioned in the subtitle of the interactive map

top_and_bottom <- map_world_forest %>%
  select(country_name_forest, forest_cover_chg) %>%
  filter(!is.na(top_and_bottom$forest_cover_chg)) %>%
  
  top_and_bottom <-top_and_bottom %>%
  arrange(desc(forest_cover_chg))

head(top_and_bottom$country_name_forest, 3) # "Viet Nam"  "Cuba" "Fiji" 
tail(top_and_bottom$country_name_forest, 3) # "Paraguay" "Nicaragua" "Cambodia"


# Assign the map to a new object
# Remove any functions and arguments that are not recognized in ggplotly

forest_map_1 <- ggplot() +
  geom_sf(data = map_world_forest, 
          aes(fill = forest_cover_chg,  text = text), 
          color = "black", 
          linewidth = 0.05) +
  scale_fill_viridis_c(begin = 0, end = 1,
                       direction = -1,
                       guide = guide_colorbar(barwidth = unit(1, "cm"),
                                              barheight = unit(9, "cm"),
                                              title.position = "top"),
                       breaks = c(seq(-10, 8, 2)),
                       limits = c(-10.5, 8.5),
                       oob = scales::squish, # to make sure that values beyond the limits or clipped onto the nearest colour
                       values = scales::rescale(c(-10.5, 1.5, 8.5))) +  # to shift the position of 0 on the legendbar
  labs( fill = NULL) +
  annotation_north_arrow(location = "bl", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = ggspatial::north_arrow_minimal(fill = c("grey40"), line_col = "grey20")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14,  face = "bold"),
        legend.text = element_text(size = 10))

# Write the title and subtitle in htlm format (as ggplotly does not support the subtitle from the ggplot)
  
forest_map_int <- ggplotly(p = forest_map_1, tooltip = "text") %>%
    layout(title = list(text = paste0('<br>','<br>','<br>', 'Forest Cover in the 21 Century',
                                      '<br>','<br>',
                                             '<sup>',
                                             'Vietnam, Cuba, and Fiji are planting the seeds for a greener future while\nParaguay, Nicaragua, and Cambodia sound the alarm for policy change', '</sup>')))
# View the interactive map  

forest_map_int
    
# Save the interactive figure
  
install.packages("htmlwidgets")
library(htmlwidgets)
  
saveWidget(forest_map_int, "Images/forest_map_interactive", selfcontained = F)

# Save workspace
save.image(file = "Data/forest_data.rdata")
  