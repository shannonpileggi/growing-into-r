library(tidyverse)  
library(gganimate)
library(ggtext)
#library(extrafont)
#library(ragg)

# import fonts ---
# set up fonts, use one time ----
# extrafont::font_import()

# watch out for where font is installed ----
#font_import(paths = "C:/Users/Shannon.Pileggi/AppData/Local/Microsoft/Windows/Fonts")


#extrafont::loadfonts(device = "win")

# font family names available for use ----
#extrafont::fonts()

# Show entire table
# fonttable() %>% view()



us_states <-  ggplot2::map_data("state")

# use <br> to render line breaks with ggtext ----
residence <- tribble(
  ~city,           ~state,  ~lat,   ~long, ~years, ~description,
  "Raleigh",         "NC", 35.82,  -78.66,  17,    "Childhood",
  "Greenville",      "NC", 35.60,  -77.37,   4,    "Undergrad at ECU",
  "Atlanta",         "GA", 33.76,  -84.42,  10,    "Grad school at Emory<br>Statistician at CDC<br>Lecturer at Emory",
  "San Luis Obispo", "CA", 35.28, -120.66,   3,    "Asst. Professor at Cal Poly SLO",
  "Williamsburg",    "VA", 37.27,  -76.71, 0.5,    "Time with family",
  "Doylestown",      "PA", 40.31,  -75.13,   2,    "Statistician at Adelphi Research"
) 

residence_connections_prelim <- residence %>% 
  mutate(
    city_order = row_number() + 1,
     # where I moved to next, for curved arrows ----
    lat_next = lead(lat),
    long_next = lead(long),
    justification = ifelse(long_next < -100, 5, -5),
    # simpler label styling with bold and italics ---
    label = glue::glue("**{city}, {state}** ({years} yrs)<br>*{description}*"),
    # inline css for styling ----
    #label = glue::glue("<span style = 'font-family:&quot;Sitka&quot;;'>{city}, {state} </span><span style = 'font-family:Ubuntu;'>({years} yrs)</span><br><span style = 'font-family:&quot;Open Sans&quot;font-style:italic;font-weight:300;;'>{description}</span>"),
    #label = glue::glue("<span style = 'font-family:&quot;Montserrat SemiBold&quot;'>{city}, {state} </span><span style = 'font-family:&quot;Montserrat&quot;'>({years} yrs)</span><br>*<span style = 'font-family:&quot;Calibri Light&quot;'>{description}</span>*"),
    label_next = lead(label)
  ) 

residence_connections <- residence_connections_prelim %>% 
  slice(1) %>% 
  mutate(
    city_order = 1,
    label_next = label,
    lat_next = lat,
    long_next = long,
    ) %>% 
  bind_rows(residence_connections_prelim) %>% 
  slice(1:6) %>% 
  dplyr::select(city_order, lat, long, lat_next, long_next, justification, label_next)


# https://alison.rbind.io/post/2017-12-18-r-ladies-presentation-ninja/
# dark purple    light purple
# "#562457"       "#88398A"     





base_map <- ggplot() +
  # states (only item don't use residence data for) ----
geom_polygon(
  data = us_states,
  aes(x = long, y = lat, group = group),
  fill = "#F2F2F2",
  color = "white"
) +
  # lines for pins, color with R ladies darker purple ----
geom_segment(data = residence,
             aes(
               x = long,
               xend = long,
               y = lat,
               yend = lat + 0.5
             ),
             color = "#181818",
             size = 0.3) +
  # pin heads, a bit above actual location, color with R ladies lighter purple ----
geom_point(
  data = residence,
  aes(x = long, y = lat + 0.5),
  size = 0.5,
  color = "#88398A"
) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  geom_curve(data = residence_connections %>% slice(-1),
             aes(y = lat - 0.1, x = long, yend = lat_next - 0.2, xend = long_next, 
                 group = seq_along(city_order)),
             color = "#A9A9A9", 
             curvature = -0.5, 
             arrow = arrow(length = unit(0.02, "npc")),
             size = 0.2) +
  geom_curve(data = residence_connections %>% slice(5),
             aes(y = lat - 0.1, x = long, yend = lat_next - 0.2, xend = long_next, 
                 group = seq_along(city_order)),
             color = "#562457", 
             curvature = -0.5, 
             arrow = arrow(length = unit(0.02, "npc")),
             size = 0.3) +
  geom_richtext(data = residence_connections %>% filter(city_order == 5),
                aes(
                  x = long_next + 0.5,
                  y = lat_next + 2.5,
                  label = label_next,
                  vjust = "top",
                  hjust = 0.37,
                  group = seq_along(city_order)
                ),
                label.colour = "white",
                color = "#562457",
                size = 3.5
  ) +
  geom_richtext(data = residence_connections %>% filter(!(city_order %in% c(2,5))),
                aes(
                  x = ifelse(long_next < -100, long_next + 1, long_next - 1),
                  y = lat_next + 2.2,
                  label = label_next,
                  vjust = "top",
                  hjust = ifelse(long_next < -100, 0, 1),
                  group = seq_along(city_order)
                ),
                label.colour = "white",
                color = "#A9A9A9",
                size = 3.5
  ) +
  geom_richtext(data = residence_connections %>% filter(city_order == 2),
                aes(
                  x = long_next,
                  y = lat_next - 2,
                  label = label_next,
                  vjust = "bottom",
                  hjust = 0.3,
                  group = seq_along(city_order)
                ),
                label.colour = "white",
                color = "#A9A9A9",
                size = 3.5
  ) #+
  #xlim(-125, -50)
  # coord map messes up curved arrows ---- 
  #coord_map() 

  #dev.off()

#agg_png(filename = "check.png")

ggsave("residences_flat_3.png", height = 6, width = 7)

