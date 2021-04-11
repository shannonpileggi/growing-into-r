library(tidyverse)
library(ggplot2)
library(ggcal)
library(ggforce)
library(ggchicklet)
library(extrafont)
library(lubridate)
library(ggtext)

extrafont::loadfonts(device = "win")

mydate <- seq(as.Date("2017-02-01"), as.Date("2017-07-22"), by="1 day")
myfills <- rnorm(length(mydate))

print(ggcal(mydate, myfills))

my_calendar <- tibble(
  date = seq(as.Date("2018-12-01"), as.Date("2018-12-31"), by="1 day"),
  event = rnorm(1)
)

ggcal(my_calendar[["date"]], my_calendar[["event"]])

dates <- my_calendar[["date"]]
fills <- my_calendar[["event"]]

events <- c(
  "applied" = 3,
  "phone\nscreen" = 5,
  "interview" = 17
)

my_ggcal <- function(dates, fills) {
  # get ordered vector of month names
  months <- format(seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by="1 month"), "%B")
  
  # get lower and upper bound to fill in missing values
  mindate <- as.Date(format(min(dates), "%Y-%m-01"))
  maxdate <- (seq(as.Date(format(max(dates), "%Y-%m-01")), length.out = 2, by="1 month")-1)[2]
  # set up tibble with all the dates.
  filler <- tibble(date = seq(mindate, maxdate, by="1 day"))
  
  t1 <- tibble(date = dates, fill=fills) %>%
    right_join(filler, by="date") %>% # fill in missing dates with NA
    mutate(dow = as.numeric(format(date, "%w"))) %>%
    mutate(month = format(date, "%B")) %>%
    mutate(woy = as.numeric(format(date, "%U"))) %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    mutate(month = factor(month, levels=months, ordered=TRUE)) %>%
    arrange(year, month) %>%
    mutate(monlabel=month)
  
  if (length(unique(t1$year))>1) { # multi-year data set
    t1$monlabel <- paste(t1$month, t1$year)
  }
  
  t2 <- t1 %>%
    mutate(monlabel = factor(monlabel, ordered=TRUE)) %>%
    mutate(monlabel = fct_inorder(monlabel)) %>%
    mutate(monthweek = woy-min(woy),
           y=max(monthweek)-monthweek+1,
           day = day(date)) %>% 
    left_join(enframe(events, name = "event_description", value = "day")) %>% 
    mutate(event = ifelse(is.na(event_description), "n", "y"))
  
  #weekdays <- c("S", "M", "T", "W", "T", "F", "S")
  
  tile_colors <- c(
    "y" = "#562457",
    "n" = "#FFFFFF"
  )
  
  text_colors <- c(
    "n" = "#562457",
    "y" = "#FFFFFF"
  )
  
  t2 %>% 
    mutate(dow = factor(dow, levels = 0:6, labels = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"))) %>% 
    ggplot(aes(1, 1)) +
    geom_chicklet(color="#562457", aes(fill = event), width = 1, show.legend = F, size = 0.1) +
    geom_text(aes(label = day, color = event), nudge_x = -0.3, nudge_y = -0.2, size = 1.8, show.legend = F, family = "Lato") +
    geom_text(aes(label = event_description), color = "white", size = 1.8, nudge_y = -0.6, family = "Permanent Marker") +
    facet_grid(monthweek ~ dow) +
    theme(panel.background=element_rect(fill=NA, color=NA),
          strip.background = element_rect(fill = NA, color = NA),
          strip.text.x = element_text(color = "#562457",
                                      family = "Lato"),
          strip.text.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title.position ="plot",
          plot.title = element_text(
            color = "#562457",
            family = "Permanent Marker"
          ),
          panel.spacing=unit(0, "lines"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
    ggtitle("December 2018") +
    scale_fill_manual(values = tile_colors) +
    scale_color_manual(values = text_colors)
}

ggsave(here::here("slides", "img", "example_calendar3.png"))

my_ggcal(my_calendar[["date"]], my_calendar[["event"]])


df <- tibble(
  id = 1:10,
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2),
)

geom_shape(aes(fill = value, group = id), 
           expand = unit(-2, 'mm'), radius = unit(5, 'mm'))

ggplot(df, aes(x, y)) +
  geom_chicklet(width = 0.75) 
  
  geom_tile(aes(fill = z), colour = "grey50", linejoin = "bevel")
