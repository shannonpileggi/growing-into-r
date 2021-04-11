library(tidyverse)
library(ggplot2)
library(ggchicklet)
library(extrafont)
library(lubridate)
library(ggtext)

extrafont::loadfonts(device = "win")

## https://github.com/jayjacobs/ggcal
#library(ggcal)
#mydate <- seq(as.Date("2017-02-01"), as.Date("2017-07-22"), by="1 day")
#myfills <- rnorm(length(mydate))
#
#print(ggcal(mydate, myfills))


my_calendar <- tibble(
  date = seq(as.Date("2018-12-01"), as.Date("2018-12-31"), by="1 day"),
  event = rnorm(1)
)

dates <- my_calendar[["date"]]
fills <- my_calendar[["event"]]

tile_colors <- c(
  "job" = "#562457",
  "comm" = "#BA68C8",
  "empty" = "#FFFFFF"
)

text_colors <- c(
  "empty" = "#562457",
  "job" = "#FFFFFF",
  "comm" = "#FFFFFF"
)


events <- tribble(
  ~"type", ~"day", ~"description",
  "job", 3, "applied", 
  "job", 5, "phone\nscreen",
  "job", 17, "interview",
  "comm", 4, "@drob\n#TidyTuesday",
  "comm", 12, "twitter\nquestion",
)




# get ordered vector of month names
months <- format(seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by="1 month"), "%B")


# CODE MODIFIED FROM https://github.com/jayjacobs/ggcal/blob/master/R/ggcal.R
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
  mutate(dow = factor(dow, levels = 0:6, labels = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"))) 


dates_1 <- t2 %>% 
  left_join(events %>% filter(day %in% c(3,5,17)), by = "day") %>% 
  mutate(type = ifelse(is.na(type), "empty", type))

dates_2 <- t2 %>% 
  left_join(events %>% filter(day %in% c(3,5,17,4)), by = "day") %>% 
  mutate(type = ifelse(is.na(type), "empty", type))

dates_3 <- t2 %>% 
  left_join(events, by = "day") %>% 
  mutate(type = ifelse(is.na(type), "empty", type))
  

#weekdays <- c("S", "M", "T", "W", "T", "F", "S")

my_cal <- function(df){
df %>% 
  ggplot(aes(1, 1)) +
  geom_chicklet(color="#562457", aes(fill = type), width = 1, show.legend = F, size = 0.1) +
  # display day number in tiles ---
  geom_text(aes(label = day, color = type), nudge_x = -0.3, nudge_y = -0.2, size = 1.8, show.legend = F, family = "Lato") +
  # add description to tiles ----
  geom_text(aes(label = description), color = "white", size = 2.2, nudge_y = -0.6, family = "Permanent Marker", lineheight = 0.8) +
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

my_cal(dates_1)
ggsave(here::here("img", glue::glue("calendar_1.png")))

my_cal(dates_2)
ggsave(here::here("img", glue::glue("calendar_2.png")))

my_cal(dates_3)
ggsave(here::here("img", glue::glue("calendar_3.png")))



get_name <- function(df){
  
  # some expression here....
  df_name <- 
  return(df_name)

  }

get_name(faithful)
"faithful"

my_cal(dates_1)
