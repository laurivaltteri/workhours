library(jsonlite)
library(magrittr)
library(tidyverse)
library(lubridate)
library(ggmap)
system.time(x <- fromJSON("~/gdrive/Takeout/Takeout/Location History/Locationhistory.json"))
y <- as_tibble(x$locations)
# as human time
y$time <- as_datetime(as.numeric(y$timestampMs)/1000, origin = lubridate::origin, tz="Europe/Helsinki")
y$date <- as_date(y$time)
# as GPS coordinates
y$lat <- y$latitudeE7 / 1e7
y$lon <- y$longitudeE7 / 1e7
# ditch useless
y %<>% select(time, date, lon, lat)
# since started at nightingale
y_since <- y %>% filter(date > as_date("2018-11-25"))
# how many points per day
#y_since %>% group_by(date) %>% summarise(n = n())
# add at work tag
lonmin <- 24.89928
lonmax <- 24.90086
latmin <- 60.19857
latmax <- 60.19993

c(lonmin,latmin) %>% 
  get_map(zoom =15) %>%
  ggmap(extent = "device") +
  geom_rect(
    aes(
      xmin=lonmin, 
      xmax=lonmax, 
      ymin=latmin, 
      ymax=latmax
    ), 
    alpha = 0.2,
    fill = "cyan"
  )

y_since %<>% 
  mutate(
    at_work = 
      lon>lonmin & 
      lon<lonmax & 
      lat>latmin & 
      lat<latmax
  )

when_if_at_work <- function(data, minmax) {
  if (sum(data$at_work)>0) {
    pivotime <-
      if (minmax) min(which(data$at_work))
      else max(which(data$at_work))
    pivotime %>% slice(data, .) %>% pull(time)
  } else {
    -6e10
  }
}

offtime_check <- function(data) {
}

workinhours <-
  y_since %>% 
  group_by(date) %>%
  nest() %>%
  mutate(
      workday = map_lgl(
        data,
        ~sum(.x$at_work)>0
      ),
      intime = 
        map_dbl(
          data,
          ~when_if_at_work(.x, 1)
        ) %>% as_datetime(tz = "Europe/Helsinki"),
      outtime = 
        map_dbl(
          data,
          ~when_if_at_work(.x, 0)
        ) %>% as_datetime(tz = "Europe/Helsinki"),
      duration = as.period(interval(intime, outtime))
  )
workinhours %>% filter(workday) %>% pull(duration) %>% period_to_seconds() %>% mean() %>% seconds_to_period()
workinhours %>% filter(workday) %>% pull(intime) %>% get_time() %>% period_to_seconds() %>% mean() %>% seconds_to_period()
#workinhours %>% filter(workday) %>% pull(duration) %>% period_to_seconds() - period_to_seconds(hms("7:45:00")) %>% seconds_to_period()

get_time <- function(time = now()) {
  time %>%
    str_split(" ") %>%
    map_chr(2) %>%
    hms()
}

#### just testing ####
oneday <- y_since %>% filter(date>as_date("2019-01-07"), date<as_date("2019-01-09"))
oneday <- y_since %>% filter(date>as_date("2019-01-10"), date<as_date("2019-01-12"))

y_since %>% filter(date>as_date("2018-11-27"), date<as_date("2018-11-29")) %>% 
  ggplot(aes(time, as.numeric(at_work))) + geom_line()
y_since %>% filter(date>as_date("2019-01-01"), date<as_date("2019-02-01")) %>% ggplot(aes(time, as.numeric(at_work))) + geom_line()

https://nvbn.github.io/2018/05/01/commute/
https://www.r-bloggers.com/how-to-map-your-google-location-history-with-r/