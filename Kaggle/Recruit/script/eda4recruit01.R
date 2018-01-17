# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation
library('ggfortify') # visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis

# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# function to extract binomial confidence levels
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))
on_kaggle <- 1

if (on_kaggle == 0){
  rpath <- ""
  wpath <- ""
  wdpath <- "1-1-16_5-31-17_Weather/"
} else {
  rpath <- "../input/recruit-restaurant-visitor-forecasting/"
  wpath <- "../input/rrv-weather-data/"
  wdpath <- "../input/rrv-weather-data/1-1-16_5-31-17_Weather/1-1-16_5-31-17_Weather/"
}
air_visits <- as.tibble(fread(str_c(rpath,'air_visit_data.csv')))
air_reserve <- as.tibble(fread(str_c(rpath,'air_reserve.csv')))
hpg_reserve <- as.tibble(fread(str_c(rpath,'hpg_reserve.csv')))
air_store <- as.tibble(fread(str_c(rpath,'air_store_info.csv')))
hpg_store <- as.tibble(fread(str_c(rpath,'hpg_store_info.csv')))
holidays <- as.tibble(fread(str_c(rpath,'date_info.csv')))
store_ids <- as.tibble(fread(str_c(rpath,'store_id_relation.csv')))
test <- as.tibble(fread(str_c(rpath,'sample_submission.csv')))
summary(air_visits)
glimpse(air_visits)
air_visits %>% distinct(air_store_id) %>% nrow()
summary(air_reserve)
glimpse(air_reserve)
air_reserve %>% distinct(air_store_id) %>% nrow()
summary(hpg_reserve)
glimpse(hpg_reserve)
hpg_reserve %>% distinct(hpg_store_id) %>% nrow()
summary(air_store)
glimpse(air_store)
summary(hpg_store)
glimpse(hpg_store)
summary(holidays)
glimpse(holidays)
summary(store_ids)
glimpse(store_ids)
summary(test)
glimpse(test)
sum(is.na(air_visits))
sum(is.na(air_reserve))
sum(is.na(hpg_reserve))
sum(is.na(air_store))
sum(is.na(hpg_store))
sum(is.na(holidays))
sum(is.na(store_ids))
sum(is.na(test))
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date))
p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = "blue") +
  labs(y = "All visitors", x = "Date")

p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "blue", bins = 30) +
  scale_x_log10()

p3 <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")
  
p4 <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
air_visits %>%
  filter(visit_date > ymd("2016-04-15") & visit_date < ymd("2016-06-15")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 1/7) +
  labs(y = "All visitors", x = "Date")
foo <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
         )

p1 <- foo %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'air' visit date")

p2 <- foo %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "blue")

p3 <- foo %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "blue") +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
foo %>%
  arrange(desc(diff_day)) %>%
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>%
  head(5)
foo <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
         )

p1 <- foo %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'hpg' visit date")

p2 <- foo %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "red")

p3 <- foo %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "red") +
  labs(x = "Time from reservation to visit [hours]")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
leaflet(air_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~air_store_id, label = ~air_genre_name,
             clusterOptions = markerClusterOptions())
p1 <- air_store %>%
  group_by(air_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")
  
p2 <- air_store %>%
  group_by(air_area_name) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(air_area_name, n, FUN = min) ,n, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)
leaflet(hpg_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~hpg_store_id, label = ~hpg_genre_name,
              clusterOptions = markerClusterOptions())
p1 <- hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (hpg_genre_name)", y = "Number of hpg restaurants")
  
p2 <- hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 20)) %>%
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (hpg_area_name)", y = "Number of hpg restaurants")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)
foo <- holidays %>%
  mutate(wday = wday(date))

p1 <- foo %>%
  ggplot(aes(holiday_flg, fill = holiday_flg)) +
  geom_bar() +
  theme(legend.position = "none")
  
p2 <- foo %>%
  filter(date > ymd("2016-04-15") & date < ymd("2016-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2016 date")

p3 <- foo %>%
  filter(date > ymd("2017-04-15") & date < ymd("2017-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2017 date")

layout <- matrix(c(1,1,2,3),2,2,byrow=FALSE)
multiplot(p1, p2, p3, layout=layout)
holidays %>% group_by(holiday_flg) %>% count() %>% spread(holiday_flg,n) %>% mutate(frac = `TRUE`/(`TRUE`+`FALSE`))
foo <- air_visits %>%
  rename(date = visit_date) %>%
  distinct(visit_date) %>%
  mutate(dset = "train")
bar <- test %>%
  separate(id, c("foo", "bar", "date"), sep = "_") %>%
  mutate(date = ymd(date)) %>%
  distinct(date) %>%
  mutate(dset = "test")

foo <- foo %>%
  bind_rows(bar) %>%
  mutate(year = year(date))
year(foo$date) <- 2017

foo %>%
  filter(!is.na(date)) %>%
  mutate(year = fct_relevel(as.factor(year), c("2017","2016"))) %>%
  ggplot(aes(date, year, color = dset)) +
  geom_point(shape = "|", size = 10) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  #scale_y_reverse() +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(color = "Data set") +
  guides(color = guide_legend(override.aes = list(size = 4, pch = 15)))

foo <- air_visits %>%
  left_join(air_store, by = "air_store_id")

foo %>%
  group_by(visit_date, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ungroup() %>%
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = "Average number of visitors to 'air' restaurants", x = "Date") +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)
p1 <- foo %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ggplot(aes(air_genre_name, mean_visitors, color = wday)) +
  geom_point(size = 4) +
  theme(legend.position = "left", axis.text.y = element_blank(),
        plot.title = element_text(size = 14)) +
  coord_flip() +
  labs(x = "") +
  scale_x_discrete(position = "top") +
  ggtitle("air_genre_name") +
  scale_color_hue()

p2 <- foo %>%
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(y = "") +
  scale_fill_cyclical(values = c("blue", "red"))

layout <- matrix(c(1,1,2,2,2),1,5,byrow=TRUE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1
foo <- air_visits %>%
  mutate(calendar_date = as.character(visit_date)) %>%
  left_join(holidays, by = "calendar_date")

p1 <- foo %>%
  ggplot(aes(holiday_flg, visitors, color = holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- foo %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  group_by(wday, holiday_flg) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ggplot(aes(wday, mean_visitors, color = holiday_flg)) +
  geom_point(size = 4) +
  theme(legend.position = "none") +
  labs(y = "Average number of visitors")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)
air_store %>%
  mutate(area = str_sub(air_area_name, 1, 12)) %>%
  ggplot(aes(area, air_genre_name)) +
  geom_count(colour = "blue") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))
hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 10)) %>%
  ggplot(aes(area, hpg_genre_name)) +
  geom_count(colour = "red") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))
air_store %>%
  group_by(air_genre_name, air_area_name) %>%
  count() %>%
  ggplot(aes(reorder(air_genre_name, n, FUN = mean), n)) +
  geom_boxplot() +
  geom_jitter(color = "blue") +
  scale_y_log10() +
  coord_flip() +
  labs(x = "Air genre", y = "Occurences per air area")
air_store %>%
  filter(air_store_id == "air_b5598d12d1b84890" | air_store_id == "air_bbe1c1a47e09f161")

air_visits %>%
  filter(air_store_id == "air_b5598d12d1b84890" | air_store_id == "air_bbe1c1a47e09f161") %>%
  arrange(visit_date) %>%
  head(10)

foobar <- hpg_store %>%
  group_by(hpg_genre_name, hpg_area_name) %>%
  count()

foobar %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = mean), n)) +
  geom_boxplot() +
  geom_jitter(color = "red") +
  scale_y_log10() +
  coord_flip() +
  labs(x = "hpg genre", y = "Cases per hpg area")
foo <- air_visits %>%
  left_join(air_store, by = "air_store_id")

bar <- air_store %>%
  group_by(air_genre_name, air_area_name) %>%
  count()

foobar <- hpg_store %>%
  group_by(hpg_genre_name, hpg_area_name) %>%
  count()

p1 <- bar %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  labs(x = "Air genres per area")

p2 <- foobar %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "red", binwidth = 1) +
  labs(x = "HPG genres per area")

p3 <- foo %>%
  group_by(air_genre_name, air_area_name) %>%
  summarise(mean_log_visit = mean(log1p(visitors))) %>%
  left_join(bar, by = c("air_genre_name","air_area_name")) %>%
  group_by(n) %>%
  summarise(mean_mlv = mean(mean_log_visit),
            sd_mlv = sd(mean_log_visit)) %>%
  replace_na(list(sd_mlv = 0)) %>%
  ggplot(aes(n, mean_mlv)) +
  geom_point(color = "blue", size = 4) +
  geom_errorbar(aes(ymin = mean_mlv - sd_mlv, ymax = mean_mlv + sd_mlv), width = 0.5, size = 0.7, color = "blue") +
  labs(x = "Cases of identical Air genres per area", y = "Mean +/- SD of\n mean log1p visitors")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
foo <- air_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(air_store_id,visit_date) %>%
  summarise(reserve_visitors_air = sum(reserve_visitors))
  
bar <- hpg_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(hpg_store_id,visit_date) %>%
  summarise(reserve_visitors_hpg = sum(reserve_visitors)) %>%
  inner_join(store_ids, by = "hpg_store_id")

all_reserve <- air_visits %>%
  inner_join(foo, by = c("air_store_id", "visit_date")) %>%
  inner_join(bar, by = c("air_store_id", "visit_date")) %>%
  mutate(reserve_visitors = reserve_visitors_air + reserve_visitors_hpg)
p <- all_reserve %>%
  filter(reserve_visitors < 120) %>%
  ggplot(aes(reserve_visitors, visitors)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_smooth(method = "lm", color = "blue")
ggMarginal(p, type="histogram", fill = "blue", bins=50)
p1 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors)) +
  geom_histogram(binwidth = 5, fill = "black") +
  coord_flip() +
  labs(x = "")

p2 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors_air)) +
  geom_histogram(binwidth = 5, fill = "blue") +
  coord_flip() +
  labs(x = "")

p3 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors_hpg)) +
  geom_histogram(binwidth = 5, fill = "red") +
  coord_flip() +
  labs(x = "")

p4 <- all_reserve %>%
  ggplot(aes(visit_date, visitors - reserve_visitors)) +
  geom_hline(yintercept = c(150, 0, -250)) +
  geom_line() +
  geom_line(aes(visit_date, visitors - reserve_visitors_air + 150), color = "blue") +
  geom_line(aes(visit_date, visitors - reserve_visitors_hpg - 250), color = "red") +
  ggtitle("Visitors - Reserved: all (black), air (blue), hpg (red)")

layout <- matrix(c(4,4,2,4,4,1,4,4,3),3,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
all_reserve %>%
  mutate(date = visit_date) %>%
  left_join(holidays, by = "date") %>%
  ggplot(aes(visitors - reserve_visitors, fill = holiday_flg)) +
  geom_density(alpha = 0.5)
air_visits <- air_visits %>%
  mutate(wday = wday(visit_date, label=TRUE),
         wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         month = month(visit_date, label=TRUE))

air_reserve <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

hpg_reserve <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# count stores in area
air_count <- air_store %>%
  group_by(air_area_name) %>%
  summarise(air_count = n())

hpg_count <- hpg_store %>%
  group_by(hpg_area_name) %>%
  summarise(hpg_count = n())

# distances
med_coord_air <- air_store %>%
  summarise_at(vars(longitude:latitude), median)
med_coord_hpg <- hpg_store %>%
  summarise_at(vars(longitude:latitude), median)

air_coords <- air_store %>%
  select(longitude, latitude)
hpg_coords <- hpg_store %>%
  select(longitude, latitude)

air_store$dist <- distCosine(air_coords, med_coord_air)/1e3
hpg_store$dist <- distCosine(hpg_coords, med_coord_hpg)/1e3

# apply counts, dist; add prefecture
air_store <- air_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(air_count, by = "air_area_name") %>%
  separate(air_area_name, c("prefecture"), sep = " ", remove = FALSE)

hpg_store <- hpg_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(hpg_count, by = "hpg_area_name") %>%
  separate(hpg_area_name, c("prefecture"), sep = " ", remove = FALSE)

p1 <- air_visits %>%
  group_by(wday) %>%
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(wday, mean_log_visitors, color = wday)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = "none")

p2 <- air_visits %>%
  mutate(visitors = log1p(visitors)) %>%
  ggplot(aes(visitors, wday, fill = wday)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(x = "log1p(visitors)", y = "")

p3 <- air_visits %>%
  group_by(month) %>%
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(month, mean_log_visitors, color = month)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = month), width = 0.5, size = 0.7) +
  theme(legend.position = "none")

p4 <- air_visits %>%
  mutate(visitors = log1p(visitors)) %>%
  ggplot(aes(visitors, month, fill = month)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(x = "log1p(visitors)", y = "")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(wday, air_genre_name) %>%
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(wday, mean_log_visitors, color = wday)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  facet_wrap(~ air_genre_name)
all_reserve %>%
  mutate(wday = wday(visit_date, label=TRUE),
         wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  ggplot(aes(wday, visitors - reserve_visitors, fill = wday)) +
  geom_boxplot() +
  theme(legend.position = "none")
p1 <- air_count %>%
  ggplot(aes(air_count)) +
  geom_histogram(binwidth = 2, fill = "blue")

p2 <- hpg_count %>%
  ggplot(aes(hpg_count)) +
  geom_histogram(binwidth = 5, fill = "red")

p3 <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, air_count) %>%
  summarise(mean_store_visit = mean(log1p(visitors))) %>%
  group_by(air_count) %>%
  summarise(mean_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>%
  ggplot(aes(air_count, mean_log_visitors)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors),
                    color = "blue", width = 0.5, size = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Air restaurants per area")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
p1 <- air_store %>%
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = "blue") +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = "Linear distance [km]")

p2 <- hpg_store %>%
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = "red") +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = "Linear distance [km]")

p3 <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, dist_group) %>%
  summarise(mean_store_visit = mean(log1p(visitors))) %>%
  group_by(dist_group) %>%
  summarise(mean_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>%
  ggplot(aes(dist_group, mean_log_visitors)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors),
                    color = "blue", width = 0.5, size = 0.7) +
  labs(x = "Distance grouping")
  

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
foo <- air_store %>% select(latitude, longitude, dist_group) %>% mutate(dset = "air")
bar <- hpg_store %>% select(latitude, longitude, dist_group) %>% mutate(dset = "hpg")

leaflet(foo) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addScaleBar() %>%
  addCircleMarkers(~longitude, ~latitude, group = "AIR",
                   color = "blue", fillOpacity = 0.5, radius = 3*foo$dist_group) %>%
  addCircleMarkers(lng = bar$longitude, lat = bar$latitude, group = "HPG",
                   color = "red", fillOpacity = 0.5, radius = 3*bar$dist_group) %>%
  addCircleMarkers(lng = med_coord_air$longitude, lat = med_coord_air$latitude, group = "Centre",
                   color = "darkgreen", fillOpacity = 1) %>%
  addLayersControl(
    overlayGroups = c("AIR", "HPG", "Centre"),
    options = layersControlOptions(collapsed = FALSE)
  )
p1 <- air_store %>%
  ggplot(aes(prefecture)) +
  geom_bar(fill = "blue") +
  coord_flip() +
  ggtitle("air prefectures - # restaurants") +
  labs(x = "")

p2 <- hpg_store %>%
  ggplot(aes(prefecture)) +
  geom_bar(fill = "red") +
  coord_flip() +
  ggtitle("hpg prefectures - # restaurants") +
  labs(x = "")

p3 <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, prefecture) %>%
  summarise(mean_store_visit = mean(log1p(visitors))) %>%
  group_by(prefecture) %>%
  summarise(mean_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>%
  ggplot(aes(prefecture, mean_log_visitors)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors),
                    color = "blue", width = 0.5, size = 0.7) +
  labs(x = "prefecture") +
  theme(axis.text.x  = element_text(angle=15, hjust=1, vjust=0.9))

layout <- matrix(c(1,2,1,2,1,2,3,3,3,3),5,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
foo <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, air_genre_name) %>%
  summarise(mean_log_visits = mean(log1p(visitors)),
            mean_log_visits = mean(log1p(visitors)),
            sd_log_visits = sd(log1p(visitors))) %>%
  ungroup()

params_ts1 <- function(rownr){
  bar <- air_visits %>%
    filter(air_store_id == foo$air_store_id[rownr])
  slope <- summary(lm(visitors ~ visit_date, data = bar))$coef[2]
  slope_err <- summary(lm(visitors ~ visit_date, data = bar))$coef[4]
  
  foobar <- tibble(
    air_store_id = foo$air_store_id[rownr],
    slope = slope,
    slope_err = slope_err
  )
  
  return(foobar)
}

params <- params_ts1(1)
for (i in seq(2,nrow(foo))){
  params <- bind_rows(params, params_ts1(i))
}

ts_params <- foo %>%
  left_join(params, by = "air_store_id")
p1 <- ts_params %>%
  ggplot(aes(mean_log_visits)) +
  geom_histogram(bins = 50, fill = "blue")

p2 <- ts_params %>%
  ggplot(aes(sd_log_visits)) +
  geom_histogram(bins = 50, fill = "blue")

p3 <- ts_params %>%
  filter(slope < 0.5) %>%
  ggplot(aes(slope)) +
  geom_histogram(bins = 50, fill = "blue") +
  labs(x = "Slope < 0.5")

p4 <- ts_params %>%
  ggplot((aes(mean_log_visits, sd_log_visits))) +
  geom_point(size = 2, color = "blue")

p5 <- ts_params %>%
  ggplot((aes(slope, slope_err))) +
  geom_point(size = 2, color = "blue")

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
ts_params %>%
  filter(abs(slope) > 0.25) %>%
  select(air_store_id, air_genre_name, slope, slope_err)
ts_params %>%
  ggplot(aes(mean_log_visits, slope, color = air_genre_name)) +
  geom_errorbarh(aes(xmin = mean_log_visits - sd_log_visits,
                    xmax = mean_log_visits + sd_log_visits),
                    color = "grey70", size = 0.7) +
  geom_errorbar(aes(ymin = slope - slope_err,
                    ymax = slope + slope_err),
                    color = "grey70", size = 0.7) +
  geom_point() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 4))) +
  labs(color = "") +
  facet_zoom(y = (slope < 0.05 & slope > -0.1))
air_id = "air_ba937bf13d40fb24"
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
  rownames_to_column()
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)
arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)
arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))
arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
  labs(x = "Time [weeks]", y = "log1p visitors vs auto.arima predictions")
plot_auto_arima_air_id <- function(air_id){

  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)

  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          stepwise = FALSE, approximation = FALSE)

  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))

  arima_visits %>%
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
    labs(x = "Time [weeks]", y = "log1p visitors vs forecast")
}
p1 <- plot_auto_arima_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_auto_arima_air_id("air_8e4360a64dbd4c50")
p3 <- plot_auto_arima_air_id("air_1c0b150f9e696a5f")
p4 <- plot_auto_arima_air_id("air_900d755ebd2f7bbd")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
air_id = "air_ba937bf13d40fb24"

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)
proph <- prophet(visits_train, changepoint.prior.scale=0.5, yearly.seasonality=FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast <- predict(proph, future)
plot(proph, fcast)
prophet_plot_components(proph, fcast)
fcast %>%
  as.tibble() %>%
  mutate(ds = date(ds)) %>%
  ggplot(aes(ds, yhat)) + 
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
  geom_line(colour = "blue") +
  geom_line(data = visits_train, aes(ds, y), colour = "black") +
  geom_line(data = visits_valid, aes(ds, y), colour = "grey50")
plot_prophet_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
          ds = visit_date)

  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  proph <- prophet(visits_train, changepoint.prior.scale=0.5, yearly.seasonality=FALSE)
  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)
  
  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c("Prophet for ", air_id))
  
  return(p)
}  
p1 <- plot_prophet_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_prophet_air_id("air_8e4360a64dbd4c50")
p3 <- plot_prophet_air_id("air_1c0b150f9e696a5f")
p4 <- plot_prophet_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
plot_prophet_air_id_holiday <- function(air_id, use_hday){
  
  air_visits_cut <- air_visits %>%
    filter(visit_date <= ymd("20160531"))
  
  hday <- holidays %>%
    filter(holiday_flg == TRUE) %>%
    mutate(holiday = "holiday") %>%
    select(ds = date, holiday)
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits_cut$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits_cut$visit_date), max(air_visits_cut$visit_date), 1))

  foo <- air_visits_cut %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
          ds = visit_date)

  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  if (use_hday == TRUE){
    proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,
                     yearly.seasonality=FALSE,
                     holidays = hday)
    ptitle = "Prophet (w/ holidays) for "
  } else {
     proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,
                     yearly.seasonality=FALSE)
    ptitle = "Prophet for "
  }
  
  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)
  
  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c(ptitle, air_id))
  
  return(p)
}  
p1 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", TRUE)
p2 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", FALSE)

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)
plot_hw_air_id <- function(air_id){

  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()

  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)

  hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))

  hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>%
    as.tibble() %>%
    bind_cols(visits_valid)

  visits_train %>%
    ggplot(aes(visit_date, visitors)) +
    geom_line() +
    geom_ribbon(data = hw_visits, aes(x = visit_date, ymin = lwr, ymax = upr), fill = "light blue") +
    geom_line(data = hw_visits, aes(visit_date, visitors), color = "grey60") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    labs(x = "Time [weeks]", y = "log1p visitors vs predictions") +
    ggtitle("HoltWinters")
}
plot_hw_air_id("air_ba937bf13d40fb24")
p1 <- plot_hw_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_hw_air_id("air_8e4360a64dbd4c50")
p3 <- plot_hw_air_id("air_1c0b150f9e696a5f")
p4 <- plot_hw_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
air_id = "air_ba937bf13d40fb24"

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)
visits_train_aug <- visits_train %>%
  tk_augment_timeseries_signature()

visits_valid_aug <- visits_valid %>%
  .$ds %>%
  tk_get_timeseries_signature()

glimpse(visits_train_aug)
fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))
pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))

pred_tk <- tibble(
    date  = visits_valid$ds,
    value = pred
    )
plot_tk_lm_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
          ds = visit_date)

  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  # augment train with ts info
  visits_train_aug <- visits_train %>%
    tk_augment_timeseries_signature()
  
  # fit lm
  fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))
  
  # augment valid with ts info
  visits_valid_aug <- visits_valid %>%
    .$ds %>%
    tk_get_timeseries_signature()
  
  # predict from lm
  pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))

  pred_tk <- tibble(
      date  = visits_valid$ds,
      y_pred = pred
      )
  
  # plot
  p <- pred_tk %>%
    ggplot(aes(date, y_pred)) +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    geom_line(colour = "blue") +
    labs(title = str_c("timetk for ", air_id))
    
  return(p)
}  
plot_tk_lm_air_id("air_ba937bf13d40fb24")
weather_stations <- as.tibble(fread(str_c(wpath,"weather_stations.csv")))
summary(weather_stations)
glimpse(weather_stations)
weather_stations %>%
  filter(date_terminated != "") %>%
  mutate(date_terminated = ymd(date_terminated)) %>%
  ggplot(aes(date_terminated)) +
  geom_histogram(bins = 30, fill = "blue") +
  ggtitle("Weather station termination date")
foobar <- weather_stations %>%
  filter(date_terminated != "")

foo <- air_store %>% select(air_store_id, latitude, longitude) %>% mutate(dset = "air")
bar <- hpg_store %>% select(hpg_store_id, latitude, longitude) %>% mutate(dset = "hpg")

leaflet(foobar) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude, popup = ~id, label = ~id, group = "Weather") %>%
  addCircleMarkers(~foo$longitude, ~foo$latitude, group = "AIR",
                   color = "purple", fillOpacity = 0.5, label = ~foo$air_store_id) %>%
  addCircleMarkers(lng = bar$longitude, lat = bar$latitude, group = "HPG",
                   color = "red", fillOpacity = 0.5, label = ~bar$hpg_store_id)
weather_data <- as.tibble(fread(str_c(wdpath,"tokyo__tokyo-kana__tonokyo.csv")))
summary(weather_data)
glimpse(weather_data)
weather_data <- weather_data %>%
  mutate(date = ymd(calendar_date),
         wday = wday(date, label = TRUE, abbr = TRUE),
         month = month(date, label = TRUE, abbr = TRUE),
         week = week(date)) %>%
  select(-calendar_date)

#library('viridis')
p1 <- weather_data %>%
  ggplot(aes(x = high_temperature, y = fct_rev(month), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1., bandwidth = 1.4) +
  scale_fill_viridis(name = "T_max [°C]", option = "C") +
  ggtitle("Maximum temperature at station \ntokyo tokyo-kana tonokyo in 2016/17") +
  labs(x = "High temperature", y = "") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank())

p2 <- weather_data %>%
  ggplot(aes(x = avg_humidity, y = fct_rev(month), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1., bandwidth = 4) +
  scale_fill_continuous(low = "white", high = "dark blue") +
  ggtitle("Average humidity at station \ntokyo tokyo-kana tonokyo in 2016/17") +
  labs(x = "Humidity", y = "", fill = "Humidity") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank())

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)
p1 <- weather_data %>%
  ggplot(aes(fct_rev(month), hours_sunlight, fill = fct_rev(month))) +
  geom_boxplot() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Month")

p2 <- weather_data %>%
  ggplot(aes(fct_rev(month), cloud_cover, fill = fct_rev(month))) +
  geom_boxplot() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Month")

p3 <- weather_data %>%
  ggplot(aes(fct_rev(month), precipitation, fill = fct_rev(month))) +
  geom_boxplot() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_y_log10() +
  labs(x = "Month")

p4 <- weather_data %>%
  ggplot(aes(fct_rev(month), avg_local_pressure, fill = fct_rev(month))) +
  geom_boxplot() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_y_log10() +
  labs(x = "Month")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
