---
categories:
- ""
- ""
date: "2020-10-16"
description: My first proper work
draft: false
image: London bikes.jpg
keywords: ""
slug: blog9
title: Excess rentals in TfL bikes sharing
---

This is the first actual work that I have conducted by myself with R, and it is about the analysis of monthly TfL bikes rental data and make evaluation over how the COVID-19 pandemic has affected the traveling habit of London people. Dr.Kostis has given the format of two relative graphs and it was my work to reproduce it.

**First, load the data**

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))
```

**The 2 graphs to be reproduced**

```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_monthly.png"), error = FALSE)
```

```{r tfl_percent_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
```

**And my work!**

**First, filter out years before 2015**

bike_after2015 <- bike %>% 
 filter(year >= 2015)


**Summarize per month data**

bike_after2015_month <- bike_after2015 %>%
 group_by(year, month) + 
 summarise(bikes_hired = mean(bikes_hired))


**Calculate monthly average throughout the year and save it in a new column called avg_hire**

bike_after2015_month <- bike_after2015_month %>% 
  group_by(month) %>% 
  mutate(average_hire = mean(bikes_hired)) %>% 
  ungroup()


**Calculate deviations from monthly average**

bike_after2015_month <- bike_after2015_month  %>% 
  mutate(change_monthlyavg = bikes_hired - average_hire)


***we now have the data of how the bikes_hired every month deviates from the monthly average, saying the blue line in the first graph, and we need to use geom_ribbon() to present the data*** 

**Interpolate a dataframe to fix graph intersections**

bike_interp <- bike_after2015_month  %>% 
 split(.$year) %>% 
 map_df(~data.frame(year = approx(.x$month, .x$bikes_hired, n = 90), nat = approx(.x$month, .x$average_hire, n = 90), year = .x$year[1], month = .x$month[1]))


**Create a vector of month names**

month_label <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


**Plot the graph based on data above**

month_dev <- ggplot(bike_interp, aes(x = nat.x,y= nat.y)) + 
  geom_line(color = "blue", size = 3) +
  geom_line(aes(nat.x, year.y), color = "black") +
  facet_wrap(~year)
  geom_ribbon(aes(ymin = nat.y, ymax = pmin(year.y, nat.y)), fill = "#EAB5B7" , alpha = 0.5) +
  geom_ribbon(aes(ymin = year.y, ymax = pmin(year.y, nat.y)), fill = "#CBECCE", alpha = 0.5) +
  scale_x_continuous(breaks= c(1,2,3,4,5,6,7,8,9,10,11,12), labels= month_label) +
  labs(title = "Monthly changes in TfL bike rentals",subtitle = "Change from monthly average shown in blue and calculated between 2015-2019", caption = "Source: TfL, London Data Store", y = "Bike rentals", x = "")


**Save plot as a picture**

ggsave("month_dev.jpg", month_dev, width = 15,height = 8)


**Place picture in code**

knitr::include_graphics(here::here("Session 2/session4-workshop2","month_dev.jpg"))

***For the second graph, we conduct the following steps:***

**Filter out data before 2015**

bike_after2015 <- bike %>% 
 filter(year >= 2015)


**Then summarizing per week data**

bike_after2015_wk <- bike_after2015 %>%
 group_by(year,week) %>%
 summarise(bikes_hired = mean(bikes_hired))


**Calculate weekly average throughout the year and mutate a new column called avg_hire_wk**

bike_after2015_wk <- bike_after2015_wk %>%
 group_by(week) %>%
 mutate(weekly_average = mean(bikes_hired)) %>%
 ungroup()


**Calculate the percentage change from weekly average, mutate "tags" to add different colors depending on whether the change is positive or negative as compared to expected level, and make "wk_background_color" to add color to the background**

bike_after2015_wk <- bike_after2015_wk %>%
 mutate(weekly_changeper = (bikes_hired - weekly_average)/weekly_average)%>%
 mutate(tags = ifelse(weekly_changeper>=0, "Above", "Below")) %>% 
 mutate(wk_background_color = if_else(week <= 13 | week >= 26 & week <=39, "white", "grey"))


**Plot the graph based on data above**

week_per_change <- ggplot(data = bike_after2015_wk, aes(x = week, y = weekly_changeper)) +  
 geom_line()+
 geom_ribbon(aes(ymin = 0, ymax = pmin(0, weekly_changeper), fill = "Above"), alpha = 0.5) + 
 geom_ribbon(aes(ymin = weekly_changeper, ymax = pmin(0, weekly_changeper), fill = "Below"), alpha = 0.5)+
 facet_wrap(~year)+
 theme(strip.background = element_rect(color="black", fill="blue"))+
 geom_tile(aes(fill = wk_background_color),
            width = 1, height = Inf, alpha = 0.3)+ 
  scale_fill_manual(values = c("red","green","grey","white"))+
  
  
**Add the rugs to match the weekly change**

  geom_rug(aes(color = tags),sides="b",alpha = 0.5) +
  theme_bw()+
  scale_x_continuous(breaks=seq(13, 53, 13))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.6,0.6)) + 
  theme(legend.position = "none") + 
  theme(axis.ticks = element_blank())+
  theme(panel.border = element_blank())+
  labs(x = "Week", y = "", title = "Weekly changes in TfL bike rentals", subtitle = "% change from weekly averages calculated between 2015-2019", caption = "Source: TfL, London Data Store")+
 coord_fixed(ratio = 25)


**Save the picture**

ggsave("weekly change.jpg",plot=week_per_change, width = 20,height = 10)

**Place the picture in code**

knitr::include_graphics(here::here("Session 2/session4-workshop2","weekly change.jpg"))

