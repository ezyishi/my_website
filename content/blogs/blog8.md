---
categories:
- ""
- ""
date: "2020-10-16"
description: Who's this guy?
draft: false
image: DJIA.jpg
keywords: ""
slug: blog8
title: DJIA Data Analysis
---

#Declaration

The following work shown is my first time ever utilizing R studio for the data analysis purpose. For the tutorial purpose, **the majority of the following codes are Dr.Kostis Christodoulou's credits**, and my input was just to plot the density table as well as summarize and mutate some parts of the data. However, they are still remarkable enough to be put here in memory of my very first coding work!

#reading the dataset

```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv"))
```

#creating a table and barplot to show the number of firms each sectors have (my work!)

```{r companies_per_sector}

#table
by_sector <- nyse %>%
 group_by(sector) %>%
 summarize(count = n())

by_sector %>%
 arrange(desc(count))

#bar plot
ggplot(data = by_sector, mapping = aes(x = reorder(sector, count), y = count)) +
geom_col() +
coord_flip()+
labs(y= "# of companies", x = "Sector", title = "# of Companies per Sector")+
  NULL
```

#Dr.Kostis work
```{r, tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


table1 <- djia[[2]] %>%
  mutate(date_added = ymd(date_added),
         
    
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>%
  c("SPY")

```




```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) 
```

```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

```{r summarise_monthly_returns}

after_20170101 <- myStocks_returns_monthly %>%
    filter(date >= "2016-12-30")

after_20170101 %>%
  group_by(symbol) %>%
  summarise(Minimum_Perc = min(monthly_returns*100), Maximum_Perc = max(monthly_returns*100), Median_Perc = median(monthly_returns*100), Mean_Perc = mean(monthly_returns*100), Standard_Deviation_Perc = sd(monthly_returns*100))
```

#plot density plots for each of the stocks' monthly return (mine again!)

```{r density_monthly_returns}

# YOUR CODE GOES HERE
 ggplot(data = after_20170101, mapping = aes(x = monthly_returns)) + 
     geom_density() +
     facet_wrap(~ symbol)+
     labs(y= "Density", x = "Monthly returns", title = "Density of Monthly Returns by Company")+
  NULL


```