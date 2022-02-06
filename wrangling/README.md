# Tutorial Content
You can find the code for each episode after the videos have been posted here: https://www.youtube.com/joelalcedo

Tutorial 1: Data Wrangling

```
library(tidyverse)
library(Quandl)

#Apple Stock Data
Quandl('WIKI/AAPL') #quandl's function to pull in data

#VWAP for apple start from 2015 to now

aapl <- Quandl('WIKI/AAPL')

vwap <- aapl %>% #then
  # ggplot()+
  # geom_line(aes(x = Date,
  #               y = Close))
  select(Date, 'Adj. Close', 'Adj. Volume') %>% #selects columns
  rename(Close = 'Adj. Close', Volume = 'Adj. Volume') %>% #rename columns
  filter(Date >= '2015-01-01') %>% #subset your rows
  #summary() #summary statistics
  mutate(Volume = Volume/sum(Volume)) %>% #modify existing columns, or create new ones
  mutate(CloseShare = Close * Volume) %>%
  summarize(VWAP = round(sum(CloseShare), 2)) #collapses and summarises the data

vwap
```
