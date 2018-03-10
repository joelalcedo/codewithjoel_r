# Tutorial Content
You can find the code for each episode after the videos have been posted here: https://www.youtube.com/joelalcedo

Tutorial 2: Data Visualization

#Libraries:
  #  - tidyverse      (wrangling + plotting)
  #  - wbstats        (data)
  #  - data.table     (wrangling)
  #  - ggthemes       (plotting - predefined themes)
  #  - psych          (stats)
  #  - plotly         (plotting - interactive/animation)
  #  - highcharter    (plotting - interactive)
  #  - quantmod       (data + finance)
  #  - TTR            (finance - technical trading rules)

library(tidyverse)
library(wbstats) 
  # SP.POP.TOTL --> population
  # NY.GDP.PCAP.CD --> gdp per capita
  # SP.DYN.LE00.IN --> life expectancy at birth
library(data.table)
library(ggthemes)
library(plotly)
library(psych)
library(highcharter)
library(quantmod)
library(TTR)

#Pull in data
population <- wb('SP.POP.TOTL', country = 'countries_only') %>%
  mutate(date = as.numeric(date), value = round(value / 1000000, 2), indicator = 'Population') %>%
  select(-indicatorID)

gdp <- wb('NY.GDP.PCAP.CD', country = 'countries_only') %>% 
  mutate(date = as.numeric(date), value = round(value, 2), indicator = 'GDP per Capita') %>%
  select(-indicatorID)

lifeexpectancy <- wb('SP.DYN.LE00.IN', country = 'countries_only') %>%  
  mutate(date = as.numeric(date), value = round(value, 2), indicator = 'Life Expectancy') %>%
  select(-indicatorID)

df <- gdp %>%
  rbind(lifeexpectancy) %>%
  rbind(population) %>%
  data.table::dcast(... ~ indicator, value.var = 'value') %>%
  na.omit()

###########################
#~~~PART 1: LINE CHARTS~~~#
###########################
line_data <- gdp %>% filter(iso3c == 'USA')
line_data2 <- gdp %>% filter(iso3c == 'ABW')

ggplot() + 
  geom_line(data = line_data,
            aes(x = date,
                y = value),
            color = '#642b72',
            size = 1) + 
  
  # geom_line(data = line_data2,
  #           aes(x = date,
  #               y = value),
  #           color = 'red',
  #           size = 1) + 
  
  labs(title = 'GDP per Capita (Current USD)',
       x = 'Year',
       y = 'Current USD',
       subtitle = paste('From ', min(line_data$date), ' to ', max(line_data$date), sep = '')) + 
  ggthemes::theme_economist_white() 


###########################
#~~~~PART 2: HISTOGRAM~~~~#
###########################
plot <- ggplot() + 
  geom_histogram(data = lifeexpectancy,
                 aes(x = value),
                 bins = 25,
                 color = '#1a2e63',
                 fill = '#3d72c6') + 
  labs(title = 'Global distribution of life expectancy at birth',
       x = 'Life Expectancy (Age)',
       y = 'Frequency')
plot

psych::describe(lifeexpectancy$value)

###########################
#~~~PART 3: SCATTER PLOT~~#
###########################
ggplot(data = df %>% filter(date == 2015)) + 
  geom_point(aes(x = `GDP per Capita`,
                 y = `Life Expectancy`,
                 color = Population,
                 size = Population)) + 
  labs(title = 'Relationship between GDP per Capita and Life Expectancy') + 
  viridis::scale_color_viridis()


#plotly
p <- ggplot(data = df) + 
  geom_point(aes(x = `GDP per Capita`,
                 y = `Life Expectancy`,
                 color = Population,
                 frame = date,
                 text = paste('Country: ', country, '<br>',
                              'GDP per capita: ', `GDP per Capita`, '<br>',
                              'Life expectancy: ', `Life Expectancy`, '<br>',
                              'Population: ', Population, 'MM', sep = ''),
                 size = Population)) + 
  labs(title = 'Relationship between GDP per Capita and Life Expectancy') + 
  viridis::scale_color_viridis() + 
  theme(text = element_text(family = 'Arial'))

plotly::ggplotly(p, tooltip = 'text')


###########################
#~~~PART 4: CHLOROPLETH~~~#
###########################
borders <- list(color = toRGB('grey'), width = 0.5)

maptype <- list(
  showframe = FALSE,
  showcoastline = TRUE,
  projection = list(type = 'Mercator')
)

#Chloropleth --> make sure to store and run in new window!
p <- plot_geo(data = lifeexpectancy %>% filter(date == 2015)) %>%
  add_trace(z = ~value,
            color = ~value,
            colors = 'Greens',
            text = ~country,
            locations = ~iso3c,
            marker = list(line = borders)) %>%
  colorbar(title = 'Life expectancy') %>%
  layout(title = 'Global life expectancy in 2015', geo = maptype)


###########################
#~~~PART 5: OHLC STOCK~~~~#
###########################
quantmod::getSymbols('AMD')
highcharter::hchart(AMD)

p2 <- highchart(type = 'stock') %>%
  hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>%
  hc_add_series(AMD, name = 'AMD') %>%
  hc_add_series(AMD$AMD.Volume, name = 'Volume', yAxis = 1, color = '#e0a328', type = 'column') %>%
  hc_add_series(TTR::RSI(Cl(AMD)), name = 'RSI', yAxis = 2, color = '#000000') %>%
  hc_add_series(xts(rep(30, nrow(AMD)), index(AMD)), name = 'Oversold', yAxis = 2, color = 'green') %>%
  hc_add_series(xts(rep(70, nrow(AMD)), index(AMD)), name = 'Overbought', yAxis = 2, color = 'red')

