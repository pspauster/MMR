library(RSocrata)
library(tidyverse)
library(scales)

mmr <- read.socrata("https://data.cityofnewyork.us/resource/rbed-zzin.csv")

housing_agencies <- mmr %>% 
  filter(agency %in% c("DCP", "HPD", "NYCHA", "DHS")) %>% 
  arrange(id, desc(valuedate))

transpo_agencies <- mmr %>% 
  filter(agency %in% c("DOT")) %>% 
  arrange(id, desc(valuedate))

plotvar <- function(value) {
  title = str_replace_all(value, "%|/", "percent")
  
  critical = housing_agencies %>%
    filter(indicator == value) %>% 
    pull(critical) %>% 
    .[1]
  
  direction = housing_agencies %>%
    filter(indicator == value) %>% 
    pull(desireddirection) %>% 
    .[1]
  
  agency = housing_agencies %>%
    filter(indicator == value) %>% 
    pull(agency) %>% 
    .[1]

  housing_agencies %>%
  filter(indicator == value) %>% 
  mutate(date = as.Date(valuedate),
         ) %>% 
  ggplot() +
    geom_line(mapping = aes(x = date, y = acceptedvalue), color = "red")+
    geom_line(mapping = aes(x = date, y = targetmmr), color = "blue")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    labs(title = value,
         subtitle = paste0("critical = ", critical, " ... ", "desired direction = ", direction))
  
  ggsave(paste0("charts/", agency, "/", title,".png"), device = "png")
}

housing_goals <- housing_agencies %>% pull(indicator) %>% unique()

plotvar("Single adults entering the DHS shelter services system")

walk(housing_goals, plotvar)
