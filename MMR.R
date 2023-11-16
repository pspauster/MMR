library(RSocrata)
library(tidyverse)
library(scales)

mmr <- read.socrata("https://data.cityofnewyork.us/resource/rbed-zzin.csv")

goals_list <- mmr %>% 
  arrange(desc(valuedate)) %>% 
  group_by(indicator) %>% 
  summarize_all(first)

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

DHS_goals <- filter(mmr, agency == "DHS")%>% 
  arrange(indicator, valuedate) %>% distinct()

DHSlist_goals <- DHS_goals %>% select(indicator, id, critical) %>% distinct

DHS_crit_goals <- filter(mmr, agency == "DHS", critical == "Yes") %>% 
  arrange(indicator, valuedate) %>% distinct()

critical_goals <- DHS_crit_goals %>% select(indicator, id) %>% unique()

plotvar("Average length of stay for families with children in shelter (days)")


