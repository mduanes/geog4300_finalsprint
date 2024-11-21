library(tidyverse)

files<-list.files("data",recursive = TRUE,full.names = TRUE,pattern="puf")
pulse<-map_df(files,read_csv)

# define lowinc calues
lowinc <- c(1)
# filter
pulse_lowinc <- pulse %>%
  select(EST_ST,INCOME,PWEIGHT) %>%
  filter(INCOME > 0) %>%
  mutate("inccat"=case_when(INCOME %in% lowinc ~ "Low Income",
                            TRUE ~ "Not Low Income")) %>%
  group_by(EST_ST) %>%
  mutate("total"=sum(PWEIGHT)) %>%
  ungroup() %>%
  mutate("low_inc_pct"=PWEIGHT/total) %>%
  group_by(EST_ST,inccat) %>%
  summarize(low_inc_pct=sum(low_inc_pct)) %>%
  ungroup() %>%
  filter(inccat=="Low Income")

pulse_change <- pulse %>%
  select(EST_ST,PRICECHNG,PWEIGHT) %>%
  filter(PRICECHNG > 0) %>%
  mutate("chngcat"=case_when(PRICECHNG == 1 ~ "Price Increase",
                            TRUE ~ "Other")) %>%
  group_by(EST_ST) %>%
  mutate("total"=sum(PWEIGHT)) %>%
  ungroup() %>%
  mutate("increase_pct"=PWEIGHT/total) %>%
  group_by(EST_ST,chngcat) %>%
  summarize(increase_pct=sum(increase_pct)) %>%
  ungroup() %>%
  filter(chngcat=="Price Increase")

pulse_stress <- pulse %>%
  select(EST_ST,PRICESTRESS,PWEIGHT) %>%
  filter(PRICESTRESS > 0) %>%
  mutate("stresscat"=case_when(PRICESTRESS == 1 ~ "Very Stressful",
                             TRUE ~ "Other")) %>%
  group_by(EST_ST) %>%
  mutate("total"=sum(PWEIGHT)) %>%
  ungroup() %>%
  mutate("stress_pct"=PWEIGHT/total) %>%
  group_by(EST_ST,stresscat) %>%
  summarize(stress_pct=sum(stress_pct)) %>%
  ungroup() %>%
  filter(stresscat=="Very Stressful")

combined <- pulse_lowinc %>%
  left_join(pulse_change) %>%
  left_join(pulse_stress)

summary(lm(combined$increase_pct ~ combined$low_inc_pct))

ggplot(combined,aes(x=low_inc_pct,y=increase_pct)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  labs(x="Low Income",y="Felt Increase In Prices") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)

summary(lm(combined$stress_pct ~ combined$low_inc_pct))

ggplot(combined,aes(x=low_inc_pct,y=stress_pct)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  labs(x="Low Income",y="Very Stressed by Rising Prices") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)

library(tmap)
library(fiftystater)
library(mapproj)

# states map
states <- tigris::states() %>%
  mutate("STATEFP_NUM"=as.numeric(STATEFP))

states %>%
  left_join(pulse_lowinc,by=c("STATEFP"="EST_ST")) %>%
  mutate(NAME=tolower(NAME)) %>%
  ggplot(aes(map_id = NAME,fill=low_inc_pct)) +
    geom_map(map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    theme_minimal() +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    scale_fill_gradient(high="darkred",low="white",name="Percent Low Income") +
  fifty_states_inset_boxes()
