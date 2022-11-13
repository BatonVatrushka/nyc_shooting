# 
# New York State data came from here:
# https://data.ny.gov/Public-Safety/Index-Violent-Property-and-Firearm-Rates-By-county/34dd-6g2j

# NYPD Shooting Data
# https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8

# 1%
# https://www.usconcealedcarry.com/resources/ccw_reciprocity_map/ny-gun-laws/
library(pacman)
p_load(tidyverse
       , lubridate
       , rgdal
       , tmap
       , sf
       , sp
       , directlabels
       , RSocrata)

# ====================
# NEW YORK CITY
# ====================
nyc <- read.socrata("https://data.cityofnewyork.us/resource/833y-fsy8.csv")

# add year column
nyc <- nyc %>% 
  mutate(year = year(occur_date))

# join the data
#munic_join <- inner_join(municipalities, )

# count shootings by boro and visualize
nyc %>% group_by(boro) %>% 
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(fct_reorder(boro, n), n)) +
  geom_col(alpha=0.85) +
  labs(x = element_blank()
       ,  y = element_blank()) +
  theme(axis.title = element_blank()
        , legend.position = "none"
        , axis.ticks = element_blank()
        , panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25))) +
  coord_flip() 

# count shootings by precinct
precinct <- nyc %>% group_by(precinct, boro) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)*100) 
# visualize
precinct %>% ggplot(aes(prop)) +
  geom_histogram(color = 'black', bins = 10)

# victim data frame
victims <- nyc %>% 
  group_by(vic_race
           , vic_sex
           , vic_age_group
           , year) %>% 
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(prop = (n/sum(n))*100) %>%
  arrange(desc(prop))

# vic_race barplot
victims %>%  ggplot(aes(fct_reorder(vic_race, n), n)) +
  geom_col(alpha=0.85) +
  labs(x = element_blank()
       ,  y = element_blank()) +
  theme(axis.title = element_blank()
        , legend.position = "none"
        , axis.ticks = element_blank()
        , panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25))) + 
  coord_flip()

# scatter plot
victims %>%
  ggplot(aes(year, n, size = prop, color = vic_race)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.15)) +
  scale_color_discrete() +
  theme(panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25))
        , legend.position = "bottom")

# # perp race
# nyc %>% group_by(perp_race) %>% 
#   summarise(n=n()) %>%
#   drop_na() %>%
#   ggplot(aes(fct_reorder(perp_race, n), n)) +
#   geom_col(alpha=0.85) +
#   labs(x = element_blank()
#        ,  y = element_blank()) +
#   theme(axis.title = element_blank()
#         , legend.position = "none"
#         , axis.ticks = element_blank()
#         , panel.background = element_rect(fill = "white")
#         , panel.grid = element_line(colour = alpha("grey", 0.25))) +
#   coord_flip()

# ==============
# year over year
# ==============
nyc %>% group_by(year) %>% 
  summarise(n=n()) %>%
  ggplot(aes(year, n)) +
  geom_line(lwd = 1) +
  scale_x_continuous(breaks = unique(nyc$year)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(c(0, 2100)) +
  theme(axis.ticks = element_blank()
        , panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25))) +
  ggtitle("Number of Firearm Incidents in NYC Year Over Year")
# ====================
# NEW YORK STATE
# ====================
nys <- read.socrata("https://data.ny.gov/resource/34dd-6g2j.json")
colnames(nys) <- colnames(nys) %>% make.names()
nys <- nys %>% mutate(firearm_count = replace_na(as.numeric(firearm_count),0))

# get the FIPS
municipalities <- read_csv('NY_Municipalities_and_county_FIPS_codes.csv')
colnames(municipalities) <- colnames(municipalities) %>% make.names()
municipalities <- municipalities %>% select(county.Name, county.FIPS) %>% distinct()

# compare county names
cbind(unique(nys$county) %>% sort()
      , unique(municipalities$county.Name) %>% sort())

# add FIPS to state data
# create grouped dataset
state_county <- nys %>% group_by(county) %>% 
  summarise(pop = median(as.numeric(population))
    , firearm_count = median(as.numeric(firearm_count))
    , rate = firearm_count/pop * 10000) %>%
  arrange(rate %>% desc()) %>% ungroup() %>%
  mutate(pop = round(pop, 0))

# join the data
state_county <- inner_join(state_county, municipalities
                          , by = c("county" = "County.Name"))

# hist of firearm rate
nys %>% ggplot(aes(as.numeric(firearm_rate))) +
  geom_histogram(color = 'black', bins = 63) +
  theme(axis.title = element_blank()) +
  ggtitle(label = "Distribution of Firearm Incidents Per-Capita in NYS (Aggregated by County)"
          , subtitle = "Source: New York State Open Data")

# state_county rate hist
state_county %>% 
  ggplot(aes(rate)) +
  geom_density(fill = 'orange', color = 'orange')

# column graph of the 
state_county %>% group_by(county) %>% 
  summarise(firearm_rate = median(rate)) %>%
  arrange(firearm_rate %>% desc()) %>%
  top_n(n = 10) %>%
  ggplot(aes(reorder(county, firearm_rate), firearm_rate)) + 
  geom_col() + 
  coord_flip() + 
  theme(axis.title = element_blank())


# per capita rate over year
nys %>%
  filter(county %in% c("Kings", "Bronx", "New York", "Erie"
                       , "Monroe", "Queens")) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(county, year) %>%
  summarise(sum_count = sum(firearm_count)) %>%
  ungroup() %>%
  ggplot(aes(year, sum_count, color = county)) +
  geom_line() +
  theme(legend.position = 'blank'
        , axis.title.y = element_blank())

# same with nyc data
nyc %>% group_by(boro, year) %>%
  summarise(n=n()) %>%
  ggplot(aes(year, n, color = boro)) +
  geom_line()


# maps
# ==========================================================================
# STATE MAP
# ==========================================================================
nys_shp <- readOGR('county/tl_2016_36_cousub.shp')

# create the FIP field in the shape file
nys_shp@data <- nys_shp@data %>% 
  mutate('FIP' = str_c(nys_shp@data$STATEFP, nys_shp@data$COUNTYFP, sep = ''))

# check if the field is there then sp::merge
nys_shp@data %>% glimpse()
nys_shp_merged <- sp::merge(nys_shp, state_county
                            , by.x = 'FIP', by.y = 'County.FIPS'
                            , duplicateGeoms = T)
# MAP
tm_shape(nys_shp_merged) + 
  tm_borders() +
  tm_fill(col = 'rate')

# ==========================================================================
# CITY MAP
# ==========================================================================
# read in the shape file
nyc_shp <- readOGR('nyc/nyu_2451_34490/nyu_2451_34490.shp')
shooting_shp <- readOGR("shooting_shape/geo_export_0d2b1cf9-afdd-4d0b-b3b0-5c39472338e9.shp")
# look at the data
shooting_shp@data %>% glimpse()

# nyc map
tm_shape(nyc_shp) +
  tm_borders() +
tm_shape(shooting_shp) +
  tm_dots(col = 'red', alpha = 0.1)

# precinct shape
prec_shp <- readOGR('sectors/geo_export_1b885b08-e529-482e-b8df-fd64cc1d9461.shp')
prec_shp@data %>% glimpse()

# prepare the precinct df for merging w/ the shape file
precinct <- precinct %>% 
  mutate(precinct = as.character(precinct)) %>%
  # pct in the shape file has three characters each
  mutate(precinct = case_when(
    str_length(precinct) < 2 ~ str_c("00", precinct)
    , str_length(precinct) < 3 ~ str_c("0", precinct)
    , TRUE ~ precinct
  ))

# merge
prec_merged <- sp::merge(prec_shp, precinct
                         , by.x = "pct", by.y = "precinct"
                         , duplicateGeoms = F)

# precinct map
tm_shape(prec_merged) +
  tm_borders() +
  tm_fill(col = 'prop')
# ==========================================================================
# ERIE 
# ==========================================================================
# data came from NYS Open Data
crime_reports <- read.socrata("https://data.ny.gov/resource/ca8h-8gjq.json")
erie <- crime_reports %>% filter(county == "Erie")

erie <- erie %>%
  filter(agency != "County Total") %>%
  drop_na() %>%
  group_by(agency) %>% 
  summarise(total_crimes = sum(as.numeric(total_index_crimes))) %>%
  ungroup() %>%
  mutate(prop = total_crimes / sum(total_crimes)) %>% 
  arrange(desc(total_crimes))

erie %>% ggplot(aes(fct_reorder(agency, prop), prop, size = prop)) +
  geom_point(color = 'red', alpha = 0.75) +
  coord_flip() +
  theme(axis.title = element_blank()
        , legend.position = "none"
        , axis.ticks = element_blank()
        , panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25)))



