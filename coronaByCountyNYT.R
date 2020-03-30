# New York Times Dataset

library(tidyverse)

url<-"https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
download.file(url = url, destfile = "counties.csv", method = "curl")
counties<-read_csv("./counties.csv")

library(plotly)
head(counties)

df<-counties %>% 
  mutate(re = stringr::str_c(stringr::str_to_lower(county),
                             " ", 
                             stringr::str_to_lower(state)))
  

county_df<-map_data("county") %>%
  mutate(re = stringr::str_c(stringr::str_to_lower(subregion),
                             " ", 
                             stringr::str_to_lower(region)))

corona_df<-inner_join(df, county_df, by = "re") %>%
  select(cases, deaths, lat, long, county, state, fips)

library(rjson)
url = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

fig <- plot_ly()

fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=counties,
  locations=corona_df$fips,
  z=corona_df$cases,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
)

fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= -95.71, lat=37.09))
)

fig




