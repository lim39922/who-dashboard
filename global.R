library(plotly)
library(shiny)
library(dplyr)
library(shinydashboard)
library(leaflet)

library(googleVis)
library(DT)
library(shinydashboard)
library(threejs)
library(maps)
library(rgdal)
library(wordcloud)
library(maptools)

library(graphics)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

#setwd("~/Documents/_Master/workspace/shiny/NGO-dashbo")

#Maternal <- read.csv(file = "maternal_mor_vs_basic_drinking_water/mortality.csv", header = TRUE, na.strings=c("","NA"), stringsAsFactors = FALSE  )

mydatag <- read.csv(file="data/Full-location.csv",header=TRUE, na.strings=c("","NA"), stringsAsFactors = FALSE)
mydatag <- mydatag[2:12]

#data_2007 <-Maternal
#data_2007['clean_water_ratio']<-round((data_2007$Basic.drinking.service.....Rural+ data_2007$Basic.drinking.service.....Urban)/2,0)
#data_2007['clean_water_ratio']<-as.integer(data_2007$clean_water_ratio)
#data_2007 <- data_2007[order(data_2007$Region,data_2007$Country),]
#data_2007$size <- data_2007$Number.of.maternal.deaths
data_2007 <-mydatag
data_2007['clean_water_ratio']<-round((data_2007$BDSR+ data_2007$BDSU)/2,0)
data_2007['clean_water_ratio']<-as.integer(data_2007$clean_water_ratio)
data_2007 <- data_2007[order(data_2007$Region,data_2007$Country),]
data_2007$size <- data_2007$Death
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951','#FFFF00')

#bdrink_avg <-round(mean(data_2007$clean_water_ratio),1)
#bdrink_avgUrb <-round(mean(data_2007$Basic.drinking.service.....Urban),1)
#bdrink_avgRur <-round(mean(data_2007$Basic.drinking.service.....Rural),1)
bdrink_avg <-round(mean(data_2007$clean_water_ratio),1)
bdrink_avgUrb <-round(mean(data_2007$BDSU),1)
bdrink_avgRur <-round(mean(data_2007$BDSR),1)

#-----

world_ogr <- readOGR("data/countries/ne_50m_admin_0_countries",
          'ne_50m_admin_0_countries',
          encoding = 'UTF-8')

# load mortality data frame
maternal_df = read.csv("data/maternal_clean.csv", header = T)


# used for the items of drop down widget in ui
world_region_list = c(
  "Entire world",
  "Africa",
  "Americas",
  "Eastern Mediterranean",
  "Europe",
  "South-East Asia",
  "Western Pacific"
)

world_area_list = c(
  "Africa",
  "Americas",
 "Eastern Mediterranean",
  "Europe",
 "South-East Asia",
  "Western Pacific"
)

# ------------------------------------------------------------------------------
get_start_and_end_index_of_area = function(area_name) {
  # the number pairs correspond the row index of countries in each area in data frame 'mortality_df'
  start_and_end_index_of_areas = switch(
    area_name,
    "World level overview" = c(1,164),
    "Africa" = c(1,41),
    "Americas" = c(42,70),
    "Eastern Mediterranean" = c(71,89),
    "Europe" = c(90,135),
    "South-East Asia" = c(136,145),
    "Western Pacific" = c(146,164)
    
  )
  return(start_and_end_index_of_areas)
}

# ------------------------------------------------------------------------------
# create vectors of country/place names, used for global sphere view in ui.R
Africa <- c(
  "Algeria",
  "Angola",
  "Benin",
  "Botswana",
  "Burkina Faso",
  "Burundi",
  "Cameroon",
  "Central African Republic",
  "Chad",
  "Comoros",
  "Congo",
  "CÌ«te d'Ivoire",
  "Equatorial Guinea",
  "Eritrea",
  "Ethiopia",
  "Gabon",
  "Gambia",
  "Ghana",
  "Guinea",
  "Guinea-Bissau",
  "Kenya",
  "Lesotho",
  "Liberia",
  "Madagascar",
  "Malawi",
  "Mali",
  "Mauritania",
  "Mauritius",
  "Mozambique",
  "Namibia",
  "Niger",
  "Nigeria",
  "Rwanda",
  "Sao Tome and Principe",
  "Senegal",
  "Sierra Leone",
  "South Africa",
  "Togo",
  "Uganda",
  "Zambia",
  "Zimbabwe"
)

Americas <- c(
  "Argentina",
  "Bahamas",
  "Barbados",
  "Belize",
  "Brazil",
  "Canada",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Cuba",
  "Dominican Republic",
  "Ecuador",
  "El Salvador",
  "Grenada",
  "Guatemala",
  "Guyana",
  "Haiti",
  "Honduras",
  "Jamaica",
  "Mexico",
  "Nicaragua",
  "Panama",
  "Paraguay",
  "Peru",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Suriname",
  "Trinidad and Tobago",
  "Uruguay"
)

Eastern_Mediterranean <- c(
  'Afghanistan',
  'Bahrain',
  'Djibouti',
  'Egypt',
  'Iraq',
  'Jordan',
  'Kuwait',
  'Lebanon',
  'Morocco',
  'Oman',
  'Pakistan',
  'Qatar',
  'Saudi Arabia',
  'Somalia',
  'Sudan',
  'Syrian Arab Republic',
  'Tunisia',
  'United Arab Emirates',
  'Yemen'
)

Europe <- c(
  'Albania',
  'Armenia',
  'Austria',
  'Azerbaijan',
  'Belarus',
  'Belgium',
  'Bosnia and Herzegovina',
  'Bulgaria',
  'Croatia',
  'Cyprus',
  'Denmark',
  'Estonia',
  'Finland',
  'France',
  'Georgia',
  'Germany',
  'Greece',
  'Hungary',
  'Iceland',
  'Ireland',
  'Israel',
  'Italy',
  'Kazakhstan',
  'Kyrgyzstan',
  'Latvia',
  'Lithuania',
  'Luxembourg',
  'Malta',
  'Montenegro',
  'Netherlands',
  'Norway',
  'Poland',
  'Portugal',
  'Romania',
  'Russian Federation',
  'Serbia',
  'Slovakia',
  'Slovenia',
  'Spain',
  'Sweden',
  'Switzerland',
  'Tajikistan',
  'Turkey',
  'Turkmenistan',
  'Ukraine',
  'Uzbekistan'
)

South_East_Asia <-c(
  
  'Bangladesh',
  'Bhutan',
  'India',
  'Indonesia',
  'Maldives',
  'Myanmar',
  'Nepal',
  'Sri Lanka',
  'Thailand',
  'Timor-Leste'
)

Western_Pacific <- c(
  'Australia',
  'Brunei Darussalam',
  'Cambodia',
  'China',
  'Fiji',
  'Japan',
  'Kiribati',
  'Lao Peoples Democratic Republic',
  'Malaysia',
  'Mongolia',
  'New Zealand',
  'Papua New Guinea',
  'Philippines',
  'Samoa',
  'Singapore',
  'Solomon Islands',
  'Tonga',
  'Vanuatu',
  'Viet Nam'
)



all_country_names = c(Africa,Americas,Eastern_Mediterranean,Europe,Western_Pacific,South_East_Asia)

# create another data frame in which each row corresponds to a country
mortality_of_countries_df <- maternal_df %>%
  filter(Country %in% all_country_names) 



# ------------------------------------------------------------------------------
get_country_names_by_area = function(area) {
  names = switch(
    area,
    "Entire world" = all_country_names,
    "Africa" = Africa,
    "Americas" = Americas,
    "Eastern Mediterranean" = Eastern_Mediterranean,
    "Europe" = Europe,
    "Western Pacific" = Western_Pacific,
    "South-East Asia" = South_East_Asia
  )
  return(names)
}


country_locations <- read.csv("data/country_location.csv")

#country_locations <-  maternal_df %>% select(c("Country","Lat","Long")) 
#names(country_locations)  <- c("country_names", "lon", "lat")

#############
# convert decimal number to a certain precision. eg: 12.34567 --> 12.34
specify_decimal <- function(x, k) {
  format(round(x, k), nsmall = k)
}

get_mortality_and_ratio_of_country_in_a_year = function(country_name, year) {
  p = mortality_of_countries_df %>% select(Country, ends_with(as.character(year)))
  # the first column of p is country names, the second column is mortality number
  
  total_mortality_all_countries = sum(p[, 2])
  country_mortality_in_one_year = p %>% filter(Place == country_name)
  
  mortality = country_mortality_in_one_year[2]
  ratio =  mortality / total_mortality_all_countries
  ratio = specify_decimal(ratio, 4)
  
  return(c(mortality, ratio))
}

draw_plotly_mortality_by_continent = function(start_row_index, end_row_index) {
  world_p = maternal_df[start_row_index:end_row_index,]
  place_column_index = 1
  mortality_number_column_index_range = 2:4
  # we only consider all the years between 1950 and 2015
  years = c("1990","2000","2015")
  
  # draw first plotly curve. all the rest curves are drawn using "add_trace"
  xformat = list(title = "Years")
  yformat = list(title = "Maternal Mortality Ratio (per 100 000 live births)")
  pn = world_p[1, place_column_index]
  p = plot_ly(
    x = years,
    y = as.numeric(world_p[1, mortality_number_column_index_range]),
    mode = "lines",
    name = pn
  )
  
  # draw the rest plotly curves
  for (i in 2:nrow(world_p)) {
    place_name = world_p[i, place_column_index]
    p = p %>% add_trace(
      x = years,
      y = as.numeric(world_p[i, mortality_number_column_index_range]),
      mode = "lines",
      evaluate = T,
      name = place_name
    ) %>%
      layout(xaxis = xformat, yaxis = yformat)
  }
  p
}
