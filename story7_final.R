library(tidyverse)

setwd('~/Desktop/CUNY_MSDS/DATA608/Story7')

#read in state codes
state_codes <- read_csv('state_codes.csv', skip = 8)
state_codes <- state_codes %>% drop_na(`State code`)
state_codes <- state_codes %>% select_if(~ !any(is.na(.)))
colnames(state_codes) <- c('State_code','State_name')

#read in energy data
energy_data <- read_csv('state_energy_data.csv')
energy_data$Data_Status <- NULL
energy_data <- energy_data %>% rename_at('StateCode', ~'STUSPS')
energy_data <- energy_data |> 
  pivot_longer(cols = !c(STUSPS, MSN), names_to = 'Year', values_to = 'Value')

energy_data$Year <- as.numeric(energy_data$Year)
energy_data$Year <- strptime(energy_data$Year, format = "%Y")



#codes of interest for 2022
energy_cons_2022 <- energy_data |> filter(MSN == 'TETCB', year(Year)==2022)
energy_prod_2022 <- energy_data |> filter(MSN == 'TEPRB', year(Year)==2022)
energy_renewable_2022 <- energy_data |> filter(MSN == 'REPRB', year(Year)==2022)
energy_crudeoil_2022 <- energy_data |> filter(MSN == 'PAPRB', year(Year)==2022)
energy_ng_2022 <- energy_data |> filter(MSN == 'NGMPB', year(Year)==2022)
energy_coal_2022 <- energy_data |> filter(MSN == 'CLPRB', year(Year)==2022)

#renaming
energy_cons_2022 <- energy_cons_2022 %>% rename_at('Value', ~'Energy_Consumption')
energy_prod_2022 <- energy_prod_2022 %>% rename_at('Value', ~'Energy_Production')
energy_renewable_2022 <- energy_renewable_2022 %>% rename_at('Value', ~'Renewable_Energy_Production')
energy_crudeoil_2022 <- energy_crudeoil_2022 %>% rename_at('Value', ~'Crude_Oil_Production')
energy_ng_2022 <- energy_ng_2022 %>% rename_at('Value', ~'Natural_Gas_Production')
energy_coal_2022 <- energy_coal_2022 %>% rename_at('Value', ~'Coal_Production')


library(sf)

us_sf <- read_sf("cb_2018_us_state_5m.shp")

#joining production and consumption data
us_sf <- left_join(us_sf, energy_cons_2022, by = 'STUSPS') |> select(-MSN)
us_sf <- left_join(us_sf, energy_prod_2022, by = 'STUSPS') |> select(-MSN)
us_sf <- left_join(us_sf, energy_renewable_2022, by = 'STUSPS') |> select(-MSN)
us_sf <- left_join(us_sf, energy_crudeoil_2022, by = 'STUSPS') |> select(-MSN)
us_sf <- left_join(us_sf, energy_ng_2022, by = 'STUSPS') |> select(-MSN)
us_sf <- left_join(us_sf, energy_coal_2022, by = 'STUSPS') |> select(-MSN)

us_sf <- us_sf %>%
  select(-starts_with("Year"))

#calculating differences and percentages
us_sf$Production_minus_Consumption <- us_sf$Energy_Production - us_sf$Energy_Consumption
us_sf$Renewable_Production_Percent_of_Total <- us_sf$Renewable_Energy_Production/us_sf$Energy_Production
us_sf$Crude_Oil_Production_Percent_of_Total <- us_sf$Crude_Oil_Production/us_sf$Energy_Production
us_sf$Natural_Gas_Production_Percent_of_Total <- us_sf$Natural_Gas_Production/us_sf$Energy_Production
us_sf$Coal_Production_Percent_of_Total <- us_sf$Coal_Production/us_sf$Energy_Production
us_sf <- us_sf |>
  mutate(Other_Production_Percent_of_Total = 1 - (Renewable_Production_Percent_of_Total +
                                                                Crude_Oil_Production_Percent_of_Total +
                                                   Natural_Gas_Production_Percent_of_Total +
                                                   Coal_Production_Percent_of_Total))
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)

#color palettes
production_palette <- colorNumeric(
  palette = "Blues", domain = us_sf$Energy_Production,
  na.color = "transparent"
)

consumption_palette <- colorNumeric(
  palette = "Blues", domain = us_sf$Energy_Consumption,
  na.color = "transparent"
)

difference_palette <- colorNumeric(
  palette = brewer.pal(11, "RdYlGn"),
  domain = us_sf$Production_minus_Consumption,
  na.color = "transparent"
)

library(scales)

#text for hovering over states
production_text <- paste(
  "State: ", us_sf$NAME, "<br/>",
  "Total Energy Production: ", format(us_sf$Energy_Production, big.mark = ','), "<br/>",
  "Renewable Energy % of Total: ", percent(us_sf$Renewable_Production_Percent_of_Total,accuracy = .1), "<br/>",
  "Crude Oil % of Total: ", percent(us_sf$Crude_Oil_Production_Percent_of_Total,accuracy = .1), "<br/>",
  "Natural Gas % of Total: ", percent(us_sf$Natural_Gas_Production_Percent_of_Total,accuracy = .1), "<br/>",
  "Coal % of Total: ", percent(us_sf$Coal_Production_Percent_of_Total,accuracy = .1), "<br/>",
  "Other % of Total: ", percent(us_sf$Other_Production_Percent_of_Total,accuracy = .1), "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML)

consumption_text <- paste(
  "State: ", us_sf$NAME, "<br/>",
  "Total Energy Consumpition: ", format(us_sf$Energy_Consumption, big.mark = ','), "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML)

difference_text <- paste(
  "State: ", us_sf$NAME, "<br/>",
  "Energy Surplus or Deficit: ", format(us_sf$Production_minus_Consumption, big.mark = ','), "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML)

# Final Map
m2 <- leaflet(us_sf) %>%
  addTiles() %>%
  setView(lng = -96, lat = 39, zoom = 3) %>%
  addPolygons(
    fillColor = ~ production_palette(Energy_Production),
    stroke = TRUE,
    fillOpacity = 0.8,
    color = "black",
    weight = 0.2,
    label = production_text,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    ), group = 'Production'
  ) %>%
  addLegend(
    pal = production_palette, values = ~Energy_Production, opacity = 0.9,
    title = "Energy Production (Billion btu)", position = "bottomleft",
    group = "Production"
  ) |> 
  addPolygons(
    fillColor = ~ consumption_palette(Energy_Consumption),
    stroke = TRUE,
    fillOpacity = 0.8,
    color = "black",
    weight = 0.2,
    label = consumption_text,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    ), group = 'Consumption') |>
    addLegend(
      pal = consumption_palette, values = ~Energy_Consumption, opacity = 0.9,
      title = "Energy Consumption (Billion btu)", position = "bottomleft",
      group = 'Consumption'
      ) |>
  addPolygons(
    fillColor = ~ difference_palette(Production_minus_Consumption),
    stroke = TRUE,
    fillOpacity = 0.8,
    color = "black",
    weight = 0.2,
    label = difference_text,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    ), group = 'Difference') |>
  addLegend(
    pal = difference_palette, values = ~Production_minus_Consumption, opacity = 0.9,
    title = "Energy Surplus or Deficit (Billion btu)", position = "bottomleft",
    group = 'Difference'
  ) |>
  addLayersControl(overlayGroups = c("Production","Consumption","Difference"),
                   options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(c('Consumption', 'Difference'))

m2

