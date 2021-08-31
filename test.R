# map
nc_state <- nc %>% 
  left_join(sum_cum_case_vac_state %>% filter(name != "All"))

leaflet(nc_state) %>%
  setView(lng = 108, lat = 4, zoom = 6) %>%
  # addTiles() %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~colorQuantile("YlOrRd", cumul_cases_new_adjust)(cumul_cases_new_adjust),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    popup = ~paste("State:", name, ", Statistics:", format(round(cumul_cases_new_adjust, digits = 1), nsmall = 1))
  )
