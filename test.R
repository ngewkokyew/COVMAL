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

# moving average
temp <- dt_daily_state %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(
    cases_new_ma7 = rollmean(cases_new, k = 7, fill = NA, align = "right"),
    cases_new_adjust_ma7 = rollmean(cases_new_adjust, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  select(date, state, cases_new, cases_new_ma7, cases_new_adjust, cases_new_adjust_ma7)

plot_ly(
  data = temp %>% filter(state %in% c("All", "W.P. Kuala Lumpur")), 
  x = ~date, y = ~cases_new_adjust, color = ~state,
  type = "bar", barmode = "stack"
) %>%
  add_trace(
    x = ~date, y = ~cases_new_adjust_ma7, color = ~state,
    type = "scatter", mode = "lines"
  ) %>%
  layout(
    yaxis = list(title = "Daily incidence case per 100K population"), 
    xaxis = list(title = "Date", rangeslider = list(type = "date")),
    legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
  )


## heatmap
dt_daily_state_last7 <- dt_daily_state %>%
  select(state, date, cases_new_adjust, deaths_new_adjust, daily_full_adjust) %>%
  group_by(state) %>%
  filter(row_number() > (n() - 7)) %>%
  pivot_wider(id_cols = state, values_from = cases_new_adjust, names_from = date) %>%
  ungroup() %>%
  arrange(desc(state))

plt_case <- plot_ly(
  x = colnames(dt_daily_state_last7 %>% select(-state)), y = dt_daily_state_last7 %>% pull(state), z = as.matrix(dt_daily_state_last7 %>% select(-state)),
  type = "heatmap", colors = "YlOrRd"
)