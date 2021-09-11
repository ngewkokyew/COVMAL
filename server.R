function(input, output, session){
  
  # menu_home -------------------------
  output$home_total_case <- renderValueBox({
    valueBox(
      value = sum_cum %>% filter(name == "All") %>% pull(cumul_cases_new),
      subtitle = "Total number of COVID-19 cases",
      color = "yellow",
      icon = icon("virus")
    )
  }) ## end of home_total_case
  
  output$home_total_death <- renderValueBox({
    valueBox(
      value = sum_cum %>% filter(name == "All") %>% pull(cumul_deaths_new),
      subtitle = "Total number of deaths due to COVID-19",
      color = "red",
      icon = icon("skull-crossbones")
    )
  }) ## end of home_total_death
  
  output$home_total_vac <- renderValueBox({
    valueBox(
      value = sum_cum %>% filter(name == "All") %>% pull(cumul_daily_full),
      subtitle = "Total number of fully vaccination",
      color = "green",
      icon = icon("syringe")
    )
  }) ## end of home_total_vac
  
  output$home_total_case_adjust <- renderValueBox({
    valueBox(
      value = formatC(sum_cum %>% filter(name == "All") %>% pull(cumul_cases_new_adjust), format = "f", digits = 1),
      subtitle = "Adjusted number of COVID-19 cases",
      color = "yellow",
      icon = icon("virus")
    )
  }) ## end of home_total_case_adjust
  
  output$home_total_death_adjust <- renderValueBox({
    valueBox(
      value = formatC(sum_cum %>% filter(name == "All") %>% pull(cumul_deaths_new_adjust), format = "f", digits = 1),
      subtitle = "Adjusted number of deaths",
      color = "red",
      icon = icon("skull-crossbones")
    )
  }) ## end of home_total_death_adjust
  
  output$home_total_vac_adjust <- renderValueBox({
    valueBox(
      value = formatC(sum_cum %>% filter(name == "All") %>% pull(cumul_daily_full_adjust), format = "f", digits = 1),
      subtitle = "Adjusted number of fully vaccination",
      color = "green",
      icon = icon("syringe")
    )
  }) ## end of home_total_vac_adjust
  
  output$home_recent_trend <- renderPlotly({
    dt_daily_state_last7 <- dt_daily_state %>%
      select(state, date, !!as.symbol(input$home_recent_stat)) %>%
      rename(values_from = !!as.symbol(input$home_recent_stat)) %>%
      group_by(state) %>%
      filter(row_number() > (n() - 7)) %>%
      pivot_wider(id_cols = state, values_from = values_from, names_from = date) %>%
      ungroup() %>%
      arrange(desc(state))
    
    plt_case <- plot_ly(
      x = colnames(dt_daily_state_last7 %>% select(-state)), y = dt_daily_state_last7 %>% pull(state), z = as.matrix(dt_daily_state_last7 %>% select(-state)),
      type = "heatmap", colors = if(input$home_recent_stat == "daily_full_adjust"){"YlGn"}else{"OrRd"}
    )
  }) ## end of home_recent_stat
  
  
  # menu_case -------------------------
  ## Adjusted daily incidence by states
  output$home_plt_case_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$home_case_state), 
      x = ~date, y = ~cases_new_adjust, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      add_trace(
        x = ~date, y = ~cases_new_adjust_ma7, color = ~state,
        type = "scatter", mode = "lines", name = ~paste("MA-7:", state)
      ) %>%
      layout(
        yaxis = list(title = "Daily incidence case per 100K population"), 
        xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of home_plt_case_state
  
  
  ## Adjusted daily incidence and not-fully vaccination rate by states
  output$home_plt_case_vac_state <- renderPlotly({
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Daily incidence case per 100K population",
      automargin = TRUE
    )

    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$home_case_state),
      x = ~date, y = ~cumul_not_full_adjust, color = ~state, legendgroup = ~state,
      type = "scatter", mode = "lines", name = ~paste("Not fully-vaccinated:", state), line = list(dash = "dash")
    ) %>%
      add_trace(
        x = ~date, y = ~cases_new_adjust_ma7, color = ~state,
        type = "scatter", mode = "lines", name = ~paste("MA (7-days):", state), line = list(dash = "solid")
      ) %>%
      add_trace(
        x = ~date, y = ~cases_new_adjust, color = ~state,
        type = "bar", barmode = "stack", yaxis = "y2", name = ~paste("Cases:", state)
      ) %>%
      layout(
        xaxis = list(title = "Date", rangeslider = list(type = "date")),
        yaxis = list(title = "Not fully-vaccinated population per 100K population"), yaxis2 = ay,
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of home_plt_case_vac_state
  
  
  ## Adjusted daily incidence and death rate by states
  output$home_plt_case_death_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$home_case_state), 
      x = ~date, y = ~deaths_new_adjust, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      add_trace(
        x = ~date, y = ~deaths_new_adjust_ma7, color = ~state,
        type = "scatter", mode = "lines", name = ~paste("MA-7:", state)
      ) %>%
      layout(
        yaxis = list(title = "Daily death cases per 1M population"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of home_plt_case_death_state
  
  
  ## Adjusted daily incidence and death rate by states
  output$home_plt_death_vac_state <- renderPlotly({
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Daily death cases per 1M population",
      automargin = TRUE
    )
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$home_case_state),
      x = ~date, y = ~cumul_not_full_adjust, color = ~state, legendgroup = ~state,
      type = "scatter", mode = "lines", name = ~paste("Not fully-vaccinated:", state), line = list(dash = "dash")
    ) %>%
      add_trace(
        x = ~date, y = ~deaths_new_adjust_ma7, color = ~state,
        type = "scatter", mode = "lines", name = ~paste("MA (7-days):", state), line = list(dash = "solid")
      ) %>%
      add_trace(
        x = ~date, y = ~deaths_new_adjust, color = ~state,
        type = "bar", barmode = "stack", yaxis = "y2", name = ~paste("Deaths:", state)
      ) %>%
      layout(
        xaxis = list(title = "Date", rangeslider = list(type = "date")),
        yaxis = list(title = "Not fully-vaccinated population per 100K population"), yaxis2 = ay,
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of home_plt_case_death_state
  
  
  ## Heatmap by states
  output$home_plt_heatmap_state <- renderLeaflet({
    
    nc_state <- nc %>% 
      left_join(sum_cum %>% filter(name != "All")) %>%
      rename(index = !!as.symbol(input$home_cumul))
    
    leaflet(nc_state) %>%
      setView(lng = 108, lat = 4, zoom = 6.5) %>%
      # addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~colorNumeric("YlOrRd", index)(index),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        popup = ~paste("State:", name, ", Statistics:", format(round(index, digits = 1), nsmall = 1)) ## Popup info
      ) %>%
      addLegend(
        "topright", pal = colorNumeric("YlOrRd", nc_state$index), values = ~index,
        title = "Intensity",
        opacity = 1
      )
  }) ## end of home_plt_heatmap_state
  
  
  # menu_hru -------------------------
  ## Heatmap by states
  output$hru_plt_heatmap_state <- renderLeaflet({
    
    nc_state <- nc %>% 
      left_join(dt_daily_state %>% 
                  rename(name = state) %>%
                  filter(name != "All") %>% 
                  group_by(name) %>% 
                  filter(row_number() == n()-1)) %>%
      rename(index = !!as.symbol(input$hru_cumul))
    
    leaflet(nc_state) %>%
      setView(lng = 108, lat = 4, zoom = 6.5) %>%
      # addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~colorNumeric("RdYlGn", index)(index),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        popup = ~paste("State:", name, ", Statistics:", format(round(index, digits = 1), nsmall = 1)) ## Popup info
      ) %>%
      addLegend(
        "topright", pal = colorNumeric("RdYlGn", nc_state$index), values = ~index,
        title = "Intensity (%)",
        opacity = 1
      )
  }) ## end of home_plt_heatmap_state
  
  
  output$hru_plt_beds_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~beds_covid_adjust, color = ~state, name = ~paste("COVID beds:", state),
      type = "scatter", mode = "lines"
    ) %>%
      add_trace(
        x = ~date, y = ~beds_noncrit_adjust, color = ~state,
        type = "scatter", mode = "lines", line = list(dash = "dash"), name = ~paste("Non-COVID beds:", state)
      ) %>%
      layout(
        yaxis = list(title = "Daily beds per 100K population"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_beds_state
  
  output$hru_plt_icu_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~beds_icu_total_adjust, color = ~state, name = ~paste("Total ICU:", state),
      type = "scatter", mode = "lines"
    ) %>%
      add_trace(
        x = ~date, y = ~beds_icu_covid_adjust, color = ~state,
        type = "scatter", mode = "lines", line = list(dash = "dash"), name = ~paste("COVID ICU:", state)
      ) %>%
      layout(
        yaxis = list(title = "Daily beds per 100K population"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_icu_state
  
  output$hru_plt_vent_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~vent_adjust, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "Ventilators per 100K population"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_vent_state
  
  output$hru_plt_pkrc_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~pkrc_beds_adjust, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "PKRC beds per 100K population"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_pkrc_state
  
  output$hru_plt_beds_avl_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~cumul_remain_bed_rate, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "Porportion of resources available (%)"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_icu_avl_state
  
  output$hru_plt_icu_avl_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~remain_icu_covid_rate, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "Porportion of resources available (%)"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_icu_avl_state
  
  output$hru_plt_vent_avl_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~remain_vent_covid_rate, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "Porportion of resources available (%)"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_vent_avl_state
  
  output$hru_plt_pkrc_avl_state <- renderPlotly({
    
    plot_ly(
      data = dt_daily_state %>% filter(state %in% input$hru_resource_state), 
      x = ~date, y = ~cumul_remain_pkrc_rate, color = ~state,
      type = "bar", barmode = "stack"
    ) %>%
      layout(
        yaxis = list(title = "Porportion of resources available (%)"), xaxis = list(title = "Date", rangeslider = list(type = "date")),
        legend = list(title = list(text = "<b> States </b>"), orientation = "h", y = 100)
      )
    
  }) ## end of hru_plt_[krc_avl_state
  
  
  # menu_cluster -------------------------
  output$cluster_plt_case <- renderPlotly({
    
    data <- dt_cluster %>%
      rename(cases = !!as.symbol(input$cluster_case)) %>%
      {if(input$cluster_status != "all") filter(., status == input$cluster_status) else .} %>%
      {if(input$cluster_state != "All") filter(., grepl(str_replace_all(input$cluster_state, "[[:punct:]]", ""), state)) else .} %>%
      group_by(category) %>%
      summarise(cumul = sum(cases, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(cumul_prop = cumul*100 /sum(cumul))
    
    plot_ly(
      data = data, labels = ~category, values = ~cumul, type = "pie",
      textposition = "outside", textinfo = "label", insidetextfont = list(color = "#FFFFFF"),
      marker = list(line = list(color = "#FFFFFF", width = 1)),
      hoverinfo = "text", text = ~paste(cumul, "cases,", format(round(cumul_prop, digits = 1), nsmall = 1), "%")
    ) %>%
      layout(
        showlegend = TRUE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        xyaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(title = list(text = "<b> Category </b>"))
      )

  })
}