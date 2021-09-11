source("./global.R")

dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "COVMAL - V00-01-02"),
  
  
  dashboardSidebar(
    width = 250,
    
    sidebarMenu(
      menuItem("Home Page", tabName = "menu_home", icon = icon("home")),
      
      menuItem("Cases", tabName = "menu_case", icon = icon("viruses")),
      
      menuItem("Healthcare resource utilization", tabName = "menu_hru", icon = icon("hospital")),
      
      menuItem("Clusters", tabName = "menu_cluster", icon = icon("project-diagram"))
      
    ) ## end of sidebarMenu
  ), ## end of dashboardSidebar
  
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "menu_home",
        
        fluidRow(
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Total number of cases", collapsible = TRUE,
            valueBoxOutput("home_total_case", width = 4),
            valueBoxOutput("home_total_death", width = 4),
            valueBoxOutput("home_total_vac", width = 4)
          ), ## end of box
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Adjusted Total number of cases per 100K population", collapsible = TRUE,
            valueBoxOutput("home_total_case_adjust", width = 4),
            valueBoxOutput("home_total_death_adjust", width = 4),
            valueBoxOutput("home_total_vac_adjust", width = 4)
          ), ## end of box
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Recent adjusted trends per 100K population", collapsible = TRUE,
            selectInput("home_recent_stat", "Select statistics:", choices = c("Choose" = "",
                                                                              "New cases" = "cases_new_adjust",
                                                                              "Deaths" = "deaths_new_adjust",
                                                                              "Fully vaccination" = "daily_full_adjust"), 
                        selected = "cases_new_adjust", multiple = FALSE),
            plotlyOutput("home_recent_trend", height = "500px") %>% withSpinner(size = 2)
          )
        ) ## end of fluidRow
      ), ## end of tabItem
      
      tabItem(
        tabName = "menu_case",
        
        fluidRow(
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Daily summary by states", collapsible = TRUE,
            
            selectInput("home_case_state", "Select states:", choices = unique(dt_daily_state$state), selected = c("All", "W.P. Kuala Lumpur"), multiple = TRUE),
            
            tabsetPanel(
              tabPanel("Adjusted daily incidence by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("home_plt_case_state", height = "800px") %>% withSpinner(size = 2))))),
              
              tabPanel("Adjusted daily incidence and not-fully vaccination rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("home_plt_case_vac_state", height = "800px") %>% withSpinner(size = 2))))),
              
              tabPanel("Adjusted daily deaths by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("home_plt_case_death_state", height = "800px") %>% withSpinner(size = 2))))),
              
              tabPanel("Adjusted daily deaths and not-fully vaccination rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("home_plt_death_vac_state", height = "800px") %>% withSpinner(size = 2)))))
            ) ## end of tabsetPanel
          ), ## end of box
          
          
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Cumulative summary by states", collapsible = TRUE,
            
            selectInput("home_cumul", "Select index:", choices = c("Choose" = "",
                                                                   "Cumulative cases" = "cumul_cases_new",
                                                                   "Cumulative cases per 100K population" = "cumul_cases_new_adjust",
                                                                   "Cumulative deaths" = "cumul_deaths_new",
                                                                   "Cumulative deaths per 1M population" = "cumul_deaths_new_adjust"), 
                        selected = "cumul_cases_new", multiple = FALSE, selectize = TRUE),
            
            leafletOutput("home_plt_heatmap_state", height = "800px") %>% withSpinner(size = 2)
          ) ## end of box
        ) ## end of fluidRow
      ), ## end of tabItem
      
      
      tabItem(
        tabName = "menu_hru",
        
        fluidRow(
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Current resource avalability by states", collapsible = TRUE,
            
            selectInput("hru_cumul", "Select index:", choices = c("Choose" = "",
                                                                  "Hospital beds" = "cumul_remain_bed_rate",
                                                                  "ICU beds" = "remain_icu_covid_rate",
                                                                  "Ventilators avalability" = "remain_vent_covid_rate",
                                                                  "PKRC beds avalability" = "cumul_remain_pkrc_rate"), 
                        selected = "cumul_remain_bed_rate", multiple = FALSE, selectize = TRUE),
            
            leafletOutput("hru_plt_heatmap_state", height = "800px") %>% withSpinner(size = 2)
          ), ## end of box
          
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Healthcare resources by states", collapsible = TRUE,
            
            selectInput("hru_resource_state", "Select states:", choices = unique(dt_daily_state$state), selected = c("All", "W.P. Kuala Lumpur"), multiple = TRUE),
            
            tabsetPanel(
              tabPanel("Hospital beds avalability rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_beds_avl_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("ICU beds avalability rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_icu_avl_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("Ventilators avalability rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_vent_avl_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("PKRC beds avalability rate by states",
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_pkrc_avl_state", height = "800px") %>% withSpinner(size = 2)))))
            ), ## end of tabsetPanel
            
            tabsetPanel(
              tabPanel("Adjusted total hospital beds by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_beds_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("Adjusted total ICU beds by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_icu_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("Adjusted total ventilators by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_vent_state", height = "800px") %>% withSpinner(size = 2))))),
              tabPanel("Adjusted total PKRC beds by states", 
                       fluidPage(fluidRow(column(12, plotlyOutput("hru_plt_pkrc_state", height = "800px") %>% withSpinner(size = 2)))))
            ) ## end of tabsetPanel
          ) ## end of box
        ) ## end of fluidRow
      ), ## end of tabItem
      
      tabItem(
        tabName = "menu_cluster",
        
        fluidRow(
          box(
            solidHeader = TRUE, width = 12, height = "auto", title = "Cases by clusters analysis", collapsible = TRUE,
            
            fluidRow(
              column(3, selectInput("cluster_case", "Select type of cases:", choices = c("All" = "cases_total", 
                                                                                         "Active" = "cases_active", 
                                                                                         "Recovered" = "recovered"), selected = "All")),
              column(3, selectInput("cluster_status", "Select cluster status:", choices = c("All" = "all", 
                                                                                            "Active" = "active", 
                                                                                            "Ended" = "ended"), selected = "All")),
              column(3, selectInput("cluster_state", "Select states:", choices = c(unique(dt_daily_state$state)), selected = "All"))
            ),
            
            plotlyOutput("cluster_plt_case", height = "800px") %>% withSpinner(size = 2)
          )
        ) ## end of fluidRow
      ) ## end of tabItem
      
    ) ## end of tabItems
    
  ) ## end of dashboardBody

  
  
)