source("global.R")

ui_who<-dashboardPage(
  dashboardHeader(title = "WHO Dashboard"),
  dashboardSidebar(
    
              sidebarMenu(id="whotabs",
                menuItem("Overview", tabName = "visual", icon = icon("th")),
                conditionalPanel("input.whotabs === 'visual'",
                                 selectInput(
                                   "year_slider",
                                   width = "100%",
                                   label = h3("Select year:"),
                                   choice = c("1990","2000","2015")
                                 )
                ),
                menuItem("Basic drinking sv", tabName = "water", icon=icon("tint",lib='glyphicon')),
                conditionalPanel("input.whotabs === 'water'",
                                 
                  #menuSubItem(icon=NULL,
                    sliderInput("rateThreshold", "basic drinking water sv.-Below average",
                                min = 0, max = 100, value = 100, step = 0.1
                    ),
                    selectInput("Region", "Select Region",
                                choices = c(
                                  "All" = "All",
                                  "Africa" = "Africa",
                                  "Americas" = "Americas",
                                  "Eastern Mediterranean" = "Eastern Mediterranean",
                                  "Europe" = "Europe",
                                  "South-East Asia" = "South-East Asia",
                                  "Western Pacific" = "Western Pacific"
                                ),
                                selected = "All"
                    )
                ),
                #),#conditional panel
                menuItem("Skilled health personnel", tabName = "skilled", icon = icon("header",lib='glyphicon')),
                conditionalPanel("input.whotabs === 'skilled'",
                                 radioButtons("region_input", "Region",
                                              choices = c("Africa",
                                                          "Americas",
                                                          "Eastern Mediterranean",
                                                          "Europe",
                                                          "South-East Asia",
                                                          "Western Pacific"),
                                              selected = "South-East Asia"),
                                 sliderInput("shp_input", "Skilled health personnel %", 0, 100, c(0, 100), post = "%")
                ),
                menuItem("Show map", tabName = "showmap", icon = icon("search", lib='glyphicon')),
                menuItem("About us", tabName = "aboutus", icon = icon("user", lib='glyphicon'))
              )
  ),
  dashboardBody(
    tabItems(
      tabItem("visual",
              tabsetPanel(
                type = "tabs",
                tabPanel("Global Mortality by Region",
                         p(""),
                         p("In global sphere view, the red vertical bars indicate the maternal mortality ratio (MMR) of a country/area in that region. "),
                         p("In global map view, all countries of that region will appear."),
                         selectInput(
                           inputId = "world_area_select_for_global_sphere",
                           label = "Choose region:",
                           width = "30%",
                           choices = world_region_list,
                           selected = "World level overview"
                         ),
                         checkboxInput("sphere_view_background", label = "black background for sphere", value = TRUE),
                         fluidRow("Global view",
                                  box(
                                    title = "Global Sphere View",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    globeOutput("global_sphere", width = "100%", height = 600)
                                  ),
                                  box(title = "Global Map View",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      leafletOutput("leaf_map", width = "100%", height = 600)
                                  )
                         )
                ),
                tabPanel("Mortality by Country",
                         p(""),
                         p("Maternal Mortality Ratio. Hover your mouse on the map, and the country name and its mortality will appear."),
                         htmlOutput("world_map_plot", width ="100%", height = 600)
                ),
                tabPanel("Global mortality by Year",
                         p(""),
                         fluidRow(title = "mortality cuntry cloud",
                                  box(
                                    title = "mortality country cloud",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput(
                                      "mortality_country_cloud",
                                      width = "100%",
                                      height = 500
                                    )
                                  ),
                                  box(
                                    title = "mortality data table in one year",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    DT::dataTableOutput(
                                      "mortality_of_one_country_data_table",
                                      width = "100%",
                                      height = 500
                                    )
                                  )
                         )
                ),
                tabPanel("Global mortality by Area",
                         h3("mortality by area 1990 to 2015:"),
                         selectInput(
                           inputId = "world_area_select",
                           label = "Choose area:",
                           width = "50%",
                           choices = world_area_list,
                          selected = "Africa"
                         ),
                         plotlyOutput(
                           "world_mortality_scatter_graph",
                           width = "100%",
                           height = 500
                         )
                )
              )
      ),#tabItem visual
      tabItem("water",
              fluidRow(
                valueBoxOutput("Average"),
                valueBoxOutput("Avg_Urban"),
                valueBoxOutput("Avg_Rural")
              ),
              fluidRow( textOutput("selected_var")),
              fluidRow( textOutput("selected_var1")),
              fluidRow( textOutput("selected_var2")),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Relationship of Maternal Mortality v. Basic drinking water (%)",
                  plotlyOutput("packagePlot", width = "100%", height = 600)
                  
                )
              )
      ),#tabitem
      tabItem("skilled",
              fluidRow(
                valueBoxOutput("value1")
                ,valueBoxOutput("value2")
                ,valueBoxOutput("value3")
              ),
              fluidRow(
              #  box(
              #    title = "Filter"
              #    ,status = "primary"
              #    ,solidHeader = TRUE
              #    ,collapsible = TRUE
              #    ,width = 4
              #    ,radioButtons("region_input", "Region",
              #                  choices = c("Africa",
              #                              "Americas",
              #                              "Eastern Mediterranean",
              #                              "Europe",
              #                              "South-East Asia",
              #                              "Western Pacific"),
              #                  selected = "South-East Asia")
              #    ,sliderInput("shp_input", "Skilled health personnel %", 0, 100, c(0, 100), post = "%")),
                box(
                  title = "Country level details"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotlyOutput("countryplot", height = "300px")
                ),
                box(
                  title = "Correlation testing"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotlyOutput("corrplot")
                ) 
              ),
              fluidRow(
                #box(
                #  title = "Correlation testing"
                #  ,status = "primary"
                #  ,solidHeader = TRUE 
                #  ,collapsible = TRUE 
                #  ,plotlyOutput("corrplot")
                #) 
              )
        
      ),#tabitem
      tabItem("showmap",
              fluidRow(
                div(
                  #class = "outer",
                  #tags$head(includeCSS("style.css")),
                  #leafletOutput("CountryMap", width = "100%", height = "auto"),
                  leafletOutput("CountryMap", width = "800", height = "600"),
                  #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  absolutePanel(id = "controls", 
                                draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto"
                                
                                #selectInput("region", label = h4("Region"), choices = c("", test_df$region), selected = "", width = "90%"),
                                #htmlOutput("pieChart")
                  ))
              )
        
      ),
      tabItem("aboutus",
              #fluidRow(
                h3("The team - Health Visioner"),
                p("WQD170045 - LIM PUI SAN"),
                p("WQD170078 - WONG XIN CI"),
                p("WQD170085 - OOI SHAU MING"),
                p("WQD180026 - LIM YAN"),
                p(""),
                p(""),
                h3("Aboout this project"),
                p("This project aim to assist WHO to discover maternal mortality ratio of the regions."),
                p("The 2 key factors will be investigated."),
                p("1) Hygience /Drinking water"),
                p("2) Skilled health personnel"),
                p(""),
                p(""),
                #h4("show data"),
                #DT::dataTableOutput("mytable")
                h3("Sources for this project"),
                a(href="http://apps.who.int/gho/data/node.main.15?lang=en","Maternal mortality"),
                br(),
                a(href="http://gamapserver.who.int/gho/interactive_charts/mdg5_mm/atlas.html","Births attended by skilled health personnel"),
                br(),
                a(href="http://apps.who.int/gho/data/node.main.ANTENATALCARECOVERAGE4?lang=en","Antenatal care coverage")
              #)
      )
    )#tabitems
  )
)
