## AMISSA SINUO DASHBOARD

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
# library(mongolite)
library(dplyr)
library(httr)
library(htmltools)
library(plotly)
library(leaflet)
source('amissaTheme.R')

## rsconnect::deployApp(appTitle = "SinuoDashboard", account = "amissa")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    title = "Amissa | Sinuo Dashboard",
    dashboardHeader(
        title = shinyDashboardLogo(
            theme = "grey_light",
            boldText = "Sinuo",
            mainText = "Dashboard",
            badgeText = "v0.1"
        ),
        tags$li(class = "dropdown",
                dropMenu(
                    dropdownButton(inputId  = "drop_btn", label= "Login", status = 'success', icon = icon('user'), circle = TRUE),
                    h3(strong('User Login')),
                    br(),
                    textInput("username",
                              "Username:",
                              value = "shiny_user"),
                    passwordInput("password",
                                  "Password:",
                                  # value = "Sh1ny",
                                  placeholder = "Sh?ny"),
                    actionButton("login_btn", label = "Login"),
                    placement = "bottom",
                    arrow = TRUE)
                
        )
        # title = "SinuoDashboard"
    ),
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        disable = TRUE,
        collapsed = TRUE,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Connection", tabName = "connection", icon = icon("link"))
        )
    ),

    # Show a plot of the generated distribution
    dashboardBody(
        # shinyDashboardThemes(
        #     theme = "grey_light"
        # ),
        amissaTheme,
        tabItems(
            # tabItem(tabName = "connection",
            #         fluidRow(
            #             box(width = 6,
            #                 textInput("username",
            #                           "Username:",
            #                           value = "shiny_user"),
            #                 passwordInput("password",
            #                               "Password:",
            #                               # value = "Sh1ny",
            #                               placeholder = "Sh?ny"),
            #                 textInput("cluster",
            #                           "Cluster URL:",
            #                           value = "testcluster1.ort2e.mongodb.net/"),
            #                 textInput("database",
            #                           "Database:",
            #                           value = "SinuoDB"),
            #                 textInput("collection",
            #                           "Collection:",
            #                           value = "device_reads")
            #                 ),
            #             box(width = 6,
            #                 textInput("query",
            #                           "Query String:",
            #                           value = '{"device_id" : 123}'),
            #                 sliderInput("limit",
            #                             "Row Limit: (0 means all)",
            #                             min = 0,
            #                             max = 1000,
            #                             step = 50,
            #                             value = 50)
            #                 )
            #         ),
            #         fluidRow(
            #             verbatimTextOutput("urlconcat")
            #         )
            #     
            # ),
            tabItem(tabName = "dashboard",
                    fluidRow(
                        actionButton("login_btn",
                                     "Refresh Data",
                                     style = "float:right",
                                     icon = icon("sync")),
                    ),
                    fluidRow(
                        box(title = "Steps",
                            width = 4,
                            plotlyOutput("sensor1_plot")),
                        box(title = "Heart Rate",
                            width = 4,
                            plotlyOutput("sensor2_plot")),
                        box(title = "Activity Breakdown",
                            width = 4,
                            plotlyOutput("sensor3_plot")),
                    ),
                    fluidRow(
                        box(title = "Map",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            leafletOutput("map_plot")),
                    ),
                    fluidRow(
                        box(title = "Table",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            tableOutput("resultstable")),
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    broker_api_base_url <- "https://sinuobrokeraf.azurewebsites.net/api/"
    broker_api_login_path = "api/Login"
    broker_api_gdr_path = "api/GetDeviceReads"
    broker_af_code = "OlSH9OYxaTamYDu3cArtapVGTLd11aLNK3XHtpaY/kaTL3Pspb4IZQ=="

    app_token <- "Am!ssa1"
    
    observeEvent(input$login_btn, {
        
        ## Login
        broker_login_body = list(
            username = input$username,
            password = input$password,
            app_token = app_token
        )
        
        broker_login_response <- POST(url = broker_api_base_url,
                                      path = broker_api_login_path,
                                      query = list(code = broker_af_code),
                                      body = broker_login_body,
                                      httr::content_type_json(),
                                      encode = "json", verbose())
        
        
        user_token <- jsonlite::fromJSON(rawToChar(broker_login_response$content))$user_token
        device_id <-jsonlite::fromJSON(rawToChar(broker_login_response$content))$device_id
        
        
        ## GetDeviceReads
        broker_gdr_body = list(
            device_id = device_id,
            username = input$username,
            user_token = user_token
        )
        
        broker_gdr_response <- POST(url = broker_api_base_url,
                                    path = broker_api_gdr_path,
                                    query = list(code = broker_af_code),
                                    body = broker_gdr_body,
                                    httr::content_type_json(),
                                    encode = "json", verbose())
        
        device_reads <- as.data.frame(jsonlite::fromJSON(rawToChar(broker_gdr_response$content)))
        
        device_reads_pretty <- device_reads %>% 
            select(timestamp,
                   latitude,
                   longitude,
                   altitude,
                   gravity,
                   rotation,
                   userAccel,
                   step,
                   heartRate,
                   motionActivity) %>% 
            mutate(timestamp = as.character(timestamp))
        
        output$resultstable <- renderTable({device_reads_pretty},
            striped = TRUE,
            hover = TRUE,
            width = "100%")
        
        sensor1_fig <- plot_ly(data = device_reads,
                               x = ~timestamp,
                               y = ~step,
                               mode = 'lines+markers',
                               text = "Steps")
        
        output$sensor1_plot <- renderPlotly({sensor1_fig})
        
        sensor2_fig <- plot_ly(data = device_reads,
                               x = ~timestamp,
                               y = ~heartRate,
                               colors = "Set1",
                               mode = 'lines+markers',
                               text = "Heart Rate")
        
        output$sensor2_plot <- renderPlotly({sensor2_fig})
        
        sensor3_fig <- plot_ly(data = device_reads,
                               labels = ~motionActivity,
                               type = 'pie')
        
        output$sensor3_plot <- renderPlotly({sensor3_fig})
        
        map_fig <- leaflet(device_reads_pretty) %>% 
            addTiles() %>% addMarkers(
                ~longitude, ~latitude,
                label = ~htmlEscape(timestamp),
                clusterOptions = markerClusterOptions()
            )
        
        output$map_plot <- renderLeaflet({map_fig})
        
    })
    
    # observeEvent(input$go_btn, {
    #     url = paste0("mongodb+srv://",
    #                  input$username,
    #                  ":",
    #                  input$password,
    #                  "@",
    #                  input$cluster,
    #                  input$database,
    #                  "?retryWrites=true&w=majority")
    #     
    #     
    #     output$urlconcat <- renderText({paste("Connection string:\n", url)})
    #     
    #     db <- mongo(collection = input$collection,
    #                 url = url)
    #     
    #     # data <- db$find()
    #     
    #     if (input$limit > 0){
    #         query <- db$find(
    #             query = input$query,
    #             limit = input$limit
    #         ) #%>% mutate(timestamp = as.character(timestamp))
    #         #%>% select(!`_id`)
    #         
    #     } else {
    #         query <- db$find(
    #             query = input$query,
    #         ) #%>% mutate(timestamp = as.character(timestamp))
    #         #%>% select(!`_id`)
    #     }
    #     
    #     output$resultstable <- renderTable({query})
    #     
    #     sensor1_fig <- plot_ly(data = query,
    #                            x = ~timestamp,
    #                            y = ~sensor1,
    #                            mode = 'lines+markers',
    #                            text = "Sensor 1")
    #     
    #     output$sensor1_plot <- renderPlotly({sensor1_fig})
    #     
    #     sensor2_fig <- plot_ly(data = query,
    #                            x = ~timestamp,
    #                            y = ~sensor2,
    #                            colors = "Set1",
    #                            mode = 'lines+markers',
    #                            text = "Sensor 2")
    #     
    #     output$sensor2_plot <- renderPlotly({sensor2_fig})
    #     
    #     sensor3_fig <- plot_ly(data = query,
    #                            labels = ~sensor3,
    #                            type = 'pie')
    #     
    #     output$sensor3_plot <- renderPlotly({sensor3_fig})
    # })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
