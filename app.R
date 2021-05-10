
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(shinyWidgets)
library(stringr)
library(ggnetwork)
options(repos = c(CRAN = "https://cran.rstudio.com/")) 

ui <- navbarPage("Intermodality and the City", id='cb', inverse = TRUE,
                 tabPanel("Introduction",
                          setBackgroundColor(color = "#292929"),
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(6,
                                       h1("Intermodality and the City"),
                                       h3("Shaun Khoo"),
                                       tags$em(h5("Urban Datascapes, Spring 2019, Columbia University")),
                                       offset = 1)
                              ),
                              fluidRow(
                                column(7, 
                                       style = "max-height: 75vh; overflow-y: auto;",
                                       
                                       h4("Intermodality, defined as the flexible usage and combination of different transportation modes (Gebhardt et al., 2016), has recently become of importance in highly urbanized areas in Europe (Gebhardt, Krajzewicz, & Oostendorp, 2017). Facing worsening traffic congestion and greater population density, city governments are under pressure to develop a public transportation network that is flexible and efficient enough to meet the growing and diverse transit needs of its residents.",
                                          style = "line-height: 150%"),
                                       h4("However, data produced by each transportation mode often only includes trips made within that particular platform, and is isolated from trip data produced by other modes. For example, in New York City, Metropolitan Transit Authority (MTA) releases ridership data for its buses and subways, but the data is kept in separate databases and not linked in any meaningful way. Bicycle sharing systems, which are often used for last-mile journeys, only provide data on the trips within their system without any reference to the broader transportation network that it is a part of.",
                                          style = "line-height: 150%"), 
                                       h4("This project explores the problems that arise from the siloed production and representation of transportation data. Commuters do not see each transportation mode as mutually exclusive options. Treating the data as though it is will hamper our ability to understand public transit patterns and hinder our planning for enhancing transportation and mobility in our cities.", 
                                          style = "line-height: 150%"), 
                                       h4("Through this interactive application, I hope to bring the audience through an exploration of how the production of Citi Bike trip data can obscure our understanding of its unique position in New York City's multi-modal public transportation network, and thus inaccurately inform planning decisions regarding Citi Bike expansion and provision throughout the city. Eschewing the spatial assumption of virtually all representations of Citi Bike data, I propose using network graphs as an alternative approach to visualizing the connections within the Citi Bike network.", 
                                          style = "line-height: 150%"),
                                       offset = 1),
                                column(3,
                                       img(src = "side_image.png", height = "450px", align = "center"),
                                       br(),
                                       tags$em(h5("A map of Citi Bike stations and subway stations in downtown Manhattan and Brooklyn.")),
                                       offset = 0)
                              )
                          )
                 ),
                 tabPanel("Astoria",
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(6,
                                       h1("Exhibit A: Astoria"), 
                                       offset = 1)
                              ),
                              fluidRow(
                                column(6,
                                       style = "max-height: 78vh; overflow-y: auto;",
                                       h2("Insufficient accessibility",
                                          style = "line-height: 150%"),
                                       h4("Exhibit A focuses on Astoria, an increasingly gentrified neighborhood in Queens (Hughes, 2017) located just across the Hudson River from Manhattan. Astoria was part of Citi Bike's second phase of expansion into Queens, building on the network established in Long Island City during the first phrase. When this was initially announced in 2015, a local councilman commented that adding the new Citi Bike stations would 'add a much-needed alternative mode of transportation to an area of the borough that is growing and vibrant, and in need of more transportation options' (Jamerson, 2015).", 
                                          style = "line-height: 150%"),
                                       h4("The interactive map on the right shows clearly why this was the case. The yellow line represents the sole subway track (with the N and W lines plying it) serving the area, while the green concentric circles represent areas which are up to 400 meters away from each subway station. For those who stay near the Hudson River, walking to the subway station could take anywhere from 15 to 25 minutes.",
                                          style = "line-height: 150%"),
                                       h2("Citi Bike usage in Astoria",
                                          style = "line-height: 150%"),
                                       h4("Using the dropdown menu on top of the interactive map, select 'Citi Bike Stations' to show all the Citi Bike stations in Astoria. Each blue circle represents a Citi Bike station, and the size of the circle corresponds to the station's popularity - the more trips that originate from that station, the larger the circle is. Clicking on the blue circle gives you information about the station's name and the number of trips originating from that station. Unsurprisingly, the Citi Bike stations nearest to the subway stations are highly utilized, but there are some pockets of high-usage stations further away from the subway.",
                                          style = "line-height: 150%"),   
                                       h4("Now, select 'Citi Bike routes' to show the routes taken within these stations. Each line represents trips that were taken between those two stations, with the thickness of the line indicating how many trips were taken between those two stations. Clicking on the line provides information about both the starting and ending stations as well as the number of trips between the two stations. For clarity, only routes with more than 10 trips were included.",
                                          style = "line-height: 150%"),
                                       h2("Intermodality in action",
                                          style = "line-height: 150%"),
                                       h4("What can we observe about how residents in Astoria integrate Citi Bikes into their daily commutes?",
                                          style = "line-height: 150%"),
                                       h4("First, there are many longitudinal movements where riders travel to and from the subway stations along the N and W lines, such as the 30th Avenue, Astoria Boulevard, and Ditmars Boulevard stations. These trips seem to be concentrated in the area west of the subway line, which seems partially a result of the greater availability of Citi Bike stations in that area. This is one clear pattern of movement.",
                                           style = "line-height: 150%"),
                                       h4("Second, we can observe several routes traveling northwards from Queens Plaza subway station, suggesting that several commuters skipped transferring to the N or W subway in favor of cycling home directly, even if the distance cycled is fairly long",
                                          style = "line-height: 150%"),
                                       offset = 1),
                                column(5,
                                       div(style="margin-left:60px",
                                       pickerInput(inputId = 'ast_vars', label = 'Select data to view',
                                                   choices = list('Walkability',
                                                                  'Citi Bike Stations',
                                                                  'Citi Bike Routes'),
                                                   selected = 'Walkability',
                                                   options = pickerOptions(actionsBox = TRUE, size = 10,
                                                                           selectedTextFormat = "values",
                                                                           noneSelectedText = 'No data selected'), 
                                                   multiple = TRUE)),
                                       leafletOutput("astoria_map", height = "600px", width = "550px")
                                )
                              )
                          )
                 ),
                 tabPanel("East Harlem",
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(6,
                                       h1("Exhibit B: East Harlem"), 
                                       offset = 1)
                              ),
                              fluidRow(
                                column(6,
                                       style = "max-height: 78vh; overflow-y: auto;",
                                       h2("Subway accessibility not an issue",
                                          style = "line-height: 150%"),
                                       h4("Exhibit B focuses on East (and Central) Harlem, which lies indirectly across the Hudson from Astoria. Being in Manhattan itself, East Harlem is well-serviced by several subway lines plying through the area, with the 4, 5, and 6 lines as the main conduit through the neighborhood, the Q line reaching the southern end, and the 2 and 3 lines servicing the western edge of East Harlem and Central Harlem as well. From the interactive map on the right, most areas are less than 400 meters away from a subway station.", 
                                          style = "line-height: 150%"),
                                       h4("So why East Harlem?",
                                          style = "line-height: 150%"),
                                       h2("Citi Bike usage in East Harlem",
                                          style = "line-height: 150%"),
                                       h4("As before, select 'Citi Bike Stations' to show all the Citi Bike stations in East Harlem.  Click on some of the more popular stations, and you will find that there are more Citi Bike trips being made in East Harlem than in Astoria. More precisely, there were 1716 trips in East Harlem alone as compared to the 1360 trips in Astoria. In addition, the usage of Citi Bikes appear to be more evenly spread out across the area unlike Astoria, where usage was concentrated on 3 or 4 Citi Bike stations.",
                                          style = "line-height: 150%"),   
                                       h4("Now, select 'Citi Bike routes' to show the routes taken within these stations. We observe patterns that are similar to what we found in Astoria. Horizontal routes across Manhattan are amongst the most popular trips taken, even though the endpoint is within 400 meters of a subway station. The Citi Bike station at the end of the Q line is also a major hotspot, with people using Citi Bikes as a last-mile connection cycling vertically up Manhattan to areas right next to the Hudson river.",
                                          style = "line-height: 150%"),
                                       h2("Intermodality in action",
                                          style = "line-height: 150%"),
                                       h4("In East Harlem, we see a very different display of intermodality. Citi Bikes are being used to overcome a significant drawback of the NYC subway system: the fact that most lines run vertically through Manhattan, rather than horizontally. The 4, 5, and 6 lines exclusively serve the east side of Manhattan, the Q runs directly through the center, and the 2 and 3 lines service the west side of Manhattan before curving eastwards to Central Harlem.",
                                          style = "line-height: 150%"),
                                       h4("The paucity of public transportation options running horizontally across Manhattan is a well-documented source of frustration amongst New Yorkers. Unlike in Astoria, Citi Bikes are not used to improve accessibility to the nearest subway station, but to enhance connectivity to the entire subway network in New York City. A simple 10 minute cycle can mean the difference between a comfortable ride direct to one's destination, and a troublesome journey with time-consuming midtown transfers.",
                                          style = "line-height: 150%"),
                                       offset = 1),
                                column(5,
                                       div(style="margin-left:60px",
                                           pickerInput(inputId = 'ehl_vars', label = 'Select data to view',
                                                       choices = list('Walkability',
                                                                      'Citi Bike Stations',
                                                                      'Citi Bike Routes'),
                                                       selected = NULL,
                                                       options = pickerOptions(actionsBox = TRUE, size = 10,
                                                                               selectedTextFormat = "values",
                                                                               noneSelectedText = 'No data selected'), 
                                                       multiple = TRUE)),
                                       leafletOutput("eharlem_map", height = "600px", width = "550px")
                                )
                              )
                          )
                 ),
                 tabPanel("Networks",
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(6,
                                       h1("Citi Bikes as Networks"),
                                       offset = 1)
                              ),
                              fluidRow(
                                column(10,
                                       style = "max-height: 80vh; overflow-y: auto;",
                                       h2("Intermodality of public transportation",
                                          style = "line-height: 150%"),
                                       h4("Both exhibits of Astoria and East Harlem illustrate the intermodality that is inherent in highly urbanized public transportation networks. It is simply unfeasible for a city to rely on only one mode of transportation to get everyone where they want to go.",
                                          style = "line-height: 150%"),
                                       h4("A common thread between both is the flexible use of Citi Bikes to overcome inefficiencies in the subway system that planners did not anticipate. Instead of transferring to a different subway line where the walking or cycling distance might be shorter, many commuters simply opted to cycle home directly from a subway line they were already on, even if this meant cycling further. We saw this with the trips from the Queens Plaza subway station and the terminal station of the Q line (96th St and 2nd Ave). East Harlem further demonstrates how intermodality is not just about simple accessibility, but also about expanding the commuters' options.",
                                          style = "line-height: 150%"),
                                       h2("Data production and representation",
                                          style = "line-height: 150%"),
                                       h4("Hence, the production of Citi Bike trip data is fundamentally incomplete in providing us a picture of how people actually integrate Citi Bikes into their transportation choices. Without a clear understanding of how local communities use these bikes as part of their daily commutes, analyzing the Citi Bike data will only provide a narrow glimpse of the possibilities that Citi Bike actually opens up for people on the ground. In the end, this will only hamper the efforts of urban planners in formulating a clear vision of an intermodal public transportation network for the city.",
                                          style = "line-height: 150%"),
                                       h4("Moreover, in this respect, the representation of Citi Bike trip data also leaves much to be desired. While a spatial representation is useful, it might not be the best visualization method to foster a deeper understanding of how individual Citi Bike stations fit into neighborhood-level networks, and in turn how these neighborhoods relate to each other as a citywide network.",
                                          style = "line-height: 150%"),
                                       h2("Citi Bikes as a relational network",
                                          style = "line-height: 150%"),
                                       h4("To do this, I reconceptualize the Citi Bike station network as a relational, rather than spatial, network of stations. This helps us focus better on the stations that are most crucial to the network, be it in terms of centrality or in terms of their links between sub-networks. The hope is that it will enable us to understand the Citi Bike network not merely as physical infrastructure, but more importantly, as a network defined by how the people who live in the city use it.",
                                          style = "line-height: 150%"),
                                       h4("I present the interactive network graph below as an example of how this may be useful. Each station in the network visualization is represented by a node, and the size of the node corresponds to the number of trips which begin or end at that particular station. Each line signifies that at least one trip was made between the two stations - the length of the line does not represent anything in this visualization. Hovering over any of the nodes in the network visualization will highlight them on the interactive map on the right, allowing you to see how the spatial and relational networks differ.",
                                          style = "line-height: 150%"),
                                       h4("Take some time to explore the visualizations below.",
                                          style = "line-height: 150%"),
                                       fluidRow(
                                         column(6,
                                                plotlyOutput("net_plot", height = "700px"),
                                                offset = 1),
                                         column(5,
                                                div(style="margin-left:40px",
                                                    pickerInput(inputId = 'net_vars', label = 'Select data to view',
                                                                choices = list('Walkability',
                                                                               'Citi Bike Routes'),
                                                                selected = 'Walkability',
                                                                options = pickerOptions(actionsBox = TRUE, size = 10,
                                                                                        selectedTextFormat = "values",
                                                                                        noneSelectedText = 'No data selected'), 
                                                                multiple = TRUE)),
                                                    leafletOutput("network_map", height = "600px", width = "550px")
                                                )
                                       ),
                                       offset = 1)
                              )
                          )
                 ),
                 tabPanel("Explore",
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(9,
                                       h1("Exploring the City's Networks"),
                                       offset = 1)
                              ),
                              fluidRow(
                                column(10,
                                       h4("You can explore the data for the entire New York City here, presented in the same form as you saw earlier. You can select which variable to color the network graph by - by borough, by gender (% utilized by women), and by median household income (by quartile). Have a look for yourself at the various networks within the larger city network of Citi Bikes.",
                                          style = "line-height: 150%"),
                                       offset = 1)
                              ),
                              fluidRow(
                                column(3,
                                       radioButtons(inputId = 'variable_choice', label = "Select variable to examine", 
                                                    inline = TRUE,
                                                    choices = list('Borough', 'Gender', "Median Income"),
                                                    selected = 'Borough'),
                                       offset = 2),
                                column(3,
                                       pickerInput(inputId = 'select_lines', label = 'Select subway lines',
                                                   choices = list('1', '2', '3', '4', '5', '6', '7',
                                                                  'A', 'B', 'C', 'D', 'E', 'F', 'G',
                                                                  'J', 'L', 'M', 'N', 'Q', 'R',
                                                                  'S', 'W', 'Z'),
                                                   selected = NULL,
                                                   options = pickerOptions(actionsBox = TRUE, size = 10,
                                                                           selectedTextFormat = "values",
                                                                           noneSelectedText = 'No lines selected'), 
                                                   multiple = TRUE)
                                ),
                                column(3,
                                       radioButtons(inputId = 'ct_choice', label = "Display median income data", 
                                                    inline = TRUE,
                                                    choices = list('Yes', 'No'),
                                                    selected = 'No')
                                )
                              ),
                              fluidRow(
                                style = "max-height: 80vh; overflow-y: auto;",
                                column(7,
                                       plotlyOutput("plot", height = "500px")
                                ),
                                column(5,
                                       leafletOutput("map", height = "500px")
                                )
                              )
                          )
                 ),
                 tabPanel("References",
                          div(class='outer',
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                column(9,
                                       h1("References"),
                                       offset = 1)),
                              fluidRow(
                                column(9,
                                       h4("Hughes, C. J. (2017) Discovering the Lost Coast of Queens. The New York Times. Retrieved April 2019 from https://www.nytimes.com/2017/02/03/realestate/discovering-the-lost-coast-of-queens.html"),
                                       h4("Jamerson, J. (2015) Citi Bike to Begin Service in Queens and Expand in Brooklyn and Manhattan. The New York Times. Retrieved April 2019 from https://www.nytimes.com/2015/07/25/us/politics/citi-bike-to-begin-service-in-queens-and-expand-in-brooklyn-and-manhattan.html"),
                                       h4("Fitzsimmons, E. G. (2016) Citi Bike Under Pressure to Expand to Low-Income Neighborhoods. The New York Times. Retrieved April 2019 from https://www.nytimes.com/2016/12/05/nyregion/citi-bike-may-need-public-funding-to-reach-more-new-yorkers.html"),
                                       h4("Gebhardt, L., Krajzewicz, D., Oostendorp, R., Goletz, M., Greger, K., Klötzke, M., … (2016) Intermodal Urban Mobility: Users, Uses, and Use Cases. Transportation Research Procedia, 14 (1), 1183 – 1992."),
                                       h4("Gebhardt, L., Krajzewicz, D., Oostendorp, R. (2017) Intermodality – key to a more efficient urban transport system? ECEEE Summer Study Proceedings."),
                                       offset = 1)
                              )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  stations <- readRDS('stations.rds')
  ggnet <- readRDS("ggnet.rds")
  tripsum <- readRDS("tripsum.rds")
  controls <- readRDS('controls.rds')
  ct2010_geojson <- geojson_read('ct2010_geojson.geojson', method = 'local', what = 'sp')
  sublines_geojson <- geojson_read('sublines_geojson.geojson', method = 'local', what = 'sp')
  final_routes <- readRDS("final_routes.rds")
  astoria_stn_details <- readRDS("astoria_stn_details.rds")
  final_e_routes <- readRDS("final_e_routes.rds")
  eharlem_stn_details <- readRDS("eharlem_stn_details.rds")
  subways <- readRDS("subways.rds")
  ehl_net <- readRDS("ehl_net.rds")
  
  output$plot <- renderPlotly({
    if (input$variable_choice == "Borough") {
      ggnetplot <- ggplot(ggnet, aes(text1 = Name,
                                     text2 = ID,
                                     text3 = Trips)) +
        geom_edges(aes(x = x, y = y, xend = xend, yend = yend),
                   color = "white", size = 0.2, 
                   alpha = 0.3) + #, curvature = 0.2
        geom_nodes(aes(x = x, y = y, 
                       size = Trips, fill = borough),
                   shape = 21, color = "white", stroke = .1, alpha = .85) +
        scale_size(range = c(1.5, 5)) +
        labs(color = "Borough") +
        theme_blank() +
        theme(plot.background = element_rect(fill = "#292929"),
              panel.background = element_blank(),
              legend.position = "bottom",
              legend.background = element_rect(fill = "#292929"),
              legend.text = element_text(color = "white")
              ) 
    }
    if (input$variable_choice == "Gender") {
      ggnetplot <- ggplot(ggnet, aes(text1 = Name,
                                     text2 = ID,
                                     text3 = Trips)) +
        geom_edges(aes(x = x, y = y, xend = xend, yend = yend),
                   color = "white", size = 0.2, 
                   alpha = 0.3) + #, curvature = 0.2
        geom_nodes(aes(x = x, y = y, 
                       size = Trips, fill = gender),
                   shape = 21, color = "white", stroke = .1) +
        scale_size(range = c(1.5, 5)) +
        scale_fill_brewer(palette = "Reds") +
        labs(color = "% of female cyclists") +
        theme_blank() +
        theme(plot.background = element_rect(fill = "#292929"),
              panel.background = element_blank(),
              legend.position = "bottom",
              legend.background = element_rect(fill = "#292929"),
              legend.text = element_text(color = "white")
              ) 
    }
    if (input$variable_choice == "Median Income") {
      ggnetplot <- ggplot(ggnet, aes(text1 = Name,
                                     text2 = ID,
                                     text3 = Trips)) +
        geom_edges(aes(x = x, y = y, xend = xend, yend = yend),
                   color = "white", size = 0.2, 
                   alpha = 0.3) + 
        geom_nodes(aes(x = x, y = y, 
                       size = Trips, fill = medinc),
                   shape = 21, color = "white", stroke = .1) +
        scale_size(range = c(1.5, 5)) +
        scale_fill_brewer(palette = "Reds") +
        labs(color = "Quartile of median income") +
        theme_blank() +
        theme(plot.background = element_rect(fill = "#292929"),
              panel.background = element_blank(),
              legend.position = "bottom",
              legend.background = element_rect(fill = "#292929"),
              legend.text = element_text(color = "white")
              ) 
    }
    ggplotly(ggnetplot, tooltip = c('text1', 'text2', 'text3'), source = "fullplot") %>%
      layout(plot_bgcolor='#292929') %>% 
      layout(paper_bgcolor='#292929')
  })
  
  output$net_plot <- renderPlotly({
    ehl_net_plot <- ggplot(ehl_net, aes(text1 = Name,
                                      text2 = ID,
                                      text3 = Trips)) +
      geom_edges(aes(x = x, y = y, xend = xend, yend = yend),
                 color = "white", size = 0.7, 
                 alpha = 0.3) + 
      geom_nodes(aes(x = x, y = y, size = Trips),
                 fill = "#ea6a54",
                 shape = 21, color = "white", stroke = .1) +
      scale_size(range = c(1.5, 6)) +
      theme_blank() +
      theme(plot.background = element_rect(fill = "black"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.background = element_rect(fill = "#292929"),
            legend.text = element_text(color = "white")
            ) 
    ggplotly(ehl_net_plot, tooltip = c('text1', 'text2', 'text3'), source = "networkplot") %>%
      layout(plot_bgcolor='#292929') %>% 
      layout(paper_bgcolor='#292929')
  })
  
  selected <- eventReactive(event_data("plotly_hover", source = "fullplot"), {
    click_data <- unlist(event_data("plotly_hover", source = "fullplot"), use.names = FALSE)
    ggnet %>% 
      filter((round(x, 5) == round(click_data[3], 5)) & (round(y, 5) == round(click_data[4], 5))) %>%
      distinct(ID)
  })
  
  selected2 <- eventReactive(event_data("plotly_hover", source = "networkplot"), {
    click_data <- unlist(event_data("plotly_hover", source = "networkplot"), use.names = FALSE)
    ehl_net %>% 
      filter((round(x, 5) == round(click_data[3], 5)) & (round(y, 5) == round(click_data[4], 5))) %>%
      distinct(ID)
  })
  
  linked <- eventReactive(selected(), {
    tripsum %>%
      filter(start_id == selected()$ID[1] | end_id == selected()$ID[1]) %>%
      select(start_id, end_id) %>%
      unlist(use.names = FALSE) %>%
      base::Filter(function(x) x != selected()$ID[1], .)
  })
  
  linked2 <- eventReactive(selected2(), {
    tripsum %>%
      filter(start_id == selected2()$ID[1] | end_id == selected2()$ID[1]) %>%
      select(start_id, end_id) %>%
      unlist(use.names = FALSE) %>%
      base::Filter(function(x) x != selected2()$ID[1], .)
  })
  
  sublines_subset <- reactive({
    sublines_geojson[str_detect(sublines_geojson@data$name, paste(input$select_lines, collapse="|")),]
  })
  
  output$map <- renderLeaflet({
    ny_lng = -73.994177
    ny_lat = 40.731030
    ct_stats_pal <- colorNumeric(palette = 'Reds', domain = ct2010_geojson$medianinc)
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ny_lng + 0.02, lat = ny_lat, zoom = 12) %>%
      setMaxBounds(lng1 = ny_lng+0.2, lat1 = ny_lat-0.1,
                   lng2 = ny_lng-0.2, lat2 = ny_lat+0.1) %>%
      addMapPane('cb', zIndex = 450) %>%
      addMapPane('sub', zIndex = 449) %>%
      addMapPane('ct', zIndex = 448) %>%
      addMapPane('highlight_point', zIndex = 451) %>%
      addCircles(lng = stations$stn_lon, lat = stations$stn_lat,
                 radius = 50, stroke = FALSE, color = '#21f9ff', weight = 2,
                 fillColor = '#255dd6', fillOpacity = 0.7,
                 group = 'Citi Bike Stations',
                 options = leafletOptions(pane = 'cb')) %>%
      addPolygons(data = ct2010_geojson, weight = 0, color = ~ct_stats_pal(ct2010_geojson$medianinc),
                  group = 'Median Income', opacity = 0.3, fillOpacity = 0.15,
                  popup = ~paste0("<b>NTA Name</b>: ", ntaname, "<br/>",
                                  "<b>Median Income</b>: ", ct2010_geojson$medianinc),
                  options = leafletOptions(pane = 'ct')) %>%
      hideGroup('Median Income') %>%
      addLayersControl(overlayGroups = c('Citi Bike Stations', 'Subway Lines', 'Median Income', 'Citi Bike Station Highlights'))
  })
  
  output$astoria_map <- renderLeaflet({
    ast_lng = -73.927523
    ast_lat = 40.764716
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ast_lng, lat = ast_lat, zoom = 14) %>%
      setMaxBounds(lng1 = ast_lng+0.07, lat1 = ast_lat-0.03,
                   lng2 = ast_lng-0.07, lat2 = ast_lat+0.03) %>%
      addMapPane('cb', zIndex = 450) %>%
      addMapPane('sub', zIndex = 449) %>%
      addMapPane('routes', zIndex = 448) %>%
      addMapPane('walkable', zIndex = 447) %>%
      addLayersControl(overlayGroups = c('Citi Bike Stations', 'Subway Lines', 'Walkability', 'Citi Bike Routes')) %>%
      addCircles(data = astoria_stn_details,
                 lat = astoria_stn_details$stn_lat, lng = astoria_stn_details$stn_lon,
                 radius = ~sqrt(count/3.141593)*12, stroke = TRUE, color = "transparent", weight = 1,
                 fillColor = '#255dd6', fillOpacity = .9,
                 group = "Citi Bike Stations",
                 options = leafletOptions(pane = 'cb'),
                 popup = ~paste0("<b>Station Name</b>: ", start_name, "<br/>",
                                 "<b>Number of trips originating</b>: ", count),
                 highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)) %>%
      addCircles(data = subways,
                 lat = subways$stop_lat, lng = subways$stop_lon,
                 stroke = FALSE, fillColor = "green", fillOpacity = .4,
                 radius = 400, group = "Walkability",
                 options = leafletOptions(pane = "walkable")) %>%
      addPolylines(data = sublines_geojson,
                   weight = 5, opacity = 0.5, color = 'yellow',
                   options = leafletOptions(pane = 'sub'),
                   group = "Subway Lines") %>%
      addPolylines(data = final_routes, color = "white",
                   weight = ~count/8,
                   opacity = 0.3, group = "Citi Bike Routes",
                   popup = ~paste0("<b>Start Station</b>: ", start_name, "<br/>",
                                   "<b>End Station</b>: ", end_name, "<br/>",
                                   "<b>Number of trips</b>: ", count),
                   options = leafletOptions(pane = 'routes'),
                   highlightOptions = highlightOptions(color = "red", bringToFront = TRUE)) %>%
      hideGroup('Citi Bike Stations') %>%
      hideGroup('Citi Bike Routes')
    
  })
  
  output$eharlem_map <- renderLeaflet({
    ehl_lng = -73.944503
    ehl_lat = 40.796859 
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ehl_lng, lat = ehl_lat, zoom = 14) %>%
      setMaxBounds(lng1 = ehl_lng+0.07, lat1 = ehl_lat-0.03,
                   lng2 = ehl_lng-0.07, lat2 = ehl_lat+0.03) %>%
      addMapPane('cb', zIndex = 450) %>%
      addMapPane('sub', zIndex = 449) %>%
      addMapPane('routes', zIndex = 448) %>%
      addMapPane('walkable', zIndex = 447) %>%
      addLayersControl(overlayGroups = c('Citi Bike Stations', 'Subway Lines', 'Walkability', 'Citi Bike Routes')) %>%
      addCircles(data = eharlem_stn_details,
                 lat = eharlem_stn_details$stn_lat, lng = eharlem_stn_details$stn_lon,
                 radius = ~sqrt(count/3.141593)*12, stroke = TRUE, color = "transparent", weight = 1,
                 fillColor = '#255dd6', fillOpacity = .9,
                 group = "Citi Bike Stations",
                 options = leafletOptions(pane = 'cb'),
                 popup = ~paste0("<b>Station Name</b>: ", start_name, "<br/>",
                                 "<b>Number of trips originating</b>: ", count),
                 highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)) %>%
      addCircles(data = subways,
                 lat = subways$stop_lat, lng = subways$stop_lon,
                 stroke = FALSE, fillColor = "green", fillOpacity = .4,
                 radius = 400, group = "Walkability",
                 options = leafletOptions(pane = "walkable")) %>%
      addPolylines(data = sublines_geojson,
                   weight = 5, opacity = 0.5, color = 'yellow',
                   options = leafletOptions(pane = 'sub'),
                   group = "Subway Lines",
                   popup = ~paste0("<b>Lines</b>: ", name)) %>%
      addPolylines(data = final_e_routes, color = "white",
                   weight = ~count/8,
                   opacity = 0.3, group = "Citi Bike Routes",
                   popup = ~paste0("<b>Start Station</b>: ", start_name, "<br/>",
                                   "<b>End Station</b>: ", end_name, "<br/>",
                                   "<b>Number of trips</b>: ", count),
                   options = leafletOptions(pane = 'routes'),
                   highlightOptions = highlightOptions(color = "red", bringToFront = TRUE)) %>%
      hideGroup('Citi Bike Stations') %>%
      hideGroup('Citi Bike Routes')
  })
  
  output$network_map <- renderLeaflet({
    ehl_lng = -73.944503
    ehl_lat = 40.796859 
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ehl_lng, lat = ehl_lat, zoom = 14) %>%
      setMaxBounds(lng1 = ehl_lng+0.07, lat1 = ehl_lat-0.03,
                   lng2 = ehl_lng-0.07, lat2 = ehl_lat+0.03) %>%
      addMapPane('cb', zIndex = 451) %>%
      addMapPane('sub', zIndex = 449) %>%
      addMapPane('routes', zIndex = 448) %>%
      addMapPane('walkable', zIndex = 447) %>%
      addMapPane('highlight_point', zIndex = 450) %>%
      addLayersControl(overlayGroups = c('Citi Bike Stations', 'Citi Bike Station Highlights', 'Walkability', 'Subway Lines', 'Citi Bike Routes')) %>%
      addCircles(data = eharlem_stn_details,
                 lat = eharlem_stn_details$stn_lat, lng = eharlem_stn_details$stn_lon,
                 radius = ~sqrt(count/3.141593)*12, stroke = TRUE, color = "transparent", weight = 1,
                 fillColor = '#255dd6', fillOpacity = .9,
                 group = "Citi Bike Stations",
                 options = leafletOptions(pane = 'cb'),
                 popup = ~paste0("<b>Station Name</b>: ", start_name, "<br/>",
                                 "<b>Number of trips originating</b>: ", count),
                 highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)) %>%
      addCircles(data = subways,
                 lat = subways$stop_lat, lng = subways$stop_lon,
                 stroke = FALSE, fillColor = "green", fillOpacity = .4,
                 radius = 400, group = "Walkability",
                 options = leafletOptions(pane = "walkable")) %>%
      addPolylines(data = sublines_geojson,
                   weight = 5, opacity = 0.5, color = 'yellow',
                   options = leafletOptions(pane = 'sub'),
                   group = "Subway Lines",
                   popup = ~paste0("<b>Lines</b>: ", name)) %>%
      addPolylines(data = final_e_routes, color = "white",
                   weight = ~count/8,
                   opacity = 0.3, group = "Citi Bike Routes",
                   popup = ~paste0("<b>Start Station</b>: ", start_name, "<br/>",
                                   "<b>End Station</b>: ", end_name, "<br/>",
                                   "<b>Number of trips</b>: ", count),
                   options = leafletOptions(pane = 'routes'),
                   highlightOptions = highlightOptions(color = "red", bringToFront = TRUE)) %>%
      hideGroup('Citi Bike Routes')
  })
  
  observeEvent(input$ct_choice, {
    if (input$ct_choice == "Yes") {
      leafletProxy('map') %>%
        showGroup('Median Income')
    } else {
      leafletProxy('map') %>%
        hideGroup('Median Income')
    }
  })
  
  observeEvent(input$ast_vars, {
    if (any(str_detect(input$ast_vars, "Citi Bike Stations"))) {
      leafletProxy('astoria_map') %>%
        showGroup('Citi Bike Stations')
    } else {
      leafletProxy('astoria_map') %>%
        hideGroup('Citi Bike Stations')
    }
    if (any(str_detect(input$ast_vars, "Walkability"))) {
      leafletProxy('astoria_map') %>%
        showGroup('Walkability')
    } else {
      leafletProxy('astoria_map') %>%
        hideGroup('Walkability')
    }
    if (any(str_detect(input$ast_vars, "Citi Bike Routes"))) {
      leafletProxy('astoria_map') %>%
        showGroup('Citi Bike Routes')
    } else {
      leafletProxy('astoria_map') %>%
        hideGroup('Citi Bike Routes')
    }
  },
  ignoreNULL = FALSE
  )
  
  observeEvent(input$ehl_vars, {
    if (any(str_detect(input$ehl_vars, "Citi Bike Stations"))) {
      leafletProxy('eharlem_map') %>%
        showGroup('Citi Bike Stations')
    } else {
      leafletProxy('eharlem_map') %>%
        hideGroup('Citi Bike Stations')
    }
    if (any(str_detect(input$ehl_vars, "Walkability"))) {
      leafletProxy('eharlem_map') %>%
        showGroup('Walkability')
    } else {
      leafletProxy('eharlem_map') %>%
        hideGroup('Walkability')
    }
    if (any(str_detect(input$ehl_vars, "Citi Bike Routes"))) {
      leafletProxy('eharlem_map') %>%
        showGroup('Citi Bike Routes')
    } else {
      leafletProxy('eharlem_map') %>%
        hideGroup('Citi Bike Routes')
    }
  },
  ignoreNULL = FALSE
  )
  
  observeEvent(input$net_vars, {
    if (any(str_detect(input$net_vars, "Walkability"))) {
      leafletProxy('network_map') %>%
        showGroup('Walkability')
    } else {
      leafletProxy('network_map') %>%
        hideGroup('Walkability')
    }
    if (any(str_detect(input$net_vars, "Citi Bike Routes"))) {
      leafletProxy('network_map') %>%
        showGroup('Citi Bike Routes')
    } else {
      leafletProxy('network_map') %>%
        hideGroup('Citi Bike Routes')
    }
  },
  ignoreNULL = FALSE
  )
  
  observeEvent(input$select_lines,
               {
                 leafletProxy('map') %>%
                   clearGroup('Subway Lines') 
                 req(input$select_lines)
                 leafletProxy('map') %>%
                   addPolylines(
                     data = sublines_subset(),
                     weight = 3,
                     opacity = 0.3,
                     color = 'yellow',
                     group = 'Subway Lines',
                     options = leafletOptions(pane = 'sub')
                   )
               },
               ignoreNULL = FALSE
  )
  
  observeEvent(linked(),
               {
                 print(selected())
                 selected_stn <- stations %>%
                   filter(stn_id == selected()$ID[1])
                 linked_stns <- stations %>%
                   filter(stn_id %in% linked())
                 leafletProxy('map') %>%
                   clearGroup('Citi Bike Station Highlights') %>%
                   addCircles(lat = selected_stn$stn_lat, lng = selected_stn$stn_lon,
                              radius = 80, stroke = TRUE, color = '#c91717', weight = 2,
                              fillColor = '#c91717', fillOpacity = 1,
                              group = 'Citi Bike Station Highlights',
                              options = leafletOptions(pane = 'highlight_point')) %>%
                   addCircles(lat = linked_stns$stn_lat, lng = linked_stns$stn_lon,
                              radius = 65, stroke = TRUE, color = '#FFFFFF', weight = 2,
                              fillColor = '#FFFFFF', fillOpacity = 1,
                              group = 'Citi Bike Station Highlights',
                              options = leafletOptions(pane = 'highlight_point')
                   )
               }
  )
  
  observeEvent(linked2(),
               {
                 print(selected2())
                 selected_stn <- eharlem_stn_details %>%
                   filter(stn_id == selected2()$ID[1])
                 if (nrow(selected_stn) == 0) {
                   selected_stn <- stations %>%
                     filter(stn_id == selected()$ID[1])
                   leafletProxy('network_map') %>%
                     clearGroup('Citi Bike Station Highlights') %>%
                     addCircles(data = selected_stn,
                                lat = selected_stn$stn_lat, lng = selected_stn$stn_lon,
                                radius = 50, stroke = TRUE, color = '#c91717', weight = 2,
                                fillColor = '#c91717', fillOpacity = 1,
                                group = 'Citi Bike Station Highlights',
                                options = leafletOptions(pane = 'highlight_point'))
                 } else {
                   leafletProxy('network_map') %>%
                     clearGroup('Citi Bike Station Highlights') %>%
                     addCircles(data = selected_stn,
                                lat = selected_stn$stn_lat, lng = selected_stn$stn_lon,
                                radius = ~(sqrt(selected_stn$count/3.141593)*12)+20, stroke = TRUE, color = '#c91717', weight = 2,
                                fillColor = '#c91717', fillOpacity = 1,
                                group = 'Citi Bike Station Highlights',
                                options = leafletOptions(pane = 'highlight_point'))
                 }
                 req(linked2())
                 linked_stns <- eharlem_stn_details %>%
                   filter(stn_id %in% linked2())
                 req(linked_stns)
                 leafletProxy('network_map') %>%
                   addCircles(data = linked_stns,
                              lat = linked_stns$stn_lat, lng = linked_stns$stn_lon,
                              radius = ~(sqrt(linked_stns$count/3.141593)*12)+20, stroke = TRUE, color = '#ffac44', weight = 2,
                              fillColor = '#ffac44', fillOpacity = 1,
                              group = 'Citi Bike Station Highlights',
                              options = leafletOptions(pane = 'highlight_point'))
               }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

