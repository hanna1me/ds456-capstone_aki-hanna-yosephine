library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(DT)
library(leaflet)
library(bslib)
library(sf)

# ---- Load precomputed, small objects ----

behavior_counts        <- readRDS("data/app/app_behavior_counts.rds")
hourly_normalized      <- readRDS("data/app/app_hourly_normalized.rds")
hourly_share           <- readRDS("data/app/app_hourly_share.rds")
start_points_beh_small <- readRDS("data/app/start_points_beh_small.rds")

top_leaky         <- readRDS("data/app/app_top_leaky.rds")
priority_routes   <- readRDS("data/app/app_priority_routes.rds")
priority_gap_tracts  <- readRDS("data/app/app_priority_gap_tracts.rds")
priority_sub_tracts  <- readRDS("data/app/app_priority_sub_tracts.rds")
tracts_ll_small      <- readRDS("data/app/app_tracts_ll_small.rds")

same_route_small       <- readRDS("data/app/app_same_route_small.rds")
start_points_map_small <- readRDS("data/app/app_start_points_map_small.rds")
end_points_map_small   <- readRDS("data/app/app_end_points_map_small.rds")
stops_ll               <- readRDS("data/app/app_stops_ll.rds")

# ---- Defensive: ensure leaflet layers are POINT geometries ----

if (inherits(sf::st_geometry(start_points_map_small), "sfc_LINESTRING")) {
  start_points_map_small <- sf::st_centroid(start_points_map_small)
}
if (inherits(sf::st_geometry(end_points_map_small), "sfc_LINESTRING")) {
  end_points_map_small <- sf::st_centroid(end_points_map_small)
}

# tracts_ll_small should be POLYGON / MULTIPOLYGON; no change needed for ggplot/leaflet

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("E-Scooter Trips in Minneapolis: What do they say about micromobility & transit access?"),
  
  tabsetPanel(
    
    tabPanel(
      "Story overview",
      
      br(),
      h2("1. How are scooters being used overall?"),
      p(
        "We classify each scooter trip into one of five behaviour types based on whether the ",
        "road segment where it starts or ends is within 30 meters of a transit stop. ",
        "Because operators provide only anonymized segments (not precise GPS points) the analysis ",
        "uses buffered road centerlines. Proximity reflects the segment’s geometry, ",
        "not the rider's exact walking distance."
      ),
      p("Behavioural categories reflect how scooters interact with transit:"),
      tags$ul(
        tags$li(strong("none:"),       " segment not near a stop; riders likely beyond transit reach."),
        tags$li(strong("first_mile:")," ends near transit; riders likely heading into the network."),
        tags$li(strong("last_mile:"), " starts near transit; riders finishing trips after transit."),
        tags$li(strong("sub_multi:"), " both ends near stops but require transfers on transit."),
        tags$li(strong("sub_one:"),   " both ends near stops served by a single bus—trip could be replaced by one route.")
      ),
      p("The bar chart shows what share of scooter trips fall into each category."),
      plotOutput("plot_behavior_comp", height = "350px"),
      p(
        strong("How to read this: "),
        "bars add to 100%. Values of ",
        strong("sub_one"),
        " and ",
        strong("sub_multi"),
        " indicate scooters replacing what could be transit trips, while a high ",
        strong("none"),
        " bar suggests scooters filling genuine service gaps."
      ),
      
      br(), br(),
      
      h2("2. When do different scooter behaviours happen?"),
      p(
        "Scooter demand follows predictable time-of-day rhythms. To compare behaviours fairly, ",
        "the charts show average hourly values across all days, separately for weekdays and weekends."
      ),
      
      h4("Average hourly scooter trips by behavior type"),
      plotOutput("plot_hourly_trips", height = "480px"),
      p(
        strong("Key takeaway – volumes: "),
        "during the peak afternoon hours, weekday trips show both ",
        strong("none"),
        " and ",
        strong("last_mile"),
        " as the two dominant lines. This suggests riders are mostly using scooters to fill transit gaps ",
        "and to commute from the transit network to another destination (often home). ",
        "There is a notable ",
        strong("first_mile"),
        " peak around 8AM, aligning with work commutes from home into the transit network. ",
        "On weekends, a visible bump around 1AM reflects post-night-out trips, while an early evening rise ",
        "around 6–7PM suggests leisure-oriented travel."
      ),
      
      h4("Average hourly scooter trip share by behavior type"),
      plotOutput("plot_hourly_share", height = "480px"),
      p(
        strong("Key takeaway – shares: "),
        "around 6AM, ",
        strong("first-mile"),
        " temporarily becomes dominant on both weekdays and weekends—suggesting scooters are used to reach transit ",
        "at the start of morning commutes. In the weekday afternoon, ",
        strong("last-mile"),
        " overtakes as riders return home from the transit network. ",
        "The relative positions of the lines remain fairly stable, indicating that while total volumes change dramatically, ",
        "the proportional mix of behaviours is surprisingly consistent throughout the day."
      ),
      
      br(), br(),
      
      h2("3. How close are scooters to transit stops? (Limitations apply)"),
      p(
        "Distances here are from the centroid of the anonymized street segment to the nearest transit stop. ",
        "Because we buffer and classify segments within 30m of stops to capture all stop locations along a street, ",
        "the boxplot reflects those thresholds: many substitution and access trips cluster near the lower end. ",
        "On long residential blocks, a 300m measured distance may still correspond to a rider who was much closer to the stop."
      ),
      plotOutput("plot_dist_box", height = "360px"),
      p(
        strong("Interpretation: "),
        "behaviours tightly clustered around shorter distances are more transit-integrated. ",
        strong("Substitution trips (sub_one / sub_multi)"),
        " being closer to stops reinforces that they represent competition with transit rather than simple access."
      ),
      
      br(), br(),
      
      h2("4. Which bus routes are most 'leaky'?"),
      p(
        "We estimate which bus routes may be losing riders to scooters using ",
        code("leakiness = scooter_substitutions / route_boardings"),
        ". High leakiness does not necessarily mean poor performance, but it highlights corridors where ",
        "scooters are acting as substitutes rather than connectors."
      ),
      p(
        strong("Data disclaimer: "),
        "the boarding/alighting dataset may not include every stop or route in Minneapolis. ",
        "Leakiness is directional—useful for identifying candidates for further evaluation, ",
        "not for estimating exact revenue loss."
      ),
      plotOutput("plot_leaky_routes", height = "360px"),
      p(
        strong("Policy angle: "),
        "these routes represent low-cost opportunities to retain riders through improvements such as ",
        "stop amenities, signal priority, pricing alignment, or integrated micromobility at key stops."
      ),
      
      br(), br(),
      
      h2("5. Where in the city are substitutions and gaps concentrated?"),
      p(
        "Using precomputed tract-level statistics, we map two outcomes:"
      ),
      tags$ul(
        tags$li(strong("Substitution share:"), " proportion of trips that could be replaced by one bus route (sub_one)."),
        tags$li(strong("Gap share:"), " proportion of trips where neither end is near a stop (none).")
      ),
      h4("Where scooters most often duplicate a single bus route"),
      plotOutput("plot_sub_map", height = "420px"),
      h4("Where scooters operate beyond walking distance of transit"),
      plotOutput("plot_gap_map", height = "420px"),
      p(
        strong("Reading the maps: "),
        "bright areas show neighbourhoods where scooters either compete with or meaningfully ",
        "compensate for transit. Darker areas reflect lower scooter activity or behaviour closer to ",
        "the city average."
      ),
      
      br(), br(),
      
      h2("6. Who is most affected? Equity-focused priority scores"),
      p(
        "To move from patterns to action, we use precomputed ",
        strong("priority scores"),
        " that highlight neighbourhoods where interventions could have the greatest impact."
      ),
      
      h4("Neighbourhoods where scooters are replacing transit for many riders"),
      p(
        "Substitution priority favours tracts where many riders are choosing scooters instead of a bus AND large numbers of riders are present:"
      ),
      tags$ul(
        tags$li("High decile of substitution share (sub_share)"),
        tags$li("High decile of population"),
        tags$li("High decile of boardings density (more impact where riders already use transit)")
      ),
      code("sub_priority_score = decile(sub_share) + decile(population) + decile(boardings_density)"),
      br(), br(),
      DTOutput("table_priority_sub_tracts"),
      br(),
      
      h4("Neighbourhoods where scooters fill serious access gaps (low income + low car ownership)"),
      p(
        "Gap priority emphasizes equity-sensitive characteristics and transit deprivation:"
      ),
      tags$ul(
        tags$li(strong("High gap share:"),       " many scooter trips far from transit"),
        tags$li(strong("Low median income:"),    " fewer economic alternatives"),
        tags$li(strong("High zero-car share:"),  " heavy reliance on non-auto modes"),
        tags$li(strong("Low boardings density:")," transit access is physically poor")
      ),
      code("gap_priority_score = decile(gap_share) + decile(zero_car_share) + decile(-boardings_density) + decile(-median_income)"),
      br(), br(),
      DTOutput("table_priority_gap_tracts"),
      
      br(), br()
    ),
    
    tabPanel(
      "Substitution explorer",
      sidebarLayout(
        sidebarPanel(
          h4("Explore scooter trips that duplicate a single bus route"),
          p(
            "Use this tab to zoom in on a specific route, see where scooters start and end ",
            "along it, and inspect the underlying trip table (downsampled for performance)."
          ),
          selectInput(
            "route_select",
            "Choose a route to examine:",
            choices = NULL
          )
        ),
        mainPanel(
          leafletOutput("subMap", height = "600px"),
          br(),
          h4("Scooter trips on this route (sample)"),
          DTOutput("subTable")
        )
      )
    ),
    
    tabPanel(
      "Gap explorer",
      sidebarLayout(
        sidebarPanel(
          h4("Explore where scooters run far from transit"),
          p(
            "This tab lets us explore tracts where a large share of scooter trips ",
            "have no nearby transit stops."
          ),
          sliderInput(
            "min_gap_share",
            "Minimum gap trip share to highlight:",
            min   = 0,
            max   = 0.8,
            value = 0.2,
            step  = 0.05
          )
        ),
        mainPanel(
          leafletOutput("gapMap", height = "600px"),
          br(),
          p(
            "Use the slider to focus on the highest-gap areas. These are good candidates ",
            "for either improving transit coverage or investing in safer micromobility infrastructure."
          )
        )
      )
    )
    
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  observe({
    choices <- same_route_small$route_short_name
    choices <- sort(unique(choices[!is.na(choices)]))
    updateSelectInput(session, "route_select", choices = choices)
  })
  
  output$plot_behavior_comp <- renderPlot({
    ggplot(behavior_counts,
           aes(x = behavior_type, y = pct, fill = behavior_type)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_viridis_d(option = "plasma", end = 0.9) +
      labs(
        title = "Composition of scooter trip types",
        x = "Behavior type",
        y = "Share of all scooter trips"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  output$plot_hourly_trips <- renderPlot({
    ggplot(hourly_normalized,
           aes(x = hour, y = avg_trips,
               color = behavior_type, group = behavior_type)) +
      geom_line(size = 1.1) +
      facet_wrap(~ day_type, ncol = 1) +
      scale_x_continuous(breaks = 0:23) +
      scale_color_viridis_d(option = "plasma", end = 0.9) +
      labs(
        title = "Average hourly scooter trips by behavior type",
        x = "Hour of day",
        y = "Average trips per hour",
        color = "Behavior type"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$plot_hourly_share <- renderPlot({
    ggplot(hourly_share,
           aes(x = hour, y = avg_share,
               color = behavior_type, group = behavior_type)) +
      geom_line(size = 1.1) +
      facet_wrap(~ day_type, ncol = 1) +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_color_viridis_d(option = "plasma", end = 0.9) +
      labs(
        title = "Average hourly scooter trip share by behavior type",
        x = "Hour of day",
        y = "Average trip share per hour",
        color = "Behavior type"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$plot_dist_box <- renderPlot({
    ggplot(start_points_beh_small,
           aes(x = behavior_type, y = dist_to_nearest_stop)) +
      geom_boxplot(outlier.alpha = 0.2, fill = "#3182bd", alpha = 0.7) +
      scale_y_continuous(
        trans  = "log10",
        breaks = c(10, 30, 100, 300, 1000),
        labels = c("10 m", "30 m", "100 m", "300 m", "1 km")
      ) +
      labs(
        title = "Distance from scooter start (segment centroid) to nearest transit stop",
        x = "Behavior type",
        y = "Distance (log scale)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$plot_leaky_routes <- renderPlot({
    ggplot(top_leaky,
           aes(x = reorder(route_short_name, leakiness), y = leakiness)) +
      geom_col(fill = "#1f78b4") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title = "Top 10 'Leaky' Routes",
        subtitle = "Approximate share of riders choosing scooters over buses",
        x = "Route short name",
        y = "Leakiness (Scooter substitutions / Bus boardings)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$plot_sub_map <- renderPlot({
    ggplot() +
      geom_sf(data = tracts_ll_small,
              aes(fill = sub_share), color = NA) +
      scale_fill_viridis_c(
        option   = "magma",
        na.value = "grey90",
        labels   = percent_format(accuracy = 1),
        name     = "Substitution share"
      ) +
      labs(
        title = "Where scooters most often duplicate a single bus route"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.title = element_blank())
  })
  
  output$plot_gap_map <- renderPlot({
    ggplot() +
      geom_sf(data = tracts_ll_small,
              aes(fill = gap_share), color = NA) +
      scale_fill_viridis_c(
        option   = "magma",
        na.value = "grey90",
        labels   = percent_format(accuracy = 1),
        name     = "Gap share"
      ) +
      labs(
        title = "Where scooters operate beyond walking distance of transit"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.title = element_blank())
  })
  
  output$table_priority_sub_tracts <- renderDT({
    priority_sub_tracts |>
      mutate(
        sub_share = percent(sub_share, accuracy = 0.1)
      ) |>
      datatable(options = list(pageLength = 5))
  })
  
  output$table_priority_gap_tracts <- renderDT({
    priority_gap_tracts |>
      mutate(
        gap_share      = percent(gap_share,      accuracy = 0.1),
        zero_car_share = percent(zero_car_share, accuracy = 0.1)
      ) |>
      datatable(options = list(pageLength = 5))
  })
  
  output$subMap <- renderLeaflet({
    req(input$route_select)
    
    selected <- same_route_small |>
      filter(route_short_name == input$route_select)
    
    ids <- unique(selected$trip_id)
    
    starts_filtered <- start_points_map_small |>
      filter(trip_id %in% ids)
    
    ends_filtered <- end_points_map_small |>
      filter(trip_id %in% ids)
    
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addCircleMarkers(
        data        = starts_filtered,
        color       = "darkblue",
        radius      = 3,
        opacity     = 0.8,
        fillOpacity = 0.8,
        popup       = ~paste("Trip ID:", trip_id)
      ) |>
      addCircleMarkers(
        data        = ends_filtered,
        color       = "violet",
        radius      = 3,
        opacity     = 0.8,
        fillOpacity = 0.8,
        popup       = ~paste("Trip ID:", trip_id)
      )
  })
  
  output$subTable <- renderDT({
    req(input$route_select)
    same_route_small |>
      filter(route_short_name == input$route_select) |>
      select(trip_id, route_id, route_short_name, route_long_name) |>
      distinct() |>
      datatable(options = list(pageLength = 10))
  })
  
  output$gapMap <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain  = tracts_ll_small$gap_share,
      na.color = "transparent"
    )
    
    tracts_filtered <- tracts_ll_small |>
      filter(gap_share >= input$min_gap_share)
    
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(
        data        = tracts_filtered,
        fillColor   = ~pal(gap_share),
        fillOpacity = 0.7,
        color       = NA
      ) |>
      addLegend(
        pal    = pal,
        values = tracts_ll_small$gap_share,
        title  = "Gap trip share"
      )
  })
  
}

shinyApp(ui, server)
