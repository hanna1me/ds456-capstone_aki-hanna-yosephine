library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(DT)
library(leaflet)
library(bslib)
library(sf)
library(httr)
library(jsonlite)
library(tidyr)


# ---- Load precomputed, small objects ----
behavior_counts        <- readRDS("data/app/app_behavior_counts.rds")
hourly_normalized      <- readRDS("data/app/app_hourly_normalized.rds")
hourly_share           <- readRDS("data/app/app_hourly_share.rds")
start_points_beh_small <- readRDS("data/app/start_points_beh_small.rds")

top_leaky            <- readRDS("data/app/app_top_leaky.rds")
priority_routes      <- readRDS("data/app/app_priority_routes.rds")
priority_gap_tracts  <- readRDS("data/app/app_priority_gap_tracts.rds")
priority_sub_tracts  <- readRDS("data/app/app_priority_sub_tracts.rds")
tracts_ll_small      <- readRDS("data/app/app_tracts_ll_small.rds")

same_route_small       <- readRDS("data/app/app_same_route_small.rds")
start_points_map_small <- readRDS("data/app/app_start_points_map_small.rds")
end_points_map_small   <- readRDS("data/app/app_end_points_map_small.rds")

stops_ll <- readRDS("data/app/app_stops_ll.rds")
stops_ll <- stops_ll %>%
  mutate(
    stop_lon = sf::st_coordinates(geometry)[, 1],
    stop_lat = sf::st_coordinates(geometry)[, 2]
  )

time_by_behavior <- readRDS("data/app/app_time_by_behavior.rds")
time_diff_dist   <- readRDS("data/app/app_time_diff_dist.rds")

# Precomputed travel-time summary tables
summary_stats  <- readRDS("data/app/summary_stats.rds")
fastest_counts <- readRDS("data/app/fastest_mode_counts.rds")
final_table    <- readRDS("data/app/final_comparison_table.rds")

# ---- small helper for NULL-safe values ----
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Google Directions helpers ----
google_directions_one_mode <- function(origin_lat, origin_lon,
                                       dest_lat, dest_lon,
                                       mode = c("bicycling", "transit")) {
  mode <- match.arg(mode)
  key  <- Sys.getenv("GOOGLE_MAPS_KEY")
  
  if (key == "") {
    return(
      tibble::tibble(
        mode         = mode,
        duration_min = NA_real_,
        distance_km  = NA_real_,
        summary      = "GOOGLE_MAPS_KEY is empty – set Sys.setenv(GOOGLE_MAPS_KEY = '...')"
      )
    )
  }
  
  # guard against NA coords
  if (any(is.na(c(origin_lat, origin_lon, dest_lat, dest_lon)))) {
    return(
      tibble::tibble(
        mode         = mode,
        duration_min = NA_real_,
        distance_km  = NA_real_,
        summary      = "Origin or destination coordinates are NA"
      )
    )
  }
  
  # guard against NULL coords
  if (any(vapply(list(origin_lat, origin_lon, dest_lat, dest_lon), is.null, logical(1)))) {
    return(tibble::tibble(
      mode = mode, duration_min = NA_real_, distance_km = NA_real_,
      summary = "Origin/destination coords are NULL (stop lookup failed)"
    ))
  }
  
  base_url <- "https://maps.googleapis.com/maps/api/directions/json"
  
  res <- httr::GET(
    url = base_url,
    query = list(
      origin      = paste(origin_lat, origin_lon, sep = ","),
      destination = paste(dest_lat,  dest_lon,  sep = ","),
      mode        = mode,
      key         = key
    )
  )
  
  x <- httr::content(res, as = "parsed", type = "application/json")
  api_status <- x$status %||% httr::http_status(res)$reason
  api_error  <- x$error_message %||% ""
  
  if (is.null(x$status) || x$status != "OK") {
    return(
      tibble::tibble(
        mode         = mode,
        duration_min = NA_real_,
        distance_km  = NA_real_,
        summary      = paste("status:", api_status, "| error:", api_error)
      )
    )
  }
  
  leg <- x$routes[[1]]$legs[[1]]
  
  duration_min  <- leg$duration$value  / 60   # seconds -> minutes
  distance_km   <- leg$distance$value / 1000  # meters  -> km
  route_summary <- x$routes[[1]]$summary %||% NA_character_
  
  tibble::tibble(
    mode         = mode,
    duration_min = duration_min,
    distance_km  = distance_km,
    summary      = route_summary
  )
}

get_google_trip_times <- function(origin_lat, origin_lon,
                                  dest_lat, dest_lon) {
  dplyr::bind_rows(
    google_directions_one_mode(origin_lat, origin_lon, dest_lat, dest_lon, mode = "bicycling"),
    google_directions_one_mode(origin_lat, origin_lon, dest_lat, dest_lon, mode = "transit")
  )
}

# ---- Defensive: ensure leaflet layers are POINT geometries ----
if (inherits(sf::st_geometry(start_points_map_small), "sfc_LINESTRING")) {
  start_points_map_small <- sf::st_centroid(start_points_map_small)
}
if (inherits(sf::st_geometry(end_points_map_small), "sfc_LINESTRING")) {
  end_points_map_small <- sf::st_centroid(end_points_map_small)
}

# ---- Global plot theme & palettes ----

beh_pal <- c(
  "none"       = "#1b9e77",
  "first_mile" = "#d95f02",
  "last_mile"  = "#7570b3",
  "sub_multi"  = "#e7298a",
  "sub_one"    = "#66a61e"
)

pal_sub <- c("#ffffb3", "#fb8072")  # substitution gradient
pal_gap <- c("#ffffb3", "#80b1d3")  # gap gradient

theme_set(
  theme_bw(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "#f9f4ec", color = NA),
      plot.background  = element_rect(fill = "#f9f4ec", color = NA),
      panel.grid.major = element_line(color = "#e1e1e1", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold"),
      legend.title     = element_text(face = "bold", align = "center"),
      legend.text      = element_text(size = 13),
      legend.background = element_rect(fill = "#f9f4ec",color = NA),
      strip.background = element_rect(fill = "#f5f1e8", color = NA),
      strip.text       = element_text(face = "bold"),
      axis.text        = element_text(color = "#333333", size = 13),
      axis.title       = element_text(color = "#333333", size = 14)
    )
)



# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(
    version   = 4,
    bootswatch = "sandstone",
    base_font = font_google("Source Sans Pro"),
    bg        = "#f9f4ec",  
    fg        = "#222222"
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        font-size: 18px;
      }
      h2 {
        margin-top: 32px;
        margin-bottom: 8px;
        font-weight: 800;
      }
      h4 {
        margin-top: 24px;
        margin-bottom: 8px;
        font-weight: 600;
      }
      .section-text p {
        line-height: 1.6;
      }
     
      .story-wrapper {
        max-width: 820px;
        margin: 0 auto;
      }
      
      .nav-tabs .nav-link,
      .tabbable > .nav > li > a {
        font-size: 16px;
        padding: 12px 24px;
      }
     
      .section-block {
        margin-bottom: 40px;
      }
      .section-block-tight {
        margin-bottom: 20px;
      }
      .shiny-plot-output {
        margin-top: 12px;
        margin-bottom: 28px;
      }
      .story-wrapper .full-width-plot {
        max-width: 80vw;
        margin-left: calc(50% - 40vw);
        margin-right: calc(50% - 40vw);
      }
      .story-wrapper .full-width-plot .shiny-plot-output {
        width: 100%;
      }

      }
    "))
  ),
  
  titlePanel("E-Scooter Trips in Minneapolis: What do they say about micromobility & transit access?"),
  
  tabsetPanel(
    
    tabPanel(
      "Story overview",
      div(class = "story-wrapper section-text",
          
          # ---- Executive summary ----
          div(class = "section-block",
              br(),
              h2("Executive summary & key assumptions"),
              p(
                strong("What this dashboard does: "),
                "This tool uses scooter trip records and transit stop data in Minneapolis to ask whether scooters are mostly ",
                strong("connecting people to transit"),
                " (first/last mile) or ",
                strong("substituting for transit"),
                " (sub_one / sub_multi), and where they appear to fill ",
                strong("access gaps"),
                " beyond walking distance of bus stops."
              ),
              p(
                strong("Key assumptions about behaviour types: "),
                "Because scooter locations are anonymized to street segments (centerlines), we infer behaviour purely from segment proximity to transit stops ",
                "(whether there are bus stops along that transit segment)."
              ),
              p(
                "These classifications are an approximation. They do not prove that riders actually boarded transit; ",
                "they show where scooters are ",
                strong("able"),
                " to act as access or substitutes based on network geometry."
              )
          ),
          
          # ---- Section 1 ----
          div(class = "section-block",
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
                tags$li(
                  strong("none: "),
                  "neither the start nor end segment is within 30 m of a transit stop. ",
                  "We interpret these as trips beyond normal walking distance of the transit network (access gaps)."
                ),
                tags$li(
                  strong("first_mile: "),
                  "the scooter trip ",
                  strong("ends"),
                  " within 30 m of a transit stop; we assume riders are heading ",
                  strong("into"),
                  " the transit network."
                ),
                tags$li(
                  strong("last_mile: "),
                  "the scooter trip ",
                  strong("starts"),
                  " within 30 m of a transit stop; we assume riders are ",
                  strong("continuing a trip after leaving"),
                  " the transit network."
                ),
                tags$li(
                  strong("sub_multi: "),
                  "both ends are near transit stops, but connected by ",
                  strong("multiple transit routes or transfers"),
                  ". Scooters may be replacing a more complex transit journey."
                ),
                tags$li(
                  strong("sub_one: "),
                  "both ends are near transit stops served by the same bus route; ",
                  "the scooter trip could reasonably be replaced by a ",
                  strong("single bus ride"),
                  "."
                )
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
              )
          ),
          
          # ---- Section 2 ----
          div(class = "section-block",
              h2("2. When do different scooter behaviours happen?"),
              p(
                "Scooter demand follows predictable time-of-day rhythms. To compare behaviours fairly, ",
                "the charts show average hourly values across all days, separately for weekdays and weekends."
              ),
              
              h4("Average hourly scooter trips by behavior type"),
              div(class = "full-width-plot",
                  plotOutput("plot_hourly_trips", height = "520px")
              ),
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
              div(class = "full-width-plot",
                  plotOutput("plot_hourly_share", height = "520px")
              ),
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
              )
          ),
          
          # ---- Section 3 ----
          div(class = "section-block",
              h2("3. How close are scooters to transit stops? (Limitations apply)"),
              p(
                "Distances here are from the centroid of the anonymized street segment to the nearest transit stop. ",
                "Because we buffer and classify segments within 30m of stops to capture all stop locations along a street, ",
                "the boxplot reflects those thresholds: many substitution and access trips cluster near the lower end. ",
                "On long residential blocks, a 300m measured distance may still correspond to a rider who was much closer to the stop."
              ),
              div(class = "full-width-plot",
                  plotOutput("plot_dist_box", height = "480px")
              ),
              p(
                strong("Interpretation: "),
                "behaviours tightly clustered around shorter distances are more transit-integrated. ",
                strong("Substitution trips (sub_one / sub_multi)"),
                " being closer to stops reinforces that they represent competition with transit rather than simple access."
              )
          ),
          
          # ---- Section 4 ----
          div(class = "section-block",
              h2("4. Which bus routes are most 'leaky'?"),
              p(
                "We estimate which bus routes may be losing riders to scooters using ",
                code("leakiness = scooter_substitutions / route_boardings"),
                ". High leakiness does not necessarily mean poor performance, but it highlights corridors where ",
                "scooters are acting as substitutes rather than connectors."
              ),
              p(
                "For example, if a route has 3,000 annual bus boardings and we observe 300 scooter trips ",
                "that start and end near stops on that route, its leakiness is ",
                code("300 / 3,000 = 0.10"),
                ", or ",
                strong("10%"),
                ". Very low-boarded routes are excluded here so that these ratios remain interpretable."
              ),
              p(
                strong("Data disclaimer: "),
                "the boarding/alighting dataset may not include every stop or route in Minneapolis. ",
                "Leakiness is directional—useful for identifying candidates for further evaluation, ",
                "not for estimating exact revenue loss."
              ),
              div(class = "full-width-plot",
                  plotOutput("plot_leaky_routes", height = "480px")
              ),
              p(
                strong("Takeaway: "),
                "these routes represent low-cost opportunities to retain riders through improvements such as ",
                "stop amenities, signal priority, pricing alignment, or integrated micromobility at key stops."
              )
          ),
          
          # ---- Sections 5 & 6 together (tighter gap) ----
          div(class = "section-block-tight",
              h2("5 & 6. Where are impacts concentrated, and who is most affected?"),
              p(
                "Using precomputed tract-level statistics, we map two outcomes and then prioritise neighbourhoods ",
                "where interventions could have the greatest impact."
              ),
              tags$ul(
                tags$li(strong("Substitution share:"), " proportion of trips that could be replaced by one bus route (sub_one)."),
                tags$li(strong("Gap share:"), " proportion of trips where neither end is near a stop (none).")
              ),
              h4("Where scooters most often duplicate a single bus route"),
              plotOutput("plot_sub_map", height = "480px"),
             
              h4("Where scooters operate beyond walking distance of transit"),
              plotOutput("plot_gap_map", height = "480px"),
              
              p(
                strong("Reading the maps: "),
                "bright areas show neighbourhoods where scooters either compete with or meaningfully ",
                "compensate for transit. Darker areas reflect lower scooter activity or behaviour closer to ",
                "the city average. Black outlines highlight the top priority tracts in the tables below."
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
                "Gap priority emphasizes equity-sensitive characteristics and transit deprivation. ",
                "We convert each variable into deciles (1–10) and then build a weighted score:"
              ),
              tags$ul(
                tags$li(strong("Gap share (more weight): "), "tracts where many scooter trips are far from transit."),
                tags$li(strong("Median income (more weight): "), "tracts with lower incomes get higher scores."),
                tags$li(strong("Zero-car share: "), "tracts with more households without cars."),
                tags$li(strong("Boardings density: "), "tracts with fewer transit boardings per km²."),
                tags$li(strong("Population: "), "tracts where more people are affected.")
              ),
              code("gap_priority_score = 1.5·decile(gap_share) + 1.5·decile(-median_income) + decile(zero_car_share) + decile(-boardings_density) + decile(population)"),
              br(), br(),
              DTOutput("table_priority_gap_tracts")

              br(), br(),
      
      h2("7. How long do scooter trips take compared to transit?"),
      p(
        "Using a pilot sample of trips, we used the Google Directions API to estimate door-to-door travel times ",
        "for both biking and transit between the same origin and destination."
      ),

      h4("Distribution of time differences (transit - scooter)"),
      plotOutput("plot_time_diff_dist", height = "380px"),
      p("Values above 0 mean transit is slower than scooters for that trip; values below 0 mean transit is faster."),
      
      br(),
      
      h4("Travel time distributions across modes"),
      p(
        "These plots compare the distribution of travel times across the observed scooter duration and Google-estimated ",
        "bike and transit durations for the same OD pairs. Distributions matter because medians can hide variability—",
        "especially for transit (waiting + transfers)."
      ),
      plotOutput("plot_mode_boxplot", height = "360px"),
      plotOutput("plot_mode_density", height = "360px"),
      
      br(), br(),
      
      br(), br(),
      
      h2("7B. Travel-time summary tables"),
      p(
        "These tables summarize the same pilot sample used in the plots. ",
        "They provide quick, report-ready numbers: central tendency by mode, ",
        "how often each mode is fastest, and the trip-level comparison table."
      ),
      
      h4("Average and median travel time by mode"),
      DTOutput("table_summary_stats"),
      br(),
      
      h4("How often each mode is fastest"),
      DTOutput("table_fastest_counts"),
      br(),
      
      h4("Trip-level comparison table"),
      DTOutput("table_final_table"),
          
      br(), br(),
      
      h3("Methods in brief"),
      p(
        strong("Behaviour classification: "),
        "Trips are classified as first_mile, last_mile, sub_multi, sub_one, or none based on whether trip ends are within 30 m of transit stops and whether the same bus route serves both ends."
      ),
      p(
        strong("Travel-time benchmarking: "),
        "Observed scooter durations are compared to Google-estimated bike and transit durations for the same OD pairs. These are estimates and do not capture every rider preference, but they provide a consistent benchmark."
      )
    ),
          
          # ---- Methods in brief ----
          div(class = "section-block",
              h3("Methods in brief"),
              p(
                strong("Behaviour classification: "),
                "Each scooter trip is linked to anonymized road segments. We mark a segment as near transit if it lies within 30 m of any transit stop. ",
                "Trips are classified as first_mile, last_mile, sub_multi, sub_one, or none based on which ends are near stops and whether the same bus route serves both ends."
              ),
              p(
                strong("Hourly averages: "),
                "For each day, hour, and behaviour type, we count trips and then average across days to get typical daily patterns separately for weekdays and weekends."
              ),
              p(
                strong("Distances to stops: "),
                "For each trip, we compute the straight-line distance from the centroid of the start segment to the nearest transit stop. ",
                "These are shown on a log scale because distances span orders of magnitude."
              ),
              p(
                strong("Route leakiness: "),
                "For each bus route we count scooter trips that could be replaced by a single bus ride (sub_one) and compare them to annual boardings on that route. ",
                "Leakiness is defined as ",
                code("leakiness = scooter_substitution_trips / route_boardings"),
                " and is only reported for routes with at least 20 substitution trips and boardings above the median so low-volume express routes do not dominate."
              ),
              p(
                strong("Tract-level substitution and gap shares: "),
                "We assign scooter trip starts to census tracts and compute the share of trips in each tract that are sub_one (substitution) or none (gap). ",
                "These shares are mapped for tracts where scooters are actually present."
              ),
              p(
                strong("Equity-focused gap priority: "),
                "We convert gap share, income, zero-car share, boardings density, and population into deciles and create ",
                code("gap_priority_score"),
                " with extra weight on gap share and lower incomes. ",
                "The highest-scoring tracts are listed in the table and outlined on the gap map."
              )
          ),
          
          # ---- Conclusion ----
          div(class = "section-block",
              h2("Conclusion"),
              p(
                strong("Overall, scooters in Minneapolis are playing two roles at once: "),
                "they are clearly helping people bridge gaps in the transit network, ",
                "but they are also replacing rides on some busy bus routes."
              ),
              p(
                "First- and last-mile patterns around commute hours show that scooters often ",
                "extend the reach of the transit system, especially for early-morning and afternoon trips. ",
                "At the same time, the leakiness analysis and substitution maps highlight specific corridors ",
                "where improving transit reliability, comfort, or integration with micromobility could win riders back."
              ),
              p(
                "Finally, the gap-priority tracts underscore that scooters are especially important in lower-income ",
                "neighbourhoods with limited car access and weak transit service. ",
                "Targeted investments in safe scooter infrastructure and better bus service in these areas could deliver ",
                "disproportionate benefits for residents who have the fewest alternatives."
              )
          )
      )
    ),
    
    
    tabPanel(
      "Substitution explorer",
      sidebarLayout(
        sidebarPanel(
          h4("Explore scooter trips that duplicate a single bus route"),
          selectInput("route_select", "Choose a route to examine:", choices = NULL)
        ),
        mainPanel(
          leafletOutput("subMap", height = "600px"),
          br(),
          DTOutput("subTable")
        )
      )
    ),
    
    tabPanel(
      "Gap explorer",
      sidebarLayout(
        sidebarPanel(
          h4("Explore where scooters run far from transit"),
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
          leafletOutput("gapMap", height = "600px")
        )
      )
    ),
    
    tabPanel(
      "Travel-time sandbox",
      sidebarLayout(
        sidebarPanel(
          h4("Compare bike vs transit time between two stops (Google Directions)"),
          selectInput("sandbox_origin", "Origin stop:", choices = NULL),
          selectInput("sandbox_dest", "Destination stop:", choices = NULL),
          actionButton("run_sandbox", "Run Google Directions"),
          br(), br(),
          verbatimTextOutput("sandbox_status")
        ),
        mainPanel(
          tableOutput("sandbox_result_tbl")
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
  
  # --- Plots ---
  output$plot_behavior_comp <- renderPlot({
    ggplot(behavior_counts, aes(x = behavior_type, y = pct, fill = behavior_type)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = beh_pal) +
      labs(
        title = "Composition of scooter trip types",
        x = "Behavior type",
        y = "Share of all scooter trips"
      ) +
      theme(legend.position = "none")
  })
  
  
  output$plot_hourly_trips <- renderPlot({
    ggplot(hourly_normalized, aes(x = hour, y = avg_trips, color = behavior_type, group = behavior_type)) +
      geom_line(size = 1.1) +
      facet_wrap(~ day_type, ncol = 1) +
      scale_x_continuous(breaks = 0:23) +
      scale_color_manual(values = beh_pal) +
      labs(
        title = "Average hourly scooter trips by behavior type",
        x = "Hour of day",
        y = "Average trips per hour",
        color = "Behavior type"
      )
  })
  
  output$plot_hourly_share <- renderPlot({
    ggplot(hourly_share, aes(x = hour, y = avg_share, color = behavior_type, group = behavior_type)) +
      geom_line(size = 1.1) +
      facet_wrap(~ day_type, ncol = 1) +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_color_manual(values = beh_pal) +
      labs(
        title = "Average hourly scooter trip share by behavior type",
        x = "Hour of day",
        y = "Average trip share per hour",
        color = "Behavior type"
      )
  })
  
  output$plot_dist_box <- renderPlot({
    ggplot(start_points_beh_small, aes(x = behavior_type, y = dist_to_nearest_stop, fill = behavior_type)) +
      geom_boxplot(outlier.alpha = 0.2, alpha = 0.8) +
      scale_fill_manual(values = beh_pal) +
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
      theme(legend.position = "none")
  })
  
  output$plot_leaky_routes <- renderPlot({
    ggplot(top_leaky, aes(x = reorder(route_short_name, leakiness_raw), y = leakiness_raw)) +
      geom_col(fill = "#d95f02") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title = "Top 10 'Leaky' routes (busy routes only)",
        subtitle = "Leakiness = scooter substitution trips / annual bus boardings\nShown only for routes with ≥ 20 substitution trips and boardings above the median.",
        x = "Route short name",
        y = "Leakiness (%)"
      ) 
  })
  
  output$plot_sub_map <- renderPlot({
    priority_sub_geoms <- tracts_ll_small |>
      dplyr::filter(GEOID %in% priority_sub_tracts$GEOID)
    
    ggplot() +
      geom_sf(data = tracts_ll_small, aes(fill = sub_share), color = NA) +
      scale_fill_gradientn(
        colours = c("#bae4bc", "#0868ac"),
        limits  = c(0, 1),
        labels  = percent_format(accuracy = 1),
        name    = "Substitution share"
      ) +
      geom_sf(data = priority_sub_geoms, fill = NA, color = "black", linewidth = 0.6) +
      labs(
        title = "Where scooters most often duplicate a single bus route",
        subtitle = "Black outlines show the top substitution-priority tracts"
      ) +
      theme(axis.title = element_blank())
  })
  
  output$plot_gap_map <- renderPlot({
    priority_gap_geoms <- tracts_ll_small |>
      dplyr::filter(GEOID %in% priority_gap_tracts$GEOID)
    
    ggplot() +
      geom_sf(data = tracts_ll_small, aes(fill = gap_share), color = NA) +
      scale_fill_gradientn(
        colours = c("#bae4bc", "#0868ac"),
        limits  = c(0, 1),
        labels  = percent_format(accuracy = 1),
        name    = "Gap share"
      ) +
      geom_sf(data = priority_gap_geoms, fill = NA, color = "black", linewidth = 0.6) +
      labs(
        title = "Where scooters operate beyond walking distance of transit",
        subtitle = "Black outlines show the top gap-priority tracts"
      ) +
      theme(axis.title = element_blank())
  })
  
  output$plot_time_diff_dist <- renderPlot({
    ggplot(time_diff_dist, aes(x = diff_min, fill = behavior_type)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_fill_viridis_d(option = "plasma", end = 0.95, name = "Behaviour") +
      labs(
        x = "Transit time − scooter time (minutes)",
        y = "Trips",
        title = "Where do scooters save time versus transit?"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # --- New mode distribution plots (from final_table RDS) ---
  viz_data <- reactive({
    req(final_table)
    final_table %>%
      select(trip_id, micromobility_min, bike_min, transit_min) %>%
      pivot_longer(cols = -trip_id, names_to = "mode", values_to = "duration") %>%
      mutate(
        mode = recode(
          mode,
          micromobility_min = "Observed scooter trip (actual)",
          bike_min          = "Google Directions: Bike",
          transit_min       = "Google Directions: Transit"
        ),
        duration = as.numeric(duration)
      ) %>%
      filter(is.finite(duration), duration > 0)
  })
  
  output$plot_mode_boxplot <- renderPlot({
    ggplot(viz_data(), aes(x = mode, y = duration, fill = mode)) +
      geom_boxplot(outlier.alpha = 0.25) +
      labs(
        title = "Comparing travel times by mode (pilot sample)",
        x = NULL,
        y = "Duration (minutes)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1)
      )
  })
  
  output$plot_mode_density <- renderPlot({
    ggplot(viz_data(), aes(x = duration, color = mode, fill = mode)) +
      geom_density(alpha = 0.25, linewidth = 1) +
      labs(
        title = "Distribution of travel times by mode (pilot sample)",
        x = "Duration (minutes)",
        y = "Density"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.title = element_blank())
  })
  
  # --- Tables: priority lists ---
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
  
  # --- Tables: travel-time pilot tables ---
  output$table_summary_stats <- renderDT({
    datatable(summary_stats, options = list(dom = "t", pageLength = 50), rownames = FALSE)
  })
  
  output$table_fastest_counts <- renderDT({
    datatable(fastest_counts, options = list(dom = "t", pageLength = 50), rownames = FALSE)
  })
  
  output$table_final_table <- renderDT({
    datatable(final_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # --- Substitution explorer ---
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
        color       = "#7570b3",
        radius      = 3,
        opacity     = 0.8,
        fillOpacity = 0.8,
        popup       = ~paste("Trip ID:", trip_id)
      ) |>
      addCircleMarkers(
        data        = ends_filtered,
        color       = "#d95f02",
        radius      = 3,
        opacity     = 0.8,
        fillOpacity = 0.8,
        popup       = ~paste("Trip ID:", trip_id)
      ) |>
      addLegend(
        position = "bottomright",
        colors   = c("#7570b3", "#d95f02"),
        labels   = c("Scooter trip start", "Scooter trip end"),
        title    = "Scooter points",
        opacity  = 1
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
  
  # --- Gap explorer ---
  output$gapMap <- renderLeaflet({
    pal <- colorNumeric(
      palette = c("#fdcc8a", "#b30000"),
      domain  = c(0, 1),
      na.color = "transparent"
    )
    
    tracts_filtered <- tracts_ll_small |>
      dplyr::filter(gap_share >= input$min_gap_share)
    
    priority_gap_geoms_leaf <- tracts_filtered |>
      dplyr::filter(GEOID %in% priority_gap_tracts$GEOID)
    
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(
        data        = tracts_filtered,
        fillColor   = ~pal(gap_share),
        fillOpacity = 0.7,
        color       = NA
      ) |>
      addPolygons(
        data        = priority_gap_geoms_leaf,
        fillOpacity = 0,
        color       = "black",
        weight      = 2
      ) |>
      addLegend(
        pal    = pal,
        values = c(0, 1),
        labFormat = labelFormat(transform = function(x) 100 * x, suffix = "%"),
        title  = "Gap trip share\n0% = none of the trips are gaps\n100% = all trips are gaps"
      )
  })
  
  # ---- Travel-time sandbox server logic (SAFE) ----
  observe({
    req(stops_ll)
    stop_choices <- sort(unique(stops_ll$stop_name))
    updateSelectInput(session, "sandbox_origin", choices = stop_choices)
    updateSelectInput(session, "sandbox_dest",   choices = stop_choices, selected = stop_choices[2])
  })
  
  get_stop_row <- function(stop_name) {
    stops_ll %>%
      filter(stop_name == .env$stop_name) %>%
      slice(1)
  }
  
  sandbox_times <- eventReactive(input$run_sandbox, {
    req(input$sandbox_origin, input$sandbox_dest)
    
    o <- get_stop_row(input$sandbox_origin)
    d <- get_stop_row(input$sandbox_dest)
    
    validate(
      need(nrow(o) == 1, "Origin stop not found (or not unique). Pick a different origin."),
      need(nrow(d) == 1, "Destination stop not found (or not unique). Pick a different destination.")
    )
    
    validate(
      need(is.finite(o$stop_lat) && is.finite(o$stop_lon), "Origin stop has invalid coordinates."),
      need(is.finite(d$stop_lat) && is.finite(d$stop_lon), "Destination stop has invalid coordinates.")
    )
    
    out <- tryCatch(
      get_google_trip_times(
        origin_lat = o$stop_lat,
        origin_lon = o$stop_lon,
        dest_lat   = d$stop_lat,
        dest_lon   = d$stop_lon
      ),
      error = function(e) {
        tibble::tibble(
          mode = c("bicycling", "transit"),
          duration_min = NA_real_,
          distance_km  = NA_real_,
          summary      = paste("R error:", conditionMessage(e))
        )
      }
    )
    
    out
  }, ignoreInit = TRUE)
  
  output$sandbox_status <- renderText({
    if (is.null(input$run_sandbox) || input$run_sandbox == 0) {
      return("Pick an origin + destination, then click Run.")
    }
    if (is.null(sandbox_times())) return("Running...")
    x <- sandbox_times()
    paste(unique(x$summary), collapse = "\n")
  })
  
  output$sandbox_result_tbl <- renderTable({
    req(sandbox_times())
    sandbox_times() %>%
      mutate(
        duration_min = round(duration_min, 1),
        distance_km  = round(distance_km, 2)
      )
  })
}

shinyApp(ui, server)

