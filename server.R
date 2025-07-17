library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(readxl)
library(leaflet.extras)
library(plotly)
library(tmaptools)
library(stringr)

### Load in Data
listings <- read_csv("data/cleaned_listings.csv", show_col_types = FALSE)
mrt_data <- read_csv("data/mrt_lrt_with_codes.csv", show_col_types = FALSE)
bus_data <- read_csv("data/bus_stops.csv", show_col_types = FALSE)
attraction_data <- read_excel("data/Cleaned_Tourist_Attractions_with_Names.xlsx")
reviews <- read.csv("data/cleaned_reviews.csv")
amenities_matrix <- read_csv("data/amenities_matrix.csv", show_col_types = FALSE)

### Make amenities_matrix columns unique
names(amenities_matrix) <- make.names(gsub("^\\[|\\]$", "", names(amenities_matrix)), unique = TRUE)

### Rename columns
mrt_data <- mrt_data %>% rename(label = Name)
bus_data <- bus_data %>% rename(label = Name)
attraction_data <- attraction_data %>% rename(label = Attraction.Name)

### Extract MRT line prefixes from STN_NO
extract_lines <- function(stn_no) {
  if (is.na(stn_no)) return(character(0))
  strsplit(stn_no, "/")[[1]] |> sapply(substr, 1, 2) |> unique()
}

### MRT line colors
line_colors <- c(
  NS = "red", EW = "green", NE = "purple", CC = "gold", DT = "blue",
  TE = "brown", CE = "cyan", BP = "lightgray", SE = "lightgray", SW = "lightgray",
  PW = "lightgray", PE = "lightgray", CG = "green", ST = "lightgray", PT = "lightgray"
)

### Assign line codes
mrt_data <- mrt_data %>%
  mutate(
    MRT_Lines = lapply(STN_NO, extract_lines),
    Line_Colors = lapply(MRT_Lines, function(lines) line_colors[unlist(lines)])
  )

create_composite_icon <- function(colors) {
  spans <- paste0(
    "<span style='background:", colors,
    "; width:10px; height:10px; border-radius:50%; display:inline-block; margin-right:1px;'></span>",
    collapse = ""
  )
  paste0("<div style='display:flex; align-items:center;'>", spans, "</div>")
}

### Distance Calculation for Attractions, MRT, Bus Stop
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000  # Radius of Earth in meters
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

### Main Shiny Server
shinyServer(function(input, output, session) {
  
  ### CHECKBOXES FOR FILTERING OPTIONS
  output$show_all_checkbox_ui <- renderUI({
    checkboxGroupInput(
      "overlay_options", "Show:",
      choices = c("MRT/LRT", "Attractions", "Bus Stops"),
      selected = c(),  # Reset to empty
      inline = TRUE
    )
  })
  
  output$show_nearest_checkbox_ui <- renderUI({
    checkboxGroupInput(
      "nearest_overlay_options", "Show Nearest:",
      choices = c("Nearest MRT", "Nearest Attraction", "Nearest Bus Stop"),
      selected = c("Nearest MRT", "Nearest Attraction", "Nearest Bus Stop"),
      inline = TRUE
    )
  })
  
  ### VISITOR TAB
  output$visitor_region_ui <- renderUI({
    checkboxGroupInput("visitor_region", "Select Region(s)",
                       choices = sort(unique(na.omit(listings$region))),
                       selected = unique(na.omit(listings$region)),
                       inline = TRUE)
  })
  
  output$visitor_neighbourhood_ui <- renderUI({
    if (is.null(input$visitor_region) || length(input$visitor_region) == 0) return(NULL)
    valid <- listings %>% filter(region %in% input$visitor_region) %>% distinct(neighbourhood_cleansed) %>% arrange(neighbourhood_cleansed) %>% pull()
    selectInput("visitor_neighbourhood", "Select Neighbourhood", choices = c("All", valid), selected = "All", width = "100%")
  })
  
  output$visitor_roomtype_ui <- renderUI({
    room_types <- unique(na.omit(listings$room_type))
    selectInput("visitor_room_type", "Select Room Type", choices = c("All", room_types), selected = "All", width = "100%")
  })
  
  output$visitor_price_ui <- renderUI({
    sliderInput("visitor_price_range", "Select Price Range",
                min = min(listings$price, na.rm = TRUE),
                max = max(listings$price, na.rm = TRUE),
                value = c(min(listings$price, na.rm = TRUE), max(listings$price, na.rm = TRUE)),
                step = 10, pre = "$", width = "100%")
  })
  
  output$visitor_min_nights_ui <- renderUI({
    sliderInput("visitor_min_nights", "Minimum Nights:",
                min = min(listings$minimum_nights, na.rm = TRUE),
                max = max(listings$minimum_nights, na.rm = TRUE),
                value = c(min(listings$minimum_nights, na.rm = TRUE),
                          max(listings$minimum_nights, na.rm = TRUE)),
                step = 1, width = "100%")
  })
  
  visitor_filtered_data <- reactive({
    req(input$visitor_region, input$visitor_price_range, input$visitor_neighbourhood)
    data <- listings %>% filter(region %in% input$visitor_region)
    if (input$visitor_neighbourhood != "All") {
      data <- data[data$neighbourhood_cleansed == input$visitor_neighbourhood, ]
    }
    if (!is.null(input$visitor_room_type) && input$visitor_room_type != "All") {
      data <- data[data$room_type == input$visitor_room_type, ]
    }
    data %>% filter(price >= input$visitor_price_range[1], price <= input$visitor_price_range[2],
                    minimum_nights >= input$visitor_min_nights[1],
                    minimum_nights <= input$visitor_min_nights[2])
  })
  
  ### TENANT TAB -> PRICE SUBTAB
  output$tenant_price_region_ui <- renderUI({
    checkboxGroupInput("tenant_price_region", "Select Region(s)",
                       choices = sort(unique(na.omit(listings$region))),
                       selected = unique(na.omit(listings$region)),
                       inline = TRUE)
  })
  
  output$tenant_price_neighbourhood_ui <- renderUI({
    if (is.null(input$tenant_price_region) || length(input$tenant_price_region) == 0) return(NULL)
    valid <- listings %>% filter(region %in% input$tenant_price_region) %>% 
      distinct(neighbourhood_cleansed) %>% arrange(neighbourhood_cleansed) %>% pull()
    selectInput("tenant_price_neighbourhood", "Select Neighbourhood", choices = c("All", valid), selected = "All", width = "100%")
  })
  
  output$tenant_price_roomtype_ui <- renderUI({
    room_types <- unique(na.omit(listings$room_type))
    selectInput("tenant_price_room_type", "Select Room Type", choices = c("All", room_types), selected = "All", width = "100%")
  })
  
  output$tenant_price_price_ui <- renderUI({
    sliderInput("tenant_price_range", "Select Price Range",
                min = min(listings$price, na.rm = TRUE),
                max = max(listings$price, na.rm = TRUE),
                value = c(min(listings$price, na.rm = TRUE), max(listings$price, na.rm = TRUE)),
                step = 10, pre = "$", width = "100%")
  })
  
  output$tenant_price_min_nights_ui <- renderUI({
    sliderInput("tenant_price_min_nights", "Minimum Nights:",
                min = min(listings$minimum_nights, na.rm = TRUE),
                max = max(listings$minimum_nights, na.rm = TRUE),
                value = c(min(listings$minimum_nights, na.rm = TRUE),
                          max(listings$minimum_nights, na.rm = TRUE)),
                step = 1, width = "100%")
  })
  
  tenant_price_filtered_data <- reactive({
    req(input$tenant_price_region, input$tenant_price_range, input$tenant_price_neighbourhood)
    data <- listings %>% filter(region %in% input$tenant_price_region)
    
    if (input$tenant_price_neighbourhood != "All") {
      data <- data[data$neighbourhood_cleansed == input$tenant_price_neighbourhood, ]
    }
    if (!is.null(input$tenant_room_type) && input$tenant_room_type != "All") {
      data <- data[data$room_type == input$tenant_room_type, ]
    }
    
    data %>% filter(price >= input$tenant_price_range[1], price <= input$tenant_price_range[2],
                    minimum_nights >= input$tenant_price_min_nights[1],
                    minimum_nights <= input$tenant_price_min_nights[2])
  })
  
  ### TENANT TAB -> LISTINGS SUBTAB
  output$tenant_listings_region_ui <- renderUI({
    checkboxGroupInput("tenant_listings_region", "Select Region(s)",
                       choices = sort(unique(na.omit(listings$region))),
                       selected = unique(na.omit(listings$region)),
                       inline = TRUE)
  })
  
  output$tenant_listings_neighbourhood_ui <- renderUI({
    if (is.null(input$tenant_listings_region) || length(input$tenant_listings_region) == 0) return(NULL)
    valid <- listings %>% filter(region %in% input$tenant_listings_region) %>% distinct(neighbourhood_cleansed) %>% arrange(neighbourhood_cleansed) %>% pull()
    selectInput("tenant_listings_neighbourhood", "Select Neighbourhood", choices = c("All", valid), selected = "All", width = "100%")
  })
  
  output$tenant_listings_roomtype_ui <- renderUI({
    room_types <- unique(na.omit(listings$room_type))
    selectInput("tenant_listings_room_type", "Select Room Type", choices = c("All", room_types), selected = "All", width = "100%")
  })
  
  output$tenant_listings_price_ui <- renderUI({
    sliderInput("tenant_listings_price_range", "Select Price Range",
                min = min(listings$price, na.rm = TRUE),
                max = max(listings$price, na.rm = TRUE),
                value = c(min(listings$price, na.rm = TRUE), max(listings$price, na.rm = TRUE)),
                step = 10, pre = "$", width = "100%")
  })
  
  output$tenant_listings_min_nights_ui <- renderUI({
    sliderInput("tenant_listings_min_nights", "Minimum Nights:",
                min = min(listings$minimum_nights, na.rm = TRUE),
                max = max(listings$minimum_nights, na.rm = TRUE),
                value = c(min(listings$minimum_nights, na.rm = TRUE),
                          max(listings$minimum_nights, na.rm = TRUE)),
                step = 1, width = "100%")
  })
  
  tenant_listings_filtered_data <- reactive({
    req(input$tenant_listings_region, input$tenant_listings_price_range, input$tenant_listings_neighbourhood)
    data <- listings %>% filter(region %in% input$tenant_listings_region)
    if (input$tenant_listings_neighbourhood != "All") {
      data <- data[data$neighbourhood_cleansed == input$tenant_listings_neighbourhood, ]
    }
    if (!is.null(input$tenant_listings_room_type) && input$tenant_listings_room_type != "All") {
      data <- data[data$room_type == input$tenant_listings_room_type, ]
    }
    data %>% filter(price >= input$tenant_listings_price_range[1], price <= input$tenant_listings_price_range[2],
                    minimum_nights >= input$tenant_listings_min_nights[1],
                    minimum_nights <= input$tenant_listings_min_nights[2])
  })
  
  ### BASE AIRBNB MAP + LISTINGS INFO
  output$airbnb_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  observe({
    data <- visitor_filtered_data()
    leafletProxy("airbnb_map") %>%
      clearGroup("listings") %>%
      addMarkers(
        lng = data$longitude,
        lat = data$latitude,
        icon = data$icon,
        popup = paste0("<b>", data$cleaned_name, "</b><br>Price: $", data$price, "<br>Minimum nights: ", data$minimum_nights),
        group = "listings"
      )
  })
  
  ### ADDING MRT, BUS STOP, ATTRACTIONS INFO
  observe({
    proxy <- leafletProxy("airbnb_map")
    proxy %>% clearGroup("mrt") %>% clearGroup("bus") %>% clearGroup("attractions")
    
    if ("MRT/LRT" %in% input$overlay_options) {
      train_icon <- makeIcon(
        iconUrl = "https://img.icons8.com/ios-filled/24/train.png",
        iconWidth = 20, iconHeight = 20,
        iconAnchorX = 10, iconAnchorY = 10
      )
      
      for (i in seq_len(nrow(mrt_data))) {
        station <- mrt_data[i, ]
        if (!is.na(station$latitude) && !is.na(station$longitude)) {
          clean_label <- gsub(" (MRT|LRT) STATION", "", station$label)
          
          html_icon <- paste0(
            "<div style='display:flex; align-items:center;'>",
            create_composite_icon(station$Line_Colors[[1]]),
            "<span style='margin-left:8px; font-size:12px;'>",
            clean_label, " (", station$STN_NO, ")</span></div>"
          )
          
          proxy %>% addMarkers(
            lng = station$longitude,
            lat = station$latitude,
            icon = train_icon,
            popup = html_icon,
            group = "mrt"
          )
        }
      }
    }
    
    if ("Bus Stops" %in% input$overlay_options) {
      bus_icon <- makeIcon(
        iconUrl = "https://img.icons8.com/fluency/24/bus.png",
        iconWidth = 20, iconHeight = 20,
        iconAnchorX = 10, iconAnchorY = 10
      )
      
      for (i in seq_len(nrow(bus_data))) {
        bus_stop <- bus_data[i, ]
        if (!is.na(bus_stop$latitude) && !is.na(bus_stop$longitude)) {
          proxy %>% addMarkers(
            lng = bus_stop$longitude,
            lat = bus_stop$latitude,
            icon = bus_icon,
            popup = bus_stop$label,
            group = "bus"
          )
        }
      }
    }
    
    if ("Attractions" %in% input$overlay_options) {
      attraction_icon <- makeIcon(
        iconUrl = "https://img.icons8.com/fluency/24/camera.png",
        iconWidth = 24, iconHeight = 24,
        iconAnchorX = 12, iconAnchorY = 12
      )
      
      for (i in seq_len(nrow(attraction_data))) {
        attraction <- attraction_data[i, ]
        if (!is.na(attraction$latitude) && !is.na(attraction$longitude)) {
          popup_text <- paste0("<b>", attraction$label, "</b><br>Opening Hours: ", attraction$Opening.Hours)
          
          proxy %>% addMarkers(
            lng = attraction$longitude,
            lat = attraction$latitude,
            icon = attraction_icon,
            popup = popup_text,
            group = "attractions",
            layerId = paste0("attraction_", attraction$label)
          )
        }
      }
    }
  })
  
  ### SHOW ATTRACTION INFORMATION ON CLICK
  output$selected_info <- renderUI({
    click <- input$airbnb_map_marker_click
    req(click)
    
    if (!is.null(click$id) && startsWith(as.character(click$id), "attraction_")) {
      clicked_label <- gsub("^attraction_", "", click$id)
      att <- attraction_data %>% filter(label == clicked_label)
      
      if (nrow(att) > 0) {
        return(HTML(paste0(
          '<div class="listing-box">',
          "<span style='font-size: 18px; color: #124944; font-weight: bold;'>", att$label, "</span><br>",
          "<b>Description:</b> ", att$Meta.Description, "<br>",
          "<b>Address:</b> ", att$Address, "<br>",
          "<b>Opening Hours:</b> ", att$Opening.Hours, "<br>",
          "<b>Website:</b> <a href='", att$URL, "' target='_blank'>", att$URL, "</a>",
          '</div>'
        )))
      }
    }
    
    clicked <- listings %>%
      filter(abs(longitude - click$lng) < 1e-5,
             abs(latitude - click$lat) < 1e-5)
    
    if (nrow(clicked) == 0) return(NULL)
    
    listing <- clicked[1, ]
    listing_id <- listing$id
    amenities_row <- amenities_matrix %>% filter(id == listing_id)
    
    ### SHOW AMENTIES INFORMATION IN LISTING
    if (nrow(amenities_row) > 0) {
      amenities_present <- amenities_row %>%
        select(-id) %>%
        select(where(~ . == 1)) %>%
        names() %>%
        head(10)
      
      amenities_html <- paste("<ul>", paste0("<li>", amenities_present, "</li>", collapse = ""), "</ul>")
    } else {
      amenities_html <- "<i>No amenity information available.</i>"
    }
    
    rating <- listing$review_scores_rating
    reviews <- listing$number_of_reviews
    
    ### DISPLAY DISTANCE TO NEAREST MRT
    mrt_distances <- haversine_distance(listing$latitude, listing$longitude, mrt_data$latitude, mrt_data$longitude)
    nearest_mrt <- mrt_data[which.min(mrt_distances), ]
    nearest_mrt_info <- paste0(nearest_mrt$label, " (", round(min(mrt_distances)), " m)")
    
    ### DISPLAY DISTANCE TO NEAREST ATTRACTION
    attraction_distances <- haversine_distance(listing$latitude, listing$longitude, attraction_data$latitude, attraction_data$longitude)
    nearest_attraction <- attraction_data[which.min(attraction_distances), ]
    nearest_attraction_info <- paste0(nearest_attraction$label, " (", round(min(attraction_distances)), " m)")
    
    ### DISPLAY DISTANCE TO NEAREST BUS STOP
    bus_distances <- haversine_distance(listing$latitude, listing$longitude, bus_data$latitude, bus_data$longitude)
    nearest_bus <- bus_data[which.min(bus_distances), ]
    nearest_bus_info <- paste0(nearest_bus$label, " (", round(min(bus_distances)), " m)")
    
    ### DISPLAY STARS FOR RATING INFORMATION
    full_stars <- floor(rating)
    half_star <- ifelse((rating - full_stars) >= 0.25 && (rating - full_stars) < 0.75, 1, 0)
    empty_stars <- 5 - full_stars - half_star
    
    svg_star <- function(fill) {
      fill_color <- switch(fill,
                           "full" = "#f1c40f",
                           "half" = "url(#halfGradient)",
                           "empty" = "#e0e0e0")
      sprintf(
        '<svg width="20" height="20" viewBox="0 0 20 20" style="margin-right:2px;" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <linearGradient id="halfGradient">
            <stop offset="50%%" stop-color="#f1c40f"/>
            <stop offset="50%%" stop-color="#e0e0e0"/>
          </linearGradient>
        </defs>
        <polygon fill="%s" stroke="#ccc" stroke-width="1"
          points="10,1 12.6,7.5 19.5,7.5 13.9,11.9 16.5,18.3 10,14 3.5,18.3 6.1,11.9 0.5,7.5 7.4,7.5"/>
      </svg>', fill_color)
    }
    
    stars_vec <- c(
      rep(svg_star("full"), full_stars),
      if (half_star) svg_star("half"),
      rep(svg_star("empty"), empty_stars)
    )
    stars_html <- paste(stars_vec, collapse = "")
    
    star_block <- sprintf(
      '<div title="Rating: %.2f out of 5 (%d reviews)" style="display:flex; align-items:center;">
      <span style="color:#555; font-size:14px; margin-right:5px;">%.1f</span>
      %s
      <span style="color:#555; font-size:14px; margin-left:5px;">(%d)</span>
    </div>',
      rating, reviews, rating, stars_html, reviews
    )
    
    leafletProxy("airbnb_map") %>%
      clearGroup("selection_links") -> proxy
    
    ### ADDING NEAREST MRT AS A MARKER ON AIRBNB MAP
    if ("Nearest MRT" %in% input$nearest_overlay_options) {
      proxy <- proxy %>%
        addAwesomeMarkers(
          lng = nearest_mrt$longitude, lat = nearest_mrt$latitude,
          icon = awesomeIcons(
            icon = 'train', library = 'fa',
            iconColor = 'white', markerColor = 'red'
          ),
          popup = paste0(
            "<div style='display:flex; align-items:center;'>",
            "<span style='background:", nearest_mrt$Line_Colors[[1]][1], 
            "; width:10px; height:10px; border-radius:50%; display:inline-block; margin-right:6px;'></span>",
            toupper(gsub(" (MRT|LRT) STATION", "", nearest_mrt$label)), 
            " (", nearest_mrt$STN_NO, ")</div>"
          ),
          group = "selection_links"
        ) %>%
        addPolylines(
          lng = c(listing$longitude, nearest_mrt$longitude),
          lat = c(listing$latitude, nearest_mrt$latitude),
          color = "#bb1e12", weight = 5, opacity = 0.7, group = "selection_links"
        )
    }
    
    ### ADDING NEAREST ATTRACTION AS MARKER ON AIRBNB MAP
    if ("Nearest Attraction" %in% input$nearest_overlay_options) {
      proxy <- proxy %>%
        addAwesomeMarkers(
          lng = nearest_attraction$longitude, lat = nearest_attraction$latitude,
          icon = awesomeIcons(
            icon = 'camera', library = 'fa',
            iconColor = 'white', markerColor = 'blue'
          ),
          popup = paste0(
            "<b>", nearest_attraction$label, "</b><br>",
            "Opening Hours: ", nearest_attraction$Opening.Hours
          ),
          group = "selection_links",
          layerId = paste0("attraction_", nearest_attraction$label)
        ) %>%
        addPolylines(
          lng = c(listing$longitude, nearest_attraction$longitude),
          lat = c(listing$latitude, nearest_attraction$latitude),
          color = "#1f628e", weight = 5, opacity = 0.7, group = "selection_links"
        )
    }
    
    ### ADDING NEAREST BUS STOP AS MARKER ON AIRBNB MAP
    if ("Nearest Bus Stop" %in% input$nearest_overlay_options) {
      proxy <- proxy %>%
        addAwesomeMarkers(
          lng = nearest_bus$longitude, lat = nearest_bus$latitude,
          icon = awesomeIcons(
            icon = 'bus', library = 'fa',
            iconColor = 'white', markerColor = 'green'
          ),
          popup = paste0("<b>", nearest_bus$label, "</b>"),
          group = "selection_links"
        ) %>%
        addPolylines(
          lng = c(listing$longitude, nearest_bus$longitude),
          lat = c(listing$latitude, nearest_bus$latitude),
          color = "#124944", weight = 5, opacity = 0.7, group = "selection_links"
        )
    }
    
    ### LISTING INFORMATION HTML
    HTML(paste0(
      '<div class="listing-box">',
      "<span style='font-size: 18px; color: #c74375; font-weight: bold;'>", listing$cleaned_name, "</span><br>",
      "<b>Region:</b> ", listing$region, "<br>",
      "<b>Neighbourhood:</b> ", listing$neighbourhood_cleansed, "<br>",
      "<b>Room Type:</b> ", listing$room_type, "<br>",
      "<b>Price:</b> $", listing$price, " per night<br>",
      "<b>Minimum Nights:</b> ", listing$minimum_nights, "<br>",
      "<b>Host Response Time:</b> ", str_to_title(listing$host_response_time), "<br>",
      "<b>Instant Bookable:</b> ", ifelse(listing$instant_bookable == TRUE, "Yes", "No"), "<br>",
      "<b>Review Rating:</b><br>", star_block,
      "<b>Nearest MRT:</b> ", nearest_mrt_info, "<br>",
      "<b>Nearest Bus Stop:</b> ", nearest_bus_info, "<br>",
      "<b>Nearest Attraction:</b> ", nearest_attraction_info, "<br>",
      "<b>Amenities:</b><br>", amenities_html,
      '</div>'
    ))
  })
  
  ### TENANT TAB -> PRICE SUBTAB HEATMAP
  output$tenant_heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  observe({
    req(input$tenant_price_region, input$tenant_price_range, input$tenant_price_neighbourhood)
    data <- tenant_price_filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    price_range <- range(data$price, na.rm = TRUE)
    
    gradient_named <- c(
      "0.0"  = "#4c4c4c", 
      "0.25" = "#5e3c99",  
      "0.5"  = "#2b8cbe",  
      "0.75" = "#08519c",  
      "1.0"  = "#081d58"   
    )
    
    pal <- colorNumeric(
      palette = unname(gradient_named),
      domain = price_range,
      reverse = FALSE
    )
    
    leafletProxy("tenant_heatmap") %>%
      clearGroup("heatmap") %>%
      clearControls() %>%
      addHeatmap(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~price,
        radius = 30,
        blur = 1,
        max = max(data$price, na.rm = TRUE),
        gradient = gradient_named,
        group = "heatmap"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = data$price,
        title = "Price Intensity ($)",
        opacity = 1
      )
  })
  
  ### TENANT TAB -> PRICE SUBTAB REGION-SPECIFIC PRICE DISTRIBUTION
  output$tenant_price_dist <- renderPlotly({
    tenant_price_filtered_data() %>%
      filter(price < quantile(price, 0.99, na.rm = TRUE)) %>%
      plot_ly(
        x = ~price,
        type = "histogram",
        nbinsx = 40,
        marker = list(color = "#df8330")
      ) %>%
      layout(
        paper_bgcolor = '#f9f0e4',
        plot_bgcolor = '#f9f0e4',
        xaxis = list(title = "Price (SGD)"),
        yaxis = list(title = "Count"),
        bargap = 0.05
      )
  })
  
  ### TENANT TAB -> PRICE SUBTAB OVERALL PRICE DISTRIBUTION (BY REGION)
  output$tenant_price_box_region <- renderPlot({
    tenant_price_filtered_data() %>%
      filter(price < quantile(price, 0.99, na.rm = TRUE)) %>%
      ggplot(aes(x = region, y = price)) +
      geom_boxplot(width = 0.4, fill = "#1a4fbf", outlier.color = "#a983b6") +
      labs(x = "Region", y = "Price (SGD)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            panel.background = element_rect(fill = "#f9f0e4", color = NA),
            plot.background = element_rect(fill = "#f9f0e4", color = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_blank())
  })
  
  ### TENANT TAB -> PRICE SUBTAB OVERALL PRICE DISTRIBUTION (BY NEIGHBOURHOOD)
  output$tenant_price_box_neighbourhood <- renderPlot({
    tenant_price_filtered_data() %>%
      filter(price < quantile(price, 0.99, na.rm = TRUE)) %>%
      ggplot(aes(
        x = reorder(neighbourhood_cleansed, price, FUN = median),
        y = price,
        fill = region
      )) +
      geom_boxplot(width = 0.8, outlier.color = "#a983b6") +
      scale_fill_manual(values = c(
        "East Region" = "#1a4fbf",
        "West Region" = "#bb1e12",
        "North-East Region" = "#ecc665",
        "Central Region" = "#124944"
      )) +
      labs(x = "Neighbourhood", y = "Price (SGD)", fill = "Region") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#f9f0e4", color = NA),
        plot.background = element_rect(fill = "#f9f0e4", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank()
      )
  })
  
  ### TENANT TAB -> LISTING SUBTAB HEATMAP
  output$tenant_count_heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  observe({
    data <- tenant_listings_filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    count_range <- c(0, max(table(paste(data$longitude, data$latitude)), na.rm = TRUE))
    
    gradient_named <- c(
      "0.0"  = "#4c4c4c",
      "0.25" = "#5e3c99",
      "0.5"  = "#2b8cbe",
      "0.75" = "#08519c",
      "1.0"  = "#081d58"
    )
    
    pal <- colorNumeric(
      palette = unname(gradient_named),
      domain = count_range,
      reverse = FALSE
    )
    
    leafletProxy("tenant_count_heatmap") %>%
      clearGroup("heatmap") %>%
      clearControls() %>%
      addHeatmap(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        intensity = rep(1, nrow(data)),
        radius = 30,
        blur = 1,
        max = count_range[2],
        gradient = gradient_named,
        group = "heatmap"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = seq(count_range[1], count_range[2]),
        title = "Listing Density",
        opacity = 1
      )
  })
  
  ### TENANT TAB -> LISTINGS SUBTAB REGION-SPECIFIC PRICE DISTRIBUTION
  output$tenant_count_dist <- renderPlotly({
    tenant_listings_filtered_data() %>%
      plot_ly(
        x = ~neighbourhood_cleansed,
        type = "histogram",
        marker = list(color = "#df8330")
      ) %>%
      layout(
        paper_bgcolor = '#f9f0e4',
        plot_bgcolor = '#f9f0e4',
        xaxis = list(title = "Neighbourhood", tickangle = -45),
        yaxis = list(title = "Number of Listings"),
        bargap = 0.05
      )
  })
  
  ### TENANT TAB -> RATINGS SUBTAB DIVERGING LOLLIPOP PLOT
  output$tenant_avg_rating_lollipop <- renderPlot({
    avg_rating <- listings %>%
      filter(!is.na(review_scores_rating), !is.na(neighbourhood_cleansed)) %>%
      group_by(neighbourhood_cleansed) %>%
      summarise(avg_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
      ungroup()
    
    median_rating <- median(avg_rating$avg_rating)
    
    ggplot(avg_rating, aes(x = avg_rating, y = reorder(neighbourhood_cleansed, avg_rating), color = avg_rating >= median_rating)) +
      geom_segment(aes(x = median_rating, xend = avg_rating, yend = neighbourhood_cleansed), size = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = round(avg_rating, 2), hjust = ifelse(avg_rating >= median_rating, -0.3, 1.3)),
                color = "black", size = 3) +
      scale_color_manual(values = c("TRUE" = "#1a4fbf", "FALSE" = "#bb1e12"), guide = "none") +
      labs(x = "Average Rating", y = NULL) +
      theme_minimal() +
      theme(
        plot.margin = margin(10, 30, 10, 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "#f9f0e4", color = NA),
        plot.background = element_rect(fill = "#f9f0e4", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$ratings_plot_title <- renderUI({
    median_rating <- median(listings$review_scores_rating, na.rm = TRUE)
    h4(paste0("Diverging Average Rating by Neighborhood (Median: 4.67)"))
  })
  
  ### TENANT TAB -> ATTRIBUTES SUBTAB DIVERGING LOLLIPOP PLOT
  output$tenant_min_nights_hist <- renderPlot({
    listings %>%
      filter(minimum_nights < quantile(minimum_nights, 0.99, na.rm = TRUE)) %>%
      ggplot(aes(x = minimum_nights)) +
      geom_histogram(binwidth = 5, fill = "#df8330", color = "white") +
      labs(
        title = "",
        x = "Minimum Nights",
        y = "Count"
      ) +
      annotate(
        "label",
        x = 120,  
        y = max(table(listings$minimum_nights)) * 0.9, 
        label = "Government Policy:\nPrivate properties: ≥ 92 nights\nHDB flats: ≥ 180 nights\nServiced apartments: ≥ 7 nights\nHotels: No minimum nights",
        size = 3.5,
        hjust = 0,  # left-align
        vjust = 1,
        label.size = 0.25,
        fill = "#f8f9fa",
        color = "black"
      ) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#f9f0e4", color = NA),
            plot.background = element_rect(fill = "#f9f0e4", color = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_blank())
  })
  
  ### TENANT TAB -> ATTRIBUTES SUBTAB DIVERGING LOCALITY CHECKER
  observeEvent(input$tenant_find_location, {
    req(input$tenant_postal_code)
    
    if (!grepl("^\\d{6}$", input$tenant_postal_code)) {
      output$tenant_locality_info <- renderUI({
        HTML("<b style='color:red;'>Please enter a valid 6-digit Singapore postal code.</b>")
      })
      return(NULL)
    }
    
    geo <- tryCatch({
      geocode_OSM(input$tenant_postal_code, as.data.frame = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(geo) || nrow(geo) == 0) {
      output$tenant_locality_info <- renderUI({
        HTML("<b style='color:red;'>Could not locate this postal code. Please try another.</b>")
      })
      return(NULL)
    }
    
    lat <- geo$lat[1]
    lon <- geo$lon[1]
    
    # Bounding box check: Singapore lat/lon ranges
    if (lat < 1.22 || lat > 1.47 || lon < 103.6 || lon > 104.1) {
      output$tenant_locality_info <- renderUI({
        HTML("<b style='color:red;'>Location found is outside Singapore. Please check your postal code.</b>")
      })
      return(NULL)
    }
    
    ### COMPUTE MRT, BUS, ATTRACTION DISTANCES
    # Median distance from each listing to its *nearest* MRT
    listing_mrt_dists <- apply(listings[, c("latitude", "longitude")], 1, function(row) {
      min(haversine_distance(row[1], row[2], mrt_data$latitude, mrt_data$longitude))
    })
    median_mrt_dist <- median(listing_mrt_dists)
    
    # Same for Bus Stops
    listing_bus_dists <- apply(listings[, c("latitude", "longitude")], 1, function(row) {
      min(haversine_distance(row[1], row[2], bus_data$latitude, bus_data$longitude))
    })
    median_bus_dist <- median(listing_bus_dists)
    
    # Same for Attractions
    listing_att_dists <- apply(listings[, c("latitude", "longitude")], 1, function(row) {
      min(haversine_distance(row[1], row[2], attraction_data$latitude, attraction_data$longitude))
    })
    median_att_dist <- median(listing_att_dists)
    
    mrt_dists <- haversine_distance(lat, lon, mrt_data$latitude, mrt_data$longitude)
    bus_dists <- haversine_distance(lat, lon, bus_data$latitude, bus_data$longitude)
    att_dists <- haversine_distance(lat, lon, attraction_data$latitude, attraction_data$longitude)
    
    nearest_mrt <- mrt_data[which.min(mrt_dists), ]
    nearest_bus <- bus_data[which.min(bus_dists), ]
    nearest_att <- attraction_data[which.min(att_dists), ]
    
    mrt_diff <- round(min(mrt_dists) - median_mrt_dist)
    bus_diff <- round(min(bus_dists) - median_bus_dist)
    att_diff <- round(min(att_dists) - median_att_dist)
    
    diff_text <- function(dist, label) {
      if (dist < 0) paste0("<span style='color:#124944;'>", abs(dist), "m CLOSER</span> than all listings' average")
      else paste0("<span style='color:#bb1e12;'>", abs(dist), "m FURTHER</span> than all listings' average")
    }
    
    output$tenant_locality_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = lon, lat = lat, zoom = 15) %>%
        addAwesomeMarkers(
          lng = lon, lat = lat,
          icon = awesomeIcons(icon = 'map-pin', library = 'fa', markerColor = 'blue'),
          popup = "📌 You are here!",
          group = "selection_links"
        ) %>%
        addAwesomeMarkers(
          lng = nearest_mrt$longitude, lat = nearest_mrt$latitude,
          icon = awesomeIcons(icon = 'train', library = 'fa', iconColor = 'white', markerColor = 'red'),
          popup = paste0(
            "<div style='display:flex; align-items:center;'>",
            "<span style='background:", nearest_mrt$Line_Colors[[1]][1], 
            "; width:10px; height:10px; border-radius:50%; display:inline-block; margin-right:6px;'></span>",
            toupper(gsub(" (MRT|LRT) STATION", "", nearest_mrt$label)),
            " (", nearest_mrt$STN_NO, ")</div>"
          ),
          group = "selection_links"
        ) %>%
        addPolylines(
          lng = c(lon, nearest_mrt$longitude),
          lat = c(lat, nearest_mrt$latitude),
          color = "#bb1e12", weight = 5, opacity = 0.7, group = "selection_links"
        ) %>%
        addAwesomeMarkers(
          lng = nearest_bus$longitude, lat = nearest_bus$latitude,
          icon = awesomeIcons(icon = 'bus', library = 'fa', iconColor = 'white', markerColor = 'green'),
          popup = paste0("<b>", nearest_bus$label, "</b>"),
          group = "selection_links"
        ) %>%
        addPolylines(
          lng = c(lon, nearest_bus$longitude),
          lat = c(lat, nearest_bus$latitude),
          color = "#124944", weight = 5, opacity = 0.7, group = "selection_links"
        ) %>%
        addAwesomeMarkers(
          lng = nearest_att$longitude, lat = nearest_att$latitude,
          icon = awesomeIcons(icon = 'camera', library = 'fa', iconColor = 'white', markerColor = 'orange'),
          popup = paste0("<b>", nearest_att$label, "</b><br>Opening Hours: ", nearest_att$Opening.Hours),
          group = "selection_links"
        ) %>%
        addPolylines(
          lng = c(lon, nearest_att$longitude),
          lat = c(lat, nearest_att$latitude),
          color = "#1f628e", weight = 5, opacity = 0.7, group = "selection_links"
        )
    })
    
    output$tenant_locality_info <- renderUI({
      HTML(paste0(
        "<b>Your Coordinates:</b> ", round(lat, 5), ", ", round(lon, 5), "<br>",
        "<b>Nearest MRT:</b> ", nearest_mrt$label, " (", round(min(mrt_dists)), " m, ", diff_text(mrt_diff), ")<br>",
        "<b>Nearest Bus Stop:</b> ", nearest_bus$label, " (", round(min(bus_dists)), " m, ", diff_text(bus_diff), ")<br>",
        "<b>Nearest Attraction:</b> ", nearest_att$label, " (", round(min(att_dists)), " m, ", diff_text(att_diff), ")<br>"
      ))
    })
  })
  
  ### TENANT TAB -> RATINGS SUBTAB REVIEWS OVER TIME LINE PLOT
  output$tenant_reviews_over_time <- renderPlot({
    monthly_data <- reviews %>%
      mutate(month = as.Date(format(as.Date(date), "%Y-%m-01"))) %>%
      count(month)
    
    ggplot(monthly_data, aes(x = month, y = n)) +
      geom_line(color = "#1a4fbf", linewidth = 1.2) +
      geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = as.Date("2022-04-01"), linetype = "dashed", color = "darkgreen", linewidth = 1) +
      geom_text(aes(x = as.Date("2020-01-01") - 15, y = max(n) * 0.95), label = "COVID-19 hits", angle = 90, vjust = -0.3, hjust = 0.6, color = "red") +
      geom_text(aes(x = as.Date("2022-04-01") - 15, y = max(n) * 0.95), label = "Borders reopen\nfor vaccinated", angle = 90, vjust = -0.3, hjust = 0.6, color = "darkgreen") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
      labs(title = "",
           x = "Year-Month",
           y = "Review Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "#f9f0e4", color = NA),
            plot.background = element_rect(fill = "#f9f0e4", color = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_blank())
  })
  
  ### TENANT TAB -> RATINGS SUBTAB RATING AGAINST AMENITIES SUBPLOT
  output$tenant_rating_vs_amenities <- renderPlot({
    amenity_counts <- amenities_matrix %>%
      mutate(num_amenities = rowSums(select(., -id))) %>%
      select(id, num_amenities)
    
    listings_with_amenities <- listings %>%
      inner_join(amenity_counts, by = "id") %>%
      filter(!is.na(review_scores_rating))
    
    ggplot(listings_with_amenities, aes(x = num_amenities, y = review_scores_rating)) +
      geom_point(alpha = 0.4, color = "#1a4fbf") +
      geom_smooth(method = "lm", se = FALSE, color = "#bb1e12") +
      labs(x = "Number of Amenities", y = "Review Score Rating") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#f9f0e4", color = NA),
        plot.background = element_rect(fill = "#f9f0e4", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank()
      )
  })
})
