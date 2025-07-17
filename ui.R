library(shiny)
library(leaflet)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(
  useShinyjs(),
  div(
    style = "background-color: #f37e75; padding: 10px 20px; display: flex; align-items: center;",
    tags$img(src = "lodgic_logo.jpg", height = "50px", style = "margin-right: 20px;"),
    tags$h2("Singapore Airbnb Explorer", class = "banner-title")
  ),
  br(),
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
  body, input, button, select, .control-label, .form-control, .checkbox label {
    font-family: 'Inter', sans-serif;
    font-size: 13px;
  }

  body {
    background-color: #f9f0e4;
  }

  h1, h2, h3, h4, h5, h6 {
    color: #f47f76 !important;
    font-weight: bold !important;
  }

  /* Selected tab background */
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover,
  .tabbable > .nav > li[class=active] > a {
    background-color: #f9f4ea !important;
    color: #000 !important;
  }
  
  .nav-tabs > li > a {
    color: #f47f76 !important;
    font-weight: bold;
  }

  .listing-box {
    background-color: #f8f9fa;
    padding: 10px;
    border-radius: 5px;
    box-shadow: 1px 1px 4px rgba(0,0,0,0.1);
    line-height: 1.5;
  }

  .well {
    background-color: #fceee9 !important;
    border: none;
    padding: 15px !important;
    margin-bottom: 20px;
    border-radius: 5px;
    box-shadow: 0px 1px 3px rgba(0, 0, 0, 0.05);
  }
  
  .banner-title {
  color: #f9f0e4 !important;
  font-weight: bold;
  font-size: 28px;
  margin: 0;
  }
  
  .irs-bar,
  .irs-bar-edge,
  .irs-single,
  .irs-from,
  .irs-to {
    background-color: #f47f76 !important;
    border-color: #f47f76 !important;
    color: #f9f0e4 !important;
    font-weight: bold !important;
  }
  
  .irs-grid-text {
    color: #f47f76;
    font-weight: bold;
  }
  
  .irs-slider {
    border-color: #f47f76 !important;
    background: #f47f76 !important;
  }
"))),
  
  tabsetPanel(
    tabPanel("📖 README",
             fluidRow(
               column(12,
                      h3("🌞 Welcome to Sunny Singapore! 😎"),
                      HTML("Looking for your dream vacation stay, without breaking the bank? We got you!<br><br>
                      This app will help you explore Airbnb listings across Singapore and tailor the search results to your needs!<br><br>
                      If you are visiting, please use the <strong>'Visitors'</strong> tab to search information about listings and amenities nearby! Filter options are available to tailor your search results to find the perfect stay for your next getaway!<br><br>
                      If you are a tenant looking to lease out your home to potential visitors, please use the <strong>'Tenants'</strong> tab! It contains an exploratory dashboard showing <strong>Price, Listings, Ratings and Attribute</strong> statistics, for you to tailor your home and maintain a competitive advantage over other homeowners!<br><br>
                      Happy exploring! 😊")
               )
             )
    ),
    
    tabPanel("🧳 Visitors",
             fluidRow(
               column(12,
                      h3("Your handy-dandy Airbnb Guide!"),
                      tags$p(HTML(
                        "This interactive map helps users explore Airbnb listings in Singapore with filtering options for region, neighbourhood, price, room type, and minimum stay. Clicking on a listing reveals detailed information on the right, including amenities, host responsiveness, and nearby transport or attractions. You can also toggle the display of MRT stations, tourist attractions, and bus stops:<br><br>
                         • Clicking on an <b>attraction</b> marker shows the name and location of the tourist site.<br>
                         • Clicking on a <b>bus stop</b> displays its bus stop number, compatible with local transport apps like SBS Transit.<br>
                         • Clicking on an <b>MRT station</b> reveals its name, station code, and line color to support easier navigation and wayfinding."
                      ), style = "color:black;"),
                      wellPanel(
                        fluidRow(
                          column(12,
                                 uiOutput("visitor_region_ui")
                          )
                        ),
                        uiOutput("visitor_neighbourhood_ui"),
                        uiOutput("visitor_roomtype_ui"),
                        uiOutput("visitor_price_ui"),
                        uiOutput("visitor_min_nights_ui")
                      )
                    )
              ),
             
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput(
                          "overlay_mode", "Display Mode for Amenities",
                          choices = c("Show All", "Show Nearest"),
                          selected = "Show All",
                          width = "200px"
                        ),
                        conditionalPanel(
                          condition = "input.overlay_mode == 'Show All'",
                          uiOutput("show_all_checkbox_ui")
                        ),
                        conditionalPanel(
                          condition = "input.overlay_mode == 'Show Nearest'",
                          uiOutput("show_nearest_checkbox_ui")
                        )
                      )
                    )
              ),
             
             fluidRow(
               column(8, leafletOutput("airbnb_map", height = "80vh")),
               column(4,
                      h4("Selected Listing Info"),
                      uiOutput("selected_info"),
                      br()
                    )
             )
    ),
    
    tabPanel("🏠 Tenants",
             tabsetPanel(
               tabPanel("💰 Price",
                        fluidRow(
                          column(12,
                                 h3("Region- / Neighbourhood-Specific Price Statistics"),
                                 tags$p("This section allows you to explore the pricing landscape of Airbnb listings across Singapore. Use the filters above to narrow down listings by region, neighbourhood, room type, price range, and minimum stay requirements. The heatmap on the left visualizes price intensity geographically, highlighting areas with higher or lower nightly rates, while the histogram on the right shows the distribution of listing prices within the filtered dataset.", style = "color:black;")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   fluidRow(
                                     column(12, uiOutput("tenant_price_region_ui"))
                                   ),
                                   uiOutput("tenant_price_neighbourhood_ui"),
                                   uiOutput("tenant_price_roomtype_ui"),
                                   uiOutput("tenant_price_price_ui"),
                                   uiOutput("tenant_price_min_nights_ui")
                                 )
                          )
                        ),
                        fluidRow(
                          column(8,
                                 leafletOutput("tenant_heatmap", height = "400px")
                          ),
                          column(4,
                                 h4("Price Distribution (Filtered)"),
                                 plotlyOutput("tenant_price_dist", height = "400px")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 h3("Overall Statistics")
                          )
                        ),
                        fluidRow(
                          column(4,
                                 h5("• Price by Region"),
                                 plotOutput("tenant_price_box_region", height = "500px"),
                                 tags$p(HTML(
                                   "This boxplot displays the distribution of Airbnb listing prices across Singapore’s five major regions.<br>
                                    Each box shows the interquartile range (25th to 75th percentile), with the line in the middle representing the median price.<br>
                                    Outliers beyond the whiskers are shown as individual dots, helping to highlight extreme values.<br><br>
                                    <b>Key findings:</b><br>
                                      • <b>Central Region</b> exhibits the highest price variability, with a wide spread and many high-priced outliers, reflecting its prime location and tourist demand.<br>
                                      • <b>East, North, and North-East Regions</b> show lower median prices and tighter spreads, indicating more budget-friendly and consistent pricing.<br>
                                      • <b>West Region</b> presents a moderate range with some listings priced comparably to Central, possibly due to proximity to business hubs or universities.<br>
                                      • The presence of extreme outliers in all regions suggests that some high-end listings or luxury accommodations exist even outside of the Central Region."
                                 ), style = "color:black;")
                          ),
                          column(8,
                                 h5("• Price by Neighbourhood"),
                                 plotOutput("tenant_price_box_neighbourhood", height = "500px"),
                                 tags$p(HTML(
                                   "This boxplot breaks down Airbnb listing prices by neighbourhood, providing a more granular view of pricing patterns across Singapore.<br>
                                    Each box shows the range and distribution of prices for listings within a neighbourhood, while the color of each box reflects its broader region.<br>
                                    Outliers are shown as purple dots, indicating listings priced significantly higher than the norm.<br><br>
                                    <b>Key findings:</b><br>
                                      • <b>Marina South, Orchard, and Central Area</b> have some of the highest median prices, reflecting their status as premium and tourist-heavy zones.<br>
                                      • <b>Neighbourhoods like Bedok, Sembawang, and Woodlands</b> show much lower prices, making them more attractive for budget-conscious travelers.<br>
                                      • <b>Significant price variability</b> is observed within some neighbourhoods, such as Downtown Core and River Valley, suggesting a wide mix of listing types from budget to luxury.<br>
                                      • Outliers are present across both expensive and affordable neighbourhoods, indicating that even in lower-priced areas, a few high-end listings do exist.<br>
                                      • The region color coding allows users to compare how pricing differs not just by neighbourhood, but also across broader geographical zones."
                                 ), style = "color:black;")
                              )
                        )
               ),
               
               tabPanel("🏘️ Listings",
                        fluidRow(
                          column(12,
                                 h3("Region- / Neighbourhood-Specific Listing Statistics"),
                                 tags$p("Use the filters below to explore Airbnb listing availability across Singapore by region, neighbourhood, room type, price, and minimum stay duration. The interactive heatmap on the left shows listing density geographically, while the bar chart on the right highlights how many listings exist in each neighbourhood based on your current filter selections.", style = "color:black;"),
                                 wellPanel(
                                   fluidRow(
                                     column(12, uiOutput("tenant_listings_region_ui"))
                                   ),
                                   uiOutput("tenant_listings_neighbourhood_ui"),
                                   uiOutput("tenant_listings_roomtype_ui"),
                                   uiOutput("tenant_listings_price_ui"),
                                   uiOutput("tenant_listings_min_nights_ui")
                                 )
                          )
                        ),
                        # Heatmap and Count Plot Side-by-Side
                        fluidRow(
                          column(8,
                                 leafletOutput("tenant_count_heatmap", height = "400px")
                          ),
                          column(4,
                                 h4("Listing Distribution (Filtered)"),
                                 plotlyOutput("tenant_count_dist", height = "400px")
                          )
                        ),
                        
               ),
               
               tabPanel("⭐ Ratings",
                        fluidRow(
                          column(8,
                                 uiOutput("ratings_plot_title"),
                                 plotOutput("tenant_avg_rating_lollipop", height = "500px"),
                                 tags$p(HTML(
                                   "This lollipop chart displays the average Airbnb review scores for each neighbourhood,<br>
                                    highlighting which areas exceed or fall below the overall median rating of 4.67.<br><br>
                                    Neighbourhoods above the median are shown in blue, and those below are shown in red,<br>
                                    offering a quick visual cue on relative guest satisfaction across Singapore.<br><br>
                                    <b>Key findings:</b><br>
                                      • <b>Top-rated areas</b> include Choa Chu Kang, Bukit Panjang, and Ang Mo Kio, all scoring 4.87 or higher.<br>
                                      • <b>Central and high-traffic zones</b> like Downtown Core, Outram, and Queenstown scored slightly below average,<br>
                                      possibly due to higher guest expectations, noise, or congestion.<br>
                                      • <b>Bishan</b> notably stands out as the lowest-rated neighbourhood with a score of 3.22,<br>
                                      indicating potential issues with listings in that area or limited supply with poor experiences.<br>
                                      • Despite geographic proximity, there is <b>substantial variability across neighbourhoods</b>,<br>
                                      suggesting local host quality and amenities heavily influence ratings."
                                 ), style = "color:black;")
                          ),
                          column(4,
                                 h4("Amenities vs Review Rating"),
                                 plotOutput("tenant_rating_vs_amenities", height = "500px"),
                                 tags$p(HTML(
                                   "This scatter plot shows how the number of amenities in a listing correlates with its review score,
                                    helping to identify if better-equipped listings receive higher ratings.<br><br>
                                    Each dot represents a single listing, while the red trend line represents the general direction of the relationship.
                                    The positive slope of the line indicates that, on average, listings with more amenities tend to receive slightly higher review scores.<br><br>
                                    <b>Key findings:</b><br>
                                      • Most listings cluster around the 4.5 - 5.0 rating range regardless of amenity count.<br>
                                      • A few outliers with low ratings and high amenities suggest that other factors like service or cleanliness may also influence reviews.<br>
                                      • Listings with minimal amenities tend to show more variability in ratings, potentially due to guest expectations not being met."
                                 ), style = "color:black;")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 h4("Monthly Airbnb Reviews Over Time"),
                                 plotOutput("tenant_reviews_over_time", height = "450px"),
                                 tags$p(HTML(
                                   "This time series plot shows the total number of Airbnb reviews received each month in Singapore,
                                    providing a lens into Airbnb activity and traveler demand over time.<br><br>
                                    Key events are highlighted: a sharp drop in early 2020 following the onset of COVID-19,
                                    and a significant recovery beginning in April 2022 when borders reopened for vaccinated travelers.<br><br>
                                    <b>Key findings:</b><br>
                                      • Steady growth in Airbnb activity from 2014 to 2019 indicates increasing popularity and adoption.<br>
                                      • COVID-19 caused an immediate and drastic decline in reviews, with fluctuations during the pandemic period.<br>
                                      • Post-reopening, the volume of reviews surged and stabilized at levels higher than pre-pandemic,
                                    suggesting a strong rebound in tourism and short-term rental demand."
                                  ), style = "color:black;")
                                )
                        )
               ),
               
               tabPanel("🔎 Attributes",
                        fluidRow(
                          column(8, offset = 2,
                                 h4("Distribution of Minimum Nights Required"),
                                 plotOutput("tenant_min_nights_hist", height = "400px"),
                                 tags$p(HTML(
                                   "This histogram visualizes how many nights are required as the minimum stay for each listing on Airbnb,<br>
                                    helping users understand how flexible or restrictive hosts are with their bookings.<br><br>
                                    The policy reference box on the right provides context for Singapore's short-term rental regulations,<br>
                                    with key thresholds such as ≥ 92 nights for private properties and ≥ 180 nights for HDB flats.<br><br>
                                    <b>Key findings:</b><br>
                                      • A large number of listings cluster at <b>short minimum stays</b> (e.g., 1–7 nights), suggesting they are likely hotels, serviced apartments, or possibly non-compliant listings.<br>
                                      • A significant peak appears at exactly <b>92 nights</b>, indicating hosts are deliberately setting values that comply with private property regulations.<br>
                                      • Very few listings meet the minimum threshold of <b>180 nights</b> required for HDB properties, highlighting the rarity of compliant short-term listings from that category.<br>
                                      • The distribution suggests a <b>bimodal trend</b>: one group aiming for tourist flexibility, the other aiming to comply with local laws."
                                  ), style = "color:black;")
                                )
                        ),
                        
                        fluidRow(
                          column(8, offset = 2,
                                 h4("Locality Insights"),
                                 tags$p("Enter your postal code to discover how close you are to key amenities like MRT stations, bus stops, and tourist attractions. This feature helps assess the convenience and accessibility of your location in comparison to typical Airbnb listings.", style = "color:black;"),
                                 textInput("tenant_postal_code", "Enter Your Postal Code:", placeholder = "e.g. 138632"),
                                 actionButton("tenant_find_location", "Find Nearby Amenities", icon = icon("map-marker")),
                                 leafletOutput("tenant_locality_map", height = "500px"),
                                 br(),
                                 htmlOutput("tenant_locality_info")
                          )
                        )
                      )
             )
        )
    )
))
