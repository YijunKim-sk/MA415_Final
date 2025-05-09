# app.R

#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("leaflet")       
#install.packages("DT")           
#install.packages("shinythemes")
#install.packages("sf")
#install.packages("slickR")
#install.packages("shinyWidgets")

library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(shinythemes)
library(sf)
library(slickR)
library(shinyWidgets)
library(dplyr)
library(lubridate)

vegetation <- read.csv("data/vegetation.csv", fileEncoding = "utf-8") %>%
  select(-lo, -la)

colnames(vegetation) <- c(
  "survey_date",       
  "species_name_kr",    
  "diameter_cm",       
  "grade_code"
)

species_dict <- c(
  "너도밤나무" = "Fagus engleriana",
  "섬벚나무" = "Prunus takesimensis",
  "마가목" = "Sorbus commixta",
  "고로쇠나무" = "Acer pictum",
  "난티나무" = "Rhododendron yedoense",
  "두메오리나무" = "Weigela subsessilis",
  "쪽동백나무" = "Styrax obassia",
  "섬단풍나무" = "Acer takesimense",
  "솔송나무" = "Abies holophylla",
  "섬피나무" = "Celtis sinensis var. japonica",
  "주목" = "Taxus cuspidata",
  "층층나무" = "Cornus controversa",
  "황벽나무" = "Phellodendron amurense",
  "음나무" = "Kalopanax septemlobus",
  "풍게나무" = "Carpinus tschonoskii"
)

vegetation$species_name_en <- species_dict[vegetation$species_name_kr]

veg_df <- vegetation %>%
  filter(!is.na(species_name_en), !is.na(diameter_cm)) %>%
  mutate(diameter_cm = as.numeric(diameter_cm))

shape_path <- "data/veg_com/veg_com.shp"
shape_data <- st_read(shape_path, options = "ENCODING=CP949")

# Rename and transform
colnames(shape_data) <- c(
  "id", "symbol", "community_name", "conservation_grade", 
  "inxnum", "inxname", "length", "area", "geometry"
)
shape_data <- st_transform(shape_data, crs = 4326)
shape_data$area_m2 <- st_area(shape_data)
shape_data <- st_zm(shape_data, drop = TRUE)

community_dict <- c(
  "우산고로쇠군락" = "Usan Maple Forest",
  "우산고로쇠-너도밤나무군락" = "Usan Maple–Fagus Forest",
  "우산고로쇠-곰솔군락" = "Usan Maple–Black Pine Forest",
  "나지" = "Bare Ground",
  "우산고로쇠나무-마가목군락" = "Usan Maple–Sorbus Forest",
  "동백나무군락" = "Camellia Forest",
  "삼나무식재림" = "Cryptomeria Plantation",
  "농경지" = "Agricultural Land",
  "개발지" = "Developed Area",
  "벌채지" = "Logged Area",
  "너도밤나무군락" = "Fagus Forest",
  "너도밤나무-솔송나무군락" = "Fagus–Korean Fir Forest",
  "향나무군락" = "Juniper Forest",
  "억새군락" = "Miscanthus Grassland",
  "후박나무군락" = "Machilus Forest",
  "소나무군락" = "Pine Forest",
  "곰솔식재림" = "Black Pine Plantation",
  "식재림" = "Planted Forest",
  "섬잣나무군락" = "Island Nutmeg Yew Forest",
  "섬잣나무-솔송나무군락" = "Nutmeg Yew–Korean Fir Forest",
  "곰솔군락" = "Black Pine Forest",
  "곰솔-우산고로쇠군락" = "Black Pine–Usan Maple Forest",
  "곰솔-동백나무군락" = "Black Pine–Camellia Forest",
  "곰솔-아까시나무군락" = "Black Pine–Acacia Forest",
  "암벽" = "Rocky Cliff",
  "마가목-우산고로쇠군락" = "Sorbus–Usan Maple Forest",
  "2차초지" = "Secondary Grassland",
  "느티나무군락" = "Zelkova Forest",
  "솔송나무-너도밤나무군락" = "Korean Fir–Fagus Forest",
  "느티나무-곰솔나무군락" = "Zelkova–Black Pine Forest"
)

shape_data$community_en <- community_dict[shape_data$community_name]

pop_df <- read_csv("data/demo_after_clean.csv") %>%
  mutate(
    population_total = as.numeric(population_total),
    population_male = as.numeric(population_male),
    population_female = as.numeric(population_female),
    year = as.integer(year)
  )

tour_df <- read_csv("data/ulleung_tourist_stats.csv")

# Remove 'X' from year names
colnames(tour_df)[-1] <- gsub("^X", "", colnames(tour_df)[-1])

tour_long <- tour_df %>%
  pivot_longer(-Month, names_to = "Year", values_to = "Visitors") %>%
  mutate(
    Year = as.integer(Year),
    Month = factor(Month, levels = month.name)  # ensure correct month order
  )

# UI
ui <- tagList(
  # CSS Definition
  tags$head(
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
      }
      
      .nav-tabs > li > a[data-value='Vegetation Dynamics'] {
       margin-left: 60px;
      }


      .hero-image.full {
        position: relative;
        width: 100%;
        height: 900px;
        background-image: url('3.png');
        background-size: cover;
        background-position: center;
      }
      

       .hero-image.strip {
        height: 300px;
        background-image: url('3.png');
        background-size: cover;
        background-position: center 45%;
        position: relative;
        width: 100%;
        overflow: hidden;
      }

      .hero-subtitle-box {
        position: absolute;
        top: 80px;
        right: 80px;
        text-align: right;
        color: white;
      }

      .hero-subtitle {
        font-size: 1.6em;
        font-weight: 300;
        margin: 1px 0;
        letter-spacing: 13px;
        color: white;
      }

      .hero-title {
        position: absolute;
        bottom: 100px;
        left: 30px;
        width: 100%;
        text-align: center;
        font-size:  12.3em;
        font-weight: 700;
        color: white;
        letter-spacing: 61px;
      }
      
      .hero-image.full .hero-title {
        font-size: 12.3em;
        bottom: 100px;
        left: 30px;
        letter-spacing: 61px;
      }
      
      .hero-image.strip .hero-title {
        font-size: 6em;
        bottom: 30px;
        letter-spacing: 61px;
      }

      .navbar {
        position: absolute !important;
        top: 0;
        width: 100%;
        z-index: 999;
        background-color: rgba(0, 0, 0, 0.4) !important;
        border: none;
      }

      .navbar-default {
        background-color: transparent !important; 
        border: none;
        box-shadow: none;
      }
  
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-brand {
        font-size: 16px !important;    
        padding-top: 30px !important;  
        padding-bottom: 30px !important;
        padding-left: 37px !important;   
        padding-right: 37px !important;  
        color: white !important; 
        font-weight: bold;
      }
  
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255,255,255,0.2) !important;  
        color: white !important;
      }

      .main-content {
        margin-top: 0px;
        padding: 70px;
      }
    ")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('setHeroClass', function(className) {
      const heroDiv = document.querySelector('.hero-image');
      if (heroDiv) {
        heroDiv.className = className;
      }
  
      // Hide subtitle if not full view
      const subtitleBox = document.getElementById('subtitle-box');
      if (subtitleBox) {
        subtitleBox.style.display = className.includes('full') ? 'block' : 'none';
      }
    });
    "))
  ),
  
  tags$div(class = "hero-image full",
           tags$div(id = "subtitle-box", class = "hero-subtitle-box",
                    tags$div("An island carved by time,", class = "hero-subtitle"),
                    tags$div("preserved by nature.", class = "hero-subtitle")
           ),
           tags$div("Ulleungdo", class = "hero-title")
  ),
  
  navbarPage(id = "navbar",
             title = tags$div(
               tags$img(src = "16.png", height = "50px", style = "margin-right: 15px; margin-left: 15px; margin-top: -5px;")
             ),
             
             tabPanel("Main",
                      fluidPage(class = "main-content",
                                tags$h2("Where the sea meets the sky,", 
                                        style = "text-align: center; font-size: 30px; font-style: italic; margin-bottom: 50px;"),
                                
                                fluidRow(
                                  column(4,
                                         tags$div(
                                           tags$img(src = "4.jpg", style = "width: 100%; height: 250px; object-fit: cover; border-radius: 10px;"),
                                           tags$p("Sorok Ridge (소록능선)", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                                         )
                                  ),
                                  column(4,
                                         tags$div(
                                           tags$img(src = "6.jpg", style = "width: 100%; height: 250px; object-fit: cover; border-radius: 10px;"),
                                           tags$p("Cheonbu Coastal Observatory (천부 해안 전망대)", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                                         )
                                  ),
                                  column(4,
                                         tags$div(
                                           tags$img(src = "7.jpg", style = "width: 100%; height: 250px; object-fit: cover; border-radius: 10px;"),
                                           tags$p("Bongnae Waterfall (봉래폭포)", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                                         )
                                  )
                                )
                      )
             ),
             
             tabPanel("About Ulleungdo",
                      fluidPage(class = "main-content",
                                
                                fluidRow(
                                  column(6,
                                         tags$img(src = "10.jpg", 
                                                  style = "width: 90%; height: auto; border-radius: 10px; margin-top: -5px; margin-left: 30px;")
                                  ),
                                  column(6,
                                         tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                  padding: 0px; 
                                  margin-top: -30px;
                                  margin-right: 70px;
                                  border-radius: 15px;",
                                                  tags$h3("Where is Ulleungdo?"),
                                                  tags$p("Ulleungdo is a volcanic island situated in the East Sea (Donghae), 
                                         approximately 120 kilometers east of the Korean Peninsula. 
                                         It is part of Ulleung County in North Gyeongsang Province, South Korea."),
                                                  tags$p("With a rich history dating back to ancient Korea, 
                                         Ulleungdo has appeared in texts such as the 'Samguk Sagi' and 'Annals of the Joseon Dynasty'. 
                                         It was historically known as 'Usan-guk' and has long been considered a strategic outpost in Korea’s eastern waters."),
                                                  tags$p("Ulleungdo is renowned for its dramatic cliffs, crystal-clear coastal waters, and dense forests. 
                                         The island is largely mountainous, with Seonginbong Peak reaching 984 meters above sea level."),
                                                  tags$p("Culturally and geopolitically, Ulleungdo is closely tied to Dokdo, another disputed islet in the East Sea. 
                                         The two islands are often regarded together in discussions of Korea’s eastern maritime territory."),
                                                  tags$p("Today, Ulleungdo is a unique destination blending natural splendor, ecological preservation, and deep-rooted history, 
                                         attracting visitors interested in both adventure and heritage.")
                                         )
                                  )
                                )
                      ),
                      fluidRow(
                        column(6,
                               tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                          padding: 5px; 
                                          border-radius: 20px; margin-top: 0px; margin-left: 90px;",
                                        tags$h3("Geography and Terrain"),
                                        tags$p("Ulleungdo was formed by submarine volcanic activity about 2.6 million years ago. 
                                 Its most prominent peak, Seonginbong (984 meters), lies at the island’s center and defines its steep topography."),
                                        tags$p("There is very little flat land on the island. Villages and farmland are limited to narrow coastal terraces, 
                                 while the interior is covered in mountainous terrain and thick forests."),
                                        tags$p("As elevation increases, the ecosystem transitions dramatically — from coastal flora and human settlements 
                                 to mid-elevation forests and high-altitude zones with rare species and protected natural monuments.")
                               )
                        ),
                        column(6,
                               tags$img(src = "11.png", 
                                        style = "width: 90%; height: auto; border-radius: 10px; margin-top: 10px;")
                        )
                      ),
                      fluidRow(
                        column(11,
                               tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                          padding: 5px; 
                                          border-radius: 20px; margin-top: -10px; margin-left: 90px;",
                                        tags$h3("Ulleungdo's location "),
                                        tags$p("The maps below show the geographic location of Ulleungdo from both local and global perspectives.
                                       The left map zooms in on the island itself, while the right map places Ulleungdo in the context of the world."
                                        )
                               )
                        )
                      ),
                      fluidRow(
                        column(5, offset = 1, style = "padding: 10px; width: 40%; margin-bottom: 30px;", leafletOutput("map_local", height = 300)),
                        column(5, style = "padding: 10px; width: 40%; ", leafletOutput("map_global", height = 300))
                      )
             ),
             
             tabPanel("Ecology", 
                      fluidPage(class = "main-content",
                                tags$h2("Ecology", style = "margin-bottom: 30px; margin-left: 60px; font-size: 40px;"),
                                
                                tabsetPanel(id = "ecology_tab",
                                            
                                            tabPanel("Vegetation Dynamics",
                                                     br(),
                                                     tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                                      padding: 20px; 
                                                      border-radius: 15px; 
                                                      margin-bottom: 20px;
                                                      margin-left: 60px;
                                                      margin-right: 60px;",
                                                              tags$h3("Vegetation Dynamics in Ulleungdo", style = "font-size: 30px;"),
                                                              tags$p("This section presents data from a vegetation survey conducted on Ulleungdo, focusing 
                                                              on native tree species, their trunk diameters, and conservation grades. The dataset 
                                                              captures species-level composition and size distribution across multiple survey dates.")
                                                     ),
                                                     
                                                     tags$h3("Distribution of Vegetation Community", style = "margin-left: 80px; margin-top: 2px;"),
                                                     
                                                     fluidRow(
                                                       column(12,
                                                              tags$p("This section visualizes the Distribution of different plant communities 
                                                              on Ulleungdo. The map on the left shows spatial distributions, while the chart 
                                                              on the right summarizes area size by conservation grade.", 
                                                                     style = "margin-left: 80px; margin-right: 80px;font-size: 14px;")
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(7,
                                                              style = "margin-top: 0px; margin-left: 80px;",
                                                              selectInput("selected_community", "Select Community:",
                                                                          choices = unique(shape_data$community_en),
                                                                          selected = "Usan Maple Forest"),
                                                              leafletOutput("community_map", height = 400)
                                                       ),
                                                       column(4,
                                                              style = "margin-top: 70px;",
                                                              plotOutput("grade_plot", height = 400)
                                                       )
                                                     ),
                                                     column(6, offset = 1,
                                                            style = "margin-top: 40px; margin-left: 80px;",
                                                            tags$h3("Top 15 Tree Species by Avg. Diameter"),
                                                            plotOutput("avg_diameter_plot", height = 400)
                                                     ),
                                                     
                                                     br(),
                                                     
                                                     tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                                                        padding: 20px; 
                                                                        border-radius: 15px; 
                                                                        margin-top: 60px;
                                                                        margin-right: 30px;",
                                                              tags$p("From this ecological data, we can observe that some species such as 'Fagus engleriana' and 'Acer takesimense' dominate 
                                                                      in both presence and average size. Continued monitoring is crucial for understanding vegetation shifts under climate change.")
                                                     ),
                                                     
                                                     br(), br(), 
                                                     
                                                     tags$h3("Full Species Observation Table", 
                                                             style = "margin-top: 300px; margin-left: 120px; margin-bottom: 30px;"),
                                                     
                                                     fluidRow(
                                                       column(2, offset = 2,
                                                              selectInput("selected_date", "Select Survey Date:", 
                                                                          choices = c("All", sort(unique(veg_df$survey_date))), 
                                                                          selected = "All")
                                                       ),
                                                       column(3,
                                                              selectInput("selected_species", "Select Tree Species:",
                                                                          choices = c("All", sort(unique(veg_df$species_name_en))), 
                                                                          selected = "All")
                                                       )
                                                     ),
                                                     
                                                     tags$div(style = "width: 80%; margin: 0 auto;",
                                                              dataTableOutput("vegetation_table")
                                                     )
                                            ),
                                            
                                            tabPanel("Marine Environment",
                                                     br(),
                                                     tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                                      padding: 20px; 
                                                      border-radius: 15px; 
                                                      margin-bottom: 20px;
                                                      margin-left: 60px;
                                                      margin-right: 60px;",
                                                              tags$h3("Marine Image Viewer", style = "font-size: 30px;"),
                                                              tags$p("This section displays marine environment images collected over time near Ulleungdo. 
                                                             The slider below lets you explore daily images captured between February and March 2025.")
                                                     ),
                                                     tags$div(style = "background-color: rgba(255,255,255,0.85); 
                                                              padding: 20px; 
                                                              border-radius: 10px; 
                                                              margin: 10px 60px 10px 60px; font-size: 14px;",
                                                              tags$h4("Key Components (Marine Eddy Map)", style = "margin-bottom: 15px; margin-top: -50px;"),
                                                              
                                                              tags$ul(
                                                                tags$li(HTML("<b>🔴 Red regions (Warm Eddy):</b> Higher sea surface height and warm water upwelling. 
                                                                              Clockwise rotation. Often associated with <i>lower biological productivity</i>.")),
                                                                tags$li(HTML("<b>🔵 Blue regions (Cold Eddy):</b> Lower sea surface height and cold, nutrient-rich water upwelling. 
                                                                              Counter-clockwise rotation. Supports <i>plankton blooms and fish</i> due to enhanced nutrient supply."))
                                                              ),
                                                              
                                                              tags$p(HTML("<b>⚫ Gray background:</b> Indicates relative sea level anomaly (SLA) from satellite data (CMEMS).")),
                                                              tags$p(HTML("<b>🟥 Red box:</b> Highlights the UWE/DCE monitoring area near the East Sea, centered around Ulleungdo.")),
                                                              tags$p(HTML("<b>➰ Contour lines:</b> Represent sea surface height changes — the <i>closer the lines, the stronger the current</i> or more defined the eddy boundary."))
                                                     ),
                                                     fluidRow(
                                                       column(4, offset = 1,
                                                              sliderInput("marine_date", "Select Date:",
                                                                          min = as.Date("2025-02-01"),
                                                                          max = as.Date("2025-03-30"),
                                                                          value = as.Date("2025-02-01"),
                                                                          timeFormat = "%Y-%m-%d",
                                                                          animate = animationOptions(interval = 800, loop = TRUE)),
                                                       ),
                                                       column(6,
                                                              uiOutput("marine_image")
                                                       )
                                                     )
                                            )
                                )
                      )
             ),
             tabPanel("Demographics", fluidPage(class = "main-content", 
                                                tags$h2("Demographics of Ulleungdo", style = "margin-bottom: 30px; margin-left: 60px; font-size: 40px;"),
                                                
                                                # Overview Section
                                                tags$div(
                                                  style = "margin-bottom: 30px;; border-radius: 10px;",
                                                  br(),
                                                  tags$h4("Population Trends by Region", style = "margin-left: 60px; margin-top: 2px; font-size: 30px;"),
                                                  tags$div(
                                                    style = "margin-left: 60px; margin-right: 60px;font-size: 14px;",
                                                    tags$p("This section explores the population dynamics of Ulleungdo, a remote volcanic island with a small and aging population. Due to limited land availability and declining birth rates, the island has experienced a gradual population decrease over the past decade."),
                                                    tags$p("The data presented here tracks total population and gender distribution by year, offering insights into demographic trends that affect regional planning, healthcare, and education services."),
                                                    tags$p("Use the sidebar to filter by town, legal district, or administrative district. You may also compare selected regions to the overall island-wide population trend. These tools help identify whether specific districts are aging faster, losing population more rapidly, or maintaining demographic balance.")
                                                  )
                                                ),
                                                
                                                # Key Trends Description
                                                tags$div(
                                                  style = "margin-bottom: 30px;; margin-top: 20px; border-radius: 10px;",
                                                  tags$h3("Key Trends", style = "margin-left: 60px; margin-top: 2px; font-size: 30px;"),
                                                  tags$div(
                                                    style = "margin-left: 40px; margin-right: 40px;font-size: 14px;",
                                                    tags$ul(
                                                      tags$li("Population has shown a gradual decline after 2012."),
                                                      tags$li("Male population consistently exceeds female population."),
                                                      tags$li("Recent years indicate a slight rebound in total numbers.")
                                                     )
                                                  )
                                                ),
                                                
                                                # Sidebar + First Plot (in one row)
                                                fluidRow(
                                                  column(3, offset=1,
                                                         wellPanel(
                                                           selectInput("selected_town", "Select Town:", choices = NULL),
                                                           selectInput("selected_legal", "Select Legal District:", choices = NULL),
                                                           selectInput("selected_admin", "Select Administrative District:", choices = NULL),
                                                           checkboxInput("show_total", "Show overall population trend", value = FALSE)
                                                         )
                                                  ),
                                                  column(7,
                                                         plotOutput("pop_trend_plot", height = "300px")
                                                  )
                                                ),
                                                
                                                # Second Plot (below)
                                                fluidRow(
                                                  column(12, offset= 1,
                                                         plotOutput("gender_bar_plot", height = "300px", width = "80%")
                                                  )
                                                )
             )),                                                
             tabPanel("Tourism", fluidPage(class = "main-content",
                                           tags$h2("Ulleungdo Tourism Statistics (2014–2023)", 
                                                   style = "margin-bottom: 30px; margin-left: 60px; font-size: 40px;"),
                                           
                                           # Overview Section
                                           tags$div(
                                             style = "margin-bottom: 30px; border-radius: 10px;",
                                             br(),
                                             tags$h4("Tourism Overview", 
                                                     style = "margin-left: 60px; margin-top: 2px; font-size: 30px;"),
                                             tags$div(
                                               style = "margin-left: 60px; margin-right: 60px; font-size: 14px;",
                                               tags$p("Ulleungdo is a remote island in the East Sea known for its dramatic cliffs, marine biodiversity, and cultural heritage. Tourism plays a vital role in the island’s economy, providing seasonal boosts and sustaining local businesses."),
                                               tags$p("This section presents annual and monthly tourist visit data from 2014 to 2023. It provides insights into long-term trends as well as the seasonal peaks that shape local infrastructure needs and environmental concerns."),
                                               tags$p("Use the year slider to explore monthly visit patterns for a specific year. This helps assess the impact of events such as COVID-19, weather anomalies, or government travel promotions on visitor flow.")
                                             )
                                           ),
                                           
                                           # Yearly Key Trends Section
                                           tags$div(
                                             style = "margin-top: 20px; margin-bottom: 40px;",
                                             tags$h3("Key Trends: Annual", style = "margin-left: 60px; font-size: 30px;"),
                                             tags$div(style = "margin: 20px 60px; background-color: #f9f9f9; padding: 25px 20px 20px 20px; border-left: 5px solid skyblue; border-radius: 5px;",
                                                      tags$ul(
                                                        tags$li("Visitor numbers remained steady from 2014 to 2019, with gradual growth."),
                                                        tags$li("Significant drop in 2020 due to the COVID-19 outbreak."),
                                                        tags$li("Sharp recovery in 2022 and 2023 as restrictions were lifted."),
                                                        tags$li("Notable declines in 2014 and 2015 due to national crises (Sewol ferry disaster and MERS).")
                                                      )
                                             )
                                           ),
                                           
                                           # Yearly Plot
                                           fluidRow(
                                             column(width = 10, offset = 1,
                                                    plotOutput("yearly_total_plot", height = "400px", width = "100%")
                                             )
                                           ),
                                           
                                           br(), br(),
                                           
                                           # Monthly Key Trends Section
                                           tags$div(
                                             style = "margin-top: 20px; margin-bottom: 40px;",
                                             tags$h3("Key Trends: Monthly", style = "margin-left: 60px; font-size: 30px;"),
                                             tags$div(style = "margin: 20px 60px; background-color: #f9f9f9; padding: 25px 20px 20px 20px; border-left: 5px solid steelblue; border-radius: 5px;",
                                                      tags$ul(
                                                        tags$li("Visitor numbers typically peak in May and August, indicating strong seasonality."),
                                                        tags$li("Winter months (December–February) consistently record the lowest tourist counts."),
                                                        tags$li("Sudden spikes can occur due to special events or holiday promotions.")
                                                      )
                                             )
                                           ),
                                           
                                           # Monthly Plot with Year Slider
                                           fluidRow(
                                             column(width = 2, offset = 1,
                                                    sliderInput("selected_year", "Select a year to view monthly trend:",
                                                                min = min(tour_long$Year),
                                                                max = max(tour_long$Year),
                                                                value = max(tour_long$Year),
                                                                step = 1,
                                                                sep = "")
                                             ),
                                             column(width = 8,
                                                    plotOutput("monthly_plot", height = "400px", width = "100%")
                                             )
                                           )
             )),
             tabPanel("Reference/Data Prep", 
                      fluidPage(class = "main-content", 
                                tags$h3("Reference and Data Sources", style = "margin-bottom: 30px; margin-left: 60px; font-size: 40px;"),
                                
                                tags$div(style = "margin-left: 60px; margin-right: 60px; font-size: 14px;",
                                         
                                         tags$h4("For all data:"),
                                         tags$h5("All proper nouns such as tree species and region names—including local forest communities (e.g., 곰솔군락) and region names (e.g., 울릉읍)—have been translated into their English equivalents throughout the application.",
                                                 style = "font-weight: 400"),
                                         
                                         tags$h4("📸 Visual Resources"),
                                         tags$ul(
                                           tags$li(HTML('Photographs of Ulleungdo: <a href="https://www.kbmaeil.com/960245" target="_blank">kbmaeil.com</a>, <a href="https://pixabay.com/ko/photos/%EC%9A%B8%EB%A6%89%EB%8F%84-coast-voyage-2320282/" target="_blank">Pixabay</a>')),
                                           tags$li(HTML('Seafloor Topography Map of Ulleungdo: <a href="http://nationalatlas.ngii.go.kr/pages/page_1214.php" target="_blank">National Atlas</a>'))
                                         ),
                                         
                                         tags$h4("🌿 Vegetation and Ecology"),
                                         tags$ul(
                                           tags$li(HTML('Distribution of Vegetation Communities: <a href="https://www.data.go.kr/data/15086100/fileData.do" target="_blank">Data.go.kr</a>')),
                                           tags$li(HTML('Vegetation Dynamics (Ecology Monitoring) with tree diameter: <a href="https://www.bigdata-environment.kr/user/data_market/detail.do?id=ab0ccb90-4cca-11ec-a070-ab81432fd4e1" target="_blank">Big Data Environment Platform</a>')),
                                         ),
                                         
                                         tags$h4("🌊 Marine Environment"),
                                         tags$ul(
                                           tags$li(HTML('Marine Environmental Image API: <a href="https://www.data.go.kr/data/15142505/openapi.do" target="_blank">Data.go.kr</a>')),
                                           tags$ul(
                                             tags$li(HTML('Requested access to use API and was automatically accepted with decoding key.')),
                                             tags$li(HTML('A script named <b>marine_csv_generator.R</b> was used to generate a CSV file containing date | url pairs.')),
                                             tags$li(HTML('A script <b>marine_download_images.R</b> was used to download the images and resize them to a width of 1000 pixels for optimized use in the Shiny app.')),
                                                   )
                                          ),
                                        
                                         tags$h4("👥 Demographics"),
                                         tags$ul(
                                           tags$li(HTML('Resident Population Statistics by Administrative Unit: <a href="https://www.data.go.kr/data/15030591/fileData.do?recommendDataYn=Y" target="_blank">Data.go.kr</a>')),
                                           tags$ul(
                                             tags$li(HTML('Names of regions were translated into English with a script named <b>demo_cleaning.R</b>'))
                                           )
                                         ),
                                         
                                         tags$h4("🧳 Tourism"),
                                         tags$ul(
                                           tags$li(HTML('Tourist Visit Statistics: <a href="https://www.data.go.kr/data/15068059/fileData.do#" target="_blank">Data.go.kr</a> (accessed via domestic network)')),
                                           tags$ul(
                                             tags$li(HTML('The tourism data was originally provided in PDF format containing embedded tables. Instead of manually copying and pasting the data into Excel, 
                                                          the table was programmatically extracted and converted into a structured CSV file using the R script file named <b>tourism_csv_generator.R</b> with the assistance of ChatGPT.'))
                                           )
                                         )
                                )
                      )
             )
             
  )
)

server <- function(input, output, session) {
  
  observe({
    tab <- input$navbar
    class_to_apply <- ifelse(tab == "Main", "hero-image full", "hero-image strip")
    session$sendCustomMessage("setHeroClass", class_to_apply)
  })
  
  # About Ulleungdo
  output$map_local <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 130.905, lat = 37.484, zoom = 11) %>%
      addMarkers(lng = 130.905, lat = 37.484, popup = "Ulleungdo (Local)")
  })
  
  output$map_global <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 130.905, lat = 37.484, zoom = 2) %>%
      addMarkers(lng = 130.905, lat = 37.484, popup = "Ulleungdo (Global)")
  })
  
  filtered_data <- reactive({
    df <- veg_df
    
    if (input$selected_date != "All") {
      df <- df %>% filter(survey_date == input$selected_date)
    }
    if (input$selected_species != "All") {
      df <- df %>% filter(species_name_en == input$selected_species)
    }
    return(df)
  })
  
  output$vegetation_table <- renderDataTable({
    datatable(
      filtered_data() %>%
        select(survey_date, species_name_en, diameter_cm),
      options = list(pageLength = 10,
                     filter = "none",
                     searching = FALSE,
                     lengthChange = FALSE
      )
    )
  })
  
  output$avg_diameter_plot <- renderPlot({
    veg_df %>%
      group_by(species_name_en) %>%
      summarise(avg_diameter = mean(diameter_cm, na.rm = TRUE)) %>%
      slice_max(order_by = avg_diameter, n = 15) %>%
      ggplot(aes(x = avg_diameter, y = reorder(species_name_en, avg_diameter))) +
      geom_col(fill = "skyblue") +
      labs(x = "Average Diameter (cm)", y = "Species") +
      theme_minimal()
  })
  
  output$grade_plot <- renderPlot({
    shape_data %>%
      st_drop_geometry() %>%
      filter(!is.na(conservation_grade)) %>%
      group_by(community_en, conservation_grade) %>%
      summarise(area = sum(as.numeric(area_m2)), .groups = "drop") %>%
      ggplot(aes(x = reorder(community_en, -area), y = area / 10000, fill = factor(conservation_grade))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Plant Community", y = "Area (ha)", fill = "Grade"
      ) +
      theme_minimal()
  })
  
  output$community_map <- renderLeaflet({
    selected_shape <- shape_data %>%
      filter(community_en == input$selected_community)
    
    leaflet(selected_shape) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = "skyblue",
        fillOpacity = 0.6,
        color = "darkblue",
        weight = 1,
        popup = ~paste0("Community: ", community_en, "<br>Grade: ", conservation_grade)
      )
  })
  
  marine_dates <- seq.Date(from = as.Date("2025-02-02"), to = as.Date("2025-04-01"), by = "day")
  marine_paths <- paste0("marine_images/", format(marine_dates, "%Y%m%d"), ".png")
  marine_df <- data.frame(date = marine_dates, path = marine_paths)
  
  available_dates <- marine_df$date  
  
  output$marine_image <- renderUI({
    target_date <- input$marine_date + 1
    selected_path <- marine_df$path[marine_df$date == target_date]
    
    if (length(selected_path) > 0 && file.exists(file.path("www", selected_path))) {
      tags$img(src = selected_path, width = "80%")
    } else {
      tags$p("No marine image available for this date.",
             style = "color: gray; font-style: italic; margin-top: 20px;")
    }
  })
  
  
  #Demograpics
  
  observe({
    updateSelectInput(session, "selected_town", choices = c("All", unique(pop_df$town)))
  })
  
  observeEvent(input$selected_town, {
    filtered <- pop_df %>% filter(town == input$selected_town | input$selected_town == "All")
    updateSelectInput(session, "selected_legal", choices = c("All", unique(filtered$legal_district)))
  })
  
  observeEvent(input$selected_legal, {
    filtered <- pop_df %>% filter(
      (town == input$selected_town | input$selected_town == "All") &
        (legal_district == input$selected_legal | input$selected_legal == "All")
    )
    updateSelectInput(session, "selected_admin", choices = c("All", unique(filtered$administrative_district)))
  })
  
  filtered_df <- reactive({
    df <- pop_df
    if (input$selected_town != "All") {
      df <- df %>% filter(town == input$selected_town)
    }
    if (input$selected_legal != "All") {
      df <- df %>% filter(legal_district == input$selected_legal)
    }
    if (input$selected_admin != "All") {
      df <- df %>% filter(administrative_district == input$selected_admin)
    }
    df
  })
  
  yearly_summary <- reactive({
    filtered_df() %>%
      filter(!is.na(year), !is.na(population_total)) %>%
      group_by(year) %>%
      summarise(population_total = sum(population_total),
                population_male = sum(population_male),
                population_female = sum(population_female),
                .groups = "drop")
  })
  
  output$pop_trend_plot <- renderPlot({
    selected_df <- yearly_summary()
    
    if (input$show_total) {
      total_df <- pop_df %>%
        filter(!is.na(year), !is.na(population_total)) %>%
        group_by(year) %>%
        summarise(population_total = sum(population_total), .groups = "drop") %>%
        mutate(label = "Total")
      
      selected_df <- selected_df %>% mutate(label = "Selected")
      
      combined <- bind_rows(selected_df, total_df)
      
      ggplot(combined, aes(x = year, y = population_total, color = label)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 2) +
        labs(title = "Population Trend (Selected vs Total)", x = "Year", y = "Population") +
        theme_minimal()
    } else {
      ggplot(selected_df, aes(x = year, y = population_total)) +
        geom_line(color = "steelblue", linewidth = 1.5) +
        geom_point(color = "darkblue", size = 2) +
        labs(title = "Total Population by Year", x = "Year", y = "Population") +
        theme_minimal()
    }
  })
  
  output$gender_bar_plot <- renderPlot({
    df_long <- yearly_summary() %>%
      pivot_longer(cols = c("population_male", "population_female"),
                   names_to = "Gender", values_to = "Population")
    
    ggplot(df_long, aes(x = factor(year), y = Population, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Gender Distribution by Year", x = "Year", y = "Population") +
      theme_minimal()
  })
  
  # Tourist Count
  
  output$yearly_total_plot <- renderPlot({
    total_row <- tour_df %>% filter(Month == "Total") %>%
      pivot_longer(-Month, names_to = "Year", values_to = "Total_Visitors") %>%
      mutate(Year = as.integer(Year))
    
    # Plot
    ggplot(total_row, aes(x = Year, y = Total_Visitors)) +
      geom_col(fill = "skyblue") +
      labs(title = "Total Tourists by Year", x = "Year", y = "Total Visitors") +
      theme_minimal()
    
  })
  
  output$monthly_plot <- renderPlot({
    req(input$selected_year)
    
    filtered <- tour_long %>%
      filter(Month != "Total", Year == input$selected_year)
    
    ggplot(filtered, aes(x = Month, y = Visitors, group = 1)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = paste("Monthly Tourists in", input$selected_year),
        x = "Month", y = "Visitors"
      ) +
      theme_minimal()
  })
  
  
}



# Run the app
shinyApp(ui, server)