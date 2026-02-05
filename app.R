# ==============================================================================
# SECTION 1: LIBRARIES
# ==============================================================================

library(shiny)          # Main framework
library(bslib)          # Modern Bootstrap 5 theming
library(bsicons)        # Icons for value boxes
library(shinyWidgets)   # Enhanced inputs (Picker, Slider)
library(dplyr)          # Data filtering and aggregation
library(ggplot2)        # Static charts
library(plotly)         # Interactive charts (wraps ggplot2)
library(DT)             # Interactive Data Tables
library(leaflet)        # Interactive Maps
library(rnaturalearth)  # World map geometry
library(rnaturalearthdata) # Map data
library(sf)             # Handling spatial data frames

# ==============================================================================
# SECTION 2: DATA LOADING & PREPARATION
# ==============================================================================

# Safety Check
if(!file.exists("BMW_Car_Sales_Classification.csv")) {
  stop("CRITICAL ERROR: 'BMW_Car_Sales_Classification.csv' not found in project directory.")
}

# Load Data
df <- read.csv("BMW_Car_Sales_Classification.csv")

# Load Map Geometry
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# ==============================================================================
# SECTION 3: THEME & CUSTOM STYLING
# ==============================================================================

bmw_theme <- bs_theme(
  version = 5,
  preset = "zephyr", 
  primary = "#1C69D4",      # BMW Blue
  "navbar-bg" = "#1C69D4",  # Header Blue
  "body-bg" = "#f4f6f9",    # Soft Grey Background
  "card-bg" = "#ffffff",    
  heading_font = font_google("Source Sans Pro"),
  base_font = font_google("Source Sans Pro")
)

custom_css <- "
  .card { 
    border: none; 
    box-shadow: 0 4px 15px rgba(0,0,0,0.05); 
    border-radius: 8px; 
    margin-bottom: 20px;
  }
  .sidebar-title { 
    font-weight: 800; 
    color: #1C69D4; 
    letter-spacing: 1px; 
    text-transform: uppercase; 
    font-size: 0.9rem;
  }
  .value-box-area .card {
    border-left: 4px solid #1C69D4; 
  }
  .about-header {
    color: #1C69D4;
    font-weight: bold;
  }
"

# ==============================================================================
# SECTION 4: USER INTERFACE (UI)
# ==============================================================================

ui <- page_sidebar(
  title = "BMW Global Performance Hub",
  theme = bmw_theme,
  tags$head(tags$style(HTML(custom_css))),
  
  # --- SIDEBAR CONTROLS ---
  sidebar = sidebar(
    width = 320,
    title = span("Filters", class = "sidebar-title"),
    
    # Global Reset
    actionButton("reset_btn", "Reset Dashboard", icon = icon("rotate-left"), 
                 class = "btn-outline-primary w-100 mb-4"),
    
    # Filter Accordion
    accordion(
      open = "Global Parameters", # Automatically opens this panel
      accordion_panel(
        "Global Parameters",
        icon = bs_icon("sliders"),
        sliderInput("year_filter", "Year Range", min = min(df$Year), max = max(df$Year), 
                    value = c(min(df$Year), max(df$Year)), step = 1, sep = ""),
        pickerInput("region_filter", "Region", choices = unique(df$Region), selected = unique(df$Region), multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("model_filter", "Model", choices = unique(df$Model), selected = unique(df$Model), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        pickerInput("color_filter", "Color", choices = unique(df$Color), selected = unique(df$Color), multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("fuel_filter", "Fuel Type", choices = unique(df$Fuel_Type), selected = unique(df$Fuel_Type), multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("trans_filter", "Transmission", choices = unique(df$Transmission), selected = unique(df$Transmission), multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("class_filter", "Sales Class", choices = unique(df$Sales_Classification), selected = unique(df$Sales_Classification), multiple = TRUE, options = list(`actions-box` = TRUE))
      )
    ),
    div(class = "mt-auto text-muted small text-center p-3", "Powered by R-Shiny")
  ),
  
  # --- MAIN CONTENT TABS ---
  navset_card_underline(
    
    # TAB 1: OVERVIEW (NOW FIRST - Loads Default)
    nav_panel("Overview", icon = bs_icon("speedometer2"),
              layout_columns(class = "value-box-area", fill = FALSE,
                             value_box(title = "Total Sales Volume", value = textOutput("kpi_sales_text"), showcase = bs_icon("car-front-fill"), theme = "white"),
                             value_box(title = "Average Selling Price", value = textOutput("kpi_price_text"), showcase = bs_icon("currency-dollar"), theme = "white"),
                             value_box(title = "Transactions", value = textOutput("kpi_vol_text"), showcase = bs_icon("file-earmark-bar-graph"), theme = "white")
              ),
              uiOutput("overview_dynamic_row_1"), 
              uiOutput("overview_dynamic_row_2")
    ),
    
    # TAB 2: TRENDS
    nav_panel("Trends", icon = bs_icon("graph-up"),
              layout_columns(col_widths = c(12, 12),
                             card(card_header("Sales Volume Over Time"), plotlyOutput("trend_line_total")),
                             card(card_header("Price Evolution (Avg USD)"), plotlyOutput("trend_price_avg"))
              )
    ),
    
    # TAB 3: PRICING
    nav_panel("Pricing", icon = bs_icon("tag-fill"),
              layout_columns(col_widths = c(6, 6),
                             card(card_header("Price Distribution"), plotlyOutput("price_hist")),
                             card(card_header("Price vs. Mileage"), plotlyOutput("price_scatter"))
              ),
              card(card_header("Price by Sales Classification"), plotlyOutput("price_box"))
    ),
    
    # TAB 4: BREAKDOWN
    nav_panel("Breakdown", icon = bs_icon("bar-chart-fill"),
              layout_columns(col_widths = c(12, 12),
                             card(card_header("Sales by Fuel Type"), plotlyOutput("sales_line_fuel")),
                             card(card_header("Top 10 Models"), plotlyOutput("sales_bar_model"))
              )
    ),
    
    # TAB 5: ADVANCED
    nav_panel("Deep Dive", icon = bs_icon("search"),
              card(card_header("Multivariate Analysis"), plotlyOutput("bubble_chart", height = "600px"), full_screen = TRUE)
    ),
    
    # TAB 6: DATA
    nav_panel("Data", icon = bs_icon("table"),
              card(DTOutput("raw_table"))
    ),
    
    # TAB 7: ABOUT & INFO (MOVED TO LAST)
    nav_panel("About & Info", icon = bs_icon("info-circle-fill"),
              layout_columns(col_widths = c(5, 7),
                             # Student Details Card
                             card(
                               card_header("Project Author Details"),
                               div(class = "p-3",
                                   h3(class="about-header", "Rushikesh Vishwasrao"),
                                   h5("Roll Number: MDS202527"),
                                   h5("Chennai Mathematical Institute"),
                                   hr(),
                                   p(tags$b("GitHub: "), a(href="https://github.com/arewecoding", "github.com/arewecoding", target="_blank")),
                                   p(tags$b("LinkedIn: "), a(href="https://linkedin.com/in/rushikesh-vishwasrao-b978b9239/", "linkedin.com/in/rushikesh-vishwasrao-b978b9239/", target="_blank")),
                                   p(tags$b("Email: "), "rushikeshv.mds2025@cmi.ac.in"),
                                   p("For any queries or issues related to this dashboard, feel free to mail me. 😊")
                               )
                             ),
                             # Guide / Abstract Card
                             card(
                               card_header("Dashboard Guide"),
                               div(class = "p-3",
                                   h4("Project Overview"),
                                   p("This dashboard analyzes global sales data for BMW vehicles. It allows users to explore pricing trends, sales volumes across different regions, and the popularity of specific models."),
                                   br(),
                                   h5("How to use this dashboard:"),
                                   tags$ul(
                                     tags$li(tags$b("Overview: "), "See high-level KPIs and a geospatial heatmap of sales."),
                                     tags$li(tags$b("Trends: "), "Analyze how sales and prices have changed over the years."),
                                     tags$li(tags$b("Pricing: "), "Investigate how mileage and sales classification affect price."),
                                     tags$li(tags$b("Filters: "), "Use the sidebar on the left to slice data by Year, Model, or Region.")
                                   ),
                                  p("When using the select tool in the graphs, you can double click to undo the selection.")
                               )
                             )
              )
    )
  )
)

# ==============================================================================
# SECTION 5: SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # --- 5.1 REACTIVE DATA FILTER ---
  filtered_data <- reactive({
    data <- df %>%
      filter(Region %in% input$region_filter) %>%
      filter(Model %in% input$model_filter) %>%
      filter(Color %in% input$color_filter) %>%
      filter(Fuel_Type %in% input$fuel_filter) %>%
      filter(Transmission %in% input$trans_filter) %>%
      filter(Sales_Classification %in% input$class_filter) %>%
      filter(Year >= input$year_filter[1] & Year <= input$year_filter[2])
    return(data)
  })
  
  # --- 5.2 RESET BUTTON ---
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "year_filter", value = c(min(df$Year), max(df$Year)))
    updatePickerInput(session, "region_filter", selected = unique(df$Region))
    updatePickerInput(session, "model_filter", selected = unique(df$Model))
    updatePickerInput(session, "color_filter", selected = unique(df$Color))
    updatePickerInput(session, "fuel_filter", selected = unique(df$Fuel_Type))
    updatePickerInput(session, "trans_filter", selected = unique(df$Transmission))
    updatePickerInput(session, "class_filter", selected = unique(df$Sales_Classification))
  })
  
  # --- 5.3 KPI CALCULATIONS ---
  output$kpi_sales_text <- renderText({ format(sum(filtered_data()$Sales_Volume, na.rm=T), big.mark=",") })
  output$kpi_price_text <- renderText({ 
    val <- mean(filtered_data()$Price_USD, na.rm=T)
    val <- ifelse(is.nan(val), 0, val)
    paste0("$", format(round(val, 0), big.mark=",")) 
  })
  output$kpi_vol_text <- renderText({ format(nrow(filtered_data()), big.mark=",") })
  
  # --- 5.4 DYNAMIC UI LAYOUTS ---
  output$overview_dynamic_row_1 <- renderUI({
    if(nrow(filtered_data()) == 0) return(card("No data available for this selection."))
    show_pie <- length(unique(filtered_data()$Fuel_Type)) > 1
    
    if (show_pie) {
      layout_columns(col_widths = c(8, 4),
                     card(card_header("Global Sales Heatmap"), leafletOutput("map_leaflet", height = "500px"), full_screen = TRUE),
                     card(card_header("Fuel Mix"), plotlyOutput("pie_plot", height = "500px"))
      )
    } else {
      layout_columns(col_widths = c(12),
                     card(card_header("Global Sales Heatmap"), leafletOutput("map_leaflet", height = "650px"), full_screen = TRUE)
      )
    }
  })
  
  output$overview_dynamic_row_2 <- renderUI({
    if(nrow(filtered_data()) == 0) return(NULL)
    n_regions <- length(input$region_filter)
    n_models  <- length(input$model_filter)
    is_single_year <- input$year_filter[1] == input$year_filter[2]
    
    if ((n_regions == 1 && n_models == 1) || (n_models == 1 && is_single_year)) return(NULL) 
    
    if (n_models == 1) {
      layout_columns(col_widths = 12, card(card_header("Sales Volume Trend"), plotlyOutput("overview_trend", height = "300px")))
    } else {
      if (is_single_year) {
        layout_columns(col_widths = 12, card(card_header(paste("Top Models in", input$year_filter[1])), plotlyOutput("overview_model_bar", height = "400px")))
      } else {
        layout_columns(col_widths = c(8, 4),
                       card(card_header("Sales Volume Trend"), plotlyOutput("overview_trend", height = "300px")),
                       card(card_header("Top 5 Models"), plotlyOutput("overview_model_bar", height = "300px"))
        )
      }
    }
  })
  
  # --- 5.5 PLOTTING LOGIC ---
  
  # >> LEAFLET MAP
  output$map_leaflet <- renderLeaflet({
    req(nrow(filtered_data()) > 0)
    sales_by_reg <- filtered_data() %>% group_by(Region) %>% summarise(Total=sum(Sales_Volume))
    
    world_viz <- world_sf %>%
      mutate(bmw_region = case_when(
        admin %in% c("Saudi Arabia", "United Arab Emirates", "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman", "Qatar", "Syria", "Turkey", "Yemen", "Bahrain", "Egypt") ~ "Middle East",
        continent == "Europe" ~ "Europe",
        continent == "Asia" ~ "Asia", 
        continent == "North America" ~ "North America",
        continent == "South America" ~ "South America",
        continent == "Africa" ~ "Africa",
        TRUE ~ "Other"
      )) %>%
      inner_join(sales_by_reg, by = c("bmw_region" = "Region"))
    
    if(nrow(sales_by_reg) == 1 || min(sales_by_reg$Total) == max(sales_by_reg$Total)) {
      map <- leaflet(world_viz) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = "#1C69D4", 
          weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.8,
          label = ~paste0(bmw_region, ": ", format(Total, big.mark=","), " units")
        )
      return(map)
    } else {
      pal <- colorNumeric(palette = c("#6CA6CD", "#1C69D4"), domain = world_viz$Total)
      leaflet(world_viz) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(Total),
          weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.8,
          label = ~paste0(bmw_region, ": ", format(Total, big.mark=","), " units")
        ) %>%
        addLegend(pal = pal, values = ~Total, opacity = 0.7, title = "Sales", position = "bottomright")
    }
  })
  
  # >> PIE CHART
  output$pie_plot <- renderPlotly({
    if(nrow(filtered_data()) == 0) return(NULL)
    d <- filtered_data() %>% count(Fuel_Type)
    plot_ly(d, labels = ~Fuel_Type, values = ~n, type = 'pie',
            marker = list(colors = c('#1C69D4', '#666666', '#262626', '#E6E6E6')), 
            textinfo = 'label+percent') %>%
      layout(showlegend = FALSE, margin = list(l=0, r=0, t=0, b=0), paper_bgcolor = "rgba(0,0,0,0)")
  })
  
  # >> OVERVIEW BAR CHART
  output$overview_trend <- renderPlotly({
    d <- filtered_data() %>% group_by(Year) %>% summarise(Sales=sum(Sales_Volume))
    p <- ggplot(d, aes(x=Year, y=Sales)) + 
      geom_bar(stat="identity", fill="#1C69D4", alpha=0.9) + 
      theme_minimal() + 
      labs(x="Year", y="Total Sales") 
    ggplotly(p)
  })
  
  # >> OVERVIEW MODEL RANKING
  output$overview_model_bar <- renderPlotly({
    is_single_year <- input$year_filter[1] == input$year_filter[2]
    top_n <- if(is_single_year) 15 else 5
    d <- filtered_data() %>% group_by(Model) %>% summarise(Vol = sum(Sales_Volume)) %>% arrange(desc(Vol)) %>% head(top_n)
    p <- ggplot(d, aes(x=reorder(Model, Vol), y=Vol)) + 
      geom_bar(stat="identity", fill="#1C69D4") + coord_flip() + 
      theme_minimal() + 
      labs(x="Car Model", y="Sales Volume")
    ggplotly(p)
  })
  
  # >> TREND LINES
  output$trend_line_total <- renderPlotly({
    d <- filtered_data() %>% group_by(Year) %>% summarise(Sales=sum(Sales_Volume))
    p <- ggplot(d, aes(x=Year, y=Sales)) + geom_line(color="#1C69D4", size=1) + geom_point(color="#1C69D4") + 
      theme_minimal() + 
      labs(x="Year", y="Total Sales Volume")
    ggplotly(p)
  })
  
  output$trend_price_avg <- renderPlotly({
    d <- filtered_data() %>% group_by(Year) %>% summarise(Price=mean(Price_USD))
    p <- ggplot(d, aes(x=Year, y=Price)) + geom_line(color="#666666", size=1) + 
      theme_minimal() + 
      labs(x="Year", y="Average Price (USD)")
    ggplotly(p)
  })
  
  # >> PRICE CHARTS
  output$price_hist <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x=Price_USD)) + 
      geom_histogram(fill="#1C69D4", color="white", bins=30) + 
      theme_minimal() + 
      labs(x="Price (USD)", y="Frequency")
    ggplotly(p)
  })
  
  output$price_scatter <- renderPlotly({
    d <- filtered_data()
    if(nrow(d)>1000) d <- sample_n(d, 1000)
    p <- ggplot(d, aes(x=Mileage_KM, y=Price_USD, color=Sales_Classification)) + 
      geom_point(alpha=0.6) + theme_minimal() + 
      scale_color_manual(values=c("High"="#1C69D4", "Medium"="#666666", "Low"="#cccccc")) +
      labs(x = "Mileage (KM)", y = "Price (USD)", color = "Sales Class")
    ggplotly(p)
  })
  
  output$price_box <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x=Sales_Classification, y=Price_USD, fill=Sales_Classification)) + 
      geom_boxplot() + theme_minimal() + theme(legend.position="none") + 
      scale_fill_manual(values=c("High"="#1C69D4", "Medium"="#666666", "Low"="#cccccc")) +
      labs(x = "Sales Classification", y = "Price (USD)")
    ggplotly(p)
  })
  
  # >> SALES BREAKDOWN CHARTS
  output$sales_line_fuel <- renderPlotly({
    d <- filtered_data() %>% group_by(Year, Fuel_Type) %>% summarise(Vol=sum(Sales_Volume))
    p <- ggplot(d, aes(x=Year, y=Vol, color=Fuel_Type)) + geom_line(size=1) + 
      theme_minimal() +
      labs(x="Year", y="Sales Volume", color="Fuel Type")
    ggplotly(p)
  })
  
  output$sales_bar_model <- renderPlotly({
    d <- filtered_data() %>% group_by(Model) %>% summarise(Vol=sum(Sales_Volume)) %>% arrange(desc(Vol)) %>% head(10)
    p <- ggplot(d, aes(x=reorder(Model, Vol), y=Vol)) + 
      geom_bar(stat="identity", fill="#1C69D4") + coord_flip() + 
      theme_minimal() + 
      labs(x="Car Model", y="Sales Volume")
    ggplotly(p)
  })
  
  # >> ADVANCED ANALYSIS
  output$bubble_chart <- renderPlotly({
    d <- filtered_data()
    if(nrow(d)>500) d <- sample_n(d, 500)
    p <- ggplot(d, aes(x=Engine_Size_L, y=Price_USD, size=Mileage_KM, color=Fuel_Type)) + 
      geom_point(alpha=0.6) + theme_minimal() + 
      labs(x="Engine Size (L)", y="Price (USD)", size="Mileage (KM)", color="Fuel Type")
    ggplotly(p)
  })
  
  # >> DATA TABLE
  output$raw_table <- renderDT({
    datatable(filtered_data(), 
              colnames = c("Model", "Year", "Region", "Color", "Fuel Type", "Transmission", 
                           "Engine Size (L)", "Mileage (KM)", "Price (USD)", 
                           "Sales Volume", "Classification"),
              options=list(pageLength=10, scrollX=T))
  })
}

shinyApp(ui, server)