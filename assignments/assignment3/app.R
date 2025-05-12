library(shiny)
library(tidyverse)
library(maps)
library(countrycode)
library(viridis)
library(plotly)

# Theme
plain_theme <- theme_minimal(base_size = 13) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Years
years <- 1980:2024


# Load world map with ISO codes
world <- map_data("world") %>%
  mutate(ISO3 = countrycode(region, origin = "country.name", destination = "iso3c"))

# Load and reshape data
raw_data <- read.csv("./data/phys_risks_climate_related_distaster_frequency.csv")

data <- raw_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Frequency") %>%
  mutate(
    Year = as.integer(sub("X", "", Year)),
    Frequency = as.numeric(Frequency),
    Indicator = gsub("Climate related disasters frequency, Number of Disasters: ", "", Indicator) # Remove prefix
  ) %>%
  filter(!is.na(ISO3))  %>%
  filter(!str_detect(Indicator, "Number of People Affected"))  %>% # Only indcude disaster count
  filter(!str_detect(Indicator, "TOTAL")) # and remove total

indicators <- sort(unique(data$Indicator))

print(indicators)

ui <- fluidPage(
  titlePanel("Climate-Related Disaster Frequencies"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range",
                  min = min(years), max = max(years), step = 1, 
                  value = c(2000, 2020), sep = ""),
      selectInput("indicators", "Select Indicator(s):",
                  choices = indicators,
                  selected = indicators,
                  multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# ====================
# === SERVER =========
# ====================

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    # Aggregate frequencies by country in selected range and for selected indicators
    freq_data <- data %>%
      filter(Year >= input$year_range[1], 
             Year <= input$year_range[2],
             Indicator %in% input$indicators) %>%
      group_by(ISO3) %>%
      summarise(Frequency = sum(Frequency, na.rm = TRUE), .groups = "drop")
    
    # Join with map geometry
    map_data_year <- world %>%
      left_join(freq_data, by = "ISO3") %>%
      mutate(Frequency = replace_na(Frequency, 0))
    
    p <- ggplot(map_data_year, aes(x = long, y = lat, group = group,
                                   fill = log1p(Frequency),,
                                   text = paste0(region, "\nDisasters: ", Frequency))) +
      geom_polygon(color = "white", size = 0.2) +
      coord_fixed(1.3) +
      scale_fill_viridis(option = "magma", 
                         direction = -1,
                         name = "Log(1 + disasters)",
                         na.value = "gray90") +
      plain_theme +
      ggtitle(paste("Disasters from", input$year_range[1], "to", input$year_range[2]))
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)
