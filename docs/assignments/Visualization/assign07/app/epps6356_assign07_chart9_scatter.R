# ============================================================
# EPPS 6356 Assignment 7
# Chart 9: Interactive Scatter Plot
# Happy Planet Index 2026
#
# Chart 9:
# GDP per capita vs. Happy Planet Index
#
# Interactive features:
# 1. GDP per capita range slider
# 2. Continent filter
# 3. Show/hide country names
# 4. Show/hide trend line
# 5. Hover information
# 6. Plotly zoom, pan, and legend interaction
# ============================================================


# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(shiny)
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(scales)


# ------------------------------------------------------------
# 2. Import the Happy Planet Index data
# ------------------------------------------------------------

hpi <- read_excel(
  path = "Happy-Planet-Index-2006-2025-public-data-set.xlsx",
  sheet = "All Data",
  range = cell_limits(
    c(2, 1),
    c(NA, 20)
  )
) |>
  clean_names()


# ------------------------------------------------------------
# 3. Prepare the 2025 data
# ------------------------------------------------------------

hpi_2025 <- hpi |>
  filter(
    year == 2025,
    !is.na(gdp_per_capita_cleaned_2024_used_for_2025),
    gdp_per_capita_cleaned_2024_used_for_2025 > 0,
    !is.na(hpi),
    !is.na(continent)
  ) |>
  transmute(
    country = country,
    
    gdp = as.numeric(
      gdp_per_capita_cleaned_2024_used_for_2025
    ),
    
    hpi = as.numeric(hpi),
    
    continent = factor(
      continent,
      levels = 1:8,
      labels = c(
        "Latin America",
        "N. America & Oceania",
        "Western Europe",
        "Middle East & N. Africa",
        "Africa",
        "South Asia",
        "Eastern Europe & Central Asia",
        "East Asia"
      )
    )
  ) |>
  filter(
    !is.na(country),
    country != "",
    is.finite(gdp),
    is.finite(hpi),
    !is.na(continent),
    gdp > 0
  )


# ------------------------------------------------------------
# 4. Values for controls
# ------------------------------------------------------------

gdp_min <- floor(
  min(
    hpi_2025$gdp,
    na.rm = TRUE
  )
)

gdp_max <- ceiling(
  max(
    hpi_2025$gdp,
    na.rm = TRUE
  )
)

continent_choices <- levels(
  hpi_2025$continent
)


# ============================================================
# 5. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # Font styling
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Roboto:wght@500;700&display=swap"
      )
    ),
    
    tags$style(
      HTML("
        body,
        input,
        button,
        select,
        textarea,
        .irs {
          font-family: 'Roboto', sans-serif;
          font-weight: 500;
        }

        h1, h2, h3, h4,
        label,
        .control-label {
          font-family: 'Roboto', sans-serif;
          font-weight: 700;
        }
      ")
    )
  ),
  
  
  # ----------------------------------------------------------
  # Main title
  # ----------------------------------------------------------
  
  titlePanel(
    "GDP and Happy Planet Index: 2025"
  ),
  
  
  # ----------------------------------------------------------
  # Sidebar layout
  # ----------------------------------------------------------
  
  sidebarLayout(
    
    # ========================================================
    # LEFT SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      h4(
        "GDP and Happy Planet Index"
      ),
      
      p(
        "Each point represents one country."
      ),
      
      p(
        "X-axis: GDP per capita",
        tags$br(),
        
        "Y-axis: Happy Planet Index (HPI)",
        tags$br(),
        
        "Color: Continent"
      ),
      
      
      # ------------------------------------------------------
      # GDP range slider
      # ------------------------------------------------------
      
      sliderInput(
        inputId = "gdp",
        label = "GDP per capita range:",
        min = gdp_min,
        max = gdp_max,
        value = c(
          gdp_min,
          gdp_max
        ),
        step = 100,
        sep = ","
      ),
      
      
      # ------------------------------------------------------
      # Continent filter
      # ------------------------------------------------------
      
      selectInput(
        inputId = "continent",
        label = "Continent:",
        choices = c(
          "All continents",
          continent_choices
        ),
        selected = "All continents"
      ),
      
      
      # ------------------------------------------------------
      # Country labels
      # ------------------------------------------------------
      
      checkboxInput(
        inputId = "show_labels",
        label = "Show country names",
        value = FALSE
      ),
      
      
      # ------------------------------------------------------
      # Trend line
      # ------------------------------------------------------
      
      checkboxInput(
        inputId = "show_trend",
        label = "Show trend line",
        value = FALSE
      ),
      
      
      hr(),
      
      
      # ------------------------------------------------------
      # Help text
      # ------------------------------------------------------
      
      helpText(
        "Hover over a point to see the country's values."
      ),
      
      helpText(
        "Use the GDP range and continent filter to compare countries and regions."
      ),
      
      helpText(
        "Turn on the trend line to see the overall GDP-HPI pattern."
      ),
      
      helpText(
        "2025 HPI dataset; GDP per capita uses 2024 values."
      )
    ),
    
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      
      div(
        style = "
          width: 94%;
          margin-left: auto;
          margin-right: auto;
        ",
        
        plotlyOutput(
          outputId = "chart9",
          height = "600px",
          width = "100%"
        ),
        
        textOutput(
          "country_count"
        ),
        
        helpText(
          "Source: Happy Planet Index 2026."
        )
      )
    )
  )
)


# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Reactive filtered data
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    req(
      length(input$gdp) == 2
    )
    
    df <- hpi_2025 |>
      filter(
        gdp >= input$gdp[1],
        gdp <= input$gdp[2]
      )
    
    
    # Apply continent filter
    if (
      input$continent != "All continents"
    ) {
      
      df <- df |>
        filter(
          continent == input$continent
        )
    }
    
    
    df
  })
  
  
  # ----------------------------------------------------------
  # Interactive scatter plot
  # ----------------------------------------------------------
  
  output$chart9 <- renderPlotly({
    
    df <- filtered_data()
    
    
    validate(
      need(
        nrow(df) > 0,
        "No countries match the selected filters."
      )
    )
    
    
    # --------------------------------------------------------
    # Create hover text
    # --------------------------------------------------------
    
    df <- df |>
      mutate(
        
        tooltip = paste0(
          
          "Country: ",
          htmltools::htmlEscape(
            country
          ),
          
          "<br>Continent: ",
          continent,
          
          "<br>GDP per capita: ",
          format(
            round(
              gdp,
              1
            ),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          
          "<br>Happy Planet Index: ",
          round(
            hpi,
            2
          )
        )
      )
    
    
    # --------------------------------------------------------
    # Create base ggplot
    # --------------------------------------------------------
    
    p <- ggplot(
      df,
      aes(
        x = gdp,
        y = hpi,
        color = continent,
        text = tooltip
      )
    ) +
      
      geom_point(
        size = 3.5,
        alpha = 0.80
      ) +
      
      
      # ------------------------------------------------------
    # Continent colors
    # ------------------------------------------------------
    
    scale_color_manual(
      values = c(
        "Latin America" = "#E88982",
        "N. America & Oceania" = "#D9A441",
        "Western Europe" = "#8AB33F",
        "Middle East & N. Africa" = "#48BD88",
        "Africa" = "#45B9C1",
        "South Asia" = "#42A5DF",
        "Eastern Europe & Central Asia" = "#B77BE5",
        "East Asia" = "#E96BB5"
      ),
      name = "Continent"
    ) +
      
      
      # ------------------------------------------------------
    # GDP log scale
    # ------------------------------------------------------
    
    scale_x_log10(
      labels = label_number(
        big.mark = ",",
        accuracy = 1
      )
    ) +
      
      
      # ------------------------------------------------------
    # Titles and labels
    # ------------------------------------------------------
    
    labs(
      title =
        "How Does GDP Relate to HPI?",
      
      subtitle =
        "Color represents continent; each point is a country",
      
      x =
        "GDP per capita (PPP, constant 2021 international $; log scale)",
      
      y =
        "Happy Planet Index (HPI)"
    ) +
      
      
      # ------------------------------------------------------
    # Theme
    # ------------------------------------------------------
    
    theme_minimal(
      base_size = 14,
      base_family = "Roboto"
    ) +
      
      theme(
        
        panel.grid.minor =
          element_blank(),
        
        plot.title =
          element_text(
            face = "bold",
            size = 18,
            hjust = 0.5
          ),
        
        plot.subtitle =
          element_text(
            face = "plain",
            size = 11,
            hjust = 0.5,
            margin = margin(
              b = 12
            )
          ),
        
        axis.title =
          element_text(
            face = "plain",
            size = 13
          ),
        
        axis.text =
          element_text(
            size = 11,
            color = "gray25"
          ),
        
        legend.position =
          "bottom",
        
        legend.title =
          element_text(
            size = 11,
            face = "bold"
          ),
        
        legend.text =
          element_text(
            size = 10
          ),
        
        plot.margin =
          margin(
            t = 15,
            r = 20,
            b = 15,
            l = 15
          )
      )
    
    
    # --------------------------------------------------------
    # Optional trend line
    # --------------------------------------------------------
    
    if (
      isTRUE(
        input$show_trend
      )
    ) {
      
      p <- p +
        
        geom_smooth(
          aes(
            group = 1
          ),
          method = "loess",
          se = FALSE,
          color = "#333333",
          linewidth = 1.2,
          show.legend = FALSE
        )
    }
    
    
    # --------------------------------------------------------
    # Optional country labels
    # --------------------------------------------------------
    
    if (
      isTRUE(
        input$show_labels
      )
    ) {
      
      p <- p +
        
        geom_text(
          aes(
            label = country
          ),
          family = "Roboto",
          size = 3,
          vjust = -1,
          check_overlap = TRUE,
          show.legend = FALSE
        )
    }
    
    
    # --------------------------------------------------------
    # Convert to Plotly
    # --------------------------------------------------------
    
    fig <- ggplotly(
      p,
      tooltip = "text"
    )
    
    
    # --------------------------------------------------------
    # Plotly layout
    # --------------------------------------------------------
    
    fig <- plotly::layout(
      
      fig,
      
      font = list(
        family = "Roboto, sans-serif",
        size = 14,
        color = "gray25"
      ),
      
      hoverlabel = list(
        font = list(
          family = "Roboto, sans-serif",
          size = 13
        )
      ),
      
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.20
      ),
      
      margin = list(
        l = 80,
        r = 25,
        t = 100,
        b = 120
      )
    )
    
    
    # --------------------------------------------------------
    # Apply Roboto font to Plotly
    # --------------------------------------------------------
    
    htmlwidgets::onRender(
      
      fig,
      
      "
      function(el, x) {

        function applyFonts() {

          el.querySelectorAll('svg text').forEach(function(node) {
            node.style.fontFamily = 'Roboto, sans-serif';
            node.style.fontWeight = '500';
          });

          el.querySelectorAll('svg .gtitle').forEach(function(node) {
            node.style.fontWeight = '700';
          });
        }

        applyFonts();

        el.on('plotly_afterplot', applyFonts);
        el.on('plotly_hover', applyFonts);

        if (document.fonts && document.fonts.ready) {
          document.fonts.ready.then(applyFonts);
        }
      }
      "
    )
  })
  
  
  # ----------------------------------------------------------
  # Number of countries displayed
  # ----------------------------------------------------------
  
  output$country_count <- renderText({
    
    paste(
      "Countries displayed:",
      nrow(
        filtered_data()
      )
    )
  })
}


# ============================================================
# 7. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)


# ============================================================
# END OF APP
# ============================================================