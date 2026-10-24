library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(htmlwidgets)
library(sysfonts)
library(showtext)

raw_hpi <- read_excel(
  "Happy-Planet-Index-2006-2025-public-data-set.xlsx",
  sheet = "All Data",
  range = cell_limits(c(2, 1), c(NA, 20))
)

contour_font <- tryCatch({
  sysfonts::font_add_google("Roboto", family = "roboto_medium",
                            regular.wt = 500, bold.wt = 700)
  "roboto_medium"
}, error = function(e) {
  message("Roboto could not be downloaded; contour uses sans instead.")
  "sans"
})

showtext::showtext_opts(dpi = 96)
showtext::showtext_auto()

scatter_app <- local({

hpi <- janitor::clean_names(raw_hpi)

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


ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  
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
        inputId = ns("gdp"),
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
        inputId = ns("continent"),
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
        inputId = ns("show_labels"),
        label = "Show country names",
        value = FALSE
      ),
      
      
      # ------------------------------------------------------
      # Trend line
      # ------------------------------------------------------
      
      checkboxInput(
        inputId = ns("show_trend"),
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
      
      # Center chart area inside the main panel
      div(
        style = "
          width: 94%;
          margin-left: auto;
          margin-right: auto;
        ",
        
        plotlyOutput(
          outputId = ns("chart9"),
          height = "600px",
          width = "100%"
        ),
        
        textOutput(
          ns("country_count")
        ),
        
        helpText(
          "Source: Happy Planet Index 2026."
        )
      )
    )
  )
  )
}

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
      input$continent !=
      "All continents"
    ) {
      
      df <- df |>
        filter(
          continent ==
            input$continent
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
    # Create ggplot
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
      labels = scales::label_number(
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
        paste(
          "Color represents continent;",
          "each point is a country"
        ),
      
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
        family =
          "Roboto, sans-serif",
        
        size = 14,
        
        color =
          "gray25"
      ),
      
      hoverlabel = list(
        
        font = list(
          family =
            "Roboto, sans-serif",
          
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
    # Make sure Plotly also uses Roboto
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


list(ui = ui, server = server)
})

bubble_app <- local({

hpi <- raw_hpi

hpi_2025 <- hpi %>%
  filter(Year == 2025) %>%
  transmute(
    country = Country,
    
    gdp = as.numeric(
      `GDP per capita (cleaned) - 2024 used for 2025`
    ),
    
    footprint = as.numeric(`Ecological Footprint`),
    
    satisfaction = as.numeric(`Life Satisfaction`)
  ) %>%
  filter(
    !is.na(country),
    country != "",
    is.finite(gdp),
    is.finite(footprint),
    is.finite(satisfaction),
    gdp > 0,
    footprint >= 0,
    satisfaction >= 0,
    satisfaction <= 10
  )

gdp_min <- floor(min(hpi_2025$gdp))
gdp_max <- ceiling(max(hpi_2025$gdp))

footprint_max <- max(hpi_2025$footprint)

if (footprint_max <= 0) {
  stop("Ecological footprint must contain positive values.")
}

footprint_breaks <- pretty(
  c(0, footprint_max),
  n = 4
)

footprint_breaks <- footprint_breaks[
  footprint_breaks > 0 &
    footprint_breaks <= footprint_max
]


ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  
  tags$head(
    
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Roboto:wght@500;700&display=swap"
      )
    ),
    
    tags$style(HTML("
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
    "))
  ),
  
  titlePanel("Wealth, Wellbeing and Footprint: 2025"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Wealth and Wellbeing"),
      
      p("Each bubble represents one country."),
      
      p(
        "X-axis: GDP per capita",
        tags$br(),
        "Y-axis: Life satisfaction",
        tags$br(),
        "Bubble area and color: Ecological footprint"
      ),
      
      sliderInput(
        inputId = ns("gdp"),
        label = "GDP per capita range:",
        min = gdp_min,
        max = gdp_max,
        value = c(gdp_min, gdp_max),
        step = 100,
        sep = ","
      ),
      
      sliderInput(
        inputId = ns("bubble_size"),
        label = "Maximum bubble size:",
        min = 1,
        max = 10,
        value = 5,
        step = 0.5
      ),
      
      checkboxInput(
        inputId = ns("log_gdp"),
        label = "Use logarithmic GDP axis",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = ns("show_labels"),
        label = "Show country names",
        value = FALSE
      ),
      
      hr(),
      
      helpText(
        "Hover over a bubble to see the country's values."
      ),
      
      helpText(
        "Larger, bluer bubbles represent a larger",
        "ecological footprint—not higher wellbeing."
      ),
      
      helpText(
        "2025 HPI dataset; GDP per capita uses 2024 values."
      )
    ),
    
    mainPanel(
      
      plotlyOutput(
        outputId = ns("bubble_chart"),
        height = "600px"
      ),
      
      textOutput(ns("country_count")),
      
      helpText(
        "Source: Happy Planet Index."
      )
    )
  )
)
}

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    req(length(input$gdp) == 2)
    
    hpi_2025 %>%
      filter(
        gdp >= input$gdp[1],
        gdp <= input$gdp[2]
      )
  })
  
  output$bubble_chart <- renderPlotly({
    
    req(input$bubble_size)
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No countries match the selected range."
      )
    )
    df <- df %>%
      arrange(desc(footprint)) %>%
      mutate(
        tooltip = paste0(
          "Country: ",
          htmltools::htmlEscape(country),
          
          "<br>GDP per capita: ",
          format(
            round(gdp, 1),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          
          "<br>Life satisfaction: ",
          round(satisfaction, 2),
          " / 10",
          
          "<br>Ecological footprint: ",
          round(footprint, 2),
          " gha per person"
        )
      )
    
    p <- ggplot(
      df,
      aes(
        x = gdp,
        y = satisfaction
      )
    ) +
      
      geom_point(
        aes(
          size = footprint,
          fill = footprint,
          text = tooltip
        ),
        shape = 21,
        color = "gray25",
        stroke = 0.5,
        alpha = 0.8
      ) +
      
      scale_fill_gradient(
        low = "mediumpurple",
        high = "steelblue",
        limits = c(0, footprint_max),
        breaks = footprint_breaks,
        name = "Ecological footprint\n(gha per person)"
      ) +
      
      scale_size_area(
        max_size = input$bubble_size,
        limits = c(0, footprint_max),
        breaks = footprint_breaks,
        name = "Ecological footprint\n(gha per person)"
      ) +
      
      scale_y_continuous(
        limits = c(0, 10),
        breaks = seq(0, 10, by = 2)
      ) +
      
      labs(
        title = "Wealth, Wellbeing and Footprint",
        
        subtitle = "Bubble area and color show ecological footprint",
        
        x = "GDP per capita (PPP, constant 2021 international $)",
        
        y = "Life satisfaction (0–10)"
      ) +
      
      theme_minimal(
        base_size = 14,
        base_family = "Roboto"
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        
        plot.title = element_text(
          face = "bold",
          size = 18
        ),
        
        plot.subtitle = element_text(
          face = "plain",
          size = 11,
          margin = margin(b = 12)
        ),
        
        axis.title = element_text(
          face = "plain",
          size = 13
        ),
        
        axis.text = element_text(
          size = 11,
          color = "gray25"
        ),
        
        legend.title = element_text(
          size = 11
        ),
        
        legend.text = element_text(
          size = 10
        ),
        
        plot.margin = margin(
          t = 15,
          r = 20,
          b = 15,
          l = 15
        )
      )
    
    if (isTRUE(input$log_gdp)) {
      
      p <- p +
        scale_x_log10(
          labels = scales::label_number(
            big.mark = ",",
            accuracy = 1
          )
        ) +
        labs(
          x = paste0(
            "GDP per capita",
            "<br>(PPP, constant 2021 international $; log scale)"
          )
        )
      
    } else {
      
      p <- p +
        scale_x_continuous(
          labels = scales::label_number(
            big.mark = ",",
            accuracy = 1
          )
        ) +
        labs(
          x = paste0(
            "GDP per capita",
            "<br>(PPP, constant 2021 international $)"
          )
        )
    }
    if (isTRUE(input$show_labels)) {
      
      p <- p +
        geom_text(
          aes(label = country),
          family = "Roboto",
          size = 3,
          vjust = -1,
          check_overlap = TRUE
        )
    }
    fig <- ggplotly(
      p,
      tooltip = "text"
    )
    
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
      )
    )
    
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
  output$country_count <- renderText({
    
    paste(
      "Countries displayed:",
      nrow(filtered_data())
    )
  })
}

list(ui = ui, server = server)
})

contour_app <- local({

hpi <- raw_hpi

hpi_2025 <- hpi %>%
  filter(Year == 2025) %>%
  transmute(
    country = Country,
    
    gdp = as.numeric(
      `GDP per capita (cleaned) - 2024 used for 2025`
    ),
    
    hpi_score = as.numeric(HPI)
  ) %>%
  filter(
    !is.na(country),
    country != "",
    is.finite(gdp),
    is.finite(hpi_score),
    gdp > 0,
    hpi_score >= 0,
    hpi_score <= 100
  )


ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  
  tags$head(
    
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Roboto:wght@500;700&display=swap"
      )
    ),
    
    tags$style(HTML("
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
    "))
  ),
  
  titlePanel("GDP and Happy Planet Index: 2025"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("GDP and HPI"),
      
      p(
        "Each point represents one country.",
        "Contours show where countries are concentrated."
      ),
      
      sliderInput(
        inputId = ns("contour_bins"),
        label = "Number of contour levels:",
        min = 3,
        max = 10,
        value = 5,
        step = 1
      ),
      
      checkboxInput(
        inputId = ns("log_gdp"),
        label = "Use logarithmic GDP axis",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = ns("show_points"),
        label = "Show countries",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = ns("show_trend"),
        label = "Show trend line",
        value = TRUE
      ),
      
      checkboxInput(
        inputId = ns("show_labels"),
        label = "Show country names",
        value = FALSE
      ),
      
      hr(),
      
      helpText(
        "Contours represent estimated country density,",
        "not exact country counts or HPI levels."
      ),
      
      helpText(
        "Changing the GDP scale also changes",
        "the density estimate and fitted trend."
      ),
      
      helpText(
        "2025 HPI dataset; GDP per capita uses 2024 values."
      )
    ),
    
    mainPanel(
      plotOutput(ns("contour_chart"), height = "600px"),
      
      downloadButton(
        ns("download_png"),
        "Download PNG"
      ),
      
      textOutput(ns("country_count"))
    )
  )
)
}

server <- function(input, output, session) {

  contour_plot <- reactive({
    
    df <- hpi_2025
    
    validate(
      need(
        nrow(df) >= 10,
        "Not enough countries with valid GDP and HPI data."
      ),
      
      need(
        n_distinct(df$gdp) > 1 &&
          n_distinct(df$hpi_score) > 1,
        "GDP and HPI must both contain varying values."
      )
    )
    
    p <- ggplot(
      df,
      aes(x = gdp, y = hpi_score)
    ) +
      geom_density_2d(
        bins = input$contour_bins,
        color = "steelblue",
        linewidth = 0.7,
        n = 150
      ) +
      labs(
        title = "How Does GDP Relate to HPI?",
        
        subtitle = paste(
          "Contours show estimated country density;",
          "points represent countries"
        ),
        
        x = "GDP per capita (PPP, constant 2021 international $)",
        
        y = "Happy Planet Index (0–100)",
        
        caption = paste(
          "Source: Happy Planet Index.",
          "2025 data; GDP uses 2024 values."
        )
      ) +
      theme_minimal(
        base_size = 14,
        base_family = contour_font
      ) +
      theme(
        panel.grid.minor = element_blank(),
        
        plot.title = element_text(
          face = "bold",
          size = 18
        ),
        
        plot.subtitle = element_text(
          face = "plain",
          size = 11,
          margin = margin(b = 12)
        ),
        
        axis.title = element_text(
          face = "plain",
          size = 13
        ),
        
        axis.text = element_text(
          size = 11,
          color = "gray25"
        ),
        
        plot.caption = element_text(
          size = 9,
          hjust = 0,
          color = "gray40",
          margin = margin(t = 12)
        ),
        
        plot.margin = margin(
          t = 15,
          r = 20,
          b = 15,
          l = 15
        )
      )
    
    if (isTRUE(input$show_points)) {
      p <- p +
        geom_point(
          shape = 21,
          fill = "mediumpurple",
          color = "gray25",
          size = 2.8,
          stroke = 0.4,
          alpha = 0.75
        )
    }
    
    if (isTRUE(input$show_trend)) {
      p <- p +
        geom_smooth(
          method = "loess",
          formula = y ~ x,
          span = 0.9,
          se = FALSE,
          color = "firebrick",
          linewidth = 1
        )
    }
    
    if (isTRUE(input$show_labels)) {
      p <- p +
        geom_text(
          aes(label = country),
          family = contour_font,
          fontface = "plain",
          size = 3,
          vjust = -1,
          check_overlap = TRUE
        )
    }
    
    if (isTRUE(input$log_gdp)) {
      
      p <- p +
        scale_x_log10(
          labels = scales::label_number(
            big.mark = ",",
            accuracy = 1
          )
        ) +
        labs(
          x = paste(
            "GDP per capita",
            "(PPP, constant 2021 international $; log scale)"
          )
        )
      
    } else {
      
      p <- p +
        scale_x_continuous(
          labels = scales::label_number(
            big.mark = ",",
            accuracy = 1
          )
        )
    }
    
    p
  })

  output$contour_chart <- renderPlot({
    
    contour_plot()
    
  }, res = 96)
  

  output$download_png <- downloadHandler(
    
    filename = function() {
      "GDP_HPI_2025_contour.png"
    },
    
    content = function(file) {
      showtext::showtext_opts(dpi = 300)
      on.exit(showtext::showtext_opts(dpi = 96), add = TRUE)
      ggsave(
        filename = file,
        plot = contour_plot(),
        device = "png",
        width = 10,
        height = 7,
        units = "in",
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$country_count <- renderText({
    
    paste(
      "Countries displayed:",
      nrow(hpi_2025)
    )
  })
}

list(ui = ui, server = server)
})

ui <- navbarPage(
  title = "HPI 2025",
  tabPanel("Scatter", scatter_app$ui("scatter")),
  tabPanel("Bubble", bubble_app$ui("bubble")),
  tabPanel("Contour", contour_app$ui("contour"))
)

server <- function(input, output, session) {
  moduleServer("scatter", scatter_app$server)
  moduleServer("bubble", bubble_app$server)
  moduleServer("contour", contour_app$server)
}

shinyApp(ui, server)
