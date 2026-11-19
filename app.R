# =============================================================================
# Final Project – Infant Mortality & The Feminist Hypothesis
# GDA – Sina-Marie Mayer
#
# Research Question:
#   Are maternal empowerment indicators stronger predictors of infant mortality
#   than GDP per capita across countries?
#
# PART 2 - SHINY APP
#
# Run locally:  shiny::runApp()  (from this folder)
# Needs clean_data.rds in the same folder (created by 01_data_and_models.R)
# =============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)

# Shiny runs apps with the app folder as working directory, locally and on shinyapps.io
clean_data <- readRDS("clean_data.rds")

# -----------------------------------------------------------------------------
# 1. MODELS (same specification as in 01_data_and_models.R)
# -----------------------------------------------------------------------------
# Model 1: Baseline — GDP alone
model_baseline <- lm(infant_mortality ~ gdp_per_cap, data = clean_data)

# Model 2: Full model — feminist variables + controls
model_full <- lm(
  infant_mortality ~ gdp_per_cap + female_lit + adol_fertility +
    female_labor + health_expend + sanitation + urban_pop,
  data = clean_data
)

# Key numbers for the About tab, computed from the data so they stay correct
# if the data is refreshed
n_countries  <- nrow(clean_data)
adj_r2_base  <- round(summary(model_baseline)$adj.r.squared, 2)
adj_r2_full  <- round(summary(model_full)$adj.r.squared, 2)
gdp_p_full   <- coef(summary(model_full))["gdp_per_cap", "Pr(>|t|)"]

# -----------------------------------------------------------------------------
# 2. SHINY APP
# -----------------------------------------------------------------------------

# Slider ranges derived from observed data; min clamped to 0 for all vars
make_range <- function(x, min_floor = 0) {
  c(max(min_floor, floor(min(x, na.rm = TRUE))),
    ceiling(max(x, na.rm = TRUE)))
}

gdp_range    <- make_range(clean_data$gdp_per_cap)
adol_range   <- make_range(clean_data$adol_fertility)
labor_range  <- make_range(clean_data$female_labor)
health_range <- make_range(clean_data$health_expend)

regions <- c("All", sort(unique(clean_data$region)))

theme <- bs_theme(
  bootswatch  = "flatly",
  primary     = "#2C3E50",
  success     = "#27AE60",
  base_font   = font_google("Inter"),
  heading_font = font_google("Inter")
)

stat_card <- function(value, label, color = "#2C3E50") {
  div(
    style = paste0("background:", color, "; color:white; border-radius:10px;
                    padding:18px 24px; text-align:center; flex:1; margin:6px;"),
    div(style = "font-size:2em; font-weight:700; line-height:1.1;", value),
    div(style = "font-size:0.82em; opacity:0.85; margin-top:4px;", label)
  )
}

ui <- page_navbar(
  title = "Infant Mortality Explorer",
  theme    = theme,
  bg       = "#2C3E50",
  inverse  = TRUE,
  fillable = FALSE,

  # ── TAB 1: ABOUT ──────────────────────────────────────────────────────────
  nav_panel("About",
    tags$head(tags$style(HTML("
      /* Active and hover nav tabs */
      .navbar .navbar-nav .nav-link         { color: rgba(255,255,255,0.75) !important; }
      .navbar .navbar-nav .nav-link:hover   { color: #ffffff !important; background: rgba(255,255,255,0.15) !important; border-radius:6px; }
      .navbar .navbar-nav .nav-link.active  { color: #ffffff !important; background: rgba(255,255,255,0.25) !important; border-radius:6px; font-weight:600; }
      .about-hero { background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
                    color: white; padding: 60px 40px; text-align: center; }
      .about-hero h1 { font-size: 2.4em; font-weight: 700; margin-bottom: 12px; }
      .about-hero p  { font-size: 1.15em; opacity: 0.9; max-width: 700px;
                       margin: 0 auto; line-height: 1.7; }
      .about-section { max-width: 860px; margin: 40px auto; padding: 0 24px; }
      .about-section h3 { color: #2C3E50; font-weight: 700; margin-top: 32px; }
      .about-section p, .about-section li { color: #444; line-height: 1.8; font-size: 1em; }
      .stat-row { display:flex; flex-wrap:wrap; justify-content:center;
                  margin: 32px 0; gap:4px; }
      .how-to { background:#f8f9fa; border-left:4px solid #3498DB;
                padding:16px 20px; border-radius:0 8px 8px 0; margin:16px 0; }
      .how-to b { color:#2C3E50; }
    "))),

    div(class = "about-hero",
      tags$h1("Does Women's Empowerment Save Lives?"),
      tags$p(paste0("An interactive exploration of infant mortality across ", n_countries,
                    " countries, and why gender equality may matter more than national wealth."))
    ),

    div(class = "about-section",
      tags$h3("The Research Question"),
      tags$p("Standard economic accounts treat national wealth as the primary driver of child
              health outcomes. This project tests an alternative view, referred to here as the ",
             tags$b("\u201Cfeminist hypothesis\u201D"), ": that indicators of women\u2019s empowerment,
              specifically female literacy, adolescent fertility, and female labour force
              participation, predict infant mortality ",
             tags$b("better than GDP per capita alone"), "."),

      tags$h3("Key Findings"),
      div(class = "stat-row",
        stat_card(paste0("Adj. R\u00B2 = ", adj_r2_base), "GDP alone", "#7F8C8D"),
        stat_card(paste0("Adj. R\u00B2 = ", adj_r2_full),
                  "Full model with feminist variables", "#27AE60"),
        stat_card(n_countries, "Countries analysed", "#2980B9"),
        stat_card(paste0("p = ", formatC(gdp_p_full, format = "f", digits = 3)),
                  "GDP significance in full model", "#E74C3C")
      ),
      tags$p("Once the empowerment variables are added to the model, GDP per capita is no longer
              statistically significant, and Adjusted R\u00B2 more than doubles, from ",
             adj_r2_base, " to ", adj_r2_full, ". However, not every empowerment indicator contributes equally. The two significant
              feminist predictors are ", tags$b("female literacy"), " (more education, fewer deaths)
              and the ", tags$b("adolescent fertility rate"),
             " (more teenage pregnancies, more deaths). ",
             tags$b("Female labour force participation"), " is not statistically significant once
              literacy and fertility are controlled, suggesting that women\u2019s education and
              reproductive autonomy matter more for infant survival than workforce participation
              per se. Among the structural controls, only ", tags$b("basic sanitation access"),
             " is a significant predictor."),

      tags$h3("Data"),
      tags$p("All indicators are drawn from the ",
             tags$b("World Bank Development Indicators (WDI)"), ". For each country, the most
              recent available value between 2015 and 2022 was used. High-income countries that
              do not report female literacy (owing to near-universal coverage) were assigned a
              value of 99%, in order to prevent disproportionate loss of the upper income bracket.
              Regional aggregates and rows with implausible or missing values were excluded from
              the sample."),

      tags$h3("How to Use the Simulator"),
      div(class = "how-to",
        tags$b("Step 1."), " Open the ", tags$b("Simulator"), " tab.", tags$br(),
        tags$b("Step 2."), " Filter by region, then select a country to load its actual values.", tags$br(),
        tags$b("Step 3."), " Adjust any slider to ask \u201Cwhat if this country had different characteristics?\u201D", tags$br(),
        tags$b("Step 4."), " The predicted infant mortality rate updates in real time, and the
                country\u2019s point shifts accordingly on the scatter plot.", tags$br(),
        tags$b("Step 5."), " Switch to the ", tags$b("World Map"), " tab to see the what-if change
                reflected globally."
      ),

      tags$p(style = "color:#aaa; font-size:0.85em; margin-top:40px;",
        "Sina-Marie Mayer \u2022 GDA Final Project \u2022 April 2026 \u2022 ",
        "Predictions based on a multiple linear regression model (Adj. R\u00B2 = ",
        round(summary(model_full)$adj.r.squared, 3), ")."
      )
    )
  ),

  # ── TAB 2: SIMULATOR ──────────────────────────────────────────────────────
  nav_panel("Simulator",
    tags$head(tags$style(HTML("
      .sidebar-scroll { max-height:92vh; overflow-y:auto; position:sticky; top:10px; }
      .pred-card { background:linear-gradient(135deg,#2C3E50,#3498DB); color:white;
                   border-radius:12px; padding:20px 24px; margin-bottom:16px; }
      .pred-card .label { font-size:0.85em; opacity:0.8; margin-bottom:4px; }
      .pred-card .value { font-size:2.2em; font-weight:700; line-height:1.1; }
      .pred-card .unit  { font-size:0.9em; opacity:0.75; }
      .section-label { font-size:0.7em; font-weight:700; letter-spacing:.08em;
                       text-transform:uppercase; color:#7f8c8d; margin:14px 0 4px; }
    "))),

    layout_sidebar(
      sidebar = sidebar(
        width  = 300,
        class  = "sidebar-scroll",

        div(class = "section-label", "Filter"),
        selectInput("region_filter", NULL, choices = regions, selected = "All"),
        selectInput("country_select", NULL,
                    choices = c("Select a country\u2026" = ""), selected = ""),
        actionButton("reset_sliders", "Reset to actual values",
                     class = "btn-sm btn-outline-secondary w-100",
                     style = "margin-bottom:8px;"),

        div(class = "section-label", "Feminist Variables"),
        sliderInput("female_lit", "Female Literacy Rate (%)", 0, 100,
                    value = round(median(clean_data$female_lit))),
        sliderInput("adol_fertility", "Adolescent Fertility Rate",
                    min = adol_range[1], max = adol_range[2],
                    value = round(median(clean_data$adol_fertility))),
        sliderInput("female_labor", "Female Labour Force Participation (%)",
                    min = labor_range[1], max = labor_range[2],
                    value = round(median(clean_data$female_labor))),

        div(class = "section-label", "Economic & Structural Controls"),
        sliderInput("gdp_per_cap", "GDP per Capita (PPP USD)",
                    min = gdp_range[1], max = gdp_range[2],
                    value = round(median(clean_data$gdp_per_cap)), step = 100),
        sliderInput("health_expend", "Health Expenditure per Capita (USD)",
                    min = health_range[1], max = health_range[2],
                    value = round(median(clean_data$health_expend)), step = 10),
        sliderInput("sanitation", "Basic Sanitation Access (%)", 0, 100,
                    value = round(median(clean_data$sanitation))),
        sliderInput("urban_pop", "Urban Population (%)", 0, 100,
                    value = round(median(clean_data$urban_pop)))
      ),

      div(
        div(class = "pred-card",
          div(class = "label", "Predicted Infant Mortality Rate"),
          div(class = "value", textOutput("predicted_imr_num")),
          div(class = "unit", "deaths per 1,000 live births \u2022 full regression model")
        ),
        navset_card_tab(
          nav_panel("Scatter Plot",
            plotlyOutput("scatter_plot", height = "430px")
          ),
          nav_panel("World Map",
            plotlyOutput("world_map", height = "430px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # When region changes, update country dropdown and reset sliders to region medians
  observeEvent(input$region_filter, {
    df <- if (input$region_filter == "All") clean_data else filter(clean_data, region == input$region_filter)
    choices <- setNames(df$iso3c, df$country)
    choices <- c("(none)" = "", choices[order(names(choices))])
    updateSelectInput(session, "country_select", choices = choices, selected = "")
    updateSliderInput(session, "gdp_per_cap",    value = round(median(df$gdp_per_cap) / 100) * 100)
    updateSliderInput(session, "female_lit",     value = round(median(df$female_lit)))
    updateSliderInput(session, "adol_fertility", value = round(median(df$adol_fertility)))
    updateSliderInput(session, "female_labor",   value = round(median(df$female_labor)))
    updateSliderInput(session, "health_expend",  value = round(median(df$health_expend) / 10) * 10)
    updateSliderInput(session, "sanitation",     value = round(median(df$sanitation)))
    updateSliderInput(session, "urban_pop",      value = round(median(df$urban_pop)))
  })

  # Reset button — restore sliders to selected country's values (or region medians)
  observeEvent(input$reset_sliders, {
    df <- if (input$region_filter == "All") clean_data else filter(clean_data, region == input$region_filter)
    if (!is.null(input$country_select) && input$country_select != "") {
      row <- clean_data %>% filter(iso3c == input$country_select)
      if (nrow(row) > 0) {
        updateSliderInput(session, "gdp_per_cap",    value = round(row$gdp_per_cap / 100) * 100)
        updateSliderInput(session, "female_lit",     value = round(row$female_lit))
        updateSliderInput(session, "adol_fertility", value = round(row$adol_fertility))
        updateSliderInput(session, "female_labor",   value = round(row$female_labor))
        updateSliderInput(session, "health_expend",  value = round(row$health_expend / 10) * 10)
        updateSliderInput(session, "sanitation",     value = round(row$sanitation))
        updateSliderInput(session, "urban_pop",      value = round(row$urban_pop))
        return()
      }
    }
    updateSliderInput(session, "gdp_per_cap",    value = round(median(df$gdp_per_cap) / 100) * 100)
    updateSliderInput(session, "female_lit",     value = round(median(df$female_lit)))
    updateSliderInput(session, "adol_fertility", value = round(median(df$adol_fertility)))
    updateSliderInput(session, "female_labor",   value = round(median(df$female_labor)))
    updateSliderInput(session, "health_expend",  value = round(median(df$health_expend) / 10) * 10)
    updateSliderInput(session, "sanitation",     value = round(median(df$sanitation)))
    updateSliderInput(session, "urban_pop",      value = round(median(df$urban_pop)))
  })

  # When a country is selected, populate sliders with its actual values
  observeEvent(input$country_select, {
    req(input$country_select != "")
    row <- clean_data %>% filter(iso3c == input$country_select)
    if (nrow(row) == 0) return()
    updateSliderInput(session, "gdp_per_cap",    value = round(row$gdp_per_cap))
    updateSliderInput(session, "female_lit",     value = round(row$female_lit))
    updateSliderInput(session, "adol_fertility", value = round(row$adol_fertility))
    updateSliderInput(session, "female_labor",   value = round(row$female_labor))
    updateSliderInput(session, "health_expend",  value = round(row$health_expend))
    updateSliderInput(session, "sanitation",     value = round(row$sanitation))
    updateSliderInput(session, "urban_pop",      value = round(row$urban_pop))
  })

  # Reactive: predicted IMR from slider values
  pred_imr <- reactive({
    new_obs <- data.frame(
      gdp_per_cap    = input$gdp_per_cap,
      female_lit     = input$female_lit,
      adol_fertility = input$adol_fertility,
      female_labor   = input$female_labor,
      health_expend  = input$health_expend,
      sanitation     = input$sanitation,
      urban_pop      = input$urban_pop
    )
    max(0, predict(model_full, newdata = new_obs))
  })

  output$predicted_imr_num <- renderText({
    round(pred_imr(), 1)
  })

  # Interactive scatter: hover shows country + key values; selected country's
  # point moves in real time as sliders change
  output$scatter_plot <- renderPlotly(suppressWarnings({
    df <- if (input$region_filter == "All") clean_data else filter(clean_data, region == input$region_filter)

    # Tooltip text for background points
    df <- df %>% mutate(
      tooltip = paste0(
        "<b>", country, "</b><br>",
        "IMR: ", round(infant_mortality, 1), "<br>",
        "GDP p.c.: $", scales::comma(round(gdp_per_cap)), "<br>",
        "Female literacy: ", round(female_lit, 1), "%<br>",
        "Adol. fertility: ", round(adol_fertility, 1), "<br>",
        "Female labor: ", round(female_labor, 1), "%"
      )
    )

    p <- ggplot(df, aes(x = gdp_per_cap, y = infant_mortality, color = region)) +
      geom_point(aes(text = tooltip), alpha = 0.65, size = 2.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#2C3E50",
                  linewidth = 1.2, alpha = 0.12) +
      scale_x_log10(labels = scales::comma) +
      labs(
        x     = "GDP per Capita, PPP (log scale)",
        y     = "Infant Mortality Rate (per 1,000 live births)",
        title = "GDP per Capita vs. Infant Mortality Rate",
        color = "Region"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", legend.text = element_text(size = 9))

    # Arrow from actual position to what-if position
    if (!is.null(input$country_select) && input$country_select != "") {
      orig <- clean_data %>% filter(iso3c == input$country_select)
      if (nrow(orig) > 0) {
        actual_tip <- paste0(
          "<b>", orig$country, " — actual</b><br>",
          "IMR: ", round(orig$infant_mortality, 1), "<br>",
          "GDP p.c.: $", scales::comma(round(orig$gdp_per_cap))
        )
        whatif_tip <- paste0(
          "<b>", orig$country, " — what if</b><br>",
          "Predicted IMR: ", round(pred_imr(), 1), "<br>",
          "GDP p.c.: $", scales::comma(input$gdp_per_cap)
        )

        p <- p +
          # Actual position
          geom_point(data = orig,
                     aes(x = gdp_per_cap, y = infant_mortality, text = actual_tip),
                     color = "#2C3E50", size = 5, shape = 16, inherit.aes = FALSE) +
          # What-if position
          geom_point(
            data = data.frame(x = input$gdp_per_cap, y = pred_imr(), text = whatif_tip),
            aes(x = x, y = y, text = text),
            color = "#e74c3c", size = 5, shape = 16, inherit.aes = FALSE
          )
      }
    }

    base_annotations <- list(
      list(text = "Line: simple regression (GDP only)",
           x = 0.5, y = -0.18, xref = "paper", yref = "paper",
           showarrow = FALSE, xanchor = "center",
           font = list(size = 11, color = "#888888"))
    )

    plt <- ggplotly(p, tooltip = "text")

    if (!is.null(input$country_select) && input$country_select != "") {
      orig <- clean_data %>% filter(iso3c == input$country_select)
      if (nrow(orig) > 0) {
        country_annotations <- c(base_annotations, list(
          list(text = "<b style='color:#2C3E50'>\u25CF Actual</b> \u2022 <b style='color:#e74c3c'>\u25CF What-if (full model)</b>",
               x = 0.5, y = -0.13, xref = "paper", yref = "paper",
               showarrow = FALSE, xanchor = "center", font = list(size = 12)),
          list(x = input$gdp_per_cap, y = pred_imr(),
               ax = orig$gdp_per_cap,  ay = orig$infant_mortality,
               xref = "x", yref = "y", axref = "x", ayref = "y",
               showarrow = TRUE, arrowhead = 2, arrowsize = 1.2,
               arrowwidth = 2, arrowcolor = "#e74c3c", text = "")
        ))
        plt <- plt %>% layout(legend = list(orientation = "h", y = -0.25),
                              annotations = country_annotations)
      }
    } else {
      plt <- plt %>% layout(legend = list(orientation = "h", y = -0.25),
                            annotations = base_annotations)
    }
    plt
  }))

  # World map shaded by actual IMR
  output$world_map <- renderPlotly({
    map_data <- clean_data

    # Replace selected country's IMR with the what-if prediction
    if (!is.null(input$country_select) && input$country_select != "") {
      map_data <- map_data %>%
        mutate(
          display_imr = ifelse(iso3c == input$country_select, pred_imr(), infant_mortality),
          tooltip = ifelse(
            iso3c == input$country_select,
            paste0("<b>", country, " (what-if)</b><br>Predicted IMR: ", round(pred_imr(), 1)),
            paste0(country, "<br>IMR: ", round(infant_mortality, 1))
          )
        )
    } else {
      map_data <- map_data %>%
        mutate(
          display_imr = infant_mortality,
          tooltip = paste0(country, "<br>IMR: ", round(infant_mortality, 1))
        )
    }

    plot_geo(map_data) %>%
      add_trace(
        z          = ~display_imr,
        locations  = ~iso3c,
        text       = ~tooltip,
        hoverinfo  = "text",
        colorscale = "Reds",
        colorbar   = list(title = "IMR per 1,000")
      ) %>%
      layout(
        title    = "Infant Mortality Rate by Country",
        dragmode = "zoom",
        geo      = list(
          showframe      = FALSE,
          showcoastlines = TRUE,
          projection     = list(type = "natural earth")
        )
      )
  })
}

shinyApp(ui = ui, server = server)
