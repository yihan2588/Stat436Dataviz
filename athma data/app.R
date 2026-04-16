# app.R
# Childhood Asthma in the US (1997-2018), NHIS data

library(shiny)
library(bslib)
library(tidyverse)
library(ggridges)
library(RColorBrewer)

DATA_PATH <- "https://raw.githubusercontent.com/yihan2588/Stat436Dataviz_Asthmadata_app/refs/heads/main/Health_conditions_among_children_under_age_18__by_selected_characteristics__United_States.csv?token=GHSAT0AAAAAADOG4NLXYGQDHMCHPWNBKSVG2M525BA"

# Fix panel names
g_condition <- function(panel) {
  case_when(
    grepl("Asthma attack", panel) ~ "Asthma Attack",
    grepl("Current asthma", panel) ~ "Current Asthma",
    TRUE ~ panel
  )
}

# Order demographic labels sensibly
order_labels <- function(x, stub_name) {
  lvl <- list(
    "Age"  = c("0-4 years", "5-9 years", "5-17 years", "10-17 years"),
    "Sex"  = c("Male", "Female"),
    "Percent of poverty level" = c("Below 100%", "100%-199%", "200%-399%", "400% or more"),
    "Health insurance status at the time of interview" =
      c("Uninsured", "Insured", "Insured: Medicaid", "Insured: Private"),
    "Hispanic origin and race" =
      c("Hispanic or Latino: All races", "Not Hispanic or Latino: All races",
        "Not Hispanic or Latino: White only",
        "Not Hispanic or Latino: Black or African American only")
  )[[stub_name]]
  if (!is.null(lvl)) factor(x, levels = intersect(lvl, unique(x))) else factor(x)
}

# Load and keep only the two asthma panels
load_data <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
    filter(!is.na(ESTIMATE), grepl("asthma", PANEL, ignore.case = TRUE)) %>%
    mutate(
      CONDITION  = g_condition(PANEL),
      YEAR_START = as.integer(str_extract(YEAR, "^\\d{4}"))
    )
}

make_pal <- function(n) {
  n <- as.integer(n)
  if (n <= 8) brewer.pal(max(3L, n), "Set2")[seq_len(n)] else hue_pal()(n)
}

DEMO_CHOICES <- c(
  "Sex"              = "Sex",
  "Race / Ethnicity" = "Hispanic origin and race",
  "Poverty Level"    = "Percent of poverty level",
  "Insurance Status" = "Health insurance status at the time of interview",
  "Age Group"        = "Age"
)

ui <- page_navbar(
  title = "Childhood Asthma in the US (1997-2018)",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c7bb6"),
  # Tab 1: line plot
  nav_panel(
    "Trends",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectInput("demo_trend", "Demographic group:", DEMO_CHOICES, selected = "Sex"),
        checkboxGroupInput("cond_trend", "Conditions:", choices = character(0)),
        helpText("Lines colored by group. Shaded ribbons show +/- 1 SE.")
      ),
      card(
        full_screen = TRUE,
        card_header("Asthma Prevalence Over Time"),
        plotOutput("trend_plot", height = "400px"),
        card_footer(class = "text-muted", style = "font-size:.8rem;",
                    "3-year rolling averages, % of children under 18. Source: NHIS.",
                    tags$br(),
                    "* Current asthma: parent answered \"yes\" to the child ever having asthma and still having asthma (from 2003).",
                    tags$br(),
                    "* Asthma attack: parent answered \"yes\" to the child ever having asthma and having an episode or attack in the past 12 months.")
      )
    )
  ),

  # Tab 2: ridge plot
  nav_panel(
    "Disparities",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectInput("demo_ridge", "Demographic dimension:",
                    DEMO_CHOICES, selected = "Percent of poverty level"),
        helpText("Each ridge shows how prevalence varies across time periods.",
                 "Dots are individual 3-year estimates.")
      ),
      card(
        full_screen = TRUE,
        card_header("Asthma Rates by Demographic Group"),
        plotOutput("ridge_plot", height = "480px"),
        card_footer(class = "text-muted", style = "font-size:.8rem;",
                    "Each ridge pools 14 time-window estimates (1997-2018). Source: NHIS.")
      )
    )
  )
)

server <- function(input, output, session) {

  raw <- load_data(DATA_PATH)
  all_conds <- sort(unique(raw$CONDITION))

  observe({
    updateCheckboxGroupInput(session, "cond_trend",
                             choices = all_conds, selected = all_conds)
  })

  # Filtered data for trend plot
  trend_data <- reactive({
    req(input$demo_trend, length(input$cond_trend) > 0)
    df <- raw %>% filter(STUB_NAME == input$demo_trend,
                         CONDITION %in% input$cond_trend)
    df$STUB_LABEL <- order_labels(df$STUB_LABEL, input$demo_trend)
    df
  })

  # Filtered data for ridge plot
  ridge_data <- reactive({
    req(input$demo_ridge)
    df <- raw %>% filter(STUB_NAME == input$demo_ridge)
    df$STUB_LABEL <- order_labels(df$STUB_LABEL, input$demo_ridge)
    df %>% mutate(STUB_LABEL = fct_rev(STUB_LABEL))
  })

  # Faceted line plot
  output$trend_plot <- renderPlot({
    df <- trend_data()
    validate(need(nrow(df) > 0, "No data for this selection."))

    pal <- make_pal(nlevels(df$STUB_LABEL))

    ggplot(df, aes(YEAR_START, ESTIMATE, colour = STUB_LABEL, fill = STUB_LABEL)) +
      geom_ribbon(aes(ymin = ESTIMATE - SE, ymax = ESTIMATE + SE),
                  alpha = 0.15, colour = NA, na.rm = TRUE) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2, shape = 21, colour = "white", stroke = 0.4) +
      facet_wrap(~CONDITION, scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = seq(1997, 2016, by = 4)) +
      scale_colour_manual(values = pal, name = NULL) +
      scale_fill_manual(values = pal, name = NULL) +
      labs(x = "Year (start of 3-yr window)", y = "Prevalence (%)") +
      theme_bw(base_size = 13) +
      theme(
        strip.background = element_rect(fill = "#2c7bb6", colour = NA),
        strip.text       = element_text(colour = "white", face = "bold", size = 10),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      )
  }, res = 110)

  # Faceted ridge plot
  output$ridge_plot <- renderPlot({
    df <- ridge_data()
    validate(need(nrow(df) > 0, "No data for this selection."))

    pal <- rev(make_pal(nlevels(df$STUB_LABEL)))

    ggplot(df, aes(x = ESTIMATE, y = STUB_LABEL,
                   fill = STUB_LABEL, colour = STUB_LABEL)) +
      geom_density_ridges(
        jittered_points = TRUE, point_size = 1.8, point_alpha = 0.7,
        alpha = 0.6, scale = 0.85, rel_min_height = 0.01
      ) +
      facet_wrap(~CONDITION, scales = "free_x", ncol = 2) +
      scale_fill_manual(values = pal, guide = "none") +
      scale_colour_manual(values = pal, guide = "none") +
      labs(x = "Prevalence (%)", y = NULL) +
      theme_bw(base_size = 12) +
      theme(
        strip.background = element_rect(fill = "#2c7bb6", colour = NA),
        strip.text       = element_text(colour = "white", face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(size = 9.5)
      )
  }, res = 110)
}

shinyApp(ui, server)