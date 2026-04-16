library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# ── 1. Load Netflix data ──────────────────────────────────────────────────────

raw <- read.csv("netflix_titles.csv", stringsAsFactors = FALSE,
                na.strings = c("", "NA", "[null]", "NULL"))

# ── 1a. Normalize rating ─────────────────────────────────────────────────────

normalize_rating <- function(r) {
  r <- trimws(r)
  dplyr::case_when(
    is.na(r) | r == ""                              ~ "Unknown",
    grepl("^\\d+\\s*min$", r)                       ~ "Unknown",
    r == "16"                                        ~ "16+",
    r == "AGES_16_"                                  ~ "16+",
    r == "AGES_18_"                                  ~ "18+",
    r %in% c("ALL", "ALL_AGES")                     ~ "All Ages",
    r %in% c("NOT_RATE", "UNRATED", "TV-NR", "NR",
             "UR")                                   ~ "NR",
    r == "TV-Y7-FV"                                  ~ "TV-Y7",
    TRUE                                             ~ r
  )
}

raw$rating_clean <- normalize_rating(raw$rating)

# ── 1b. Normalise type ───────────────────────────────────────────────────────

raw$type_clean <- dplyr::case_when(
  is.na(raw$type)                    ~ "Unknown",
  trimws(raw$type) == "Movie"        ~ "Movie",
  trimws(raw$type) == "TV Show"      ~ "TV Show",
  TRUE                               ~ trimws(raw$type)
)

# ── 1c. Clean country: take first listed country, NA → "Unknown" ─────────────

clean_country <- function(country_vec) {
  out <- trimws(country_vec)
  out <- sapply(strsplit(out, ","), function(parts) trimws(parts[1]))
  out[is.na(out) | out == ""] <- "Unknown"
  out
}

raw$country_clean <- clean_country(raw$country)

# ── 1d. Keep only columns we need ────────────────────────────────────────────

df <- raw %>%
  select(show_id, type = type_clean, country = country_clean,
         rating = rating_clean) %>%
  filter(!is.na(show_id))

# ── 2. Rating factor (ordering set dynamically by % in the chart) ────────────

rating_order <- c(
  "All Ages", "TV-Y", "TV-Y7", "7+", "TV-G", "G",
  "PG", "TV-PG", "PG-13", "TV-14", "13+",
  "16+", "TV-MA", "18+", "R", "NC-17",
  "NR", "Unknown"
)

rating_levels <- rating_order[rating_order %in% unique(df$rating)]
extra <- setdiff(unique(df$rating), rating_levels)
rating_levels <- c(rating_levels, sort(extra))

df$rating <- factor(df$rating, levels = rating_levels)

# ── 3. UI ─────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Interactive Rating Explorer",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),

  sidebar = sidebar(
    width = 220,
    selectInput(
      "type_filter",
      label = "Content Type",
      choices = sort(unique(df$type[df$type != "Unknown"])),
      selected = "Movie"
    ),
    hr(),
    helpText(
      "Click a bar in the Rating Distribution chart to drill down",
      "into the Country breakdown."
    ),
    actionButton("reset_click", "Reset Selection", class = "btn-sm btn-outline-secondary mt-2")
  ),

  layout_columns(
    col_widths = c(5, 7),

    card(
      card_header("Rating Distribution (% of catalog)"),
      plotlyOutput("rating_bar", height = "500px")
    ),

    card(
      card_header(textOutput("drilldown_title")),
      plotlyOutput("country_bar", height = "500px")
    )
  )
)

# ── 5. Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── 5a. Filtered base data (type filter) ────────────────────────────────────

  df_filtered <- reactive({
    df %>% filter(type == input$type_filter)
  })

  # ── 5b. Track clicked rating ────────────────────────────────────────────────

  selected_rating <- reactiveVal(NULL)

  observeEvent(event_data("plotly_click", source = "rating_chart"), {
    click <- event_data("plotly_click", source = "rating_chart")
    if (!is.null(click)) {
      clicked_val <- click$customdata[[1]]
      current <- selected_rating()
      if (!is.null(current) && current == clicked_val) {
        selected_rating(NULL)
      } else {
        selected_rating(clicked_val)
      }
    }
  })

  observeEvent(input$reset_click, {
    selected_rating(NULL)
  })

  observeEvent(input$type_filter, {
    selected_rating(NULL)
  })

  # ── 5c. Visual 1: Rating Distribution ───────────────────────────────────────

  output$rating_bar <- renderPlotly({
    data <- df_filtered()
    total <- nrow(data)

    if (total == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filter"))
    }

    rating_summary <- data %>%
      count(rating, name = "n") %>%
      mutate(pct = round(100 * n / total, 1)) %>%
      arrange(pct) %>%
      mutate(
        rating = factor(rating, levels = rating),
        highlight = if (!is.null(selected_rating())) {
          ifelse(as.character(rating) == selected_rating(), "selected", "other")
        } else {
          "selected"
        }
      )

    bar_color_map <- c("selected" = "#00A8E1", "other" = "#CCE8F4")

    p <- ggplot(
      rating_summary,
      aes(
        x = pct, y = rating, fill = highlight,
        customdata = rating,
        text = paste0(
          "<b>Rating:</b> ", rating, "<br>",
          "<b>Count:</b> ", n, "<br>",
          "<b>Share:</b> ", pct, "%"
        )
      )
    ) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = bar_color_map, guide = "none") +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Share of Catalog (%)", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_text(size = 11)
      )

    ggplotly(p, tooltip = "text", source = "rating_chart") %>%
      layout(
        clickmode = "event", dragmode = FALSE,
        margin = list(l = 10, r = 10, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── 5d. Drilldown title ────────────────────────────────────────────────────

  output$drilldown_title <- renderText({
    sel <- selected_rating()
    if (is.null(sel)) {
      "Country Breakdown — All Ratings"
    } else {
      paste0("Country Breakdown — Rating: ", sel)
    }
  })

  # ── 5e. Visual 2: Country Bar Chart ─────────────────────────────────────────

  output$country_bar <- renderPlotly({
    data <- df_filtered() %>%
      filter(country != "Unknown")

    sel <- selected_rating()
    if (!is.null(sel)) {
      data <- data %>% filter(as.character(rating) == sel)
    }

    if (nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No known-country data for this selection"))
    }

    # Identify top-10 countries by total count
    top10_countries <- data %>%
      count(country, name = "n") %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) %>%
      pull(country)

    plot_data <- data %>%
      filter(country %in% top10_countries) %>%
      count(country, name = "n") %>%
      mutate(
        country = factor(country, levels = rev(top10_countries))
      )

    p2 <- ggplot(
      plot_data,
      aes(
        x = n, y = country,
        text = paste0(
          "<b>", country, "</b><br>",
          "Titles: ", n
        )
      )
    ) +
      geom_col(width = 0.7, fill = "#E50914") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Number of Titles", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_text(size = 11)
      )

    ggplotly(p2, tooltip = "text") %>%
      layout(
        dragmode  = FALSE,
        margin    = list(l = 10, r = 10, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
