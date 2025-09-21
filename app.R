library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)


# Load the 12 districts data as defined in Data.Rmd
bea_analysis <- read_csv("bea_district_analysis_complete.csv", show_col_types = FALSE)
all_tables <- read_csv("all_tables_combined.csv", show_col_types = FALSE)

# Define the 12 districts exactly as created in Data.Rmd
districts_12 <- tibble(
  district_id = 1:12,
  district_name = c(
    "Mining", 
    "Manufacturing",
    "Wholesale Trade",
    "Retail Trade", 
    "Transportation & Warehousing",
    "Information",
    "Finance & Real Estate",
    "Professional Services", 
    "Educational & Health Services",
    "Arts & Entertainment",
    "Government - Federal",
    "Government - State & Local"
  )
)

# --- Normalized Metric Calculation ---

# Helper: normalize to 0–100
normalize_0_100 <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  rng <- range(x, na.rm = TRUE)
  if(diff(rng) == 0) return(rep(50, length(x))) # flat distribution
  100 * (x - rng[1]) / diff(rng)
}

calculate_bea_district_metrics <- function(district_data, district_name) {
  
  latest_year <- max(district_data$year, na.rm = TRUE)
  
  # 1. Total Output
  total_output <- district_data %>%
    filter(table_name == "current_value_added", year == latest_year) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>% pull(total)
  
  # 2. Employment
  total_employment <- district_data %>%
    filter(table_name == "employment", year == latest_year) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>% pull(total)
  if(length(total_employment) == 0) total_employment <- 0
  
  # 3. Growth (CAGR 2020 → latest)
  growth_data <- district_data %>%
    filter(table_name == "current_value_added", year %in% c(2020, latest_year)) %>%
    group_by(year) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(year)
  
  growth_rate <- if(nrow(growth_data) >= 2 && growth_data$total[1] > 0) {
    calculate_cagr(growth_data$total[1], growth_data$total[nrow(growth_data)], 
                   nrow(growth_data)-1)
  } else { 0 }
  
  # 4. High-Value Ratio (tech/manufacturing/information subsectors)
  high_value_ratio <- case_when(
    district_name == "Manufacturing" ~ 
      (district_data %>%
         filter(table_name == "current_value_added", year == latest_year,
                grepl("computer.*electronic|other transportation equipment", tolower(industry))) %>%
         summarise(hv = sum(value, na.rm = TRUE)) %>% pull(hv)) / max(total_output, 1) * 100,
    district_name == "Professional Services" ~ 
      (district_data %>%
         filter(table_name == "current_value_added", year == latest_year,
                grepl("computer systems design", tolower(industry))) %>%
         summarise(hv = sum(value, na.rm = TRUE)) %>% pull(hv)) / max(total_output, 1) * 100,
    district_name == "Information" ~ 
      (district_data %>%
         filter(table_name == "current_value_added", year == latest_year,
                grepl("broadcasting.*telecommunications|data processing", tolower(industry))) %>%
         summarise(hv = sum(value, na.rm = TRUE)) %>% pull(hv)) / max(total_output, 1) * 100,
    TRUE ~ 0
  )
  
  # 5. Recovery Speed (2019 → 2020 → latest)
  recovery_data <- district_data %>%
    filter(table_name == "current_value_added", year %in% c(2019, 2020, latest_year)) %>%
    group_by(year) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(year)
  
  recovery_speed <- 0
  if(nrow(recovery_data) >= 3) {
    output_2019 <- recovery_data$total[recovery_data$year == 2019]
    output_2020 <- recovery_data$total[recovery_data$year == 2020]
    if(length(output_2019) > 0 && length(output_2020) > 0 && output_2019[1] > 0) {
      decline <- max(0, (output_2019[1] - output_2020[1]) / output_2019[1])
      if(decline > 0.01) {
        recovery_speed <- (total_output - output_2020[1]) / max(output_2019[1] - output_2020[1], 1)
      }
    }
  }
  
  # 6. Productivity (output per worker)
  productivity <- if(total_employment > 0) total_output / total_employment else 0
  
  # Return tidy metrics
  tibble(
    total_output = total_output,
    total_employment = total_employment,
    growth_rate = growth_rate,
    high_value_ratio = high_value_ratio,
    recovery_speed = recovery_speed,
    productivity = productivity
  )
}

# Note: The actual data processing is done in Data.Rmd
# This app loads the pre-processed results

ui <- navbarPage(
  title = "🚀 Space Economy 12 Districts Analysis",
  theme = shinytheme("cyborg"),
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #0b0c10;
          color: #f5f5f5;
          font-family: 'Orbitron', sans-serif;
        }
        h1, h2, h3 {
          font-family: 'Orbitron', sans-serif;
          color: #FFD700;
          text-shadow: 0px 0px 10px #ffae00, 0px 0px 20px #ff4500;
        }
        .box {
          background-color: #1c1c1c;
          border-radius: 12px;
          padding: 15px;
          box-shadow: 0px 0px 15px rgba(255, 215, 0, 0.3);
          margin-bottom: 20px;
        }
        .tribute-card {
          background: linear-gradient(145deg, #1c1c1c, #0e0e0e);
          border: 2px solid #FFD700;
          border-radius: 15px;
          padding: 20px;
          margin: 10px;
          color: #FFD700;
          text-align: center;
          box-shadow: 0px 0px 20px rgba(255, 215, 0, 0.4);
        }
      ")),
      tags$link(href="https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap", rel="stylesheet")
    )
  ),
  
  tabPanel("🏠 Home",
           fluidPage(
             div(class="box",
                 h1("🚀 Space Economy 12 Districts Analysis"),
                p("Welcome to the comprehensive analysis of the US space economy organized into 12 economic districts. 
                  This dashboard shows how the 12 districts contribute to and benefit from space economy activities."),
                p("Navigate through the tabs above to see:"),
                tags$ul(
                  tags$li("🏆 Rankings — District performance rankings"),
                  tags$li("🔥 District Race — Economic output comparison"),
                  tags$li("📆 Metrics Dashboard — Key performance indicators"),
                  tags$li("🎴 District Cards — Detailed district profiles")
                ),
                p("Data based on BEA economic analysis 2012-2023 organized into 12 districts 📊")
             )
           )
  ),
  
  tabPanel("🏆 Rankings",
           fluidPage(
             div(class="box",
                 h3("12 Districts Rankings"),
                 DTOutput("rankingTable")
             )
           )
  ),
  
  tabPanel("🔥 District Race",
           fluidPage(
             div(class="box",
                 h3("Economic Output by District"),
                 plotlyOutput("racePlot", height="600px")
             )
           )
  ),
  
  tabPanel("📆 Metrics Dashboard",
           fluidPage(
             div(class="box",
                 h3("District Performance Metrics"),
                 plotlyOutput("metricsPlot", height="600px")
             )
           )
  ),
  
  tabPanel("🎴 District Cards",
           fluidPage(
             h3("12 District Profiles"),
             uiOutput("sectorCards")
           )
  )
)

server <- function(input, output, session) {
  
  # Rankings Table
  output$rankingTable <- renderDT({
    datatable(
      bea_analysis %>%
        select(district_id, district_name, total_output, total_employment, composite_score, tier, rank) %>%
        arrange(rank),
      rownames = FALSE,
      options = list(pageLength = 12, dom = 't'),
      class = "display cell-border compact stripe",
      colnames = c("District ID", "District Name", "Total Output ($M)", "Employment", "Composite Score", "Tier", "Rank")
    ) %>%
      formatStyle("composite_score", backgroundColor = styleInterval(
        c(10, 20, 30),
        c("#8B0000", "#FF8C00", "#FFD700", "#ADFF2F")
      )) %>%
      formatCurrency("total_output", currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c("composite_score"), 1)
  })
  
  # District Race
  output$racePlot <- renderPlotly({
    # Use the 12 districts data, filter for significant activity
    race_df <- bea_analysis %>%
      filter(total_output >= 500) %>%  # Only show districts with significant activity
      arrange(desc(total_output))
    
    plot_ly(race_df,
            x = ~total_output,
            y = ~reorder(district_name, total_output),
            type = 'bar',
            orientation = 'h',
            text = ~paste0(district_name, ": $", format(total_output, big.mark = ","), "M (Rank ", rank, ")"),
            hoverinfo = "text",
            marker = list(
              color = ~composite_score,
              colorscale = list(
                c(0, 'rgb(139,0,0)'),      # Dark red for low scores
                c(0.25, 'rgb(255,140,0)'), # Orange
                c(0.5, 'rgb(255,215,0)'),  # Gold
                c(0.75, 'rgb(173,255,47)'), # Green yellow
                c(1, 'rgb(186,85,211)')    # Purple for high scores
              ),
              line = list(color="white", width=1.5)
            )) %>%
      layout(
        title = "🔥 Space Economy Output by District (Top Performers)",
        xaxis = list(title="Total Output ($ Millions)", color="white", gridcolor="#333"),
        yaxis = list(title="Districts", color="white", gridcolor="#333"),
        plot_bgcolor = "#0b0c10",
        paper_bgcolor = "#0b0c10",
        font = list(family="Orbitron", color="#FFD700")
      )
  })
  
  # Metrics Dashboard
  output$metricsPlot <- renderPlotly({
    # Select top 6 districts by rank and key metrics
    metrics_data <- bea_analysis %>%
      filter(total_output >= 500) %>%  # Only districts with significant activity
      arrange(rank) %>%  # Sort by rank (best first)
      head(6) %>%
      select(district_name, growth_rate, high_value_ratio, recovery_speed, composite_score) %>%
      pivot_longer(-district_name, names_to = "metric", values_to = "value") %>%
      mutate(
        metric = case_when(
          metric == "growth_rate" ~ "Growth Rate (%)",
          metric == "high_value_ratio" ~ "High Value Ratio (%)",
          metric == "recovery_speed" ~ "Recovery Speed",
          metric == "composite_score" ~ "Composite Score",
          TRUE ~ metric
        )
      )
    
    plot_ly(metrics_data,
            x = ~metric,
            y = ~value,
            color = ~district_name,
            type = 'bar',
            hoverinfo = "text",
            text = ~paste(district_name, ":", round(value,1))) %>%
      layout(
        barmode = 'group',
        title = "Key Performance Metrics by District",
        xaxis = list(title="Metric", color="white"),
        yaxis = list(title="Score", color="white"),
        plot_bgcolor = "#0b0c10",
        paper_bgcolor = "#0b0c10",
        font = list(family="Orbitron", color="#FFD700")
      )
  })
  
  # District Cards
  output$sectorCards <- renderUI({
    # Sort districts by rank and show all
    sorted_districts <- bea_analysis %>%
      arrange(rank)
    
    cards <- lapply(1:nrow(sorted_districts), function(i) {
      district <- sorted_districts[i, ]
      div(class="tribute-card",
          h3(paste("District", district$district_id, " - ", district$district_name)),
          p(paste("🏆 Rank:", district$rank)),
          p(paste("💰 Total Output:", paste0("$", format(district$total_output, big.mark = ","), "M"))),
          p(paste("👥 Employment:", format(district$total_employment, big.mark = ","))),
          p(paste("📈 Growth Rate:", round(district$growth_rate, 1), "%")),
          p(paste("🎯 High Value Ratio:", round(district$high_value_ratio, 1), "%")),
          p(paste("⚡ Recovery Speed:", round(district$recovery_speed, 1))),
          p(paste("🏅 Composite Score:", round(district$composite_score, 1))),
          p(paste("🎖 Tier:", district$tier))
      )
    })
    fluidRow(
      do.call(tagList, lapply(cards, function(card) {
        column(4, card)
      }))
    )
  })
}


shinyApp(ui, server)
