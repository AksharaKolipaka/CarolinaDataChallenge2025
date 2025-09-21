library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)


championship <- read_csv("space_economy_championship.csv", show_col_types = FALSE)
df <- read_csv("all_tables_combined.csv", show_col_types = FALSE)

ui <- navbarPage(
  title = "🚀 Space Economy Hunger Games: Capitol Broadcast",
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
                 h1("🎥 Capitol Broadcast: The Space Economy Hunger Games"),
                 p("Welcome, citizens of Panem... uh, Earth 🚀. 
                   This is the official Capitol transmission where sectors (districts) battle for dominance 
                   in the Space Economy Arena."),
                 p("Navigate through the tabs above to see:"),
                 tags$ul(
                   tags$li("🏆 Rankings — District standings"),
                   tags$li("🔥 Arena Race — Animated competition over time"),
                   tags$li("📊 Strength Profiles — Economic and innovation stats"),
                   tags$li("🎴 Tribute Cards — District spotlights")
                 ),
                 p("May the odds be ever in your favor ✨")
             )
           )
  ),
  
  tabPanel("🏆 Rankings",
           fluidPage(
             div(class="box",
                 h3("Capitol Rankings"),
                 DTOutput("rankingTable")
             )
           )
  ),
  
  tabPanel("🔥 Arena Race",
           fluidPage(
             div(class="box",
                 h3("Animated Capitol Arena Race"),
                 plotlyOutput("racePlot", height="600px")
             )
           )
  ),
  
  tabPanel("📊 Strength Profiles",
           fluidPage(
             div(class="box",
                 h3("District Strength Profiles"),
                 plotlyOutput("metricsPlot", height="600px")
             )
           )
  ),
  
  tabPanel("🎴 Tribute Cards",
           fluidPage(
             h3("Meet the District Tributes"),
             uiOutput("tributeCards")
           )
  )
)

server <- function(input, output, session) {
  
  # Rankings Table
  output$rankingTable <- renderDT({
    datatable(
      championship %>%
        select(rank, district_number, district_name, tier, competitive_index) %>%
        arrange(rank),
      rownames = FALSE,
      options = list(pageLength = 13, dom = 't'),
      class = "display cell-border compact stripe"
    ) %>%
      formatStyle("competitive_index", backgroundColor = styleInterval(
        c(25, 50, 75),
        c("#8B0000", "#FF8C00", "#FFD700", "#ADFF2F")
      ))
  })
  
  # Arena Race
  output$racePlot <- renderPlotly({
    race_df <- df %>%
      filter(table_name == "real_value_added") %>%
      group_by(year, district_name) %>%
      summarise(total = sum(value, na.rm=TRUE), .groups='drop') %>%
      group_by(year) %>%
      mutate(rank = rank(-total),
             share = total / sum(total) * 100) %>%
      arrange(year, rank)
    
    plot_ly(race_df,
            x = ~share,
            y = ~reorder(district_name, -share),
            frame = ~year,
            type = 'bar',
            orientation = 'h',
            text = ~paste0(district_name, ": ", round(share,1), "%"),
            hoverinfo = "text",
            marker = list(
              color = ~share,
              colorscale = list(
                c(0, 'rgb(255,69,0)'),
                c(0.5, 'rgb(255,215,0)'),
                c(1, 'rgb(186,85,211)')
              ),
              line = list(color="white", width=1.5)
            )) %>%
      layout(
        title = "🔥 Capitol Broadcast: District Output Race",
        xaxis = list(title="Share (%)", color="white", gridcolor="#333"),
        yaxis = list(title="Districts", color="white", gridcolor="#333"),
        plot_bgcolor = "#0b0c10",
        paper_bgcolor = "#0b0c10",
        font = list(family="Orbitron", color="#FFD700")
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = TRUE) %>%
      animation_slider(currentvalue = list(
        prefix = "Year: ",
        font = list(color = "#FFD700", size = 18, family="Orbitron")
      ))
  })
  
  # Strength Profiles
  output$metricsPlot <- renderPlotly({
    metrics_long <- championship %>%
      select(district_name, economic_growth, employment_resilience, private_dominance, innovation_intensity) %>%
      pivot_longer(-district_name, names_to = "metric", values_to = "value")
    
    plot_ly(metrics_long,
            x = ~metric,
            y = ~value,
            color = ~district_name,
            type = 'bar',
            hoverinfo = "text",
            text = ~paste(district_name, metric, round(value,1))) %>%
      layout(
        barmode = 'group',
        title = "District Strength Profiles",
        xaxis = list(title="Metric", color="white"),
        yaxis = list(title="Score / %", color="white"),
        plot_bgcolor = "#0b0c10",
        paper_bgcolor = "#0b0c10",
        font = list(family="Orbitron", color="#FFD700")
      )
  })
  
  # Tribute Cards
  output$tributeCards <- renderUI({
    cards <- lapply(1:nrow(championship), function(i) {
      dist <- championship[i, ]
      div(class="tribute-card",
          h3(paste("District", dist$district_number, "-", dist$district_name)),
          p(paste("🏆 Rank:", dist$rank)),
          p(paste("📈 Growth:", round(dist$economic_growth, 1), "%")),
          p(paste("🛡 Resilience:", round(dist$employment_resilience, 1), "%")),
          p(paste("💼 Private Dominance:", round(dist$private_dominance, 1), "%")),
          p(paste("💡 Innovation:", round(dist$innovation_intensity, 1), "%")),
          p(paste("🎖 Tier:", dist$tier))
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
