## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(conflicted)
library(plotly)
library(ranger)
tidymodels_prefer()
conflict_prefer("box", "shinydashboard")

theme_set(theme_minimal())
theme_update(text = element_text(size = 14),
             axis.title = element_text(face = 'bold'),
             strip.text = element_text(size = 12),
             legend.key = element_rect(fill = NA),
             legend.text = element_text(size = 12),
             axis.ticks = element_blank(),
             # aspect.ratio = 1,
             panel.spacing = unit(2, 'lines'),
             strip.background = element_blank(),
             strip.text.x = element_text(face = 'bold'),
             panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(linetype = 3, color = 'gray'))
 
rf_fit <- readRDS('./rf_fit.rds')

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = 'Phase predictor'),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tabItem(
      tags$head(tags$style(HTML(
        '
        .content {
          padding-left: 30px;
          padding-right: 30px;
          max-width: 1500px;
        }
        
        .parameters {
          text-align: center;
        }
        
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
          background: #685cac;
          border-top: 1px solid #685cac;
          border-bottom: 1px solid #685cac;
        }
        
        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
          background: #685cac;
          border-top: 1px solid #685cac;
          border-bottom: 1px solid #685cac;
        }
        
        .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {
          background: #685cac;
          border-top: 1px solid #685cac;
          border-bottom: 1px solid #685cac;
        }
        
        .btn-default {
          background-color: #685cac;
          border-color: #685cac;
          color: white;
        }
        
        .btn-default:hover {
          background-color: #c36c8c;
          border-color: #c36c8c;
          color: white;
        }
        
        .irs-grid-text {font-size: 0.8em}
        
        .irs-single {
          font-size: 0.9em !important;
          background-color: #685cac !important;
          color: white;
        }
        
        .irs-min, .irs-max {
          font-size: 1em !important;
          background-color: white !important;
          color: black;
        }
        
        .box {
          border-top-color: #685cac;
        }
        
        .control-label {font-size: 1.2em;}
        '
      ))),
      tabName = 'phase_tab',
      
      fluidRow(
        
        h3("Set the predictors' values:", class = 'parameters'),
        box(sliderInput('dHmix', 'HMix', 
                        min = -80, max = 10, value = -10, step = 1),
            width = 4),
        box(sliderInput('Elect.Diff', 'Electronegativity', 
                        min = 0, max = 1, value = 0.16, step = 0.01),
            width = 4),
        box(sliderInput('VEC', 'VEC', 
                        min = 1, max = 12, value = 6, step = 0.1),
            width = 4)
        
      ),
      
      fluidRow(
        
        box(title = 'Random forest phase predictor', plotlyOutput('preds_plot'), 
            width = 12),
        downloadButton('download_results', 'Download results')
        
      )
    )
  )
  
)

server <- function(input, output) { 
  
  new_hea <- reactive({
    tibble(
      dHmix = input$dHmix,
      Elect.Diff = input$Elect.Diff,
      VEC = input$VEC
    )
  })
  
  predictions <- reactive({
    predict(rf_fit, new_data = new_hea(), type = 'prob') |> 
      pivot_longer(everything(), names_to = 'phase', values_to = 'prob') |> 
      mutate(phase = str_remove(phase, '.pred_'),
             label = paste0(round(prob * 100, 2), '%'))
  })
  
  results <- reactive({
    new_hea() |> 
      bind_cols(
        predictions() |> 
          select(phase, prob) |> 
          pivot_wider(names_from = phase, values_from = prob)
      )
  })
  
  output$preds_plot <- renderPlotly({
    p <- predictions() |> 
      mutate(phase = str_replace(phase, 'FCC\\+BCC\\+IM', 'FCC+BCC+\nIM')) |> 
      ggplot(aes(x = phase, y = prob)) +
      geom_col(aes(fill = prob), show.legend = FALSE, alpha = 0.7) +
      geom_text(aes(label = label), nudge_y = 0.03) +
      scale_y_continuous(
        labels = label_percent(), 
        breaks = seq(0, 1, by = 0.25),
        limits = c(0, 1)
      ) +
      scale_fill_viridis_c(option = 'B', begin = 0.1, end = 0.9) +
      labs(x = NULL, y = 'Predicted probability') 
    
    ggplotly(p)  
  })
  
  output$download_results <- downloadHandler(
    filename = function() 'Phase prediction.csv',
    content = function(file) {
      write_csv(results(), file)
    }
  )
  
}

shinyApp(ui, server)