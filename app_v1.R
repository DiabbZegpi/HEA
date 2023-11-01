## app.R v1##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(conflicted)
library(plotly)
library(ranger)
tidymodels_prefer()
conflict_prefer('box', 'shinydashboard')
conflict_prefer('config', 'plotly')

theme_set(theme_minimal())
theme_update(
  text = element_text(size = 14),
  axis.title = element_text(face = 'bold'),
  strip.text = element_text(size = 12),
  legend.key = element_rect(fill = NA),
  legend.text = element_text(size = 12),
  axis.ticks = element_blank(),
  panel.spacing = unit(2, 'lines'),
  strip.background = element_blank(),
  strip.text.x = element_text(face = 'bold'),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = 3, color = 'gray')
)

rf_fit <- readRDS('./rf_fit.rds')

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = 'Phase Predictor v1'),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    includeCSS('www/style.css'),
    
    tabItem(
      tabName = 'phase_tab',
      
      fluidRow(
        
        align = 'center',
        h3("Set the values of the predictors:", class = 'parameters'),
        
        box(
          numericInput('dHmix', 'HMix', min = -80, max = 10, value = -10, step = 0.1),
          width = 3
        ),
        box(
          numericInput('Elect.Diff', 'Electronegativity', min = 0, max = 1, value = 0.16, step = 0.01),
          width = 3
        ),
        box(
          numericInput('VEC', 'VEC', min = 1, max = 12, value = 6, step = 0.1),
          width = 3
        )
        
      ),
      
      fluidRow(
        
        box(
          # title = 'Random forest phase predictor', 
          plotlyOutput('preds_plot'), 
          width = 12
        ),
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
      mutate(
        phase = str_remove(phase, '.pred_'),
        `predicted prob` = paste0(round(prob * 100, 2), '%')
      )
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
      ggplot(aes(x = phase, y = prob, label = `predicted prob`)) +
      geom_col(aes(fill = prob), show.legend = FALSE, alpha = 0.7) +
      geom_text(nudge_y = 0.03) +
      scale_y_continuous(
        labels = label_percent(), 
        breaks = seq(0, 1, by = 0.25),
        limits = c(0, 1.05)
      ) +
      scale_fill_viridis_c(option = 'B', begin = 0.1, end = 0.9) +
      labs(
        x = NULL, 
        y = 'Predicted probability',
        title = 'Random Forest Phase Predictor v1'
      ) 
    
    ggplotly(p, tooltip = c("x", "label")) |> 
      config(modeBarButtonsToRemove = c(
        'sendDataToCloud',
        'zoom2d',
        'pan2d',
        'select2d',
        'lasso2d',
        'zoomIn2d',
        'zoomOut2d',
        'autoScale2d',
        'resetScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian'
        
      ))
  })
  
  output$download_results <- downloadHandler(
    filename = function() 'Phase prediction.csv',
    content = function(file) {
      write_csv(results(), file)
    }
  )
  
}

shinyApp(ui, server)