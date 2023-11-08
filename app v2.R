## app.R v2.01 ##
# Last update:
# New plot for breakdown additive effects
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)

library(tidyverse)
library(tidymodels)
library(conflicted)
library(ranger)
library(DALEX)
library(DALEXtra)


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

rf_fit <- readRDS('./rf_fit_v2.rds')
rf_explainer <- readRDS('./general_rf_v2_explainer.rds')

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = 'Phase Predictor v2'),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    includeCSS('www/style.css'),
    
    tabItem(
      tabName = 'phase_tab',
      
      fluidRow(
        
        h3("Set the values of the predictors:", class = 'parameters'),
        box(
          numericInput('dHmix', 'dHMix', min = -80, max = 10, value = -10, step = 0.1),
          width = 3
        ),
        box(
          numericInput('Elect.Diff', 'Electronegativity', min = 0, max = 1, value = 0.16, step = 0.01),
          width = 3
        ),
        box(
          numericInput('VEC', 'VEC', min = 1, max = 12, value = 6, step = 0.1),
          width = 3
        ),
        box(
          numericInput('gamma_factor', 'Atomic Packing Factor', min = 0, max = 2, value = 1.7, step = 0.01),
          width = 3
        )
        
      ),
      
      fluidRow(
        
        box(
          title = h3('Random Forest Phase Predictor (v2)'),
          plotlyOutput('preds_plot') |> withSpinner(type = 3, color = 'purple', color.background = 'white'), 
          width = 12
        ),
        downloadButton('download_results', 'Download results')
        
      ),
      
      fluidRow(
        
        box(
          title = h3(textOutput('bd_plot_title')),
          plotOutput('break_down_plot') |> withSpinner(type = 3, color = 'purple', color.background = 'white'), 
          width = 12
        )
      )
    )
  )
  
)

server <- function(input, output) { 
  
  new_hea <- reactive({
    tibble(
      dHmix = input$dHmix,
      Elect.Diff = input$Elect.Diff,
      VEC = input$VEC,
      gamma_factor = input$gamma_factor
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
        y = 'Predicted probability'
        # title = 'Random Forest Phase Predictor (v2)'
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
  
  pred_label <- reactive({
    predictions() |> 
      slice_max(prob, n = 1, with_ties = FALSE) |> 
      pull(phase)
  })
  
  bd_profile <- reactive({
    predict_parts(
      explainer = rf_explainer,
      new_observation = new_hea(),
      type = 'break_down'
    ) |> 
      as_tibble() |> 
      filter(label == paste0('rf.', pred_label()))
  })
  
  output$bd_plot_title <- renderText({
    paste0('Break Down Feature Additive Contribution - Prediction: ', pred_label())
  })
  
  output$break_down_plot <- renderPlot({
    prediction_contribution <- bd_profile()[bd_profile()$variable == 'prediction', ][['cumulative']] - bd_profile()[bd_profile()$variable == 'intercept', ][['cumulative']]
    prediction_label <- paste0(ifelse(prediction_contribution > 0, '+', '-'), round(prediction_contribution, 3), ' = ', round(bd_profile()[bd_profile()$variable == 'prediction', ][['cumulative']], 3))
    
    plot_df <- bd_profile() |> 
      mutate(
        variable = str_replace(variable, 'gamma_factor', 'atomic packing factor'),
        variable = str_replace(variable, 'Elect.Diff', 'electronegativity'),
        variable = fct_rev(fct_inorder(variable)),
        x = cumulative - contribution,
        x = ifelse(
          variable == 'prediction', 
          bd_profile()[bd_profile()$variable == 'intercept', ][['contribution']],
          x
        ),
        color_flag = case_when(
          variable == 'intercept' ~ 'intercept',
          variable == 'prediction' ~ 'prediction',
          contribution > 0 ~ 'positive',
          .default = 'negative'
        ),
        label = paste0(ifelse(contribution > 0, '+', ''), round(contribution, 3)),
        label = ifelse(variable == 'prediction', prediction_label, label)
      )
    
    plot_df |> 
      ggplot(aes(x = contribution, y = variable)) +
      geom_vline(
        xintercept = bd_profile()[bd_profile()$variable == 'intercept', ][['contribution']],
        linetype = 'dashed'
      ) +
      geom_segment(
        aes(
          x = x, xend = cumulative,
          y = variable, yend = variable,
          color = color_flag
        ),
        linewidth = 3,
        show.legend = FALSE,
        lineend = 'round',
        arrow = arrow(angle = 40, length = unit(3, 'mm'), type = 'closed')
      ) +
      geom_text(
        aes(x = pmax(x, cumulative), label = label),
        hjust = 0,
        nudge_x = 0.01,
        size = 7
      ) +
      scale_color_manual(values = c(
        'intercept' = 'gray20',
        'prediction' = viridis::inferno(7, begin = 0.1, end = 0.9)[7],
        'positive' = 'green4',
        'negative' = 'red3'
      )) +
      scale_x_continuous(
        limits = c(0, 1.05), 
        breaks = seq(0, 1, by = 0.25),
        expand = expansion(add = c(0.01, 0.1))
      ) +
      labs(
        x = 'Feature contribution',
        y = NULL
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1, color = 'gray'),
        text = element_text(size = 20)
      )
    # ggplotly(p, tooltip = c("x", "label")) |> 
    #   config(modeBarButtonsToRemove = c(
    #     'sendDataToCloud',
    #     'zoom2d',
    #     'pan2d',
    #     'select2d',
    #     'lasso2d',
    #     'zoomIn2d',
    #     'zoomOut2d',
    #     'autoScale2d',
    #     'resetScale2d',
    #     'hoverClosestCartesian',
    #     'hoverCompareCartesian'
    #     
    #   ))
  })
  
  output$download_results <- downloadHandler(
    filename = function() 'Phase prediction.csv',
    content = function(file) {
      write_csv(results(), file)
    }
  )
  
}

shinyApp(ui, server)