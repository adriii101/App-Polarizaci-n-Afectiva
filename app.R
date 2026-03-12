library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Forest plot de odds ratios"),
  plotOutput("forest_plot", height = 520)
)

server <- function(input, output, session) {
  model_results <- reactive({
    tibble::tibble(
      Predictor = c("Edad", "Ingresos", "Ideología", "Educación", "Interés político"),
      OR = c(1.42, 0.84, 1.18, 0.72, 1.05),
      CI_Lower = c(1.14, 0.71, 0.98, 0.58, 0.88),
      CI_Upper = c(1.77, 0.99, 1.41, 0.90, 1.24),
      p = c(0.002, 0.041, 0.078, 0.006, 0.423)
    )
  })

  output$forest_plot <- renderPlot({
    forest_df <- model_results() %>%
      mutate(
        sig = p < 0.05,
        direction = if_else(OR > 1, "OR > 1", "OR < 1"),
        point_size = pmax(abs(log(OR)), 0.03) * if_else(sig, 1.4, 1)
      )

    ggplot(forest_df, aes(y = reorder(Predictor, OR), x = OR)) +
      geom_segment(
        aes(x = CI_Lower, xend = CI_Upper, yend = Predictor, alpha = sig),
        linewidth = 4,
        color = "#2C7FB8"
      ) +
      geom_point(
        aes(size = point_size, color = direction, alpha = sig)
      ) +
      geom_vline(
        xintercept = 1,
        color = "red3",
        linewidth = 0.8,
        linetype = "dashed"
      ) +
      scale_color_manual(values = c("OR > 1" = "#1B9E77", "OR < 1" = "#D7301F")) +
      scale_alpha_manual(values = c(`FALSE` = 0.45, `TRUE` = 1), guide = "none") +
      scale_size_continuous(range = c(2.5, 10), guide = "none") +
      labs(
        x = "Odds Ratio (OR)",
        y = NULL,
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui, server)
