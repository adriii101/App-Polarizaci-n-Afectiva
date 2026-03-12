library(shiny)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)
library(scales)

ui <- fluidPage(
  titlePanel("Visualizaciones CART y ROC"),
  fluidRow(
    column(6, plotOutput("cart_plot", height = 420)),
    column(6, plotOutput("roc_plot", height = 420))
  )
)

server <- function(input, output, session) {
  # Datos de ejemplo con variable ordinal
  train_data <- reactive({
    df <- mtcars
    df$am <- factor(df$am, labels = c("Auto", "Manual"))
    df$gear_ordinal <- ordered(df$gear, levels = c(3, 4, 5), labels = c("Bajo", "Medio", "Alto"))
    df
  })

  model_cart <- reactive({
    rpart(am ~ mpg + wt + hp + gear_ordinal, data = train_data(), method = "class")
  })

  output$cart_plot <- renderPlot({
    paleta_nodos <- c("#E8F1FA", "#A9C7E8", "#4A90D9", "#003087")

    rpart.plot(
      model_cart(),
      type = 4,
      extra = 104,
      fallen.leaves = TRUE,
      box.palette = paleta_nodos,
      branch.lty = 1,
      shadow.col = "gray85",
      nn = TRUE,
      main = "Árbol CART",
      cex.main = 1.2,
      col = "#003087",
      split.col = "#003087",
      nn.col = "#003087",
      tweak = 1.05,
      faclen = 0
    )

    # Leyenda para decodificar niveles ordinales (alternativa a lbl())
    mtext(
      "gear_ordinal: Bajo=3 | Medio=4 | Alto=5",
      side = 1,
      line = 6,
      cex = 0.9,
      col = "#003087"
    )
  })

  roc_obj <- reactive({
    probs <- as.numeric(train_data()$am == "Manual") * 0.7 + runif(nrow(train_data()), 0, 0.3)
    roc(response = train_data()$am, predictor = probs, levels = c("Auto", "Manual"), quiet = TRUE)
  })

  output$roc_plot <- renderPlot({
    roc_df <- data.frame(
      specificity = roc_obj()$specificities,
      sensitivity = roc_obj()$sensitivities
    )
    roc_df$fpr <- 1 - roc_df$specificity

    auc_val <- as.numeric(auc(roc_obj()))

    ggplot(roc_df, aes(x = fpr, y = sensitivity)) +
      geom_ribbon(
        aes(ymin = 0, ymax = sensitivity, fill = fpr),
        alpha = 0.15,
        color = NA
      ) +
      annotate(
        "segment",
        x = 0, xend = 1,
        y = 0, yend = 1,
        color = alpha("#003087", 0.25),
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_line(color = "#003087", linewidth = 3) +
      annotate(
        "text",
        x = 0.05,
        y = 0.95,
        label = sprintf("AUC = %.3f", auc_val),
        hjust = 0,
        vjust = 1,
        color = "#003087",
        fontface = "bold",
        size = 5
      ) +
      scale_fill_gradient(low = "#003087", high = "#4A90D9", guide = "none") +
      coord_equal() +
      labs(
        title = "Curva ROC",
        x = "1 - Especificidad (FPR)",
        y = "Sensibilidad (TPR)"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
