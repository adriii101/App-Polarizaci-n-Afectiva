# Love/Hate plot helpers
#
# This utility transforms "most loved" percentages to negative values and
# keeps "most hated" percentages in positive values so the X axis pivots at 0.
# It is designed to be used from a Shiny output$plot_lovehate render function.

lovehate_long <- function(df) {
  required_cols <- c("party", "pct_mas_amado", "pct_mas_odiado")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  loved <- data.frame(
    party = df$party,
    side = "% más amado",
    value = -abs(df$pct_mas_amado),
    abs_value = abs(df$pct_mas_amado),
    stringsAsFactors = FALSE
  )

  hated <- data.frame(
    party = df$party,
    side = "% más odiado",
    value = abs(df$pct_mas_odiado),
    abs_value = abs(df$pct_mas_odiado),
    stringsAsFactors = FALSE
  )

  out <- rbind(loved, hated)

  if ("PSOE" %in% out$party) {
    others <- setdiff(unique(out$party), "PSOE")
    out$party <- factor(out$party, levels = c(others, "PSOE"))
  }

  out
}

build_lovehate_plot <- function(df, PARTY_COLS) {
  long_df <- lovehate_long(df)

  suppressPackageStartupMessages({
    library(ggplot2)
    library(plotly)
  })

  p <- ggplot(
    long_df,
    aes(
      x = value,
      y = party,
      fill = party,
      text = paste0(
        "Partido: ", party,
        "<br>", side, ": ", sprintf("%.1f", abs_value), "%"
      )
    )
  ) +
    geom_col(width = 0.72, alpha = 0.95) +
    geom_vline(xintercept = 0, linewidth = 0.7, color = "#4f4f4f") +
    scale_fill_manual(values = PARTY_COLS, drop = FALSE) +
    scale_x_continuous(
      labels = function(x) abs(x),
      name = "% más amado ⟵  |  ⟶ % más odiado"
    ) +
    labs(y = NULL, fill = "Partido") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "right"
    ) +
    annotate("text", x = min(long_df$value, na.rm = TRUE), y = 1, label = "Izquierda = % más amado", hjust = 0, vjust = 1.8, size = 3.5, color = "#444444") +
    annotate("text", x = max(long_df$value, na.rm = TRUE), y = 1, label = "Derecha = % más odiado", hjust = 1, vjust = 1.8, size = 3.5, color = "#444444")

  ggplotly(p, tooltip = "text")
}

# Example use in server:
# output$plot_lovehate <- plotly::renderPlotly({
#   build_lovehate_plot(lovehate_df(), PARTY_COLS)
# })
