library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
    ),
    tags$style(HTML("\n      :root {\n        --unir-blue: #003b7a;\n        --unir-light: #f4f7fb;\n      }\n      body {\n        background-color: var(--unir-light);\n        padding-bottom: 84px;\n      }\n      .kpi-card {\n        background: #fff;\n        border: 1px solid #dbe5f1;\n        border-radius: 12px;\n        box-shadow: 0 4px 14px rgba(0, 25, 62, 0.08);\n        padding: 22px 20px;\n        margin-bottom: 16px;\n      }\n      .kpi-title {\n        font-size: 14px;\n        color: #4e6179;\n        text-transform: uppercase;\n        letter-spacing: .6px;\n        margin-bottom: 6px;\n      }\n      .kpi-value {\n        font-size: 34px;\n        font-weight: 700;\n        color: var(--unir-blue);\n        line-height: 1.1;\n      }\n      .unir-footer {\n        position: fixed;\n        bottom: 0;\n        left: 0;\n        width: 100%;\n        z-index: 1030;\n        background: var(--unir-blue);\n        color: #fff;\n        font-size: 13px;\n        text-align: center;\n        padding: 12px 16px;\n        box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.15);\n      }\n    ")),
    tags$script(HTML("\n      (function () {\n        function formatValue(value, decimals) {\n          return Number(value).toLocaleString('es-ES', {\n            minimumFractionDigits: decimals,\n            maximumFractionDigits: decimals\n          });\n        }\n\n        function animateCounter(el, duration) {\n          var target = Number(el.dataset.target || '0');\n          var decimals = Number(el.dataset.decimals || '0');\n          var startTs = null;\n\n          function tick(ts) {\n            if (!startTs) startTs = ts;\n            var progress = Math.min((ts - startTs) / duration, 1);\n            var eased = 1 - Math.pow(1 - progress, 3);\n            var current = target * eased;\n            el.textContent = formatValue(current, decimals);\n\n            if (progress < 1) {\n              window.requestAnimationFrame(tick);\n            } else {\n              el.textContent = formatValue(target, decimals);\n            }\n          }\n\n          window.requestAnimationFrame(tick);\n        }\n\n        document.addEventListener('DOMContentLoaded', function () {\n          var counters = document.querySelectorAll('.js-counter');\n          counters.forEach(function (el) {\n            animateCounter(el, 1200);\n          });\n        });\n      })();\n    "))
  ),

  titlePanel("Panel de KPIs"),

  fluidRow(
    column(
      width = 4,
      div(
        class = "kpi-card animate__animated animate__fadeInUp",
        style = "animation-delay: 0.1s;",
        div(class = "kpi-title", "Mensajes analizados"),
        div(class = "kpi-value js-counter", `data-target` = "12540", "0")
      )
    ),
    column(
      width = 4,
      div(
        class = "kpi-card animate__animated animate__fadeInUp",
        style = "animation-delay: 0.2s;",
        div(class = "kpi-title", "Usuarios únicos"),
        div(class = "kpi-value js-counter", `data-target` = "3870", "0")
      )
    ),
    column(
      width = 4,
      div(
        class = "kpi-card animate__animated animate__fadeInUp",
        style = "animation-delay: 0.3s;",
        div(class = "kpi-title", "Índice de polarización"),
        div(class = "kpi-value js-counter", `data-target` = "68.4", `data-decimals` = "1", "0")
      )
    )
  ),

  tags$footer(
    class = "unir-footer",
    "Universidad Internacional de La Rioja (UNIR) · Trabajo Fin de Máster"
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
