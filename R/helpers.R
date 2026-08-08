# ============================================================
# FUNCIONES AUXILIARES
# Polarización afectiva en España
#
# Funciones utilizadas por app.R
# ============================================================


# ------------------------------------------------------------
# safe_num
# ------------------------------------------------------------
safe_num <- function(x, default = 0) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!is.finite(v)] <- default
  v
}


# ------------------------------------------------------------
# safe_num_na
# ------------------------------------------------------------
safe_num_na <- function(x) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!is.finite(v)] <- NA_real_
  v
}


# ------------------------------------------------------------
# safe_median
# ------------------------------------------------------------
safe_median <- function(x, default = 0) {
  x <- safe_num_na(x)
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(med) || is.na(med)) med <- default
  as.numeric(med)
}


# ------------------------------------------------------------
# median_fill
# ------------------------------------------------------------
median_fill <- function(x, default = 0) {
  x <- safe_num_na(x)
  med <- safe_median(x, default)
  x[is.na(x)] <- med
  x
}


# ------------------------------------------------------------
# z_std
# ------------------------------------------------------------
z_std <- function(x) {
  x <- safe_num_na(x)
  s <- suppressWarnings(sd(x, na.rm = TRUE))
  m <- suppressWarnings(mean(x, na.rm = TRUE))
  if (!is.finite(s) || is.na(s) || s == 0) return(rep(0, length(x)))
  as.numeric((x - m) / s)
}


# ------------------------------------------------------------
# z_safe
# ------------------------------------------------------------
z_safe <- function(x) {
  x <- safe_num_na(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}


# ------------------------------------------------------------
# to_bin_extreme
# ------------------------------------------------------------
to_bin_extreme <- function(x, low_max, high_min) {
  x <- safe_num_na(x)
  ifelse(is.na(x), NA_integer_, ifelse(x <= low_max | x >= high_min, 1L, 0L))
}


# ------------------------------------------------------------
# to_bin_1
# ------------------------------------------------------------
to_bin_1 <- function(x) {
  x <- safe_num_na(x)
  ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
}


# ------------------------------------------------------------
# fmt_num
# ------------------------------------------------------------
fmt_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  x[!is.finite(x) | is.na(x)] <- 0
  formatC(round(x, digits), format = "f", digits = digits, decimal.mark = ",")
}


# ------------------------------------------------------------
# fmt_pct
# ------------------------------------------------------------
fmt_pct <- function(x, digits = 2) {
  paste0(fmt_num(x, digits), "%")
}


# ------------------------------------------------------------
# fmt_count
# ------------------------------------------------------------
fmt_count <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x) | is.na(x)] <- 0
  prettyNum(round(x), big.mark = ".", decimal.mark = ",")
}


# ------------------------------------------------------------
# p_fmt
# ------------------------------------------------------------
p_fmt <- function(p) {
  if (length(p) == 0 || is.null(p) || is.na(p) || !is.finite(p)) return("N/D")
  if (p < 0.001) return("< 0,001")
  formatC(round(as.numeric(p), 3), format = "f", digits = 3, decimal.mark = ",")
}


# ------------------------------------------------------------
# predictor_label
# ------------------------------------------------------------
predictor_label <- function(x) {
  out <- PREDICTOR_LABELS[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}


# ------------------------------------------------------------
# collapse_labels
# ------------------------------------------------------------
collapse_labels <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x) == 0) return("ninguna")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " y "))
  paste0(paste(x[-length(x)], collapse = ", "), " y ", x[length(x)])
}


# ------------------------------------------------------------
# safe_mean
# ------------------------------------------------------------
safe_mean <- function(x, default = 0) {
  v <- suppressWarnings(mean(x, na.rm = TRUE))
  if (!is.finite(v) || is.na(v)) default else as.numeric(v)
}


# ------------------------------------------------------------
# normalize_name
# ------------------------------------------------------------
normalize_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


# ------------------------------------------------------------
# find_col_flexible
# ------------------------------------------------------------
find_col_flexible <- function(x, candidates) {
  nms <- if (is.data.frame(x)) names(x) else x
  if (length(nms) == 0) return(NA_character_)
  nms_norm <- normalize_name(nms)
  cand_norm <- normalize_name(candidates)
  hit <- match(cand_norm, nms_norm, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit) == 0L) return(NA_character_)
  nms[hit[1L]]
}


# ------------------------------------------------------------
# has_variation
# ------------------------------------------------------------
has_variation <- function(x) {
  x <- x[!is.na(x)]
  length(x) > 0L && length(unique(x)) > 1L
}


# ------------------------------------------------------------
# interaction_varies
# ------------------------------------------------------------
interaction_varies <- function(term, data) {
  vars <- strsplit(term, ":", fixed = TRUE)[[1L]]
  if (!all(vars %in% names(data))) return(FALSE)
  vals <- data[[vars[1L]]]
  if (length(vars) > 1L) {
    for (v in vars[-1L]) vals <- vals * data[[v]]
  }
  has_variation(vals)
}


# ------------------------------------------------------------
# row_mean_safe
# ------------------------------------------------------------
row_mean_safe <- function(data, cols) {
  if (length(cols) == 0L) return(rep(NA_real_, nrow(data)))
  out <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  out[rowSums(!is.na(data[, cols, drop = FALSE])) == 0L] <- NA_real_
  out
}


# ------------------------------------------------------------
# safe_auc
# ------------------------------------------------------------
safe_auc <- function(model) {
  if (is.null(model)) return(NA_real_)
  tryCatch(as.numeric(pROC::auc(pROC::roc(model$y, model$fitted.values, quiet = TRUE))), error = function(e) NA_real_)
}


# ------------------------------------------------------------
# safe_AIC
# ------------------------------------------------------------
safe_AIC <- function(model) {
  if (is.null(model)) return(NA_real_)
  tryCatch(AIC(model), error = function(e) NA_real_)
}


# ------------------------------------------------------------
# safe_BIC
# ------------------------------------------------------------
safe_BIC <- function(model) {
  if (is.null(model)) return(NA_real_)
  tryCatch(BIC(model), error = function(e) NA_real_)
}


# ------------------------------------------------------------
# calc_nagelkerke
# ------------------------------------------------------------
calc_nagelkerke <- function(model) {
  tryCatch({
    if (is.null(model) || !inherits(model, "glm")) return(0)
    ll_mod  <- -model$deviance / 2
    ll_null <- -model$null.deviance / 2
    n <- length(model$y)
    if (!is.finite(n) || is.na(n) || n <= 0) return(0)
    r2_cox <- 1 - exp((2 / n) * (ll_null - ll_mod))
    r2_max <- 1 - exp((2 / n) * ll_null)
    if (!is.finite(r2_cox) || is.na(r2_cox)) return(0)
    if (!is.finite(r2_max) || is.na(r2_max) || r2_max == 0) return(0)
    res <- unname(r2_cox / r2_max)
    if (!is.finite(res) || is.na(res)) return(0)
    max(0, min(1, res))
  }, error = function(e) 0)
}


# ------------------------------------------------------------
# calc_hosmer_lemeshow
# ------------------------------------------------------------
calc_hosmer_lemeshow <- function(model, g = 10) {
  tryCatch({
    if (is.null(model)) return(list(chi_sq = 0, df = 0, p_val = 1))
    obs <- as.numeric(model$y)
    pred <- as.numeric(model$fitted.values)
    pred[!is.finite(pred) | is.na(pred)] <- NA_real_
    obs[!is.finite(obs) | is.na(obs)] <- NA_real_
    keep <- !is.na(pred) & !is.na(obs)
    pred <- pred[keep]
    obs <- obs[keep]
    if (length(pred) < 2 || length(unique(pred)) < 2) return(list(chi_sq = 0, df = 0, p_val = 1))
    cuts <- unique(quantile(pred, probs = seq(0, 1, length.out = g + 1), na.rm = TRUE))
    if (length(cuts) < 3) return(list(chi_sq = 0, df = 0, p_val = 1))
    grp <- cut(pred, breaks = cuts, include.lowest = TRUE)
    obs_1 <- tapply(obs, grp, sum)
    exp_1 <- tapply(pred, grp, sum)
    obs_0 <- tapply(1 - obs, grp, sum)
    exp_0 <- tapply(1 - pred, grp, sum)
    chi_sq <- sum((obs_1 - exp_1)^2 / pmax(exp_1, 1e-8), na.rm = TRUE) +
      sum((obs_0 - exp_0)^2 / pmax(exp_0, 1e-8), na.rm = TRUE)
    df <- max(0, length(obs_1) - 2)
    p_val <- if (df > 0) 1 - pchisq(chi_sq, df) else 1
    list(chi_sq = round(chi_sq, 3), df = df, p_val = round(p_val, 4))
  }, error = function(e) list(chi_sq = 0, df = 0, p_val = 1))
}


# ------------------------------------------------------------
# calc_sens_spec
# ------------------------------------------------------------
calc_sens_spec <- function(model, threshold = 0.5) {
  tryCatch({
    if (is.null(model)) return(list(tp = 0, tn = 0, fp = 0, fn = 0, sens = 0, spec = 0, acc = 0))
    p <- fitted(model)
    y <- model$y
    w <- model$prior.weights
    if (is.null(w)) w <- rep(1, length(y))
    pred <- ifelse(p >= threshold, 1, 0)
    tp <- sum(w[pred == 1 & y == 1], na.rm = TRUE)
    tn <- sum(w[pred == 0 & y == 0], na.rm = TRUE)
    fp <- sum(w[pred == 1 & y == 0], na.rm = TRUE)
    fn <- sum(w[pred == 0 & y == 1], na.rm = TRUE)
    sens <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    spec <- ifelse((tn + fp) == 0, 0, tn / (tn + fp))
    acc  <- ifelse(sum(w, na.rm = TRUE) == 0, 0, sum(w[pred == y], na.rm = TRUE) / sum(w, na.rm = TRUE))
    list(tp = tp, tn = tn, fp = fp, fn = fn, sens = sens, spec = spec, acc = acc)
  }, error = function(e) list(tp = 0, tn = 0, fp = 0, fn = 0, sens = 0, spec = 0, acc = 0))
}


# ------------------------------------------------------------
# safe_hl_groups
# ------------------------------------------------------------
safe_hl_groups <- function(pred, obs, g = 10) {
  pred <- as.numeric(pred)
  obs  <- as.numeric(obs)
  pred[!is.finite(pred) | is.na(pred)] <- NA_real_
  obs[!is.finite(obs) | is.na(obs)] <- NA_real_
  keep <- !is.na(pred) & !is.na(obs)
  pred <- pred[keep]
  obs  <- obs[keep]
  if (length(pred) < 2 || length(unique(pred)) < 2) return(NULL)
  breaks_vec <- unique(quantile(pred, probs = seq(0, 1, length.out = g + 1), na.rm = TRUE))
  if (length(breaks_vec) < 3) return(NULL)
  grp <- cut(pred, breaks = breaks_vec, include.lowest = TRUE)
  out <- data.frame(
    Decil = seq_len(nlevels(grp)),
    Obs_pct = as.numeric(tapply(obs, grp, mean)),
    Exp_pct = as.numeric(tapply(pred, grp, mean))
  )
  out <- out[is.finite(out$Obs_pct) | is.finite(out$Exp_pct), , drop = FALSE]
  if (nrow(out) == 0) return(NULL)
  out
}


# ------------------------------------------------------------
# abrir_png_alta_calidad
# ------------------------------------------------------------
abrir_png_alta_calidad <- function(file, width = 2400, height = 1600, res = 300) {
  if (identical(PNG_DEVICE, "ragg")) {
    ragg::agg_png(filename = file, width = width, height = height, units = "px", res = res, background = "white")
  } else {
    png(filename = file, width = width, height = height, res = res, bg = "white", type = "cairo")
  }
}


# ------------------------------------------------------------
# cerrar_png
# ------------------------------------------------------------
cerrar_png <- function() {
  grDevices::dev.off()
}


# ------------------------------------------------------------
# par_png_unir
# ------------------------------------------------------------
par_png_unir <- function(mar = c(5.5, 13, 4.5, 2.5)) {
  par(
    mar = mar,
    family = "sans",
    bg = "#FBFCFE",
    fg = "#2D3748",
    col.axis = "#4A5568",
    col.lab = UNIR_NAVY,
    col.main = UNIR_NAVY,
    cex.axis = 1.08,
    cex.lab = 1.22,
    cex.main = 1.42,
    font.main = 2,
    las = 1,
    lend = "round",
    ljoin = "round"
  )
}


# ------------------------------------------------------------
# alpha_col
# ------------------------------------------------------------
alpha_col <- function(col, alpha = 1) {
  grDevices::adjustcolor(col, alpha.f = alpha)
}


# ------------------------------------------------------------
# add_export_blobs
# ------------------------------------------------------------
add_export_blobs <- function(xlim, ylim) {
  invisible(NULL)
}


# ------------------------------------------------------------
# add_export_card
# ------------------------------------------------------------
add_export_card <- function(xlim, ylim) {
  xr <- diff(xlim)
  yr <- diff(ylim)
  rect(xlim[1] + 0.012 * xr, ylim[1] - 0.01 * yr, xlim[2] + 0.012 * xr, ylim[2] - 0.01 * yr,
       col = alpha_col("#CBD5E1", 0.24), border = NA)
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = alpha_col("white", 0.97), border = NA)
}


# ------------------------------------------------------------
# draw_hbar_clay
# ------------------------------------------------------------
draw_hbar_clay <- function(x0, x1, y, col, lwd = 24) {
  xr <- diff(par("usr")[1:2])
  segments(x0, y - 0.035, x1, y - 0.035, lwd = lwd + 8, col = alpha_col("#CBD5E1", 0.46))
  segments(x0, y, x1, y, lwd = lwd, col = alpha_col(col, 0.96))
  hi_x0 <- x0 + 0.02 * xr
  hi_x1 <- max(hi_x0, x1 - 0.05 * xr)
  if (is.finite(hi_x1) && hi_x1 > hi_x0) {
    segments(hi_x0, y + 0.052, hi_x1, y + 0.052, lwd = max(2, lwd * 0.26), col = alpha_col("white", 0.40))
  }
}


# ------------------------------------------------------------
# draw_vbar_clay
# ------------------------------------------------------------
draw_vbar_clay <- function(x, y0, y1, col, lwd = 16) {
  yr <- diff(par("usr")[3:4])
  segments(x + 0.018, y0, x + 0.018, y1 - 0.01 * yr, lwd = lwd + 6, col = alpha_col("#CBD5E1", 0.40))
  segments(x, y0, x, y1, lwd = lwd, col = alpha_col(col, 0.96))
  if (is.finite(y1) && y1 > y0) {
    segments(x - 0.012, y0 + 0.04 * yr, x - 0.012, max(y0 + 0.04 * yr, y1 - 0.06 * yr), lwd = max(2, lwd * 0.24), col = alpha_col("white", 0.36))
  }
}


# ------------------------------------------------------------
# clean_export_df
# ------------------------------------------------------------
clean_export_df <- function(df, digits = 2, p_digits = 3) {
  df <- as.data.frame(df)
  if (nrow(df) == 0 || ncol(df) == 0) return(df)
  for (nm in names(df)) {
    if (is.numeric(df[[nm]]) || is.integer(df[[nm]])) {
      x <- as.numeric(df[[nm]])
      if (grepl("(^n$|^n_|casos|eventos|observaciones|menciones|decil|gl|df|predictores|variables)", nm, ignore.case = TRUE)) {
        df[[nm]] <- round(x, 0)
      } else if (grepl("(^p$|p_|_p$|pvalor|p_val|p.valor|p-valor|p_modelo|hl_p)", nm, ignore.case = TRUE)) {
        df[[nm]] <- round(x, p_digits)
      } else {
        df[[nm]] <- round(x, digits)
      }
    }
  }
  df
}


# ------------------------------------------------------------
# write_clean_csv
# ------------------------------------------------------------
write_clean_csv <- function(df, file, digits = 2, p_digits = 3) {
  utils::write.csv(
    clean_export_df(df, digits = digits, p_digits = p_digits),
    file,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


# ------------------------------------------------------------
# abrir_png_exportacion
# ------------------------------------------------------------
abrir_png_exportacion <- function(file, width = 4200, height = 4200, res = 520) {
  abrir_png_alta_calidad(file, width = width, height = height, res = res)
}


# ------------------------------------------------------------
# axis_num_unir
# ------------------------------------------------------------
axis_num_unir <- function(side = 1, at, digits = 2) {
  axis(side, at = at, labels = fmt_num(at, digits), col = "#CBD5E0", col.axis = "#4A5568", lwd = 1, lwd.ticks = 1)
}


# ------------------------------------------------------------
# plot_empty_export
# ------------------------------------------------------------
plot_empty_export <- function(message) {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  add_export_card(c(0, 1), c(0, 1))
  add_export_blobs(c(0, 1), c(0, 1))
  text(0.5, 0.57, message, col = "#4A5568", cex = 1.18, font = 2)
  text(0.5, 0.48, "No hay información suficiente para exportar esta figura.", col = "#718096", cex = 0.94)
}


# ------------------------------------------------------------
# read_csv_flexible
# ------------------------------------------------------------
read_csv_flexible <- function(path, guess_max = 5000L) {
  head_lines <- tryCatch(
    readLines(path, n = 50, warn = FALSE, encoding = "UTF-8"),
    error = function(e) readLines(path, n = 50, warn = FALSE)
  )
  head_lines <- head_lines[nzchar(head_lines)]
  count_sep <- function(lines, sep) {
    stats::median(vapply(lines, function(x) {
      m <- gregexpr(sep, x, fixed = TRUE)[[1L]]
      if (identical(m, -1L)) 0L else length(m)
    }, integer(1)), na.rm = TRUE)
  }
  delims <- c(";", ",", "\t", "|")
  delim_order <- names(sort(sapply(setNames(delims, delims), function(d) count_sep(head_lines, d)), decreasing = TRUE))
  encodings <- c("UTF-8", "latin1", "Windows-1252")
  attempts <- list()
  for (enc in encodings) {
    for (delim in delim_order) {
      attempts[[length(attempts) + 1L]] <- eval(bquote(function() {
        utils::read.table(
          file = .(path),
          header = TRUE,
          sep = .(delim),
          fileEncoding = .(enc),
          stringsAsFactors = FALSE,
          quote = "\"",
          comment.char = "",
          fill = TRUE,
          na.strings = c("", "NA", "N/A", "NULL", "null")
        )
      }))
    }
  }
  best <- NULL
  best_score <- -Inf
  for (fn in attempts) {
    out <- tryCatch(fn(), error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0L) {
      score <- ncol(out)
      if (score > best_score) {
        best <- out
        best_score <- score
      }
      if (ncol(out) > 1L) return(out)
    }
  }
  if (!is.null(best) && nrow(best) > 0L) return(best)
  stop("No se pudo leer el archivo con separador/codificación detectados.")
}


# ------------------------------------------------------------
# sanitize_formula
# ------------------------------------------------------------
sanitize_formula <- function(formula, data) {
  terms_obj <- terms(formula)
  lhs <- deparse(formula[[2L]])
  term_labels <- attr(terms_obj, "term.labels")
  keep <- vapply(term_labels, function(term) {
    vars <- strsplit(term, ":", fixed = TRUE)[[1L]]
    ok_main <- all(vapply(vars, function(v) {
      v %in% names(data) && has_variation(data[[v]])
    }, logical(1)))
    if (!ok_main) return(FALSE)
    if (grepl(":", term, fixed = TRUE)) return(interaction_varies(term, data))
    TRUE
  }, logical(1))
  rhs <- term_labels[keep]
  if (length(rhs) == 0L) {
    as.formula(paste(lhs, "~ 1"))
  } else {
    as.formula(paste(lhs, "~", paste(rhs, collapse = " + ")))
  }
}


# ------------------------------------------------------------
# get_num_from_candidates
# ------------------------------------------------------------
get_num_from_candidates <- function(df, candidates, default = NA_real_) {
  nm <- find_col_flexible(df, candidates)
  if (is.na(nm) || !nm %in% names(df)) return(rep(default, nrow(df)))
  safe_num_na(df[[nm]])
}


# ------------------------------------------------------------
# get_bin_equals_1
# ------------------------------------------------------------
get_bin_equals_1 <- function(df, candidates) {
  x <- get_num_from_candidates(df, candidates, default = NA_real_)
  ifelse(is.na(x), 0L, as.integer(x == 1))
}


# ------------------------------------------------------------
# safe_complete_cases
# ------------------------------------------------------------
safe_complete_cases <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) return(rep(TRUE, nrow(data)))
  stats::complete.cases(data[, vars, drop = FALSE])
}


# ------------------------------------------------------------
# fit_glm_clean
# ------------------------------------------------------------
fit_glm_clean <- function(formula, data) {
  tryCatch({
    f_use <- sanitize_formula(formula, data)
    y_name <- all.vars(f_use)[1]
    x_names <- all.vars(f_use)[-1]
    if (!y_name %in% names(data)) return(NULL)
    keep_vars <- unique(c(y_name, x_names, "peso"))
    keep_vars <- keep_vars[keep_vars %in% names(data)]
    model_data <- data[, keep_vars, drop = FALSE]
    cc <- safe_complete_cases(model_data, c(y_name, x_names))
    model_data <- model_data[cc, , drop = FALSE]
    if (nrow(model_data) == 0) return(NULL)
    y_vals <- safe_num_na(model_data[[y_name]])
    if (all(is.na(y_vals))) return(NULL)
    model_data[[y_name]] <- ifelse(y_vals == 1, 1L, ifelse(y_vals == 0, 0L, NA_integer_))
    model_data <- model_data[!is.na(model_data[[y_name]]), , drop = FALSE]
    if (nrow(model_data) < 10) return(NULL)
    if (length(unique(model_data[[y_name]])) < 2) return(NULL)
    if (length(x_names) > 0) {
      valid_x <- x_names[sapply(x_names, function(v) {
        if (!v %in% names(model_data)) return(FALSE)
        vv <- model_data[[v]]
        vv <- vv[!is.na(vv)]
        length(unique(vv)) > 1
      })]
    } else {
      valid_x <- character(0)
    }
    f_use <- if (length(valid_x) == 0) {
      as.formula(paste(y_name, "~ 1"))
    } else {
      as.formula(paste(y_name, "~", paste(valid_x, collapse = " + ")))
    }
    w <- if ("peso" %in% names(model_data)) model_data$peso else rep(1, nrow(model_data))
    w[!is.finite(w) | is.na(w) | w <= 0] <- 1
    mod <- suppressWarnings(glm(
      formula = f_use,
      family = binomial(link = "logit"),
      data = model_data,
      weights = w,
      na.action = na.omit,
      control = glm.control(maxit = 100, epsilon = 1e-08)
    ))
    co <- tryCatch(coef(mod), error = function(e) NULL)
    if (is.null(co) || any(!is.finite(co))) return(NULL)
    mod
  }, error = function(e) NULL)
}


# ------------------------------------------------------------
# fit_models_from_formulas
# ------------------------------------------------------------
fit_models_from_formulas <- function(data, formulas) {
  list(
    m0 = fit_glm_clean(formulas$B0, data),
    m1 = fit_glm_clean(formulas$B1, data),
    m2 = fit_glm_clean(formulas$B2, data),
    m3 = fit_glm_clean(formulas$B3, data),
    m4 = fit_glm_clean(formulas$B4, data)
  )
}


# ------------------------------------------------------------
# get_best_model
# ------------------------------------------------------------
get_best_model <- function(mods) {
  candidates <- list(mods$m4, mods$m3, mods$m2, mods$m1, mods$m0)
  idx <- which(!sapply(candidates, is.null))
  if (length(idx) == 0) return(NULL)
  candidates[[idx[1]]]
}


# ------------------------------------------------------------
# get_best_model_name
# ------------------------------------------------------------
get_best_model_name <- function(mods) {
  candidates <- list(m4 = mods$m4, m3 = mods$m3, m2 = mods$m2, m1 = mods$m1, m0 = mods$m0)
  ok <- names(candidates)[!sapply(candidates, is.null)]
  if (length(ok) == 0) return(NA_character_)
  ok[1]
}


# ------------------------------------------------------------
# empty_plotly
# ------------------------------------------------------------
empty_plotly <- function(text = "No hay información suficiente para mostrar este gráfico.") {
  plotly::plot_ly() %>%
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        text = text,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "#718096")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}


# ------------------------------------------------------------
# pretty_model_name
# ------------------------------------------------------------
pretty_model_name <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("Modelo no disponible")
  val <- BLOCK_LABELS[[as.character(x)]]
  if (is.null(val) || is.na(val)) return(as.character(x))
  unname(val)
}


# ------------------------------------------------------------
# build_model_summary_html
# ------------------------------------------------------------
build_model_summary_html <- function(metricas, coef_df, model_name) {
  sig <- coef_df[is.finite(coef_df$p) & !is.na(coef_df$p) & coef_df$p < 0.05, , drop = FALSE]
  marginal <- coef_df[is.finite(coef_df$p) & !is.na(coef_df$p) & coef_df$p >= 0.05 & coef_df$p < 0.10, , drop = FALSE]
  no_sig <- coef_df[is.finite(coef_df$p) & !is.na(coef_df$p) & coef_df$p >= 0.10, , drop = FALSE]
  positivos <- sig$Variable[sig$OR > 1]
  negativos <- sig$Variable[sig$OR < 1]
  modelo_legible <- pretty_model_name(model_name)
  par1 <- paste0(
    "<p>Se estimó una regresión logística binaria en cuatro pasos sucesivos y el modelo que resume mejor el análisis es <b>", modelo_legible,
    "</b>. El contraste global frente al modelo nulo fue ",
    if (metricas$omnibus_p < 0.05) "estadísticamente significativo" else "no significativo",
    " (χ²(", fmt_num(metricas$omnibus_gl, 0), ") = ", fmt_num(metricas$omnibus_chi, 2),
    ", <i>p</i> ", p_fmt(metricas$omnibus_p), "). El ajuste alcanzado se sitúa en <b>R² de Nagelkerke = ",
    fmt_num(metricas$r2_nag, 3), "</b>, una magnitud que puede interpretarse como moderada pero relevante para un fenómeno tan complejo, contextual y relacional como la polarización afectiva.</p>"
  )
  par2 <- if (nrow(sig) > 0) {
    paste0(
      "<p>Dentro del modelo final, los resultados más sólidos se concentran en <b>",
      collapse_labels(sig$Variable), "</b>. Cuando las <i>odds ratio</i> superan 1, la probabilidad de fractura afectiva extrema tiende a aumentar; esto ocurre, sobre todo, en <b>",
      if (length(positivos) > 0) collapse_labels(positivos) else "los factores que intensifican la exposición, la polarización o el malestar político",
      "</b>. En cambio, las asociaciones con OR inferiores a 1 sugieren un efecto protector o amortiguador, especialmente en <b>",
      if (length(negativos) > 0) collapse_labels(negativos) else "aquellos factores que reducen la probabilidad observada de polarización extrema",
      "</b>.</p>"
    )
  } else {
    "<p>El modelo no concentra toda su capacidad explicativa en un único predictor dominante, sino en la combinación de varios factores que actúan de forma simultánea. Ese patrón es coherente con la idea de que la polarización afectiva extrema responde a una acumulación de señales políticas, relacionales y emocionales.</p>"
  }
  par3 <- if (nrow(no_sig) > 0 || nrow(marginal) > 0) {
    "<p>Al mismo tiempo, no todos los componentes del cuestionario presentan un peso equivalente una vez se controlan conjuntamente. Esto obliga a interpretar el modelo de forma matizada: algunas variables conservan capacidad explicativa propia, mientras que otras quedan absorbidas por factores más centrales o comparten parte de la misma señal. En conjunto, el modelo ofrece una lectura amplia del fenómeno sin reducirlo a una sola dimensión.</p>"
  } else {
    "<p>En conjunto, la ecuación final ofrece una lectura amplia y coherente del fenómeno, integrando dimensiones de identidad política, exposición, entorno cercano y clima afectivo sin perder interpretabilidad.</p>"
  }
  paste0(par1, par2, par3)
}


# ------------------------------------------------------------
# build_leaders_conclusion
# ------------------------------------------------------------
build_leaders_conclusion <- function(info) {
  tabla <- info$tabla
  if (nrow(tabla) == 0) return("No se dispone de información suficiente para interpretar el bloque de líderes.")
  top_mencion <- tabla$Lider[1]
  top_mencion_pct <- tabla$Porcentaje[1]
  top_prob_idx <- which.max(tabla$Probabilidad)
  top_prob_name <- tabla$Lider[top_prob_idx]
  top_prob <- tabla$Probabilidad[top_prob_idx]
  dif <- tabla$Diferencia[top_prob_idx]
  paste0(
    "La prevalencia media de fractura afectiva extrema en la muestra es del ", fmt_pct(info$base_prev * 100, 2),
    ". En el plano descriptivo,", top_mencion,  " concentra la mayor proporción de menciones (",
    fmt_pct(top_mencion_pct, 2), "), lo que lo sitúa como el liderazgo más presente en el imaginario polarizante de la muestra. ",
    "Sin embargo, la mayor intensidad observada de polarización extrema aparece entre quienes mencionan a ", top_prob_name,
    ", con una probabilidad estimada de ", fmt_pct(top_prob * 100, 2),
    ", es decir, ", ifelse(dif >= 0, "+", "-"), fmt_pct(abs(dif) * 100, 2),
    " puntos respecto a la media general. ",
    "Esta diferencia no debe leerse en clave causal, pero sí como un indicador útil del tipo de liderazgos que se asocian a un clima afectivo más tensionado. ",
    "En términos interpretativos, el bloque de líderes sugiere que la polarización no se distribuye de forma homogénea: algunos nombres concentran más visibilidad, mientras que otros parecen activar con mayor intensidad una lógica de distancia afectiva y conflicto simbólico."
  )
}


# ------------------------------------------------------------
# prepare_atlas_data
# ------------------------------------------------------------
prepare_atlas_data <- function(df) {
  df <- as.data.frame(df)
  for (i in seq_along(df)) {
    if (inherits(df[[i]], "haven_labelled") || inherits(df[[i]], "labelled")) {
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  names(df) <- normalize_name(names(df))
  names(df) <- gsub("^x(?=[0-9])", "", names(df), perl = TRUE)
  n <- nrow(df)
  peso_col <- find_col_flexible(df, c("peso", "weight", "ponderacion", "factor_ponderacion"))
  if (is.na(peso_col)) df$peso <- 1 else df$peso <- safe_num(df[[peso_col]], 1)
  df$peso[df$peso <= 0 | !is.finite(df$peso) | is.na(df$peso)] <- 1
  edad_raw <- get_num_from_candidates(df, c("edad"))
  if (all(is.na(edad_raw))) edad_raw <- rep(45, n)
  df$edad <- median_fill(edad_raw, 45)
  df$edad_z <- z_std(df$edad)
  uso_rrss_raw <- get_num_from_candidates(df, c("uso_rrss", "rrss", "uso_redes_sociales"))
  df$uso_rrss_bin <- ifelse(uso_rrss_raw == 1, 1L, ifelse(uso_rrss_raw == 2, 0L, 0L))
  union_raw <- get_num_from_candidates(df, c("union_posible", "union", "union_espana"))
  df$union_pesimismo <- ifelse(union_raw == 1, 1L, ifelse(union_raw == 2, 0L, 0L))
  df$lazo_regional_bin <- get_bin_equals_1(df, c("lazo_regional", "amistad_regional", "amistades_regionalistas"))
  df$lazo_podemos_bin  <- get_bin_equals_1(df, c("lazo_podemos", "amistad_podemos", "amistades_podemos"))
  df$lazo_vox_bin      <- get_bin_equals_1(df, c("lazo_vox", "amistad_vox", "amistades_vox"))
  df$lazo_pp_bin       <- get_bin_equals_1(df, c("lazo_pp", "amistad_pp", "amistades_pp"))
  df$lazo_psoe_bin     <- get_bin_equals_1(df, c("lazo_psoe", "amistad_psoe", "amistades_psoe"))
  df$lazo_sumar_bin    <- get_bin_equals_1(df, c("lazo_sumar", "amistad_sumar", "amistades_sumar"))
  df$exp_cambiado_idea_bin <- get_bin_equals_1(df, c("exp_cambiado_idea", "cambio_idea", "ha_cambiado_idea"))
  ideol_raw <- get_num_from_candidates(df, c("ideologia", "ideologia_ubicacion", "autoubicacion_ideologica"))
  ideol_raw[!(ideol_raw %in% 0:10)] <- NA_real_
  ideol_raw <- median_fill(ideol_raw, 5)
  df$ideologia_ext <- abs(ideol_raw - 5)
  df$ideologia_ext_z <- z_std(df$ideologia_ext)
  df$ideologia_extreme_bin <- to_bin_extreme(ideol_raw, 2, 8)
  serv_raw <- get_num_from_candidates(df, c("servicios_publicos", "servicios_publicos_privatizacion", "privatizacion"))
  serv_raw[!(serv_raw %in% 1:6)] <- NA_real_
  serv_raw <- median_fill(serv_raw, 3.5)
  df$servicios_publicos_z <- z_std(serv_raw)
  l1 <- get_num_from_candidates(df, c("lider_1", "lider1", "lider_principal_1"), default = 0)
  l2 <- get_num_from_candidates(df, c("lider_2", "lider2", "lider_principal_2"), default = 0)
  l1[is.na(l1)] <- 0
  l2[is.na(l2)] <- 0
  df$lider_1 <- l1
  df$lider_2 <- l2
  top_leaders <- c(1, 2, 3, 5, 6, 8)
  for (id in top_leaders) {
    nm <- paste0("lider_", id, "_bin")
    df[[nm]] <- as.integer(df$lider_1 == id | df$lider_2 == id)
  }
  aff_cols <- names(df)[grepl("^p_afectiva_", names(df))]
  if (length(aff_cols) == 0L) aff_cols <- names(df)[grepl("^afectiva_", names(df))]
  for (nm in aff_cols) {
    x <- safe_num_na(df[[nm]])
    x[x == 8] <- NA_real_
    df[[nm]] <- x
  }
  col_madrileno_neg <- find_col_flexible(df, c("p_afectiva_madrileno_neg", "p_afectiva_madrileno", "afectiva_madrileno", "sentimiento_madrileno"))
  if (!is.na(col_madrileno_neg)) {
    df$p_afectiva_madrileno_neg <- median_fill(df[[col_madrileno_neg]], 4)
    df$p_afectiva_madrileno_neg_z <- z_safe(df[[col_madrileno_neg]])
  } else {
    df$p_afectiva_madrileno_neg <- rep(4, n)
    df$p_afectiva_madrileno_neg_z <- rep(0, n)
  }
  if (length(aff_cols) > 0L) {
    aff_mat <- as.matrix(df[, aff_cols, drop = FALSE])
    affect_gap <- apply(aff_mat, 1, function(x) {
      x <- x[!is.na(x)]
      if (length(x) < 2L) return(NA_real_)
      max(x) - min(x)
    })
    df$affect_gap_z  <- z_safe(affect_gap)
    df$affect_gap_z2 <- df$affect_gap_z^2
  } else {
    df$affect_gap_z  <- rep(0, n)
    df$affect_gap_z2 <- rep(0, n)
  }
  left_aff_cols <- unique(na.omit(c(
    find_col_flexible(df, c("p_afectiva_psoe", "afectiva_psoe")),
    find_col_flexible(df, c("p_afectiva_podemos", "afectiva_podemos")),
    find_col_flexible(df, c("p_afectiva_sumar", "afectiva_sumar")),
    find_col_flexible(df, c("p_afectiva_iu", "p_afectiva_izquierda_unida", "afectiva_iu", "afectiva_izquierda_unida")),
    find_col_flexible(df, c("p_afectiva_mas_pais", "p_afectiva_mas_madrid", "afectiva_mas_pais", "afectiva_mas_madrid"))
  )))
  right_aff_cols <- unique(na.omit(c(
    find_col_flexible(df, c("p_afectiva_pp", "afectiva_pp")),
    find_col_flexible(df, c("p_afectiva_vox", "afectiva_vox")),
    find_col_flexible(df, c("p_afectiva_ciudadanos", "p_afectiva_cs", "afectiva_ciudadanos", "afectiva_cs"))
  )))
  left_mean  <- row_mean_safe(df, left_aff_cols)
  right_mean <- row_mean_safe(df, right_aff_cols)
  df$iai_abs_z <- z_safe(abs(left_mean - right_mean))
  percep_div_raw <- get_num_from_candidates(df, c("percepcion_division_z", "percepcion_division", "division_social", "percepcion_polarizacion", "percepcion_division_social"))
  df$percepcion_division_z <- if (all(is.na(percep_div_raw))) rep(0, n) else z_std(median_fill(percep_div_raw, safe_median(percep_div_raw, 0)))
  interes_pol_raw <- get_num_from_candidates(df, c("interes_pol_z", "interes_pol", "interes_politica", "interes_politico"))
  if (!all(is.na(interes_pol_raw))) interes_pol_raw <- ifelse(interes_pol_raw %in% 1:4, 5 - interes_pol_raw, interes_pol_raw)
  df$interes_pol_z <- if (all(is.na(interes_pol_raw))) rep(0, n) else z_std(median_fill(interes_pol_raw, safe_median(interes_pol_raw, 0)))
  frec_noticias_raw <- get_num_from_candidates(df, c("frec_noticias_z", "frec_noticias", "frecuencia_noticias", "seguimiento_noticias", "noticias_politicas"))
  if (!all(is.na(frec_noticias_raw))) frec_noticias_raw <- ifelse(frec_noticias_raw %in% 1:5, 6 - frec_noticias_raw, frec_noticias_raw)
  df$frec_noticias_z <- if (all(is.na(frec_noticias_raw))) rep(0, n) else z_std(median_fill(frec_noticias_raw, safe_median(frec_noticias_raw, 0)))
  lider_bin_raw <- get_num_from_candidates(df, c("lider_bin", "atrib_lideres", "atribuye_lideres", "polarizacion_lideres"))
  if (all(is.na(lider_bin_raw))) {
    df$lider_bin <- as.integer((df$lider_1 > 0) | (df$lider_2 > 0))
  } else {
    df$lider_bin <- ifelse(lider_bin_raw == 1, 1L, ifelse(lider_bin_raw == 2, 0L, as.integer(lider_bin_raw > 0)))
  }
  prob_votar_raw <- get_num_from_candidates(df, c("probabilidad_votar", "prob_votar", "prob_voto"))
  df$prob_votar_ext <- to_bin_extreme(prob_votar_raw, 3, 8)
  simpatia_raw <- get_num_from_candidates(df, c("simpatia", "simpatia_politica"))
  df$simpatia_ext <- to_bin_extreme(simpatia_raw, 3, 7)
  df$responsable_monarquia_bin  <- to_bin_1(get_num_from_candidates(df, c("responsable_monarquia", "responsable_casa_real")))
  df$responsable_ong_bin        <- to_bin_1(get_num_from_candidates(df, c("responsable_ong", "responsable_ongs")))
  df$responsable_ciudadanos_bin <- to_bin_1(get_num_from_candidates(df, c("responsable_ciudadanos", "responsable_ciudadania")))
  df$responsable_jueces_bin     <- to_bin_1(get_num_from_candidates(df, c("responsable_jueces")))
  igualdad_raw <- get_num_from_candidates(df, c("igualdad_genero", "igualdad_genero_z"))
  clima_raw    <- get_num_from_candidates(df, c("cambio_climatico", "cambio_climatico_z"))
  inmig_raw    <- get_num_from_candidates(df, c("immigracion", "inmigracion", "immigracion_z", "inmigracion_z"))
  territ_raw   <- get_num_from_candidates(df, c("modelo_territorial", "modelo_territorial_z"))
  df$igualdad_genero_z    <- if (all(is.na(igualdad_raw))) rep(0, n) else z_safe(median_fill(igualdad_raw, safe_median(igualdad_raw, 0)))
  df$cambio_climatico_z   <- if (all(is.na(clima_raw))) rep(0, n) else z_safe(median_fill(clima_raw, safe_median(clima_raw, 0)))
  df$immigracion_z        <- if (all(is.na(inmig_raw))) rep(0, n) else z_safe(median_fill(inmig_raw, safe_median(inmig_raw, 0)))
  df$modelo_territorial_z <- if (all(is.na(territ_raw))) rep(0, n) else z_safe(median_fill(territ_raw, safe_median(territ_raw, 0)))
  df$ideol_ext_z <- df$ideologia_ext_z
  df$pesimismo_union <- df$union_pesimismo
  df$uso_rrss <- df$uso_rrss_bin
  conflict_candidates <- list(
    exp_evita_hablar  = c("exp_evita_hablar", "evita_hablar", "conflicto_evita_hablar"),
    exp_discusion     = c("exp_discusion", "discusion", "conflicto_discusion"),
    exp_roto_relacion = c("exp_roto_relacion", "roto_relacion", "ha_roto_relacion"),
    exp_whatsapp      = c("exp_whatsapp", "conflicto_whatsapp", "whatsapp"),
    exp_navidad       = c("exp_navidad", "conflicto_navidad", "navidad"),
    exp_criticado     = c("exp_criticado", "criticado", "ha_sido_criticado")
  )
  conflict_vars <- lapply(conflict_candidates, function(cands) {
    x <- get_num_from_candidates(df, cands, default = NA_real_)
    ifelse(is.na(x), NA_integer_, as.integer(x == 1))
  })
  conflict_df <- as.data.frame(conflict_vars)
  available_counts <- rowSums(!is.na(conflict_df))
  if (ncol(conflict_df) == 0 || all(available_counts == 0)) {
    df$n_conflictos_politicos <- NA_real_
    df$Y_BIN <- NA_integer_
  } else {
    conflict_df_filled <- conflict_df
    conflict_df_filled[is.na(conflict_df_filled)] <- 0L
    df$n_conflictos_politicos <- rowSums(conflict_df_filled, na.rm = TRUE)
    threshold_row <- ifelse(available_counts >= 6, 4, pmax(1, ceiling(available_counts * 0.6)))
    df$Y_BIN <- ifelse(available_counts == 0, NA_integer_, as.integer(df$n_conflictos_politicos >= threshold_row))
  }
  num_vars <- c(
    "peso", "edad", "edad_z", "uso_rrss_bin", "union_pesimismo",
    "lazo_regional_bin", "lazo_podemos_bin", "lazo_vox_bin",
    "lazo_pp_bin", "lazo_psoe_bin", "lazo_sumar_bin",
    "exp_cambiado_idea_bin", "ideologia_ext", "ideologia_ext_z", "ideologia_extreme_bin",
    "servicios_publicos_z", "p_afectiva_madrileno_neg", "p_afectiva_madrileno_neg_z",
    "iai_abs_z", "affect_gap_z", "affect_gap_z2", "percepcion_division_z",
    "interes_pol_z", "frec_noticias_z", "lider_bin", "ideol_ext_z",
    "pesimismo_union", "uso_rrss", "n_conflictos_politicos",
    "lider_1", "lider_2", "lider_1_bin", "lider_2_bin", "lider_3_bin",
    "lider_5_bin", "lider_6_bin", "lider_8_bin",
    "prob_votar_ext", "simpatia_ext",
    "responsable_monarquia_bin", "responsable_ong_bin",
    "responsable_ciudadanos_bin", "responsable_jueces_bin",
    "igualdad_genero_z", "cambio_climatico_z", "immigracion_z", "modelo_territorial_z"
  )
  for (v in num_vars) {
    if (!v %in% names(df)) df[[v]] <- 0
    df[[v]] <- safe_num(df[[v]], 0)
  }
  if (!"Y_BIN" %in% names(df)) df$Y_BIN <- NA_integer_
  df$Y_BIN <- safe_num_na(df$Y_BIN)
  df$Y_BIN <- ifelse(df$Y_BIN == 1, 1L, ifelse(df$Y_BIN == 0, 0L, NA_integer_))
  df$.__diag_y_valid__ <- sum(!is.na(df$Y_BIN))
  df$.__diag_y_ones__ <- sum(df$Y_BIN == 1, na.rm = TRUE)
  df$.__diag_y_zeros__ <- sum(df$Y_BIN == 0, na.rm = TRUE)
  df
}


# ------------------------------------------------------------
# create_demo_atlas_data
# ------------------------------------------------------------
create_demo_atlas_data <- function(n = NULL, seed = NULL) {
  # Demo sintética: no pretende reproducir el Atlas. Cada ejecución cambia tamaño,
  # distribución interna y rendimiento del modelo para que la app se vea como una demo real.
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  set.seed(seed)
  if (is.null(n)) n <- sample(c(1379L, 1644L, 1917L, 2236L, 2873L, 3198L, 3641L), 1L)
  
  inv_logit <- function(x) 1 / (1 + exp(-x))
  clamp_int <- function(x, min_val, max_val) {
    as.integer(pmin(max_val, pmax(min_val, round(x))))
  }
  draw_bin_code <- function(prob_yes) {
    ifelse(runif(n) < pmin(0.98, pmax(0.01, prob_yes)), 1L, 2L)
  }
  draw_scale <- function(mu, min_val = 1, max_val = 6, sd_val = 1.35) {
    clamp_int(rnorm(n, mu, sd_val), min_val, max_val)
  }
  
  # Distribuciones deliberadamente extrañas: mezcla de perfiles, extremos ideológicos
  # sobrerrepresentados y patrones de respuesta menos parecidos a una encuesta real.
  perfil <- sample(c("ruido", "hiperpolitizado", "desenganchado", "territorial", "climatico"), n, replace = TRUE,
                   prob = c(0.25, 0.28, 0.17, 0.16, 0.14))
  edad <- clamp_int(ifelse(perfil == "desenganchado", rnorm(n, 29, 8),
                           ifelse(perfil == "hiperpolitizado", rnorm(n, 54, 13),
                                  ifelse(perfil == "territorial", rnorm(n, 46, 12), rnorm(n, 41, 17)))), 18, 89)
  
  ideologia <- integer(n)
  ideologia[perfil == "ruido"] <- sample(0:10, sum(perfil == "ruido"), replace = TRUE)
  ideologia[perfil == "hiperpolitizado"] <- sample(c(0,1,2,8,9,10), sum(perfil == "hiperpolitizado"), replace = TRUE,
                                                   prob = c(0.12,0.16,0.17,0.18,0.18,0.19))
  ideologia[perfil == "desenganchado"] <- sample(3:7, sum(perfil == "desenganchado"), replace = TRUE,
                                                 prob = c(0.10,0.18,0.44,0.18,0.10))
  ideologia[perfil == "territorial"] <- sample(c(1,2,3,6,7,8), sum(perfil == "territorial"), replace = TRUE,
                                               prob = c(0.18,0.17,0.14,0.14,0.18,0.19))
  ideologia[perfil == "climatico"] <- sample(c(0,1,2,3,7,8,9,10), sum(perfil == "climatico"), replace = TRUE,
                                             prob = c(0.18,0.19,0.14,0.08,0.08,0.11,0.12,0.10))
  ideol_ext <- abs(ideologia - 5)
  interes_lat <- as.numeric(scale(0.55 * ideol_ext + 0.75 * (perfil == "hiperpolitizado") - 0.70 * (perfil == "desenganchado") + rnorm(n, 0, 1.1)))
  
  interes_politica <- clamp_int(3.4 - 1.05 * interes_lat + rnorm(n, 0, 0.45), 1, 4)
  frecuencia_noticias <- clamp_int(3.6 - 0.95 * interes_lat + 0.45 * (perfil == "ruido") + rnorm(n, 0, 0.70), 1, 5)
  
  uso_rrss_yes <- rbinom(n, 1, inv_logit(-0.05 + 0.75 * interes_lat + 0.22 * ideol_ext - 0.015 * (edad - 44) + 0.55 * (perfil == "ruido")))
  union_pess_yes <- rbinom(n, 1, inv_logit(-0.10 + 0.35 * ideol_ext + 0.42 * uso_rrss_yes + 0.35 * (perfil %in% c("territorial", "hiperpolitizado")) + rnorm(n, 0, 0.35)))
  
  lazo_pp_yes <- rbinom(n, 1, inv_logit(0.05 + 0.22 * (ideologia - 5) + 0.18 * (perfil == "hiperpolitizado")))
  lazo_psoe_yes <- rbinom(n, 1, inv_logit(0.05 - 0.08 * (ideologia - 5) + 0.15 * (perfil == "ruido")))
  lazo_sumar_yes <- rbinom(n, 1, inv_logit(-0.05 - 0.34 * (ideologia - 5) + 0.32 * (perfil == "climatico")))
  lazo_podemos_yes <- rbinom(n, 1, inv_logit(-0.30 - 0.32 * (ideologia - 5) + 0.20 * interes_lat + 0.20 * (perfil == "climatico")))
  lazo_vox_yes <- rbinom(n, 1, inv_logit(-0.42 + 0.42 * (ideologia - 5) + 0.25 * interes_lat + 0.20 * (perfil == "hiperpolitizado")))
  lazo_regional_yes <- rbinom(n, 1, inv_logit(-0.55 + 0.38 * (perfil == "territorial") + 0.18 * ideol_ext + 0.25 * interes_lat))
  
  servicios_publicos <- draw_scale(3.6 + 0.28 * (ideologia - 5) - 0.40 * (perfil == "climatico"), 1, 6, 1.45)
  igualdad_genero <- draw_scale(3.4 + 0.38 * (ideologia - 5) + 0.25 * (perfil == "ruido"), 1, 6, 1.40)
  cambio_climatico <- draw_scale(3.3 + 0.55 * (ideologia - 5) - 0.85 * (perfil == "climatico"), 1, 6, 1.55)
  immigracion <- draw_scale(3.4 + 0.50 * (ideologia - 5) + 0.30 * (perfil == "territorial"), 1, 6, 1.55)
  modelo_territorial <- draw_scale(3.7 + 0.42 * (ideologia - 5) + 0.65 * (perfil == "territorial"), 1, 6, 1.65)
  percepcion_division <- clamp_int(rnorm(n, 3.6 + 0.30 * union_pess_yes + 0.22 * ideol_ext + 0.40 * (perfil == "ruido"), 1.05), 1, 5)
  
  # Variable casi separadora en la demo: genera diagnósticos más irregulares.
  latent_rare <- inv_logit(-3.05 + 0.70 * uso_rrss_yes + 0.55 * union_pess_yes + 0.32 * ideol_ext +
                             0.85 * (perfil == "hiperpolitizado") + 0.65 * (perfil == "territorial") + rnorm(n, 0, 0.55))
  event_latent <- rbinom(n, 1, latent_rare)
  exp_cambiado_idea_yes <- ifelse(event_latent == 1,
                                  rbinom(n, 1, 0.93),
                                  rbinom(n, 1, inv_logit(-2.35 + 0.20 * interes_lat)))
  
  probabilidad_votar <- clamp_int(rnorm(n, 6.9 + 0.85 * interes_lat + 0.18 * ideol_ext - 0.55 * (perfil == "desenganchado"), 2.35), 0, 10)
  simpatia <- sample(c(1:14, 16:18), n, replace = TRUE, prob = c(0.07,0.07,0.06,0.08,0.08,0.03,0.06,0.04,0.04,0.03,0.02,0.02,0.06,0.03,0.12,0.14,0.12))
  
  # Ranking inventado: Pedro primero, Yolanda segunda y Abascal tercero.
  leader_probs <- c(`1` = 0.255, `5` = 0.215, `3` = 0.170, `6` = 0.125, `8` = 0.095, `2` = 0.080, `13` = 0.060)
  lider_1 <- as.integer(sample(names(leader_probs), n, replace = TRUE, prob = leader_probs))
  second_probs <- c(`5` = 0.245, `3` = 0.160, `6` = 0.150, `1` = 0.145, `8` = 0.120, `2` = 0.100, `13` = 0.080)
  lider_2 <- as.integer(sample(names(second_probs), n, replace = TRUE, prob = second_probs))
  lider_2[lider_2 == lider_1] <- 0L
  
  aff_noise <- function(sd = 1.35) rnorm(n, 0, sd)
  p_afectiva_psoe <- clamp_int(4.1 + 0.36 * (ideologia - 5) + 0.30 * (perfil == "ruido") + aff_noise(), 1, 7)
  p_afectiva_pp <- clamp_int(4.0 - 0.30 * (ideologia - 5) + 0.20 * (perfil == "ruido") + aff_noise(), 1, 7)
  p_afectiva_vox <- clamp_int(4.5 - 0.46 * (ideologia - 5) + 0.25 * (perfil == "hiperpolitizado") + aff_noise(), 1, 7)
  p_afectiva_sumar <- clamp_int(4.2 + 0.36 * (ideologia - 5) - 0.30 * (perfil == "climatico") + aff_noise(), 1, 7)
  p_afectiva_podemos <- clamp_int(4.4 + 0.39 * (ideologia - 5) - 0.25 * (perfil == "climatico") + aff_noise(), 1, 7)
  p_afectiva_madrileno <- clamp_int(3.4 + 0.20 * ideol_ext + 0.40 * (perfil == "territorial") + 0.20 * union_pess_yes + aff_noise(1.55), 1, 7)
  
  responsable_monarquia <- draw_bin_code(0.34 + 0.16 * (ideologia <= 3) + 0.06 * (perfil == "ruido"))
  responsable_ong <- draw_bin_code(0.26 + 0.11 * (ideologia >= 7) + 0.08 * (perfil == "ruido"))
  responsable_ciudadanos <- draw_bin_code(0.40 + 0.15 * union_pess_yes + 0.07 * (perfil == "desenganchado"))
  responsable_jueces <- draw_bin_code(0.31 + 0.10 * ideol_ext + 0.06 * (perfil == "territorial"))
  
  risk <- as.numeric(scale(
    0.95 * uso_rrss_yes +
      1.15 * exp_cambiado_idea_yes +
      0.55 * union_pess_yes +
      0.30 * ideol_ext +
      0.42 * lazo_regional_yes +
      0.32 * lazo_podemos_yes +
      0.34 * lazo_vox_yes +
      0.30 * (p_afectiva_madrileno - 4) -
      0.020 * (edad - 45) +
      0.50 * (perfil == "ruido") +
      rnorm(n, 0, 0.95)
  ))
  event_final <- rbinom(n, 1, inv_logit(-2.85 + 0.95 * risk))
  event_final <- ifelse(event_latent == 1 & runif(n) < 0.72, 1L, event_final)
  conflict_prob <- function(base, boost) pmin(0.94, pmax(0.01, base + boost * event_final + 0.08 * pmax(-1.5, pmin(1.5, risk))))
  
  data.frame(
    edad = edad,
    ideologia = ideologia,
    uso_rrss = ifelse(uso_rrss_yes == 1, 1L, 2L),
    union_posible = ifelse(union_pess_yes == 1, 1L, 2L),
    lazo_regional = ifelse(lazo_regional_yes == 1, 1L, 2L),
    lazo_podemos = ifelse(lazo_podemos_yes == 1, 1L, 2L),
    lazo_vox = ifelse(lazo_vox_yes == 1, 1L, 2L),
    lazo_pp = ifelse(lazo_pp_yes == 1, 1L, 2L),
    lazo_psoe = ifelse(lazo_psoe_yes == 1, 1L, 2L),
    lazo_sumar = ifelse(lazo_sumar_yes == 1, 1L, 2L),
    servicios_publicos = servicios_publicos,
    exp_cambiado_idea = ifelse(exp_cambiado_idea_yes == 1, 1L, 2L),
    interes_politica = interes_politica,
    frecuencia_noticias = frecuencia_noticias,
    percepcion_division = percepcion_division,
    probabilidad_votar = probabilidad_votar,
    simpatia = simpatia,
    responsable_monarquia = responsable_monarquia,
    responsable_ong = responsable_ong,
    responsable_ciudadanos = responsable_ciudadanos,
    responsable_jueces = responsable_jueces,
    igualdad_genero = igualdad_genero,
    cambio_climatico = cambio_climatico,
    immigracion = immigracion,
    modelo_territorial = modelo_territorial,
    p_afectiva_psoe = p_afectiva_psoe,
    p_afectiva_pp = p_afectiva_pp,
    p_afectiva_vox = p_afectiva_vox,
    p_afectiva_sumar = p_afectiva_sumar,
    p_afectiva_podemos = p_afectiva_podemos,
    p_afectiva_madrileno = p_afectiva_madrileno,
    lider_1 = lider_1,
    lider_2 = lider_2,
    exp_evita_hablar = ifelse(runif(n) < conflict_prob(0.09, 0.40), 1L, 2L),
    exp_discusion = ifelse(runif(n) < conflict_prob(0.06, 0.46), 1L, 2L),
    exp_roto_relacion = ifelse(runif(n) < conflict_prob(0.015, 0.36), 1L, 2L),
    exp_whatsapp = ifelse(runif(n) < conflict_prob(0.02, 0.34), 1L, 2L),
    exp_navidad = ifelse(runif(n) < conflict_prob(0.018, 0.32), 1L, 2L),
    exp_criticado = ifelse(runif(n) < conflict_prob(0.04, 0.42), 1L, 2L),
    peso = pmax(0.20, pmin(3.80, rlnorm(n, meanlog = 0, sdlog = 0.40))),
    demo_perfil_sintetico = perfil,
    demo_seed = seed
  )
}

