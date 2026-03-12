# =========================
# Constantes
# =========================

LABELS <- list(
  # Completar con todas las claves/etiquetas definidas en el prompt de negocio.
  # Ejemplo de estructura:
  # genero = c("1" = "Hombre", "2" = "Mujer"),
  # voto = c("A" = "Partido A", "B" = "Partido B")
)


# =========================
# Helpers
# =========================

lbl <- function(var, val) {
  if (!is.character(var) || length(var) != 1L || is.na(var) || !nzchar(var)) {
    stop("`var` debe ser un string no vacío de longitud 1.", call. = FALSE)
  }

  value_labels <- LABELS[[var]]
  if (is.null(value_labels)) {
    warning(sprintf("Variable '%s' no definida en LABELS.", var), call. = FALSE)
    return(val)
  }

  val_chr <- as.character(val)
  label_keys <- names(value_labels)

  if (is.null(label_keys) || length(label_keys) == 0L) {
    return(val_chr)
  }

  idx <- match(val_chr, label_keys)
  out <- val_chr

  matched <- !is.na(idx)
  matched_labels <- as.character(unname(value_labels[idx[matched]]))
  replace_ok <- !is.na(matched_labels)

  if (any(replace_ok)) {
    matched_pos <- which(matched)
    out[matched_pos[replace_ok]] <- matched_labels[replace_ok]
  }

  out[is.na(val)] <- NA_character_
  out
}


# =========================
# Modelos
# =========================
