#' Flags de missing por variable con relevancia específica por variable
#'
#' Si una regla del `spec` NO tiene condiciones (sin `when_vars` o vacías),
#' se considera "obligatoria para todos" y el flag se define únicamente como
#' `is.na(var)` (1 si está missing, 0 si no), sin aplicar relevancia.
#'
#' @param data data.frame o tibble con los datos.
#' @param spec tibble/data.frame con al menos la columna `var`. Opcionalmente:
#'   - `when_vars` (list-col chr) y `when_values` (list-col) para condiciones.
#'   - `how` ("all"/"any") o `manual_expr` (expresión AND/OR/NOT).
#' @param prefix Prefijo para los flags. Default "m".
#' @param na_when_not_relevant Si TRUE, pone NA cuando no es relevante (cuando SÍ hay condiciones).
#'   Ignorado si no hay condiciones (relevancia incondicional).
#' @param numeric_conds Si TRUE, `when_vars` se comparan como numéricas y `when_values` deben ser numéricas.
#' @param coerce_target Si TRUE, fuerza a numérica la variable objetivo antes de `is.na()`.
#'
#' @return `data` con columnas nuevas `<prefix>_<var>`.
#' @export
flags_missing_por_variable <- function(data,
                                       spec,
                                       prefix = "m",
                                       na_when_not_relevant = FALSE,
                                       numeric_conds = TRUE,
                                       coerce_target = FALSE) {
  stopifnot(is.data.frame(data))
  if (!"var" %in% names(spec)) {
    stop("`spec` debe contener al menos la columna `var`.")
  }

  # Si faltan columnas, créalas como list-cols vacías
  if (!"when_vars" %in% names(spec)) {
    spec$when_vars <- replicate(nrow(spec), character(0), simplify = FALSE)
  }
  if (!"when_values" %in% names(spec)) {
    spec$when_values <- replicate(nrow(spec), list(), simplify = FALSE)
  }
  # Tipado defensivo
  if (!is.list(spec$when_vars) || !is.list(spec$when_values)) {
    stop("`when_vars` y `when_values` deben ser columnas tipo lista (list-col).")
  }

  # ---- helpers
  safe_as_numeric <- function(x, colname) {
    before <- sum(!is.na(x))
    xn <- suppressWarnings(as.numeric(x))
    after <- sum(!is.na(xn))
    if (after < before) {
      warning(sprintf("Conversión a numeric en `%s` introdujo %d NA(s) nuevos.",
                      colname, before - after))
    }
    xn
  }

  normalize_when_values <- function(wvars, wvals) {
    if (!is.list(wvals)) stop("`when_values` debe ser lista (posicional o nombrada).")
    if (is.null(names(wvals))) {
      if (length(wvals) != length(wvars)) {
        stop("Lista posicional `when_values` debe tener la misma longitud que `when_vars`.")
      }
      return(wvals)
    } else {
      if (!all(wvars %in% names(wvals))) {
        faltan <- setdiff(wvars, names(wvals))
        stop(sprintf("Faltan claves en `when_values` para: %s",
                     paste(faltan, collapse = ", ")))
      }
      return(unname(wvals[wvars]))
    }
  }

  validate_when_values_numeric_only <- function(wvals, wvars) {
    for (j in seq_along(wvals)) {
      vj <- wvals[[j]]
      if (is.null(vj)) stop(sprintf("`when_values` para `%s` contiene NULL.", wvars[j]))
      if (!is.numeric(vj)) {
        stop(sprintf("`when_values` para `%s` debe ser NUMÉRICO cuando numeric_conds=TRUE.", wvars[j]))
      }
    }
    invisible(TRUE)
  }

  build_condition_vectors <- function(data, when_vars, wvals_norm, numeric_conds) {
    lapply(seq_along(when_vars), function(j) {
      wj <- when_vars[j]
      vj <- wvals_norm[[j]]
      if (isTRUE(numeric_conds)) {
        x <- safe_as_numeric(data[[wj]], wj)
        x %in% vj
      } else {
        data[[wj]] %in% vj
      }
    })
  }

  eval_manual_expr <- function(expr, cond_map) {
    if (!is.character(expr) || length(expr) != 1L) {
      stop("`manual_expr` debe ser un string de longitud 1.")
    }
    expr2 <- expr
    expr2 <- gsub("\\bAND\\b", "&", expr2, ignore.case = TRUE)
    expr2 <- gsub("\\bOR\\b",  "|", expr2, ignore.case = TRUE)
    expr2 <- gsub("\\bNOT\\b", "!", expr2, ignore.case = TRUE)
    expr2 <- gsub("\\s+", " ", expr2)
    expr2 <- gsub("!\\s*`([^`]+)`", "!(`\\1`)", expr2)
    expr2 <- gsub("!\\s*([A-Za-z0-9_\\.]+)", "!(\\1)", expr2, perl = TRUE)

    names_raw <- gsub("[()!&|]", " ", expr2)
    toks <- unique(strsplit(names_raw, "\\s+")[[1]])
    toks <- toks[nzchar(toks)]
    toks_clean <- gsub("^`|`$", "", toks)
    unknown <- setdiff(toks_clean, names(cond_map))
    if (length(unknown)) {
      stop(sprintf("Nombres en `manual_expr` no definidos en when_vars: %s",
                   paste(unknown, collapse = ", ")))
    }

    env <- list2env(cond_map, parent = baseenv())
    out <- try(eval(parse(text = expr2), envir = env), silent = TRUE)
    if (inherits(out, "try-error") || !is.logical(out)) {
      stop("`manual_expr` no se pudo evaluar como vector lógico. Revisa la sintaxis.")
    }
    if (length(out) != length(cond_map[[1L]])) {
      stop("`manual_expr` evaluó a una longitud distinta a nrow(data).")
    }
    out
  }

  is_empty_wvars <- function(wvars) {
    is.null(wvars) ||
      (is.atomic(wvars) && length(wvars) == 0) ||
      (is.list(wvars) && length(wvars) == 0) ||
      (is.character(wvars) && (length(wvars) == 0 || all(!nzchar(wvars) | is.na(wvars))))
  }

  # ---- loop por cada regla
  for (i in seq_len(nrow(spec))) {
    v         <- spec$var[i]
    wvars     <- spec$when_vars[[i]]
    wvals     <- spec$when_values[[i]]
    how_i     <- if ("how" %in% names(spec)) spec$how[i] else NA_character_
    manual_i  <- if ("manual_expr" %in% names(spec)) spec$manual_expr[i] else NA_character_

    if (!v %in% names(data)) {
      warning(sprintf("Variable objetivo `%s` no existe. Se omite.", v))
      next
    }

    # Target: sólo si se solicita
    if (isTRUE(coerce_target)) {
      data[[v]] <- safe_as_numeric(data[[v]], v)
    }

    new_nm <- paste0(prefix, "_", v)

    # -------- Caso SIN condiciones: obligatoria para todos --------
    if (is_empty_wvars(wvars)) {
      # relevancia incondicional => flag = 1 si target es NA, 0 en otro caso
      data[[new_nm]] <- ifelse(is.na(data[[v]]), 1L, 0L)
      next
    }

    # -------- Caso CON condiciones --------
    # Verificar existencia de when_vars en data
    if (!is.character(wvars)) {
      warning(sprintf("`when_vars` inválido para `%s`. Se asume sin condiciones => is.na(target).", v))
      data[[new_nm]] <- ifelse(is.na(data[[v]]), 1L, 0L)
      next
    }
    faltantes <- setdiff(wvars, names(data))
    if (length(faltantes)) {
      warning(sprintf("Variables de relevancia faltantes para `%s`: %s. Se omite.",
                      v, paste(faltantes, collapse = ", ")))
      next
    }

    # Normalizar/validar when_values
    wvals <- if (is.null(wvals)) list() else wvals
    wvals_norm <- normalize_when_values(wvars, wvals)
    if (isTRUE(numeric_conds)) validate_when_values_numeric_only(wvals_norm, wvars)

    # Construir condiciones
    conds <- build_condition_vectors(data, wvars, wvals_norm, numeric_conds)
    names(conds) <- wvars

    # Relevancia
    if (!is.na(manual_i) && nzchar(manual_i)) {
      rel <- eval_manual_expr(manual_i, cond_map = conds)
    } else {
      how_i <- match.arg(if (is.na(how_i) || !nzchar(how_i)) "all" else how_i, c("all","any"))
      rel <- if (how_i == "all") Reduce(`&`, conds) else Reduce(`|`, conds)
    }

    # Flag con relevancia
    flag_vec <- ifelse(rel & is.na(data[[v]]), 1L, 0L)
    if (isTRUE(na_when_not_relevant)) flag_vec[!isTRUE(rel)] <- NA_integer_
    data[[new_nm]] <- flag_vec
  }

  data
}
