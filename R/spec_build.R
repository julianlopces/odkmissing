#' Construir especificación de flags de missing a partir de ODK
#'
#' Esta función toma un data.frame de datos y un cuestionario ODK filtrado
#' (con columnas de metadatos como `type`, `name`, `relevance`, etc.) y genera
#' una especificación (`spec_for_flags`) adecuada para ser utilizada con la
#' función \code{flags_missing_por_variable()}.
#'
#' El flujo incluye:
#' \itemize{
#'   \item Traducción de expresiones de relevancia ODK a tokens estandarizados
#'         (\code{selected()}, \code{not(selected())}, \code{=}, \code{!=},
#'         y comparadores \code{>, >=, <, <=}).
#'   \item Creación de variables dummy para los comparadores (\code{X > 4}, etc.)
#'         directamente en los datos.
#'   \item Creación de dummies faltantes (tokens que aparecen en la relevancia
#'         pero no existen en los datos) inicializados en cero.
#'   \item Inclusión de todas las variables obligatorias: si una pregunta no
#'         tiene condición de relevancia, igualmente se agrega al spec con
#'         \code{when_vars = character(0)} y \code{when_values = list()} para
#'         que sea evaluada como obligatoria en todo caso.
#' }
#'
#' @param datos data.frame o tibble con los datos recolectados.
#' @param ODK_filtrado data.frame o tibble del cuestionario ODK previamente
#'   filtrado a los tipos de pregunta relevantes (\code{select}, \code{integer},
#'   \code{text}, \code{begin group}, \code{end group}).
#' @param usar_col (chr) nombre de la columna de relevancia a usar. Por defecto
#'   se prioriza \code{relevance_final} si existe en \code{ODK_filtrado}, en
#'   caso contrario se usa \code{relevance}.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{spec_for_flags}{tibble con las reglas por variable objetivo:
#'         \code{var}, \code{when_vars}, \code{when_values}, \code{how},
#'         y \code{manual_expr}.}
#'   \item{datos_tokens}{data.frame igual a \code{datos} pero con las columnas
#'         adicionales creadas para tokens de relevancia (dummies de opciones
#'         múltiples, comparadores, etc.).}
#' }
#'
#' @examples
#' \dontrun{
#' res <- build_spec_for_flags(datos, ODK_filtrado)
#' spec_for_flags <- res$spec_for_flags
#' datos_tokens   <- res$datos_tokens
#'
#' out <- flags_missing_por_variable(
#'   data          = datos_tokens,
#'   spec          = spec_for_flags,
#'   prefix        = "m",
#'   numeric_conds = TRUE,
#'   coerce_target = FALSE
#' )
#' }
#'
#' @export
build_spec_for_flags <- function(datos,
                                 ODK_filtrado,
                                 usar_col = if ("relevance_final" %in% names(ODK_filtrado)) "relevance_final" else "relevance") {
  stopifnot(is.data.frame(datos), is.data.frame(ODK_filtrado))

  # --- 1) Preguntas (no grupos) + columna de relevancia a usar (sin %>%)
  ODK_pregs <- dplyr::filter(
    ODK_filtrado,
    !tolower(.data$type) %in% c("begin group","begin_group","end group","end_group")
  )
  ODK_pregs <- dplyr::mutate(ODK_pregs, relevance_use = .data[[usar_col]])

  # --- 2) Normalizador mínimo
  norm_expr <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- stringr::str_replace_all(x, "\\s+", " ")
    x <- stringr::str_trim(x)
    x <- ifelse(x == "", NA_character_, x)
    x
  }

  # --- 3) Traductor ODK -> tokens + manual_expr (incluye comparadores)
  translate_relevance <- function(expr) {
    out <- list(
      manual_expr = NA_character_,
      when_vars   = character(0),
      when_values = list(),
      comparators = tibble::tibble(token=character(), var=character(), op=character(), thr=numeric())
    )
    expr <- norm_expr(expr)
    if (is.na(expr)) return(out)

    work <- expr
    tokens <- c()
    token_values <- list()
    comp_specs <- list()

    # A) not(selected(${VAR}, 'code')) -> NOT VAR_code
    notsel_pat <- "not\\s*\\(\\s*selected\\s*\\(\\s*\\$\\{([^}]+)\\}\\s*,\\s*'([^']+)'\\s*\\)\\s*\\)"
    while (TRUE) {
      m <- stringr::str_match(work, notsel_pat)
      if (all(is.na(m))) break
      full <- m[1]; var <- m[2]; code <- m[3]
      token <- paste0(var, "_", code)
      work  <- stringr::str_replace(work, stringr::fixed(full), paste0("NOT ", token))
      tokens <- c(tokens, token)
      token_values[[token]] <- 1L
    }

    # B) selected(${VAR}, 'code') -> VAR_code
    sel_pat <- "selected\\s*\\(\\s*\\$\\{([^}]+)\\}\\s*,\\s*'([^']+)'\\s*\\)"
    while (TRUE) {
      m <- stringr::str_match(work, sel_pat)
      if (all(is.na(m))) break
      full <- m[1]; var <- m[2]; code <- m[3]
      token <- paste0(var, "_", code)
      work  <- stringr::str_replace(work, stringr::fixed(full), token)
      tokens <- c(tokens, token)
      token_values[[token]] <- 1L
    }

    # C) ${VAR} = n -> VAR (si 1) o VAR_eq_n
    eq_pat <- "\\$\\{([^}]+)\\}\\s*=\\s*(\\d+)"
    while (TRUE) {
      m <- stringr::str_match(work, eq_pat)
      if (all(is.na(m))) break
      var <- m[2]; val <- as.integer(m[3])
      token <- if (val == 1L) var else paste0(var, "_eq_", val)
      work  <- stringr::str_replace(work, eq_pat, token)
      tokens <- c(tokens, token); token_values[[token]] <- val
    }

    # D) ${VAR} != n -> NOT <token>
    neq_pat <- "\\$\\{([^}]+)\\}\\s*!=\\s*(\\d+)"
    while (TRUE) {
      m <- stringr::str_match(work, neq_pat)
      if (all(is.na(m))) break
      var <- m[2]; val <- as.integer(m[3])
      token <- if (val == 1L) var else paste0(var, "_eq_", val)
      work  <- stringr::str_replace(work, neq_pat, paste0("NOT ", token))
      tokens <- c(tokens, token); token_values[[token]] <- val
    }

    # E) ${VAR} >,<,>=,<= thr -> VAR__gt_thr, etc. (metadato para crear dummies)
    comp_map <- list(">"="__gt_", ">="="__ge_", "<"="__lt_", "<="="__le_")
    comp_pat <- "\\$\\{([^}]+)\\}\\s*(>=|<=|>|<)\\s*(\\d+(?:\\.\\d+)?)"
    while (TRUE) {
      m <- stringr::str_match(work, comp_pat)
      if (all(is.na(m))) break
      var <- m[2]; op <- m[3]; thr <- as.numeric(m[4])
      token <- paste0(var, comp_map[[op]], gsub("\\.", "_", as.character(thr)))
      work  <- stringr::str_replace(work, comp_pat, token)
      tokens <- c(tokens, token)
      token_values[[token]] <- 1L
      comp_specs[[length(comp_specs)+1]] <- tibble::tibble(token=token, var=var, op=op, thr=thr)
    }

    # F) Operadores a estándar
    work <- stringr::str_replace_all(work, "(?i)\\band\\b", "AND")
    work <- stringr::str_replace_all(work, "(?i)\\bor\\b",  "OR")
    work <- stringr::str_replace_all(work, "(?i)\\bnot\\b", "NOT")
    work <- stringr::str_squish(work)

    out$manual_expr <- work
    out$when_vars   <- unique(tokens)
    out$when_values <- unname(mget(out$when_vars, envir = list2env(token_values), ifnotfound = list(NULL)))
    names(out$when_values) <- out$when_vars
    out$comparators <- if (length(comp_specs)) dplyr::bind_rows(comp_specs) else out$comparators
    out
  }

  # --- 4) Aplicar traductor a todas las preguntas (sin %>%)
  res_list <- purrr::map(ODK_pregs$relevance_use, translate_relevance)

  spec_tokens <- tibble::tibble(
    var            = ODK_pregs$name,
    manual_expr    = purrr::map_chr(res_list, "manual_expr"),
    when_vars      = purrr::map(res_list, "when_vars"),
    when_values    = purrr::map(res_list, "when_values"),
    comparators    = purrr::map(res_list, "comparators"),
    sin_relevancia = purrr::map_lgl(ODK_pregs$relevance_use, ~ is.na(norm_expr(.x)))
  )

  # --- 5) Crear dummies de comparadores en `datos` (sin %>%)
  crear_dummies_comparadores <- function(datos, spec_df) {
    comp_all <- dplyr::bind_rows(spec_df$comparators)
    if (nrow(comp_all) == 0) return(datos)
    for (j in seq_len(nrow(comp_all))) {
      tk  <- comp_all$token[j]
      v   <- comp_all$var[j]
      op  <- comp_all$op[j]
      thr <- comp_all$thr[j]
      if (!v %in% names(datos)) {
        warning(sprintf("No existe la columna '%s' para comparador '%s'; se crea NA.", v, tk))
        datos[[tk]] <- NA_integer_
        next
      }
      x <- suppressWarnings(as.numeric(datos[[v]]))
      cmp <-
        if (op == ">")  x >  thr else
          if (op == ">=") x >= thr else
            if (op == "<")  x <  thr else
              if (op == "<=") x <= thr else NA
      datos[[tk]] <- ifelse(isTRUE(cmp), 1L, 0L)
    }
    datos
  }

  datos_cmp <- crear_dummies_comparadores(datos, spec_tokens)

  # --- 6) Asegurar tokens faltantes como 0 (para selected(), etc.) (sin %>%)
  todos_tokens <- unique(unlist(spec_tokens$when_vars))
  faltan <- setdiff(todos_tokens, names(datos_cmp))
  if (length(faltan)) {
    message("Creando dummies faltantes como 0: ", paste(faltan, collapse = ", "))
    for (nm in faltan) datos_cmp[[nm]] <- 0L
  }

  # --- 7) Armar spec_for_flags (sin %>%)
  spec_for_flags <- dplyr::transmute(
    spec_tokens,
    var,
    when_vars   = purrr::map2(when_vars, sin_relevancia, ~ if (.y) character(0) else .x),
    when_values = purrr::map2(when_values, sin_relevancia, ~ if (.y) list() else .x),
    how         = NA_character_,
    manual_expr = manual_expr
  )

  # salida
  list(
    spec_for_flags = spec_for_flags,
    datos_tokens   = datos_cmp
  )
}

