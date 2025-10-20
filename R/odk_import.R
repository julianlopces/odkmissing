#' Importar XLSForm ODK y propagar relevancias de grupos (filtrando obligatorias)
#'
#' Lee el archivo ODK (XLS/XLSX), filtra tipos relevantes y propaga las
#' condiciones de \code{relevance} desde \emph{begin group} hasta su \emph{end group}
#' correspondiente (emparejando por \code{name}), concatenando con \code{" and "}
#' y respetando anidamientos. Luego filtra para dejar sólo las preguntas
#' marcadas como obligatorias según \code{required_value}.
#'
#' @param path Ruta al archivo XLS/XLSX del formulario ODK.
#' @param sheet (opcional) Nombre o índice de hoja a leer. Default: primero.
#' @param select_regex Regex para tipos que empiezan con "select". Default: \code{"^select"}.
#' @param extra_types Tipos adicionales a conservar. Default: \code{c("integer","begin group","end group","text")}.
#' @param drop_begin_end Si \code{TRUE}, remueve filas \code{begin group}/\code{end group}. Default: \code{TRUE}.
#' @param required_value Cadena que indica cómo se marcó "obligatorio" en la columna
#'   \code{required}. Usar típicamente \code{"TRUE"} o \code{"yes"} (no sensible a mayúsculas/minúsculas).
#'   Default: \code{"TRUE"}.
#'
#' @return \code{data.frame} con columnas \code{type, name, label, required, relevance, constraint},
#'   donde \code{relevance} es la relevancia propagada. Sólo incluye preguntas obligatorias.
#'
#' @examples
#' # out <- import_odk_propagate_required("ruta/al/formulario.xlsx", required_value = "yes")
#'
#' @export
import_odk_propagate_required <- function(path,
                                          sheet = NULL,
                                          select_regex = "^select",
                                          extra_types = c("integer","begin group","end group","text"),
                                          drop_begin_end = TRUE,
                                          required_value = "TRUE") {
  # --- Lectura
  Encuesta_ODK <- readxl::read_excel(path, sheet = sheet)

  # --- Selección de columnas y tipos
  ODK_filtrado <- Encuesta_ODK |>
    dplyr::select(type, name, label, required, relevance, constraint) |>
    dplyr::filter(
      grepl(select_regex, type) |
        type %in% extra_types
    )

  # --- Helpers locales
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  paren <- function(x) {
    x <- trim(x)
    ifelse(nchar(x) == 0, "",
           ifelse(grepl("^\\(.*\\)$", x), x, paste0("(", x, ")")))
  }
  and_join <- function(parts) {
    parts <- parts[nchar(parts) > 0]
    if (length(parts) == 0) return("")
    paste(paren(parts), collapse = " and ")
  }
  ctx_from_stack <- function(stk) {
    if (length(stk) == 0) return("")
    and_join(vapply(stk, function(e) e$rel, character(1)))
  }

  # --- Normalizaciones
  rel_chr <- ifelse(is.na(ODK_filtrado$relevance), "", as.character(ODK_filtrado$relevance))
  type_lc <- tolower(ODK_filtrado$type)
  is_begin <- type_lc %in% c("begin group","begin_group")
  is_end   <- type_lc %in% c("end group","end_group")

  # --- Pila de grupos activos y propagación
  stack <- list()
  rel_final <- character(nrow(ODK_filtrado))

  for (i in seq_len(nrow(ODK_filtrado))) {
    nm_i   <- as.character(ODK_filtrado$name[i])
    currel <- rel_chr[i]

    if (is_begin[i]) {
      ctx_before <- ctx_from_stack(stack)
      ctx_this   <- and_join(c(ctx_before, currel))
      rel_final[i] <- ifelse(nchar(ctx_this) == 0, NA_character_, ctx_this)
      stack[[length(stack) + 1]] <- list(name = nm_i, rel = currel)

    } else if (is_end[i]) {
      if (length(stack) == 0) {
        warning(sprintf("Fila %d: 'end group' sin 'begin group' activo.", i))
      } else {
        idx_match <- NA_integer_
        for (k in seq_along(stack)) {
          kk <- length(stack) - k + 1L
          if (identical(stack[[kk]]$name, nm_i)) { idx_match <- kk; break }
        }
        if (is.na(idx_match)) {
          warning(sprintf("Fila %d: 'end group' con name='%s' sin 'begin group' correspondiente.", i, nm_i))
          stack <- if (length(stack) > 0) stack[-length(stack)] else stack
        } else {
          stack <- if (idx_match > 1) stack[-idx_match] else stack[-1]
        }
      }
      ctx_now <- ctx_from_stack(stack)
      rel_final[i] <- ifelse(nchar(ctx_now) == 0, NA_character_, ctx_now)

    } else {
      ctx_now  <- ctx_from_stack(stack)
      combined <- and_join(c(ctx_now, currel))
      rel_final[i] <- ifelse(nchar(combined) == 0, NA_character_, combined)
    }
  }

  ODK_filtrado$relevance_final <- rel_final

  # --- Limpiezas finales
  out <- ODK_filtrado
  if (isTRUE(drop_begin_end)) {
    out <- out |> dplyr::filter(!type %in% c("begin group","end group"))
  }

  # Filtrar SOLO preguntas obligatorias según `required_value`
  req_norm <- tolower(trim(out$required))
  out <- out |>
    dplyr::mutate(required_norm = req_norm) |>
    dplyr::filter(required_norm == tolower(trim(required_value))) |>
    dplyr::select(-required_norm)

  # Importante: NO filtramos por !is.na(relevance_final); se mantienen obligatorias sin condición
  out |>
    dplyr::mutate(relevance = .data$relevance_final) |>
    dplyr::select(type, name, label, required, relevance, constraint)
}
