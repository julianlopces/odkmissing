#' Pipeline de MISSINGS: importar ODK, construir spec y generar flags de missing
#'
#' Ejecuta el flujo completo para crear flags de missing a partir de un XLSForm ODK
#' y una base de datos de respuestas:
#' \enumerate{
#'   \item Importa y propaga relevancias del ODK (filtrando obligatorias).
#'   \item Construye \code{spec_for_flags} y genera tokens/dummies necesarios.
#'   \item Crea columnas de alertas de missing (\code{prefix_}*) por variable.
#'   \item Suma las alertas y crea \code{total_missing} y \code{flag_missing}.
#' }
#'
#' Requiere que existan previamente las funciones:
#' \itemize{
#'   \item \code{import_odk_propagate_required()}
#'   \item \code{build_spec_for_flags()}
#'   \item \code{flags_missing_por_variable()}
#' }
#'
#' @param path_odk Ruta al archivo XLS/XLSX del formulario ODK.
#' @param required_value Valor que indica "obligatorio" en la columna \code{required}
#'   del ODK (p.ej. \code{"yes"} o \code{"TRUE"}). Default: \code{"yes"}.
#' @param path_datos Ruta al archivo de datos (XLS/XLSX/CSV). Si es Excel,
#'   usar \code{sheet_datos} para especificar la hoja.
#' @param sheet_datos (opcional) Nombre o índice de hoja en \code{path_datos}
#'   cuando es Excel. Default: \code{"data_raw"}.
#' @param prefix Prefijo para los flags de missing por variable. Default: \code{"m"}.
#' @param numeric_conds Si \code{TRUE}, las condiciones se comparan como numéricas
#'   y los \code{when_values} deben ser numéricos. Default: \code{TRUE}.
#' @param coerce_target Si \code{TRUE}, convierte el target a numérico antes de
#'   evaluar \code{is.na}; si \code{FALSE}, no lo convierte. Default: \code{FALSE}.
#' @param return_all Si \code{TRUE}, devuelve una lista con objetos intermedios
#'   (ODK filtrado, spec, datos con tokens). Si \code{FALSE}, devuelve sólo
#'   el data.frame final con flags y sumas. Default: \code{TRUE}.
#'
#' @return Si \code{return_all = TRUE}, una lista con:
#' \describe{
#'   \item{data}{Base final con flags \code{<prefix>_var}, \code{total_missing} y \code{flag_missing}.}
#'   \item{ODK_filtrado}{Salida de \code{import_odk_propagate_required()}.}
#'   \item{spec_for_flags}{Especificación construida por \code{build_spec_for_flags()}.}
#'   \item{datos_tokens}{Datos con tokens/dummies añadidos por \code{build_spec_for_flags()}.}
#' }
#' Si \code{return_all = FALSE}, devuelve sólo \code{data}.
#'
#' @examples
#' \dontrun{
#' res <- create_missing_vars(
#'   path_odk      = "C:/Users/julia/Downloads/Encuesta Niubiz MS 2025 (2).xlsx",
#'   required_value = "yes",
#'   path_datos    = "C:/Users/julia/Downloads/Alertas - NIUBIZ.xlsx",
#'   sheet_datos   = "data_raw",
#'   prefix        = "m",
#'   numeric_conds = TRUE,
#'   coerce_target = FALSE
#' )
#'
#' head(res$data)
#' }
#' @export
create_missing_vars <- function(path_odk,
                                required_value = "yes",
                                path_datos,
                                sheet_datos = "data_raw",
                                prefix = "m",
                                numeric_conds = TRUE,
                                coerce_target = FALSE,
                                return_all = TRUE) {
  # --- 1) Importar ODK y propagar relevancias (filtrando obligatorias)
  ODK_filtrado <- import_odk_propagate_required(
    path           = path_odk,
    required_value = required_value
  )

  # --- 2) Importar datos
  # Detectar extensión de path_datos (xls/xlsx/csv)
  ext <- tolower(tools::file_ext(path_datos))
  if (ext %in% c("xls","xlsx")) {
    datos <- readxl::read_excel(path_datos, sheet = sheet_datos)
  } else if (ext %in% c("csv")) {
    datos <- utils::read.csv(path_datos, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Extensión de archivo de datos no soportada: ", ext,
         ". Usa .xlsx/.xls o .csv.")
  }

  # --- 3) Construir spec_for_flags y datos_tokens (tokens/dummies)
  build_res <- build_spec_for_flags(datos = datos, ODK_filtrado = ODK_filtrado)
  spec_for_flags <- build_res$spec_for_flags
  datos_tokens   <- build_res$datos_tokens

  # --- 4) Crear flags de missing por variable
  base_missing <- flags_missing_por_variable(
    data          = datos_tokens,
    spec          = spec_for_flags,
    prefix        = prefix,
    numeric_conds = numeric_conds,
    coerce_target = coerce_target
  )

  # --- 5) Sumar flags y crear flag global
  flag_cols <- grep(paste0("^", gsub("([\\^\\$\\*\\+\\?\\|\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", prefix), "_"),
                    names(base_missing), value = TRUE)

  if (length(flag_cols) == 0) {
    # Si no se generó ninguna alerta (caso extremo), añade columnas en 0
    base_missing$total_missing <- 0L
    base_missing$flag_missing  <- 0L
  } else {
    base_missing$total_missing <- rowSums(base_missing[, flag_cols, drop = FALSE], na.rm = TRUE)
    base_missing$flag_missing  <- as.integer(base_missing$total_missing > 0)
  }

  if (isTRUE(return_all)) {
    return(list(
      data           = base_missing,
      ODK_filtrado   = ODK_filtrado,
      spec_for_flags = spec_for_flags,
      datos_tokens   = datos_tokens
    ))
  } else {
    return(base_missing)
  }
}
