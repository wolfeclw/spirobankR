

#' Calculate Maximum Spirometry Values by Test
#'
#' @param df a data frame created by `read_spiro_sql()` or `read_spiro_pdf().`
#' @param grp_vars bldj
#' @param basic_parameters logical; if TRUE, the default, only values for the following parameters are returned:
#' - FEV1
#' - FVC
#' - FEV6
#' - FEV1/FVC
#' - FEF2575
#' - Peak Flow (PEF)
#'
#' @return a tibble.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_max_values(df, test_threshold = NULL, test_col = 'Spiro_Event', grp_vars = NULL,
#'                basic_parameters = TRUE)
#'}
#'
get_max_values <- function(df, grp_vars = NULL, basic_parameters = TRUE) {


  if(is.null(grp_vars)) {
    warning('No grouping variables have been named. Output values represent the maximum values of all observations in the data frame.',
            call. = FALSE)
  } else if (!is.null(grp_vars) & (('spiro_test' %in% names(df)) & !'spiro_test' %in% grp_vars)) {
    warning('`spiro_test` is listed in the input data frame, but not included as a grouping variable.',
            call. = FALSE)
  } else if (!is.null(grp_vars) & (('ID' %in% names(df)) & !'ID' %in% grp_vars)) {
    warning('`ID` is listed in the input data frame, but not included as a grouping variable.',
            call. = FALSE)
  }

  d_grp <- df %>%
    dplyr::group_by(dplyr::across({{grp_vars}})) %>%
    dplyr::mutate(n_trials = dplyr::n())

  d_max <- d_grp %>%
    dplyr::summarise(dplyr::across(where(is.numeric), max, .names = "{.col}_max"),
                     .groups = 'keep')

  if (basic_parameters == TRUE) {
    d_max <- d_max %>%
      dplyr::select(n_trials = n_trials_max,
                    FEV1_max, FVC_max, FEV6_max, FEV1FVC_max, FEF2575_max, PEF_max)
  }

  d_max
}
