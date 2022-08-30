
#' Calculate Maximum Spirometry Values by Test
#'
#' `get_max_values`   Within each test, the maximum value for each spirometric parameter (FEV1, FVC, etc.) is calculated. .
#' Users can specify grouping variables, and table operations will be performed 'by group."
#'
#' @param df a data frame created by `read_spiro_sql()` or `read_spiro_pdf().`
#' @param grp_vars character; string of column names to used to calculate maximum values by group (i.e. 'id', 'spiro_test', etc.).
#' See \code{\link[dplyr]{dplyr::group_by()}}.
#' @param basic_parameters logical; if TRUE, the default, only values for the following parameters are returned:
#' - FEV1
#' - FVC
#' - FEV6
#' - FEV1/FVC
#' - FEF2575
#' - Peak Flow (PEF)
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_max_values(df, test_threshold = NULL, test_col = 'Spiro_Event', grp_vars = c('ID', 'spiro_test'),
#'                basic_parameters = TRUE)
#'}
#'
get_max_values <- function(df, grp_vars = NULL, sum_fun = 'max', basic_parameters = TRUE) {


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
    dplyr::group_by(dplyr::across({{grp_vars}}))

  d_trials <- d_grp %>%
    dplyr::summarise(n_trials = dplyr::n())

  d_sum <- d_grp %>%
    dplyr::summarise(dplyr::across(where(is.numeric), list({{sum_fun}}), .names = "{.col}_{sum_fun}"),
                     .groups = 'keep')

  if (basic_parameters == TRUE) {
    d_sum <- d_sum %>%
      dplyr::select(dplyr::starts_with(c('FEV1_', 'FVC_', 'FEV6_', 'FEV1FVC_', 'FEF2575_', 'PEF_'))) %>%
      dplyr::rename_with(., ~stringr::str_replace(., paste('n_trials_', sum_fun), 'balls'))
  }

  d_sum$n_trials <- d_trials$n_trials

  d_sum %>%
    dplyr::relocate(n_trials, .after = dplyr::group_cols())
}
