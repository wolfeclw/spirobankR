

#' Calculate Maximum Spirometry Values by Test
#'
#' @param df a data frame created by `read_spiro_sql()` or `read_spiro_pdf().`
#' @param test_threshold numeric; threshold value (seconds) used partition data into discrete testing sessions.
#' If the time difference between the last trial and and a new trial exceeds the threshold value, the current
#' trial is designated as belonging to a new test. For example, the threshold value is set at 600 seconds (10 minutes)
#' and a patient performs 6 trials  The first 3 trials are done consecutively within 10 minutes.  The fourth trial occurs
#' about one hour after the third.  The fifth and sixth trial are completed within 10 minutes of the fourth trial.
#' These six trials represent two separate testing sessions according the user defined threshold.
#'
#' If NULL, the default, the Spirobank defined session variable (`spiro_session`) will be used.
#'
#' @param test_col character; name of column to denote testing sessions if `test_threshold` is specified,
#' otherwise it will be ignored.  Default = 'Spiro_Event.'
#' @param sample_col character; name of column to denote patient sample number. Useful if a patient has
#' more than one period of data collection recorded in the input data frame.
#' @param participant_id string; overwrites value of `ID` in the input data frame.
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
#' get_max_values(df, test_threshold = NULL, test_col = 'Spiro_Event', sample_col = NULL, participant_id = NULL,
#'                basic_parameters = TRUE)
#'}
#'
get_max_values <- function(df, test_threshold = NULL, test_col = 'Spiro_Event', sample_col = NULL, participant_id = NULL,
                     basic_parameters = TRUE) {


  if (!'ID' %in% names(df)) {
    stop('Column `ID` must be present in the input data frame in order to calculate maximum values by patient.')
  }

  if (is.null(test_threshold)) {
    d_event <- dplyr::arrange(df, Date_Time)
    message(crayon::green('Maximum values will be calculated by Spirobank defined test session (`spiro_session`).'))
  } else {
    d_event <- df %>%
      dplyr::arrange(Date_Time) %>%
      dplyr::mutate(lag_tm = dplyr::lag(Date_Time),
                    tdiff_sec = (lubridate::ymd_hms(Date_Time) - lubridate::ymd_hms(lag_tm)),
                    break_yn = ifelse(is.na(tdiff_sec), 0,
                                      ifelse(tdiff_sec > test_threshold, 1, 0)),
                    {{test_col}} := cumsum(break_yn) + 1) %>%
      dplyr::select(-c(lag_tm, tdiff_sec, break_yn))
  }


  if (is.null(test_threshold)) {

    if (!is.null(sample_col)) {
      d_grp <- d_event %>%
        dplyr::group_by(ID, spiro_session, .data[[sample_col]]) %>%
        dplyr::mutate(n_trials = dplyr::n())
    } else {
      d_grp <- d_event %>%
        dplyr::group_by(ID, spiro_session) %>%
        dplyr::mutate(n_trials = dplyr::n())
    }
  } else {
    if (!is.null(sample_col)) {
      d_grp <- d_event %>%
        dplyr::group_by(ID, .data[[test_col]], .data[[sample_col]]) %>%
        dplyr::mutate(n_trials = dplyr::n())
    } else {
      d_grp <- d_event %>%
        dplyr::group_by(ID, .data[[test_col]]) %>%
        dplyr::mutate(n_trials = dplyr::n())
    }
  }

  d_max <- d_grp %>%
    dplyr::summarise(dplyr::across(where(is.numeric), max, .names = "{.col}_max"),
                     .groups = 'keep')

  if (!is.null(participant_id)) {
    d_max$ID <- participant_id
  }

  if (is.null(sample_col)) {
    message(crayon::cyan('Hint: `sample_col` should be defined if in the input data frame includes patients with more than one data collection period.'))
  }

  if (basic_parameters == TRUE) {
    d_max <- d_max %>%
      dplyr::select(n_trials = n_trials_max,
                    FEV1_max, FVC_max, FEV6_max, FEV1FVC_max, FEF2575_max, PEF_max)
  }

  d_max
}
