

#' Read Spirobank SQLite database
#'
#' `read_spiro_sql` reads Spirobank patient data that has been exported to a SQL database.  In order to read the data properly,
#'  all files (Spirobank.sqlite, Spirobank.sqlite-shm, Spirobank.sqlite-wal) must be in the directory, unless the SQL file has
#'  been previously opened with another software (i.e. DB Browser).
#'
#' @param sqldb character; path of Spirobank SQLite database
#' @param demo logical; include patient age, height, and weight? Default = FALSE.
#' @param inspiratory logical, include inspiratory maneuvers? Default = FALSE.
#' @param tzone character, time zone of location where measurments were recorded.
#' Default = 'America/New_York'.
#' @param participant_id user defined string to denote a patient identifier.
#' If specified, a new column is created ('ID') listing the string. Default is NULL.
#' @param sample_col; character; user defined character string specifying the name of the
#' column to denote a sample ID. This column is useful to denote repeated periods of
#' data collection by individual patents. Default is NULL.
#' @param sample_id; user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL.
#' @param test_threshold numeric; allows a user to partition Spirobank trials into discrete tests according to a user defined
#' threshold value (seconds). If the time difference between the last trial and and a new trial exceeds the threshold value,
#' the current trial is designated as belonging to a new test. For example, the threshold value is set at 600 seconds (10 minutes)
#' and a patient performs 6 trials  The first 3 trials are done consecutively within 10 minutes.  The fourth trial occurs
#' about one hour after the third.  The fifth and sixth trial are completed within 10 minutes of the fourth trial.
#' These six trials represent two separate testing sessions according the user defined threshold. Spirobank test number is indicated
#' under the column `spiro_test.`
#'
#' If NULL, the default, `spiro_test` will be defined according to Spirobank software settings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_spiro_sql(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York',
#'                participant_id = NULL, sample_col = NULL, sample_id = NULL )
#' }
#'
read_spiro_sql <- function(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York',
                           test_threshold = NULL, participant_id = NULL, sample_col = NULL,
                           sample_id = NULL) {

  con <- DBI::dbConnect(RSQLite::SQLite(), sqldb)
  slist <- as.data.frame(DBI::dbListTables(con))
  sdf <- DBI::dbReadTable(con, slist[3, ])

  sdf <- sdf %>%
    tidyr::as_tibble() %>%
    dplyr::select(Z_PK, ZSESSION, ZDATE:ZQUALITYCODE, ZDEVICESERIALNUMBER, ZPATIENTID) %>%
    dplyr::rename(trial_index = Z_PK,
                  spiro_test = ZSESSION,
                  ID = ZPATIENTID,
                  PEF_min = ZPEF) %>%
    dplyr::mutate(date_time = lubridate::as_datetime(ZDATE, origin = '2001-01-01', tz = tzone),
                  date = as.Date(date_time, tz = tzone),
                  PEF = PEF_min*60) %>%
    dplyr::select(-c(ZDATE, PEF_min))


  DBI::dbDisconnect(con)

  if (demo == FALSE) {
    sdf <- sdf %>%
      dplyr::select(-dplyr::contains('PATIENT'))
  } else {
    sdf <- sdf %>%
      dplyr::mutate(ZPATIENTBIRTHDATE = as.Date(lubridate::as_datetime(ZPATIENTBIRTHDATE, origin = '2001-01-01', tz = tzone),
                                                tz = tzone))
  }

  if (inspiratory == FALSE) {
    sdf <- sdf %>%
      dplyr::select(-c(ZFIF25:ZFIVC, ZPIF))
  }

  sdf <- sdf %>%
    dplyr::rename_with(., ~ (gsub("Z", "", .x, fixed = TRUE))) %>%
    dplyr::relocate(ID, date_time, date, FEV1, PEF) %>%
    dplyr::relocate(trial_index, spiro_test, .after = date) %>%
    dplyr::mutate(dplyr::across(ID, ~ dplyr::na_if(., ''))) %>%
    tidyr::fill(ID, .direction = 'downup')

  message(paste('The input data frame includes', max(sdf$spiro_test), 'tests as defined by the Spirobank software.'))

  if(is.null(test_threshold)) {
    sdf <- sdf
  } else {
    sdf <- sdf %>%
      dplyr::arrange(date_time) %>%
      dplyr::mutate(lag_tm = dplyr::lag(date_time),
                    tdiff_sec = (lubridate::ymd_hms(date_time) - lubridate::ymd_hms(lag_tm)),
                    break_yn = ifelse(is.na(tdiff_sec), 0,
                                      ifelse(tdiff_sec > test_threshold, 1, 0)),
                    spiro_test = cumsum(break_yn) + 1) %>%
      dplyr::select(-c(lag_tm, tdiff_sec, break_yn))

    message(paste('The input data frame includes', max(sdf$spiro_test), 'tests as defined by `test_threshold` criteria.'))


  }

  if(!is.null(participant_id)) {
    sdf$ID <- participant_id
  }

  if(sum(!is.null(sample_col), !is.null(sample_id)) == 1) {
    stop('Both `sample_col` and `sample_id` must have values.', call. = FALSE)
  }

  if(sum(!is.null(sample_col), !is.null(sample_id)) == 2) {
    sdf <- sdf %>%
      dplyr::mutate({{sample_col}} := {{sample_id}}) %>%
      dplyr::relocate({{sample_col}}, .after = ID)
  }

  sdf

}

