

#' Read Spirobank SQLite database
#'
#' @param sqldb character; path of Spirobank SQLite database
#' @param demo logical; include patient demographics and patient anthropometrics?
#' Default = FALSE.
#' @param inspiratory logical, include inspiratory maneauvers? Default = FALSE.
#' @param tzone character, time zone of location where measurments were recorded.
#' Default = 'America/New_York'.
#' @param participant_id user defined string to denote a patient identifier.
#' If specified, a new column is created ('ID') listing the string. Default is NULL.
#' @param sample_col; character; user defined character string specifying the name of the
#' column to denote a sample ID. This column is useful to denote repeated periods of
#' data collection by individual patents. Default is NULL.
#' @param sample_id; user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL
#'
#' @return a tibble with spirometry values.
#' @export
#'
#' @examples
#' \dontrun{
#' read_spiro_sql(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York',
#'                participant_id = NULL, sample_col = NULL, sample_id = NULL )
#' }
#'
read_spiro_sql <- function(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York',
                           participant_id = NULL, sample_col = NULL, sample_id = NULL) {

  con <- DBI::dbConnect(RSQLite::SQLite(), sqldb)
  slist <- as.data.frame(DBI::dbListTables(con))
  sdf <- DBI::dbReadTable(con, slist[3, ])

  sdf <- sdf %>%
    tidyr::as_tibble() %>%
    dplyr::select(Z_PK, ZSESSION, ZDATE:ZQUALITYCODE, ZDEVICESERIALNUMBER, ZPATIENTID) %>%
    dplyr::rename(trial_index = Z_PK,
                  spiro_session = ZSESSION,
                  ID = ZPATIENTID,
                  PEF_min = ZPEF) %>%
    dplyr::mutate(Date_Time = lubridate::as_datetime(ZDATE, origin = '2001-01-01', tz = tzone),
                  Date = as.Date(Date_Time, tz = tzone),
                  PEF = PEF_min*60) %>%
    dplyr::select(-c(ZDATE, PEF_min))


  DBI::dbDisconnect(con)

  if (demo == FALSE) {
    sdf <- sdf %>%
      dplyr::select(-dplyr::contains('PATIENT'))
  }

  if (inspiratory == FALSE) {
    sdf <- sdf %>%
      dplyr::select(-c(ZFIF25:ZFIVC, ZPIF))
  }

  sdf <- sdf %>%
    dplyr::rename_with(., ~ (gsub("Z", "", .x, fixed = TRUE))) %>%
    dplyr::relocate(ID, Date_Time, Date, FEV1, PEF) %>%
    dplyr::relocate(trial_index, spiro_session, .after = dplyr::last_col()) %>%
    dplyr::mutate(dplyr::across(ID, ~ dplyr::na_if(., ''))) %>%
    tidyr::fill(ID, .direction = 'downup')

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

