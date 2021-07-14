

#' Read Spirobank SQLite database
#'
#' @param sqldb character; path of Spirobank SQLite database
#' @param demo logical; include patient demographics and patient anthropometrics?
#' Default = FALSE.
#' @param inspiratory logical, include inspiratory maneauvers? Default = FALSE.
#' @param tzone character, time zone of location where measurments were recorded.
#' Default = 'America/New_York'.
#'
#' @return a tibble with spirometry values.
#' @export
#'
#' @examples
#' \dontrun {
#' read_spiro_sql <- function(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York')
#' }
#'
read_spiro_sql <- function(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York') {

  con <- DBI::dbConnect(RSQLite::SQLite(), sqldb)
  slist <- as.data.frame(DBI::dbListTables(con))
  sdf <- DBI::dbReadTable(con, slist[3, ])

  sdf <- sdf %>%
    tidyr::as_tibble() %>%
    dplyr::select(Z_PK, ZSESSION, ZDATE:ZQUALITYCODE, ZDEVICESERIALNUMBER, ZPATIENTID) %>%
    dplyr::rename(trial_index = Z_PK,
                  Spiro_Event = ZSESSION,
                  ID = ZPATIENTID,
                  PEF_min = ZPEF) %>%
    dplyr::mutate(Date_Time = lubridate::as_datetime(ZDATE, origin = '2001-01-01', tz = tzone),
                  Date = as.Date(Date_Time),
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

  sdf %>%
    dplyr::rename_with(., ~ (gsub("Z", "", .x, fixed = TRUE))) %>%
    dplyr::relocate(ID, Date_Time, Spiro_Event, Date, FEV1, PEF) %>%
    dplyr::relocate(trial_index, .after = dplyr::last_col()) %>%
    dplyr::mutate(dplyr::across(ID, ~ dplyr::na_if(., ''))) %>%
    tidyr::fill(ID, .direction = 'downup')

}

