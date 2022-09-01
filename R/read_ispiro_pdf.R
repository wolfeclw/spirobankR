
#' Read iSpirometry PDF Files
#'
#'`read_ispirometry_pdf()` reads exported PDF files from the iSpirometry phone app. Text is
#'extracted using the Tesseract OCR engine ([Tesseract](https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html))
#'
#' @param pdf character; path of of iSpirometry PDF export.
#' @param pages numeric; vector of page numbers to extract. If NULL, the default, all pages are extracted.
#' @param tzone character; time zone of location where measurements were recorded.
#' Default = 'America/New_York'.
#' @param participant_id user defined string to denote a patient identifier.
#' If specified, a new column is created ('ID') listing the string. Default is NULL.
#' @param sample_col character; user defined character string specifying the name of the
#' column to denote a sample ID. This column is useful to denote repeated periods of
#' data collection by individual patents. Default is NULL.
#' @param sample_id user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL.
#' @param test_threshold numeric; allows a user to partition Spirobank trials into discrete
#' tests according to a user defined threshold value (seconds). If the time difference
#' between the last trial and and a new trial exceeds the threshold value, the current
#' trial is designated as belonging to a new test. For example, the threshold value is
#' set at 600 seconds (10 minutes) and a patient performs 6 trials. The first 3 trials are
#' done consecutively within 10 minutes. The fourth trial occurs about one hour after the
#' third. The fifth and sixth trial are completed within 10 minutes of the fourth trial. These
#' six trials represent two separate testing sessions according the user defined threshold.
#' Spirobank test number is indicated under the column `spiro_test.`
#'#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' read_ispiro_pdf(pdf, pages = NULL, tzone = 'America/New_York', participant_id = NULL,
#'                 sample_col = NULL, sample_id = NULL, test_threshold = NULL)
#' }
#'
#'
read_ispirometry_pdf <- function(pdf, pages = NULL, tzone = 'America/New_York',
                                 participant_id = NULL,sample_col = NULL, sample_id = NULL,
                                 test_threshold = NULL) {

  if(sum(!is.null(sample_col), !is.null(sample_id)) == 1) {
    stop('  Both `sample_col` and `sample_id` must have values.', call. = FALSE)
  }

  t <- pdftools::pdf_ocr_text(pdf, pages = pages, dpi = 300)

  t_lines <- stringr::str_c(t, collapse = '')
  t_split <- unlist(stringr::str_split(t_lines, '\n'))

  t_minus_toprows <- t_split[-c(1:5)]

  t_FEV_lines <- -grep("FEV", t_minus_toprows)
  t_minus_FlowLines <- t_minus_toprows[t_FEV_lines]
  nums_lines <- grep("[0-9]+", t_minus_FlowLines)
  t_num_only <- t_minus_FlowLines[nums_lines]

  ispiro_lines <- -grep("iSpirometry", t_num_only, ignore.case = TRUE)
  spiro_value_timeday <- t_num_only[ispiro_lines]

  date_lines <- seq(1,length(spiro_value_timeday) -1 ,2)
  spiro_date_lines <- spiro_value_timeday[date_lines]

  spiro_dt_string <- spiro_date_lines %>%
    gsub('at|,', '', .) %>%
    stringr::str_split(' ') %>%
    purrr::map(., `[`, 1:6) %>%
    purrr::map_chr(., ~stringr::str_c(., collapse = ' ')) %>%
    lubridate::parse_date_time(., '%B%d%Y %I:%M:%S %p', tz = tzone)

  d_spiro_dt <- tibble::enframe(spiro_dt_string, name = NULL, value = 'date_time')
  d_spiro_dt <- d_spiro_dt %>%
    dplyr::mutate(date = as.Date(date_time, tz = tzone),)


  value_lines <- seq(2,length(spiro_value_timeday) ,2)

  d_spiro_values <- spiro_value_timeday[value_lines] %>%
    stringr::str_trim(side = "both") %>%
    # stringr::str_replace_all(pattern = "/", "") %>%
    # stringr::str_replace_all('\\$', '3') %>%
    stringr::str_split(., ' ') %>%
    purrr::map(., ~tibble::enframe(.)) %>%
    purrr::map_df(., tidyr::pivot_wider) %>%
    purrr::set_names(.,  c('FVC', 'FEV1', 'FEV6', 'FEV1FVC', 'FEF2575', 'PEF')) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~round(., digits = 2))) %>%
    dplyr::mutate(PEF = ifelse(nchar(PEF) > 3, substr(d_spiro$PEF, 2, 4), PEF))

  d_spiro <- dplyr::bind_cols(d_spiro_dt, d_spiro_values)

  d_spiro <- d_spiro %>%
    dplyr::arrange(date_time) %>%
    dplyr::mutate(trial_number = dplyr::row_number()) %>%
    dplyr::relocate(trial_number, .after = date)

  if(!is.null(participant_id)) {
    d_spiro$ID <- participant_id
    d_spiro <- dplyr::relocate(d_spiro, ID)
  }

  if(sum(!is.null(sample_col), !is.null(sample_id)) == 2) {
    d_spiro <- d_spiro %>%
      dplyr::mutate({{sample_col}} := {{sample_id}}) %>%
      dplyr::relocate({{sample_col}}, .before = date_time)
  }

  if(is.null(test_threshold)) {
    d_spiro <- d_spiro
    warning('  Spirometry trials have not been classified into tests.', call. = FALSE)
  } else {
    d_spiro <- d_spiro %>%
      dplyr::arrange(date_time) %>%
      dplyr::mutate(lag_tm = dplyr::lag(date_time),
                    tdiff_sec = (lubridate::ymd_hms(date_time) - lubridate::ymd_hms(lag_tm)),
                    break_yn = ifelse(is.na(tdiff_sec), 0,
                                      ifelse(tdiff_sec > test_threshold, 1, 0)),
                    spiro_test = cumsum(break_yn) + 1) %>%
      dplyr::select(-c(lag_tm, tdiff_sec, break_yn)) %>%
      dplyr::relocate(spiro_test, .before = FVC)

    message(paste('The input data includes', max(d_spiro$spiro_test), 'tests as defined by `test_threshold` criteria.'))
  }

  d_spiro
}
