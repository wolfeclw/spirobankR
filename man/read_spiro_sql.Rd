% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_spiro_sql.R
\name{read_spiro_sql}
\alias{read_spiro_sql}
\title{Read Spirobank SQLite database}
\usage{
read_spiro_sql(
  sqldb = NULL,
  demo = FALSE,
  inspiratory = FALSE,
  tzone = "America/New_York",
  test_threshold = NULL,
  participant_id = NULL,
  sample_col = NULL,
  sample_id = NULL
)
}
\arguments{
\item{sqldb}{character; path of Spirobank SQLite database}

\item{demo}{logical; include patient age, height, and weight? Default = FALSE.}

\item{inspiratory}{logical; include inspiratory maneuvers? Default = FALSE.}

\item{tzone}{character; time zone of location where measurements were recorded.
Default = 'America/New_York'.}

\item{test_threshold}{numeric; allows a user to partition Spirobank trials into discrete tests according to a user defined
threshold value (seconds). If the time difference between the last trial and and a new trial exceeds the threshold value,
the current trial is designated as belonging to a new test. For example, the threshold value is set at 600 seconds (10 minutes)
and a patient performs 6 trials  The first 3 trials are done consecutively within 10 minutes.  The fourth trial occurs
about one hour after the third.  The fifth and sixth trial are completed within 10 minutes of the fourth trial.
These six trials represent two separate testing sessions according the user defined threshold. Spirobank test number is indicated
under the column \code{spiro_test.}

If NULL, the default, \code{spiro_test} will be defined according to Spirobank software settings.}

\item{participant_id}{user defined string to denote a patient identifier. If NULL, the default, the patient identifier
entered into the MIR Spirobank app is used. Supplying a value to \code{participant_id} will overwrite the value entered
into the phone app. The patient identifier is listed under the \code{ID} column in the output data frame.}

\item{sample_col}{character; user defined character string specifying the name of the
column to denote a sample ID. This column is useful to denote repeated periods of
data collection by individual patents. Default is NULL.}

\item{sample_id}{user defined string to denote sample ID. If assigned, a
value must also be supplied to \code{sample_col}. Default is NULL.}
}
\description{
\code{read_spiro_sql} reads Spirobank patient data that has been exported to a SQL database.  In order to read the data properly,
all files (Spirobank.sqlite, Spirobank.sqlite-shm, Spirobank.sqlite-wal) must be in the directory, unless the SQL file has
been previously opened with another software (i.e. DB Browser).
}
\examples{
\dontrun{
read_spiro_sql(sqldb = NULL, demo = FALSE, inspiratory = FALSE, tzone = 'America/New_York',
               participant_id = NULL, sample_col = NULL, sample_id = NULL )
}

}
