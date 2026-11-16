\name{LKTGRUCandidateTimeFeatures}
\alias{LKTGRUCandidateTimeFeatures}
\title{Timing inputs for independent GRU candidates}
\usage{LKTGRUCandidateTimeFeatures(history, data, time_col = "CF..Time.",
  item_cols = "KC..Default.", subject_col = "Anon.Student.Id", time_units = "seconds")}
\arguments{
\item{history}{Complete committed exposure log, including unscored exposures.}
\item{data}{Independent candidate rows. They are never added to history.}
\item{time_col}{Numeric or POSIXct timestamp column.}
\item{item_cols}{Columns identifying an item.}
\item{subject_col}{Student identity column.}
\item{time_units}{Units of numeric timestamps: seconds, minutes, hours, or days.}
}
\value{Candidate rows with gru_item_age_seconds and gru_mean_prior_spacing_seconds.}
\description{Prepare the committed exposure history once, then compute every candidate's
item age and mean prior spacing independently. Equivalent to appending each candidate
separately to the same history and applying LKTGRUTimeFeatures. No candidate becomes
another candidate's prior exposure. Candidates preceding committed item history fail.}
