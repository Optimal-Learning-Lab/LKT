#' Item age and prior spacing for GRU timing inputs
#'
#' Compute timing inputs on the complete exposure log, including tests, drills and
#' studies, before selecting scored response rows. Outcomes are neither needed nor
#' read. The returned data preserve input order and contain two additional columns.
#'
#' @param data Complete exposure data frame, including unscored study events.
#' @param time_col Numeric or POSIXct exposure timestamp column.
#' @param item_cols Columns identifying an item, independent of GRU history grouping.
#' @param subject_col Student identifier column.
#' @param time_units Units of numeric timestamps: seconds, minutes, hours, or days.
#' @details Item age is current time minus the first exposure time for this student
#'   and item. Mean prior spacing is the arithmetic mean of consecutive gaps among
#'   strictly earlier rows for this student and item; the current timestamp is excluded.
#'   It is missing (NA) with fewer than two prior exposures.
#'   Rows are processed in timestamp order within student;
#'   ties retain input order. Every supplied row counts as an exposure.
#'
#'   All values are returned in seconds. GRUModel's default time_feature_cols
#'   use both returned columns alongside recency, and its default time_missing_cols
#'   retains undefined prior spacing as missing. Prepare these columns before
#'   fitting the default GRU; alternative inputs can be selected explicitly.
#'   During continuation, compute the
#'   same columns from the retained complete exposure log plus the candidate row;
#'   append that candidate to the log only when its exposure actually occurs.
#' @return A data frame with gru_item_age_seconds and
#'   gru_mean_prior_spacing_seconds, in the original row order.
#' @export
LKTGRUTimeFeatures <- function(data,time_col='CF..Time.',item_cols='KC..Default.',
                               subject_col='Anon.Student.Id',time_units='seconds') {
  # Use the same timestamp validation and unit conversion as the GRU. Item identities
  # are explicit: interleaved practice of other items must not count as repetitions.
  for(value in list(item_cols,subject_col,time_col))
    if(!is.character(value) || !length(value) || anyNA(value) ||
       any(!nzchar(value)) || anyDuplicated(value))stop('Invalid timing identity columns.',call.=FALSE)
  if(length(subject_col)!=1L || length(time_col)!=1L || subject_col %in% item_cols ||
     !is.character(time_units) || length(time_units)!=1L || is.na(time_units) ||
     !time_units %in% c('seconds','minutes','hours','days'))stop('Invalid timing configuration.',call.=FALSE)
  result <- as.data.frame(data)
  outputs <- c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')
  if(any(outputs %in% names(result)))stop('Timing output columns already exist; supply the original exposure log.',call.=FALSE)
  times <- lkt_gru_time(result,list(time_col=time_col,time_units=time_units))
  keys <- lkt_gru_keys(result,c(subject_col,item_cols))
  age <- numeric(nrow(result));spacing <- rep(NA_real_,nrow(result))
  for(idx in split(seq_len(nrow(result)),keys)) {
    idx <- idx[order(times[idx],seq_along(idx),method='radix')]
    t <- times[idx];n <- length(idx)
    age[idx] <- t-t[1L]
    # Consecutive prior gaps telescope to (last prior - first)/(prior count - 1).
    # Indexing only prior rows makes the current and future timestamps irrelevant
    # to this trial's mean spacing, without constructing a quadratic gap matrix.
    if(n>=3L)spacing[idx[3L:n]] <- (t[2L:(n-1L)]-t[1L])/seq_len(n-2L)
  }
  result[[outputs[1L]]] <- age
  result[[outputs[2L]]] <- spacing
  result
}
