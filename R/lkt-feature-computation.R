# Feature computation and spacing helpers for LKT.
# These functions are sourced before latency/model orchestration modules
# and collated before LKTfunctions.R during package installation.




#' Compute spacing predictors for LKT features
#'
#' @description Adds repetition-spacing history columns for one or more component columns. These columns are required by recency, spacing, and forgetting features in `LKT()`. If `CF..reltime.` or `CF..Time.` is missing, the function derives it from `Duration..sec.` when possible.
#'
#' @param data Data frame or data table containing `Anon.Student.Id`, `CF..ansbin.`, and each component named in `KCs`. Requires either `CF..reltime.` or `Duration..sec.`.
#' @param KCs Character vector of component columns for which spacing predictors should be computed.
#' @return The input data with added spacing-history columns for each component: `spacing`, `relspacing`, `prev`, `meanspacing`, `relmeanspacing`, and `spacinglagged` suffixes.
#' @examples
#' data(samplelkt)
#' data.table::setDT(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' spacing_data <- computeSpacingPredictors(samplelkt, "KC..Default.")
#' head(spacing_data$KC..Default.spacing)
#' @export
computeSpacingPredictors <- function(data, KCs) {
  required_cols <- c("Anon.Student.Id", "CF..ansbin.")
  missing_required <- setdiff(required_cols, colnames(data))
  if (length(missing_required) > 0) {
    stop("computeSpacingPredictors missing required column(s): ",
         paste(missing_required, collapse = ", "), call. = FALSE)
  }
  missing_components <- setdiff(KCs, colnames(data))
  if (length(missing_components) > 0) {
    stop("computeSpacingPredictors missing component column(s): ",
         paste(missing_components, collapse = ", "), call. = FALSE)
  }
  if (!("CF..reltime." %in% colnames(data))) {
    if (!("Duration..sec." %in% colnames(data))) {
      stop("computeSpacingPredictors needs CF..reltime. or Duration..sec. to compute relative time.",
           call. = FALSE)
    }
    data$CF..reltime. <- practiceTime(data)
  }
  if (!("CF..Time." %in% colnames(data))) {
    data$CF..Time. <- data$CF..reltime.
  }
  for (component in KCs) {
    data$index <- paste(data[[component]], data$Anon.Student.Id, sep = "")
    spacing_col <- paste0(component, "spacing")
    relspacing_col <- paste0(component, "relspacing")
    prev_col <- paste0(component, "prev")
    meanspacing_col <- paste0(component, "meanspacing")
    relmeanspacing_col <- paste0(component, "relmeanspacing")
    spacinglagged_col <- paste0(component, "spacinglagged")

    data[[spacing_col]] <- componentspacing(data, data$index, data$CF..Time.)
    data[[relspacing_col]] <- componentspacing(data, data$index, data$CF..reltime.)
    data[[prev_col]] <- componentprev(data, data$index, data$CF..ansbin.)
    data[[meanspacing_col]] <- meanspacingf(data, data$index, data[[spacing_col]])
    data[[relmeanspacing_col]] <- meanspacingf(data, data$index, data[[relspacing_col]])
    data[[spacinglagged_col]] <- laggedspacingf(data, data$index, data[[spacing_col]])
  }
  return(data)
}

lkt_apply_autokc <- function(data, component, feature_index, cluster_count,
                             randomize = FALSE) {
  CF..ansbin. <- Anon.Student.Id <- NULL

  aggdata <- data[, mean(CF..ansbin.),
                  by = list(get(component), Anon.Student.Id)]
  colnames(aggdata) <- c(component, "Anon.Student.Id", "CF..ansbin.")
  aggdata <- aggdata[with(aggdata, order(get(component))), ]

  mydata <- data.table::dcast(
    aggdata,
    stats::as.formula(paste(component, "~ Anon.Student.Id")),
    value.var = "CF..ansbin.")
  rownamesmydata <- mydata[[component]]
  mydata <- mydata[, -1]

  nm <- names(mydata)[colSums(is.na(mydata)) != 0]
  mydata[, (nm) := lapply(nm, function(x) {
    x <- get(x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  })]

  mydata <- log(mydata / (1 - mydata))
  mydata[mydata > 2] <- 2
  mydata[mydata < -2] <- -2
  rownames(mydata) <- rownamesmydata

  mydata[, names(mydata) := lapply(.SD, function(x) x - mean(x)),
         .SDcols = names(mydata)]
  df <- mydata[, as.matrix(.SD) %*% t(as.matrix(.SD)),
               .SDcols = names(mydata)]
  df <- df / nrow(df)
  rownames(df) <- 1:nrow(mydata)
  colnames(df) <- rownames(mydata)
  rownames(df) <- colnames(df)

  cm <- pam(df, cluster_count)
  KCmodel <- as.data.frame(cm$clustering)
  cluster_col <- paste("AC", feature_index, sep = "")
  colnames(KCmodel)[1] <- cluster_col
  KCmodel[[cluster_col]] <- as.character(KCmodel[[cluster_col]])
  if (isTRUE(randomize)) {
    KCmodel[[cluster_col]] <- sample(KCmodel[[cluster_col]])
  }

  KCmodel$rows <- rownames(KCmodel)
  merged_data <- merge(data, KCmodel, by.y = "rows", by.x = component, sort = FALSE)
  merged_data <- merged_data[order(merged_data$Anon.Student.Id, merged_data$CF..Time.), ]

  list(data = merged_data, component = cluster_col, model = KCmodel)
}

lkt_require_columns <- function(data, columns, context) {
  missing_columns <- setdiff(columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(context, " missing required column(s): ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

lkt_component_column <- function(data, component, suffix) {
  column <- paste0(component, suffix)
  lkt_require_columns(data, column, "LKT feature computation")
  data[[column]]
}

lkt_component_context_cache_key <- function(component) {
  paste0("component_context::", component)
}

lkt_apply_component_context <- function(data, context) {
  for (name in names(context)) {
    data[[name]] <- context[[name]]
  }
  data
}

lkt_extract_component_context <- function(data, include_counts) {
  columns <- c("index", "indexcomp")
  if (isTRUE(include_counts)) {
    columns <- c(columns, "cor", "icor")
  }
  columns <- columns[columns %in% names(data)]
  stats::setNames(lapply(columns, function(column) data[[column]]), columns)
}

lkt_ppe_static_context_names <- function(component) {
  prefix <- paste0(".lkt.", make.names(component), ".ppe.")
  c(
    tn = paste0(prefix, "Tn"),
    space = paste0(prefix, "space")
  )
}

lkt_prepare_ppe_static_context <- function(data, index, component) {
  context_columns <- lkt_ppe_static_context_names(component)
  if (all(context_columns %in% names(data))) {
    return(data)
  }
  spacing_lagged <- lkt_component_column(data, component, "spacinglagged")
  mintime <- ave(data$CF..Time., index, FUN = min)
  transformed_spacing <- ifelse(
    spacing_lagged == 0, 0, 1 / log(spacing_lagged + exp(1)))
  cumulative_spacing <- ave(
    transformed_spacing, index, FUN = function(x) cumsum(x))
  data[[context_columns[["tn"]]]] <- data$CF..Time. - mintime
  data[[context_columns[["space"]]]] <- ifelse(
    (data$cor + data$icor) <= 1,
    0,
    cumulative_spacing / (data$cor + data$icor - 1))
  data
}

lkt_prepare_component_context <- function(data, component, feature,
                                          context_cache = NULL) {
  cache_key <- lkt_component_context_cache_key(component)
  needs_counts <- !(feature %in% c("numer", "intercept"))
  if (!is.null(context_cache) && !is.null(context_cache[[cache_key]])) {
    cached_context <- context_cache[[cache_key]]
    has_counts <- all(c("cor", "icor") %in% names(cached_context))
    if (!needs_counts || has_counts) {
      data <- lkt_apply_component_context(data, cached_context)
      return(list(data = data, context_cache = context_cache))
    }
  }

  if (length(grep("%", component)) > 0) {
    parts <- strsplit(component, "%")[[1]]
    if (length(parts) != 3L) {
      stop("Component specification with '%' must have three parts: ",
           component, call. = FALSE)
    }
    lkt_require_columns(data, c(parts[1], parts[2]), "LKT component context")
    data$index <- paste(data[[parts[1]]], data$Anon.Student.Id, sep = "")
    data$indexcomp <- paste(data[[parts[1]]], sep = "")
    data$cor <- as.numeric(countOutcomeGen(
      data, data$index, "CORRECT", data[[parts[2]]], parts[3]))
    data$icor <- as.numeric(countOutcomeGen(
      data, data$index, "INCORRECT", data[[parts[2]]], parts[3]))
  } else if (length(grep("\\?", component)) > 0) {
    parts <- strsplit(component, "\\?")[[1]]
    if (length(parts) != 4L) {
      stop("Component specification with '?' must have four parts: ",
           component, call. = FALSE)
    }
    lkt_require_columns(data, c(parts[1], parts[3]), "LKT component context")
    data$indexcomp <- NULL
    data$cor <- as.numeric(countOutcomeOther(
      data, data$Anon.Student.Id, "CORRECT", data[[parts[3]]], parts[4],
      data[[parts[1]]], parts[2]))
    data$icor <- as.numeric(countOutcomeOther(
      data, data$Anon.Student.Id, "INCORRECT", data[[parts[3]]], parts[4],
      data[[parts[1]]], parts[2]))
  } else {
    lkt_require_columns(data, component, "LKT component context")
    component_values <- data[[component]]
    data$index <- paste(component_values, data$Anon.Student.Id, sep = "")
    data$indexcomp <- component_values
    if (!(feature %in% c("numer", "intercept"))) {
      data$cor <- countOutcome(data, data$index, "CORRECT")
      data$icor <- countOutcome(data, data$index, "INCORRECT")
    }
  }
  if (!is.null(context_cache)) {
    context_cache[[cache_key]] <- lkt_extract_component_context(
      data, include_counts = needs_counts)
    return(list(data = data, context_cache = context_cache))
  }
  data
}




#' Compute an LKT feature vector
#'
#' @description Computes one feature vector for a prepared LKT component context. This is a legacy public helper used by examples and advanced workflows; most users should call `LKT()` instead.
#'
#' @param data Data frame or data table after LKT component preparation.
#' @param feat Feature name to compute.
#' @param par1 First nonlinear feature parameter.
#' @param par2 Second nonlinear feature parameter.
#' @param index Student-by-component index vector.
#' @param index2 Component-level index vector.
#' @param par3 Third nonlinear feature parameter.
#' @param par4 Fourth nonlinear feature parameter.
#' @param par5 Fifth nonlinear feature parameter.
#' @param fcomp Component column name used by features that read component-level spacing or numeric data.
#' @return A vector suitable for use as one model-matrix input column.
#' @examples
#' data(samplelkt)
#' data.table::setDT(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' prepared <- computeSpacingPredictors(samplelkt, "KC..Default.")
#' prepared$history_index <- paste(prepared$KC..Default., prepared$Anon.Student.Id)
#' prepared$index <- prepared$history_index
#' prepared$indexcomp <- prepared$KC..Default.
#' prepared$cor <- countOutcomeold(prepared, "history_index", "CORRECT")
#' prepared$icor <- countOutcomeold(prepared, "history_index", "INCORRECT")
#' head(computefeatures(
#'   prepared, "lineafm", 0, 0,
#'   prepared$index, prepared$indexcomp, 0, 0, 0, "KC..Default."
#' ))
#' @export
computefeatures <- function(data, feat, par1, par2, index, index2,
                             par3, par4, par5, fcomp) {
  # dispatch on feature name to generate a numeric vector ---
  feat <- gsub("[$@]", "", feat)
  if (feat == "intercept") {
    return(as.character(index2))
  }
  if (feat == "numer") {
    lkt_require_columns(data, fcomp, "LKT numer feature")
    temp <- data[[fcomp]]
    return(temp)
  }
  if (feat == "lineafm") {
    return((data$cor + data$icor))
  }
  if (feat == "logafm") {
    return(log(1 + data$cor + data$icor))
  }
  if (feat == "powafm") {
    return((data$cor + data$icor)^par1)
  }
    if (feat == "recency") {
    data$rec <- lkt_component_column(data, fcomp, "spacing")
    return(ifelse(data$rec == 0, 0, data$rec^-par1))
  }
    if (feat == "recencytest") {
    data$rec <- lkt_component_column(data, fcomp, "spacing")
    data$stu <- lkt_component_column(data, fcomp, "previousstudy")
    return(ifelse(data$stu== 0, ifelse(data$rec == 0,0,data$rec^-par1),0))
  }
  if (feat == "recencystudy") {
    data$rec <- lkt_component_column(data, fcomp, "spacing")
    data$stu <- lkt_component_column(data, fcomp, "previousstudy")
    return(ifelse(data$stu== 1, ifelse(data$rec == 0,0,data$rec^-par1),0))
  }
  if (feat == "expdecafm") {
    return(ave(rep(1, length(data$CF..ansbin.)), index, FUN = function(x) slideexpdec(x, par1)))
  }
  if (feat == "base") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$CF..age. <- data$CF..Time. - data$mintime
    return(log(1 + data$cor + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }
  if (feat == "base2") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    return(log(1 + data$cor + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }
  if (feat == "base4") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    data$meanspace <- lkt_component_column(data, fcomp, "meanspacing")
    data$meanspacerel <- lkt_component_column(data, fcomp, "relmeanspacing")
    data$meanspace2 <- par2 * (data$meanspace - data$meanspacerel) + data$meanspacerel
    return(ifelse(data$meanspace <= 0,
                  par4 * log(1 + data$cor + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)),
                  data$meanspace2^par3 * log(1 + data$cor + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1))
    ))
  }
  if (feat == "ppe") {
    data <- lkt_prepare_ppe_static_context(data, index, fcomp)
    ppe_context <- lkt_ppe_static_context_names(fcomp)
    data$Nc <- (data$cor + data$icor)^par1
    data$Tn <- data[[ppe_context[["tn"]]]]
    data$space <- data[[ppe_context[["space"]]]]
    data$tw <- ave(data$Tn, index, FUN = function(x) slideppetw(x, par4))
    return(data$Nc * data$tw^-(par2 + par3 * data$space))
  }
  if (feat == "ppes") {
    data <- lkt_prepare_ppe_static_context(data, index, fcomp)
    ppe_context <- lkt_ppe_static_context_names(fcomp)
    data$Nc <- (data$cor)^(par1)
    data$Tn <- data[[ppe_context[["tn"]]]]
    data$space <- data[[ppe_context[["space"]]]]
    data$tw <- ave(data$Tn, index, FUN = function(x) slideppetw(x, par4))

    ppes_result <- data$Nc * data$tw^-(par2 + par3 * data$space)

    # Optional debug logging when PPES_DEBUG option is set
    if (getOption("PPES_DEBUG", FALSE)) {
      log_file <- "ppes_debug.txt"
      final_idx <- length(ppes_result)
      if (final_idx > 0) {
        # Extract student ID from index if possible
        student_id <- if(length(index) >= final_idx) strsplit(index[final_idx], fcomp)[[1]][2] else "unknown"

        # Get item ID if available in data
        item_id <- if("CF..Stim.File.Index." %in% names(data)) data$CF..Stim.File.Index.[final_idx] else "unknown"

        # Only log the key inputs actually used by PPES
        final_cor <- data$cor[final_idx]
        final_icor <- data$icor[final_idx]
        cf_time_vector <- paste(round(data$CF..Time., 2), collapse = ",")
        final_ppes <- round(ppes_result[final_idx], 6)

        cat(paste(
          as.character(Sys.time()),  # timestamp
          student_id,                # student
          fcomp,                     # component/KC
          item_id,                   # item being computed for
          final_cor,                 # cor value used
          final_icor,                # icor value used
          cf_time_vector,            # CF..Time. vector used
          final_ppes,                # final ppes result
          sep = "\t"
        ), "\n", file = log_file, append = TRUE)
      }
    }

    return(ppes_result)
  }
  if (feat == "ppef") {
    data <- lkt_prepare_ppe_static_context(data, index, fcomp)
    ppe_context <- lkt_ppe_static_context_names(fcomp)
    data$Nc <- (data$icor)^(par1)
    data$Tn <- data[[ppe_context[["tn"]]]]
    data$space <- data[[ppe_context[["space"]]]]
    data$tw <- ave(data$Tn, index, FUN = function(x) slideppetw(x, par4))
    return(data$Nc * data$tw^-(par2 + par3 * data$space))
  }
  if (feat == "base5suc") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    data$meanspace <- lkt_component_column(data, fcomp, "meanspacing")
    data$meanspacerel <- lkt_component_column(data, fcomp, "relmeanspacing")
    data$meanspace2 <- par2 * (data$meanspace - data$meanspacerel) + (data$meanspacerel)
    return(ifelse(data$meanspace <= 0,
                  par4 * 10 * (log((par5 * 10) + data$cor)) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)),
                  data$meanspace2^par3 * (log((par5 * 10) + data$cor)) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1))
    ))
  }
  if (feat == "base5fail") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    data$meanspace <- lkt_component_column(data, fcomp, "meanspacing")
    data$meanspacerel <- lkt_component_column(data, fcomp, "relmeanspacing")
    data$meanspace2 <- par2 * (data$meanspace - data$meanspacerel) + (data$meanspacerel)
    return(ifelse(data$meanspace <= 0,
                  par4 * 10 * (log((par5 * 10) + data$icor)) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)),
                  data$meanspace2^par3 * (log((par5 * 10) + data$icor)) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1))
    ))
  }

  if (feat == "dashafm") {
    data$x <- ave(data$CF..Time., index, FUN = function(x) countOutcomeDash(x, par1))
    return(log(1 + data$x))
  }
  if (feat == "dashsuc") {
    dataV <- data.frame(data$CF..Time., data$Outcome, index)
    h <- countOutcomeDashPerf(dataV, "CORRECT", par1)
    return(log(1 + h))
  }
  # single factor dynamic features
  if (feat == "diffrelcor1") {
    return(countRelatedDifficulty1(data, data$index, "CORRECT"))
  }
  if (feat == "diffrelcor2") {
    return(countRelatedDifficulty2(data, data$index, "CORRECT"))
  }
  if (feat == "diffcor1") {
    return(countOutcomeDifficulty1(data, data$index, "CORRECT"))
  }
  if (feat == "diffcor2") {
    return(countOutcomeDifficulty2(data, data$index, "CORRECT"))
  }
  if (feat == "diffcorComp") {
    return(countOutcomeDifficulty1(data, data$index, "CORRECT") - countOutcomeDifficulty2(data, data$index, "CORRECT"))
  }
  if (feat == "diffincorComp") {
    return(countOutcomeDifficulty1(data, data$index, "INCORRECT") - countOutcomeDifficulty2(data, data$index, "INCORRECT"))
  }
  if (feat == "diffallComp") {
    return(countOutcomeDifficultyAll1(data, data$index) - countOutcomeDifficultyAll2(data, data$index))
  }
  if (feat == "diffincor1") {
    return(countOutcomeDifficulty1(data, data$index, "INCORRECT"))
  }
  if (feat == "diffincor2") {
    return(countOutcomeDifficulty2(data, data$index, "INCORRECT"))
  }
  if (feat == "diffall1") {
    return(countOutcomeDifficultyAll1(data, data$index))
  }
  if (feat == "diffall2") {
    return(countOutcomeDifficultyAll2(data, data$index))
  }
  if (feat == "logsuc") {
    return(log(1 + data$cor))
  }
  if (feat == "logsucadj") {
    return(log(1 + data$cor)/(par1*20+log(1 + data$cor)))
  }
  if (feat == "linesuc") {
    return(data$cor)
  }
  if (feat == "logfail") {
    return(log(1 + data$icor))
  }
  if (feat == "linefail") {
    return(data$icor)
  }
  if (feat == "recencyfail") {
    data$rec <- lkt_component_column(data, fcomp, "spacing")
    data$prev <- lkt_component_column(data, fcomp, "prev")
    return(ifelse(data$rec == 0, 0, (1 - data$prev) * data$rec^-par1))
  }
  if (feat == "recencysuc") {
    data$rec <- lkt_component_column(data, fcomp, "spacing")
    data$prev <- lkt_component_column(data, fcomp, "prev")
    return(ifelse(data$rec == 0, 0, data$prev * data$rec^-par1))
  }
  if (feat == "expdecsuc") {
    return(ave(data$CF..ansbin., index, FUN = function(x) slideexpdec(x, par1)))
  }
  if (feat == "expdecfail") {
    return(ave(1 - data$CF..ansbin., index, FUN = function(x) slideexpdec(x, par1)))
  }
  if (feat == "basesuc") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$CF..age. <- data$CF..Time. - data$mintime
    return(log(1 + data$cor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }
  if (feat == "basefail") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$CF..age. <- data$CF..Time. - data$mintime
    return(log(1 + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }
  if (feat == "base2fail") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    return(log(1 + data$icor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }
  if (feat == "base2suc") {
    data$mintime <- ave(data$CF..Time., index, FUN = min)
    data$minreltime <- ave(data$CF..reltime., index, FUN = min)
    data$CF..trueage. <- data$CF..Time. - data$mintime
    data$CF..intage. <- data$CF..reltime. - data$minreltime
    data$CF..age. <- (data$CF..trueage. - data$CF..intage.) * par2 + data$CF..intage.
    return(log(1 + data$cor) * ave(data$CF..age., index, FUN = function(x) baselevel(x, par1)))
  }

  # double factor dynamic features
  if (feat == "linecomp") {
    return((data$cor - data$icor))
  }
  if (feat == "logit") {
    return(log((.1 + par1 * 30 + data$cor) / (.1 + par1 * 30 + data$icor)))
  }
  if (feat == "errordec") {
    return(ave(data$pred_ed - data$CF..ansbin., index, FUN = function(x) slideerrordec(x, par1)))
  }
  if (feat == "propdec") {
    return(ave(data$CF..ansbin., index, FUN = function(x) slidepropdec(x, par1)))
  }
  if (feat == "propdec2") {
    return(ave(data$CF..ansbin., index, FUN = function(x) slidepropdec2(x, par1)))
  }
  if (feat == "logitdec") {
    return(ave(data$CF..ansbin., index, FUN = function(x) slidelogitdec(x, par1)))
  }

  if (feat == "logitdecevol") {
    return(ave(data$CF..ansbin., index2, FUN = function(x) slidelogitdecfree(x, par1)))
  }
  if (feat == "baseratepropdec") {
    return(as.numeric(ave(index2, data$Anon.Student.Id, FUN = function(x) baserateslidedec(x, par1))))
  }
  if (feat == "prop") {
    ifelse(is.nan(data$cor / (data$cor + data$icor)), .5, data$cor / (data$cor + data$icor))
  }
}

getFeedDur <- function(data, index) {
  temp <- rep(0, length(data$CF..ansbin.))
  for (i in unique(index)) {
    le <- length(data$time_to_answer[index == i])
    subtemp <- data$time_since_prior_probe[index == i] - data$time_to_answer_inferred[index == i]
    subtemp <- subtemp[2:(le - 1)]
    subtemp <- c(subtemp, median(subtemp, na.rm = TRUE))
    # if huge outlier make median for subject from that subject from that index
    cutoff <- which(subtemp > 3600)
    subtemp[cutoff] <- median(subtemp[-cutoff], na.rm = TRUE)
    # function returns NA for feedDur if subject only did one trial in index
    # replaced with Median (overall) outside function
    temp[index == i] <- subtemp
  }
  return(temp)
}

# convenience function
right <- function(string, char) {
  substr(string, nchar(string) - (char - 1), nchar(string))
}

#' Count prior outcomes within an index
#'
#' @description Legacy exported helper that counts previous rows whose `Outcome` value equals `response` within each `index` group. It is retained for compatibility with older examples; new package code uses internal helpers.
#'
#' @param data Data frame or data table containing an `Outcome` column.
#' @param index Grouping column name or data.table `by` expression that defines the history stream for each row.
#' @param response Outcome value to count, such as `"CORRECT"` or `"INCORRECT"`.
#' @return Numeric vector of lagged cumulative counts, one value per row.
#' @examples
#' data(samplelkt)
#' data.table::setDT(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' samplelkt$history_index <- paste(samplelkt$KC..Default., samplelkt$Anon.Student.Id)
#' head(countOutcomeold(samplelkt, "history_index", "CORRECT"))
#' @export
countOutcomeold <- function(data, index, response) {
  temp <- Outcome <- NULL
  data[, temp := cumsum(Outcome == response), by = index]
  data[Outcome == response, temp := temp - 1, by = index]
  data$temp
}

countOutcome <- function(data, index, response) {
  temp <- Outcome <- NULL
  data[, temp := cumsum(Outcome == response), by = index]
  data[Outcome == response, temp := temp - 1]
  return(data$temp)
}


countOutcomeDash <- function(times, scalev) {
  l <- length(times)
  v1 <- c(rep(0, l))
  v2 <- c(rep(0, l))
  v1[1] <- 0
  v2[1] <- v1[1] + 1
  if (l > 1) {
    spacings <- times[2:l] - times[1:(l - 1)]
    for (i in 2:l) {
      v1[i] <- v2[i - 1] * exp(-spacings[i - 1] / (scalev * 86400))
      v2[i] <- v1[i] + 1
    }
  }
  return(v1)
}

countOutcomeDashPerf <- function(datav, seeking, scalev) {
  temp <- rep(0, length(datav[, 1]))

  groups <- split(seq_len(nrow(datav)), datav[, 3], drop = TRUE)
  for (idx in groups) {
    times <- as.numeric(datav[idx, 1])
    l <- length(times)
    v1 <- c(rep(0, l))
    v2 <- c(rep(0, l))
    r <- as.character(datav[idx, 2]) == seeking
    v1[1] <- 0
    v2[1] <- v1[1] + r[1]
    if (l > 1) {
      spacings <- times[2:l] - times[1:(l - 1)]
      for (i in 2:l) {
        v1[i] <- v2[i - 1] * exp(-spacings[i - 1] / (scalev * 86400))
        v2[i] <- v1[i] + r[i]
      }
    }
    temp[idx] <- v1
  }
  return(temp)
}

# count confusable outcome difficulty effect
countOutcomeDifficulty1 <- function(data, index, r) {
  temp <- data$curvefeat
  temp <- ifelse(data$Outcome == r, temp, 0)
  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

countRelatedDifficulty1 <- function(data, index, r) {
  temp <- (data$contran)
  temp <- ifelse(data$Outcome == r, temp, 0)
  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

countRelatedDifficulty2 <- function(data, index, r) {
  temp <- (data$contran)^2
  temp <- ifelse(data$Outcome == r, temp, 0)
  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

countOutcomeDifficulty2 <- function(data, index, r) {
  temp <- data$curvefeat^2
  temp <- ifelse(data$Outcome == r, temp, 0)
  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

countOutcomeDifficultyAll1 <- function(data, index) {
  temp <- data$curvefeat

  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

countOutcomeDifficultyAll2 <- function(data, index) {
  temp <- data$curvefeat^2

  data$temp <- ave(temp, index, FUN = function(x) as.numeric(cumsum(x)))
  data$temp <- data$temp - temp
  data$temp
}

# specific cause to self
# notation indexfactor%sourcefactor%sourcevalue
# for the index (student by KC) count prior values if a particular source column equals value
# differential KC learning for each item within KC
countOutcomeGen <- function(data, index, item, sourcecol, sourc) {
  data$tempout <- paste(data$Outcome, sourcecol)
  item <- paste(item, sourc)
  data$temp <- as.numeric(ave(as.character(data$tempout), index, FUN = function(x) as.numeric(cumsum(tolower(x) == tolower(item)))))
  data$temp <- data$temp - as.numeric(tolower(as.character(data$tempout)) == tolower(item))
  as.numeric(data$temp)
}

# notation targetcol?whichtarget?sourcecol?whichsource
# specific cause to any
# for the index (student by KC) count prior values if a particular source column equals value
#      but only when a particular target value is in the target column is present
# item to item learning within skill
countOutcomeOther <- function(data, index, item, sourcecol, sourc, targetcol, target) {
  data$tempout <- paste(data$Outcome, sourcecol)
  item <- paste(item, sourc)
  targetcol <- as.numeric(targetcol == target)
  data$temp <- ave(as.character(data$tempout), index, FUN = function(x) as.numeric(cumsum(tolower(x) == tolower(item))))
  data$temp[tolower(as.character(data$tempout)) == tolower(item)] <- as.numeric(data$temp[tolower(as.character(data$tempout)) == tolower(item)]) - 1
  as.numeric(data$temp) * targetcol
}

# computes practice times using trial durations only
practiceTime <- function(data) {
  if (!("Anon.Student.Id" %in% colnames(data)) ||
      !("Duration..sec." %in% colnames(data))) {
    stop("practiceTime requires Anon.Student.Id and Duration..sec. columns.",
         call. = FALSE)
  }
  as.numeric(ave(data$Duration..sec., data$Anon.Student.Id, FUN = function(x) {
    if (length(x) > 1) {
      c(0, utils::head(cumsum(x), -1))
    } else {
      0
    }
  }))
}

# computes spacing from prior repetition for index (in seconds)
componentspacing <- function(data, index, times) {

  temp <- numeric(nrow(data)) # initialize temp as a numeric vector

  # calculate the differences within each group and assign to temp
  temp <- ave(times, index, FUN=function(x) c(0, diff(x)))

  return(temp)
}

prevstudy <- function(data, index, outcomes) {

  temp <- logical(nrow(data)) # initialize temp as a numeric vector

  # calculate the differences within each group and assign to temp
  temp <- ave(outcomes, index, FUN=function(x) c(F, head(x,-1)=="STUDY"))

  return(temp)
}


componentprev <- function(data, index, answers) {
  prev_answers <- ave(answers, index, FUN = function(x) c(0, head(x, -1)))
  return(prev_answers)
}

# computes mean spacing
meanspacingf <- function(data, index, spacings) {  temp <- ave(spacings, index, FUN= function(x) {
    j <- length(x)
    tempx <- rep(0,j)
    if (j > 1) {
      tempx[2] <- -1
    }
    if (j == 3) {
      tempx[3] <- x[2]
    }
    if (j > 3) {
      tempx[3:j] <- cumsum(x[2:(j - 1)]) / (1:(j - 2))
    }
    tempx
  })

  return(temp)
}

laggedspacingf <- function(data, index, spacings) {

  temp <- ave(spacings, index, FUN=function(x) c(0, head(x, -1)))
  return(temp)
}

errordec <- function(v, d) {
  w <- length(v)
  sum((c(0, v[1:w]) * d^((w):0)) / sum(d^((w + 1):0)))
}

slideerrordec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- errordec(x[1:i], d)
  }
  return(c(0, v[1:length(x) - 1]))
}

# exponetial decy for trial
expdec <- function(v, d) {
  w <- length(v)
  sum(v[1:w] * d^((w - 1):0))
}

# 3 failed ghosts RPFA success function
propdec2 <- function(v, d) {
  w <- length(v)
  sum((v[1:w] * d^((w - 1):0)) / sum(d^((w + 2):0)))
}

# 2 failed and 1 success ghost RPFA success function bug fixed 10/17/23 upload to github
propdec <- function(v, d) {
  w <- length(v)
  #  (cat(v,d,w,"\n"))
  corv <- sum(c(1, v[1:w]) * d^(w:0))
  incorv <- sum(c(1, abs(v[1:w] - 1)) * d^(w:0))
  corv / (corv+incorv)
}

logitdec <- function(v, d) {
  w <- length(v)
  #  (cat(v,d,w,"\n"))
  corv <- sum(c(1, v[1:w]) * d^(w:0))
  incorv <- sum(c(1, abs(v[1:w] - 1)) * d^(w:0))
  log(corv / incorv)
}


logitdec4 <- function(v, d) {
  w <- length(v)
  #  (cat(v,d,w,"\n"))
  corv <- sum(c(1,1,1,1, v[1:w]) * d^((w+3):0))
  incorv <- sum(c(1,1,1,1, abs(v[1:w] - 1)) * d^((w+3):0))
  log(corv / incorv)
}

slidelogitdecfree <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- logitdec(x[1:i], d)
  }
  return(c(0, v[1:length(x) - 1]))
}

slidelogitdec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- logitdec(x[max(1, i - 59):i], d)
  }
  return(c(0, v[1:length(x) - 1]))
}

baseratepropdec <- function(v, d) {
  w <- length(v)
  targetvalue <- v[w]
  v <- v==targetvalue
  corv <- sum(c(1, v[1:w]) * d^(w:0))
  incorv <- sum(c(1, abs(v[1:w] - 1)) * d^(w:0))
  log(corv / incorv)
}

baseratepropdec <- function(v, d) {
  w <- length(v)
  targetvalue <- v[w]
  #print(v)
  v <- v==targetvalue
 #print(v)
  corv <- sum((v[1:w-1]) * d^((w-1):1))
  incorv <- sum(d^((w + 2):1))
#  print(corv/incorv)
  (corv / incorv)
}

baserateslidedec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- baseratepropdec(x[1:i], d)
  }
  return(v[1:length(x) ])
}

# exponential decay for sequence
slideexpdec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- expdec(x[1:i], d)
  }
  return(c(0, v[1:length(x) - 1]))
}

# proportion exponential decay for sequence
slidepropdec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- propdec(x[1:i], d)
  }
  return(c(.5, v[1:length(x) - 1]))
}

slidepropdec2 <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- propdec2(x[1:i], d)
  }
  return(c(0, v[1:length(x) - 1]))
}



# PPE weights
ppew <- function(times, wpar) {
  times^-wpar *
    (1 / sum(times^-wpar))
}

# PPE time since practice
ppet <- function(times) {
  times[length(times)] - times
}

# ppe adjusted time for each trial in sequence
ppetw <- function(x, d) {
  v <- length(x)
  ppetv <- ppet(x)[1:(v - 1)]
  ppewv <- ppew(ppetv, d)
  ifelse(is.nan(crossprod(ppewv[1:(v - 1)], ppetv[1:(v - 1)])),
         1,
         crossprod(ppewv[1:(v - 1)], ppetv[1:(v - 1)])
  )
}

# PPE adjusted times for entire sequence
slideppetw <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x)) {
    v[i] <- ppetw(x[1:i], d)
  }
  return(c(v[1:length(x)]))
}

# tkt main function
baselevel <- function(x, d) {
  l <- length(x)
  return(c(0, x[2:l]^-d)[1:l])
}

# find the time that corresponds to the longest break in the sequence
splittimes <- function(times) {
  (match(max(rank(diff(times))), rank(diff(times))))
}
