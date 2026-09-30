#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(RNetCDF)
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)

usage <- paste(
  "Usage:",
  "Rscript nwm_outlet_flow_workflow.R [--input FILE] [--output FILE]",
  "[--start-year YYYY] [--end-year YYYY] [--max-hours N] [--timeout SEC]",
  "",
  "Defaults:",
  "  --input  random_forest_data_new.csv",
  "  --output nwm_outlet_flow_summary.csv",
  "  --start-year 2010",
  "  --end-year   2010",
  "",
  "Notes:",
  "  This script targets the NOAA NWM v2.1 retrospective CHRTOUT files",
  "  hosted at https://noaa-nwm-retrospective-2-1-pds.s3.amazonaws.com/.",
  sep = "\n"
)

parse_args <- function(args) {
  opts <- list(
    input = "random_forest_data_new.csv",
    output = "nwm_outlet_flow_summary.csv",
    start_year = 2010L,
    end_year = 2010L,
    max_hours = NA_integer_,
    timeout = 120L
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--help")) {
      cat(usage, "\n")
      quit(save = "no", status = 0)
    }

    if (!startsWith(arg, "--")) {
      stop("Unexpected argument: ", arg, call. = FALSE)
    }

    if (i == length(args)) {
      stop("Missing value for ", arg, call. = FALSE)
    }

    value <- args[[i + 1L]]
    key <- sub("^--", "", arg)

    if (identical(key, "input")) {
      opts$input <- value
    } else if (identical(key, "output")) {
      opts$output <- value
    } else if (identical(key, "start-year")) {
      opts$start_year <- as.integer(value)
    } else if (identical(key, "end-year")) {
      opts$end_year <- as.integer(value)
    } else if (identical(key, "max-hours")) {
      opts$max_hours <- as.integer(value)
    } else if (identical(key, "timeout")) {
      opts$timeout <- as.integer(value)
    } else {
      stop("Unknown option: ", arg, call. = FALSE)
    }

    i <- i + 2L
  }

  if (is.na(opts$start_year) || is.na(opts$end_year)) {
    stop("Both --start-year and --end-year must be integers.", call. = FALSE)
  }

  if (opts$end_year < opts$start_year) {
    stop("--end-year must be greater than or equal to --start-year.", call. = FALSE)
  }

  opts
}

build_timestamp_index <- function(start_year, end_year, max_hours = NA_integer_) {
  start_ts <- as.POSIXct(sprintf("%d-01-01 00:00:00", start_year), tz = "UTC")
  end_ts <- as.POSIXct(sprintf("%d-12-31 23:00:00", end_year), tz = "UTC")
  timestamps <- seq(from = start_ts, to = end_ts, by = "hour")

  if (!is.na(max_hours)) {
    timestamps <- head(timestamps, max_hours)
  }

  timestamps
}

build_chrtout_url <- function(timestamp) {
  year <- format(timestamp, "%Y")
  stamp <- format(timestamp, "%Y%m%d%H%M")
  sprintf(
    "https://noaa-nwm-retrospective-2-1-pds.s3.amazonaws.com/model_output/%s/%s.CHRTOUT_DOMAIN1.comp",
    year,
    stamp
  )
}

safe_download <- function(url, destfile, timeout = 120L) {
  tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        quiet = TRUE,
        cacheOK = FALSE,
        method = "libcurl",
        extra = c("--connect-timeout", as.character(timeout))
      )
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

read_first_available_var <- function(nc, candidates) {
  for (candidate in candidates) {
    value <- tryCatch(RNetCDF::var.get.nc(nc, candidate), error = function(e) NULL)
    if (!is.null(value)) {
      return(value)
    }
  }

  stop(
    "None of these variables were found in the NetCDF file: ",
    paste(candidates, collapse = ", "),
    call. = FALSE
  )
}

read_nwm_chrtout <- function(path) {
  nc <- RNetCDF::open.nc(path)
  on.exit(RNetCDF::close.nc(nc), add = TRUE)

  feature_ids <- read_first_available_var(
    nc,
    c("feature_id", "station_id", "station")
  )

  streamflow <- read_first_available_var(
    nc,
    c("streamflow", "qlink1", "streamflow_troute")
  )

  list(
    feature_ids = as.integer(feature_ids),
    streamflow = as.numeric(streamflow)
  )
}

summarise_nwm_streamflow <- function(site_df, timestamps, timeout = 120L) {
  comids <- as.integer(site_df$comid)
  years <- sort(unique(as.integer(format(timestamps, "%Y"))))

  flow_sum <- matrix(0, nrow = nrow(site_df), ncol = length(years))
  flow_count <- matrix(0L, nrow = nrow(site_df), ncol = length(years))
  colnames(flow_sum) <- years
  colnames(flow_count) <- years

  first_good_file <- NULL
  matched_feature_index <- rep(NA_integer_, length(comids))
  missing_file_count <- 0L

  temp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(temp_nc), add = TRUE)

  for (idx in seq_along(timestamps)) {
    ts <- timestamps[[idx]]
    url <- build_chrtout_url(ts)

    ok <- safe_download(url, temp_nc, timeout = timeout)
    if (!ok) {
      missing_file_count <- missing_file_count + 1L
      message("Skipping missing/unreachable file: ", url)
      next
    }

    chrtout <- read_nwm_chrtout(temp_nc)

    if (is.null(first_good_file)) {
      first_good_file <- url
      matched_feature_index <- match(comids, chrtout$feature_ids)

      missing_comids <- is.na(matched_feature_index)
      if (any(missing_comids)) {
        message(
          "COMIDs not found in NWM routing network: ",
          paste(site_df$comid[missing_comids], collapse = ", ")
        )
      }
    }

    year_key <- format(ts, "%Y")
    year_col <- match(year_key, colnames(flow_sum))
    values <- rep(NA_real_, length(comids))
    valid_index <- !is.na(matched_feature_index)
    values[valid_index] <- chrtout$streamflow[matched_feature_index[valid_index]]

    keep <- !is.na(values)
    flow_sum[keep, year_col] <- flow_sum[keep, year_col] + values[keep]
    flow_count[keep, year_col] <- flow_count[keep, year_col] + 1L

    if (idx %% 100L == 0L || idx == length(timestamps)) {
      message("Processed ", idx, " of ", length(timestamps), " hourly files.")
    }
  }

  if (is.null(first_good_file)) {
    stop("No NWM files could be downloaded. Check network access and requested years.", call. = FALSE)
  }

  annual_summary <- bind_rows(lapply(seq_along(years), function(i) {
    tibble(
      Site = site_df$Site,
      comid = site_df$comid,
      year = years[[i]],
      mean_annual_streamflow_cms = ifelse(flow_count[, i] > 0L, flow_sum[, i] / flow_count[, i], NA_real_),
      hourly_values_used = flow_count[, i]
    )
  }))

  period_summary <- tibble(
    Site = site_df$Site,
    comid = site_df$comid,
    period_start = min(timestamps),
    period_end = max(timestamps),
    mean_period_streamflow_cms = ifelse(rowSums(flow_count) > 0L, rowSums(flow_sum) / rowSums(flow_count), NA_real_),
    hourly_values_used = rowSums(flow_count)
  )

  list(
    annual_summary = annual_summary,
    period_summary = period_summary,
    first_good_file = first_good_file,
    missing_file_count = missing_file_count
  )
}

main <- function() {
  opts <- parse_args(args)

  if (!file.exists(opts$input)) {
    stop("Input file not found: ", opts$input, call. = FALSE)
  }

  options(timeout = max(300L, opts$timeout))

  site_df <- readr::read_csv(opts$input, show_col_types = FALSE) %>%
    transmute(
      Site = as.character(.data$Site),
      comid = as.integer(.data$comid)
    ) %>%
    distinct() %>%
    filter(!is.na(.data$comid))

  if (nrow(site_df) == 0L) {
    stop("No valid COMIDs found in input file.", call. = FALSE)
  }

  timestamps <- build_timestamp_index(
    start_year = opts$start_year,
    end_year = opts$end_year,
    max_hours = opts$max_hours
  )

  message("Input rows: ", nrow(site_df))
  message("Requested timestamps: ", length(timestamps))
  message("Years: ", opts$start_year, " to ", opts$end_year)

  results <- summarise_nwm_streamflow(
    site_df = site_df,
    timestamps = timestamps,
    timeout = opts$timeout
  )

  out_annual <- opts$output
  out_period <- sub("\\.csv$", "_period.csv", opts$output)
  if (identical(out_annual, out_period)) {
    out_period <- paste0(opts$output, "_period.csv")
  }

  readr::write_csv(results$annual_summary, out_annual, na = "")
  readr::write_csv(results$period_summary, out_period, na = "")

  message("First good file: ", results$first_good_file)
  message("Missing/unreachable hourly files skipped: ", results$missing_file_count)
  message("Wrote annual summary: ", out_annual)
  message("Wrote period summary: ", out_period)
}

main()
