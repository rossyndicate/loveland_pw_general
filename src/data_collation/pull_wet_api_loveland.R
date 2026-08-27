#' @title Download water quality data from WET website for use in the Big Thompson Basin
#'
#' @description
#' Downloads raw water quality monitoring data from the Water and Earth Technology platform for
#' four water quality sites maintained by Loveland Water and Power, Reservoir Bypass (reservoir_bypass), Sulzer Creek (sul),
#' Big Thompson Near the Cherry Store (cherry), and North Fork of the Big Thompson (north_fork) for the specified time period.
#'
#' This script filters data for the specified time period and sets any values less than -9000 to NA, as these are error values from the WET sensors. It also rounds the datetime to the nearest 15 minute interval and converts it to both UTC and Mountain Time for easier joining with other datasets.
#' Note the timezones for the returned dataset and the input timestamps.
#'
#' @param target_site Character string for specific site of interest (see list below)
#'                    Available sites: "reservoir_bypass", "sulzer_creek", "big_thompson_narrows", "north_fork"
#'
#' @param wet_api_df Dataframe containing the sensor numbers, parameters, and units for each site. Default is a CSV file located at "data/raw/sensor/wet_sites_big_t.csv" that contains the relevant information for the four sites. The dataframe should have the following columns:
#' This dataframe is used within the function to map the sensor numbers to their corresponding parameters and units. 
#' 
#' @param start_datetime POSIXct timestamp or string (format: "%Y-%M-%D %H:%M) indicating the starting point for data retrieval.
#' POSIXct timesteps will be converted to `America/Denver` timezones. ** String DTs are assumed to be in `America/Denver` timezone.**
#' Default is the current system time (Sys.time()- days(7)).
#'
#' @param end_datetime POSIXct timestamp or string (format: "%Y-%M-%D %H:%M) indicating the end point for data retrieval.
#' POSIXct timesteps will be converted to `America/Denver` timezones. ** String DTs are assumed to be in `America/Denver` timezone.**
#' Default is the current system time (Sys.time()).
#'
#' @return Dataframe containing the water quality data for a specific site.
#' The dataframe contain the following columns:
#' - `site`: The site where the measurement was taken (e.g., "sfm", "chd", pfal)
#' - `DT_round`: Datetime of the measurement in UTC, rounded to the nearest 15 minute interval.
#' - `DT_round_MT`: Datetime of the measurement in America/Denver timezone, rounded to the nearest 15 minute interval.
#' - `DT_join`: Character form of DT_round
#' - `parameter`: The water quality parameter being measured (e.g., "DO", "Turbidity", etc.), supplied by the `sensor_numbers` dataframe that is created within this function.
#' - `value`: The raw value of the measurement for the specified parameter. NAs included
#' - `units`: The units of the measurement (e.g., "mg/L", "NTU", etc.), supplied by the `sensor_numbers` dataframe that is created within this function.
#'
#'
#' @examples
#'
#' Call for multiple sites for a specific time period (last 7 seven days)
#' sites <- c("reservoir_bypass", "sulzer_creek", "big_thompson_narrows", "north_fork")
#' new_WET_data <- map_dfr(sites,
#'                        ~pull_wet_api_loveland(
#'                          target_site = .x,
#'                          start_datetime = Sys.time()-days(7),
#'                          end_datetime = Sys.time()
#'                        ))
#'
#'
pull_wet_api_loveland <- function(target_site, wet_api_df = read_csv(here("data/raw/sensor/wet_sites_big_t.csv"), show_col_types = FALSE), start_datetime, end_datetime = Sys.time()) {
  
  # Pre-define constant id numbers
  site_info <- wet_api_df
  if(nrow(site_info) == 0 | is.null(site_info)) {
    stop("The wet_api_df is empty or NULL. Please check the CSV file or dataframe provided.")
  }
  
  # Check to see if the target site is one of our sites
  if (!(target_site %in% site_info$site_code)) {
    stop("Invalid target site. Please choose from: ", paste(site_info$site_code, collapse = ", "))
  }
  
  # Validation and Setup
  site_info <- site_info%>%
    filter(site_code == target_site & parameter != "Stage")
  
  parse_dt <- function(dt) {
    if (is.character(dt)){
      attempted_parse <- lubridate::ymd_hm(dt, tz = "America/Denver")
      if(is.na(attempted_parse)) {
        stop("datetime could not be parsed. Please check formatting")
      }
      return(attempted_parse)
    }
    return(lubridate::with_tz(dt, tzone = "America/Denver"))
  }
  
  start_dt_parsed <- parse_dt(start_datetime) - lubridate::minutes(5)
  end_dt_parsed <- parse_dt(end_datetime) + lubridate::minutes(5)
  
  # Calculate increments using namespaced lubridate/base functions
  increments <- round(as.numeric(difftime(lubridate::with_tz(Sys.time() + lubridate::hours(1), "America/Denver"),
                                          start_dt_parsed, units = "hours")) * 12, 0)
  
  # Generate a list of httr2 request objects
  req_list <- purrr::map(site_info$sensor_number, ~{
    httr2::request("https://wetmapgc.wetec.us/cgi-bin/datadisp_q") |>
      httr2::req_url_query(ID = .x, NM = increments) |>
      httr2::req_retry(max_tries = 3)
  })
  
  # --- Perform all requests in parallel ---
  resps <- httr2::req_perform_parallel(req_list, on_error = "continue", progress = FALSE)
  
  # Process responses
  WQ_data_list <- purrr::map2(resps, seq_along(resps), function(resp, idx) {
    if (inherits(resp, "httr2_response")) {
      tryCatch({
        content <- httr2::resp_body_string(resp, encoding = "UTF-8")
        lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
        
        # Fast filter for lines with dates
        data_lines <- lines[grep("\\d{2}/\\d{2}/\\d{4}", lines)]
        if (length(data_lines) == 0) return(NULL)
        
        # Vectorized split
        cols <- data.table::tstrsplit(data_lines, "\\s+", fill = "right")
        
        #get the appropriate parameter and units for the current sensor number
        urls_dt <- site_info%>%
          filter(sensor_number == site_info$sensor_number[idx])%>%
          slice(1)
        
        # Build DT
        dt <- data.table::data.table(
          site = target_site,
          # Use tryCatch or more robust parsing to avoid NA coercion warnings on malformed lines
          DT_round_MT = tryCatch({
            as.POSIXct(paste(cols[[1]], cols[[2]]), format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
          }, error = function(e) as.POSIXct(NA)),
          parameter = urls_dt$parameter[1],
          value = as.numeric(cols[[3]]),
          units = urls_dt$units[1]
        ) %>%
          dplyr::filter(!is.na(DT_round_MT))
        return(dt)
      }, error = function(e) NULL)
    }
    return(NULL)
  })
  
  # Combine Results
  WQ_data <- data.table::rbindlist(WQ_data_list)
  
  if (nrow(WQ_data) > 0) {
    # Filter using data.table i syntax
    WQ_data <- WQ_data[DT_round_MT >= start_dt_parsed & DT_round_MT <= end_dt_parsed]
    
    # Batch processing with lubridate and data.table specialized functions
    WQ_data[, `:=`(
      DT_round = lubridate::with_tz(lubridate::round_date(DT_round_MT, unit = "15 minutes"), tz = "UTC"),
      value = data.table::fifelse((value < -90 & parameter != "Hydrocarbon") | is.nan(value), NA_real_, value)
    )]
    
    WQ_data[, DT_join := as.character(DT_round)]
    
    data.table::setcolorder(WQ_data, c("site", "DT_round", "DT_round_MT", "DT_join", "parameter", "value", "units"))
  }
  
  return(WQ_data)
}
