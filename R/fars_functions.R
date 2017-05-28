#' Read in csv-File as dataframe
#'
#' This function checks if a given csv-file exists and if so reads it in silently as a dataframe
#'
#' @param filename A character string giving the path to the file to read in
#' 
#' @return This function returns the data from the given file as a dataframe
#'
#' @note Errors can result from the given filename not being a valid path to an existing file or the file not being encoded in the right format
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate filename for given year
#'
#' This function determines the filename including extensions of the accident data
#' of the given year
#'
#' @param year The year for which to generate the filename as integer or type that can be meaningfullz converted with as.integer
#' 
#' @return This function returns the filename in the format "accident_YEAR.csv.bz2" with YEAR replaced with the given year
#'
#' @examples
#' make_filename(1991)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read in data for several years
#'
#' This function reads in the accident data for multiple years as a list of dataframes. If one of the years can not be 
#' read in the function skips that year with a warning
#'
#' @param years Collection of years as vector or list where each element is in integer format or can be converted with as.integer
#' 
#' @return This function returns a list of dataframes where each dataframe contains the data for one year
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @note Errors in this function can result from errors thrown by the fars_read function or invalid years parameter being passed
#' 
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize Monthly Accident Number for each year
#'
#' For a given collection of years this function reads in the accident data and then returning a summary of the data
#' which contains one row for each calendar month containing the number of accidents in that month for different years
#' in seperate columns
#'
#' @param years Collection of years as vector or list where each element is in integer format or can be converted with as.integer
#' 
#' @return This function returns a data frame with dimension (12,length(years)). 
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Visualize Accidents on State Map
#'
#' For a selected year and state the locations of all accidents are shown on the map of that state
#'
#' @param state.num Valid State number as integer selecting which state to plot
#' @param year Year for which to plot the data as integer
#' 
#' @return Returns plot displaying longitude and latitude of the accidents on the map
#' 
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
