#' Download LOCA2 netCDF files
#'
#' This function can be used to download daily netCDF files from LOCA2.
#'
#' @param model character vector of models. These must exactly
#'   match the LOCA2 model names (Ex: "ACCESS-CM2")
#' @param run integer vector of model runs. A run of 1 corresponds to the
#'   first run of the model (Ex: r1i1p1f1), a run of 2 corresponds to the second
#'   run of the model, and so on
#' @param scenario character vector of future climate scenarios. Each value
#'   in this vector must be either historical, ssp245, ssp370, or ssp585
#' @param var character vector of variables to download. Each value in this
#'   vector must be either pr (precipitation), tasmax (maximum temperature),
#'   tasmin (minimum temperature), or DTR (difference between maximum and
#'   minimum temperature)
#' @param out_dir name of the output directory that will store the
#'   downloaded files
#'
#' @details When filtering for files to download, this function excludes files
#'   that include "yearly" or "monthly", and it only downloads files that are
#'   at least 1 gigabyte in size. Keep in mind that even downloading the files
#'   for just one run of one model can take multiple hours, as these models
#'   contain some large files.
#'
#' @seealso [loca2_model_names]
#'
#' @source \url{https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/}
#'
#' @examples
#' \dontrun{
#' # Download all files from first LOCA2 model
#' download_loca2(model = "ACCESS-CM2",
#'                run = 1,
#'                scenario = "ssp585",
#'                var = c("pr", "tasmax", "tasmin"))
#'
#' # Download all files with multiple models, runs, and scenarios
#' download_loca2(model = c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR"),
#'                run = c(1, 2, 3),
#'                scenario = c("ssp245", "ssp370", "ssp585"))
#' }
#' @export
download_loca2 <- function(model,
                           run = 1,
                           scenario = c("historical", "ssp585"),
                           var = c("pr", "tasmax", "tasmin"),
                           out_dir = paste0(getwd())) {

  # Argument error checking
  if (!is.numeric(run) || !all(run %% 1 == 0)) {
    stop("The run argument must be a positive integer.")
  } else if (!all(scenario %in% c("historical", "ssp245",
                                  "ssp370", "ssp585"))) {
    stop("Each scenario must be historical, ssp245, ssp370, or ssp585.")
  } else if (!all(model %in% mapler::loca2_model_names)) {
    stop("Invalid model name (see loca2_model_names).")
  } else if (!is.character(out_dir) || length(out_dir) != 1) {
    stop("out_dir must be a character vector of length 1")
  } else if (!dir.exists(out_dir)) {
    stop("Out directory does not exist.")
  } else if (!all(var %in% c("pr", "tasmax", "tasmin", "DTR"))) {
    stop("Each variable must be pr, tasmax, tasmin, or DTR.")
  }

  # Increase timeout boundary for large files
  options(timeout = 21600) # 6 hours, just in case

  # Define base url
  base_url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"

  # For each given model...
  for (given_model in model) {
    # Create model directory
    model_dir <- file.path(out_dir, given_model)
    dir.create(model_dir, recursive = TRUE)

    # For each given run...
    for (given_run in run) {
      # Check for what run folders exist
      run_url <- paste0(base_url, given_model, "/0p0625deg/")
      run_page <- httr::GET(run_url)
      run_pagehtml <- XML::htmlParse(run_page)
      run_nodes <- XML::getNodeSet(run_pagehtml, "//table")
      run_table <- XML::readHTMLTable(run_nodes[[1]])
      run_table <- run_table[, -1] |>
        dplyr::filter(!is.na(.data$Name) & .data$Name != "Parent Directory")
      run_names <- run_table$Name

      # Throw an error if run doesn't exist
      run_folder <- run_names[stringr::str_detect(run_names,
                                                  paste0("^r", given_run, "i"))]
      if (length(run_folder) == 0) {
        warning(paste0("Run number ", given_run, " for model ",
                       given_model, " does not exist."))
        next
      }

      # Create run URL
      model_url <- paste0(base_url, given_model, "/0p0625deg/", run_folder)

      # For both historical and future...
      for (period in scenario) {
        # Create scenario URL
        period_url <- paste0(model_url, period)

        # Throw an error if scenario doesn't exist
        if (!RCurl::url.exists(period_url)) {
          warning(paste0("Climate scenario ", period, " for model ",
                         given_model, " for run ", given_run,
                         " does not exist."))
          next
        }

        # For each variable...
        for (var_name in var) {
          # Create variable directory
          var_dir <- file.path(model_dir, "0p0625deg", run_folder,
                               period, var_name)
          dir.create(var_dir, recursive = TRUE)

          # Find desired files
          data_url <- paste0(model_url, period, "/", var_name)
          data_page <- httr::GET(data_url)
          data_pagehtml <- XML::htmlParse(data_page)
          data_nodes <- XML::getNodeSet(data_pagehtml, "//table")
          data_table <- XML::readHTMLTable(data_nodes[[1]])
          data_table <- data_table[, -1] |>
            dplyr::filter(!is.na(.data$Name) & .data$Name != "Parent Directory" &
                            !stringr::str_detect(.data$Name, "monthly") &
                            !stringr::str_detect(.data$Name, "yearly") &
                            stringr::str_detect(.data$Size, "G"))

          # Download desired files
          for (file_name in data_table$Name) {
            file_url <- paste0(data_url, "/", file_name)
            output_file <- file.path(var_dir, file_name)
            utils::download.file(file_url, output_file, mode = "wb")
            message(paste0("Downloaded file to ", output_file))
          }
        }
      }
    }
  }
}
