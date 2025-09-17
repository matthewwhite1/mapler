# Initialize empty vectors
farms <- c()
addresses <- c()
states <- c()
regions <- c()

# Define base url
base_url <- "https://www.maplesyrupfarms.org/"

# Get US state abbreviations
us_states <- sf::read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp") |>
  dplyr::filter(!(STUSPS %in% c("PR", "DC", "AS", "VI", "GU", "MP"))) |>
  dplyr::select(STUSPS) |>
  unclass()
us_states <- us_states$STUSPS

# For each state...
for (state in us_states) {
  # Define state url and read it
  state_url <- paste0(base_url, state, ".php")
  page <- rvest::read_html(state_url)

  # Get state regions
  state_links <- rvest::html_elements(page, "a") |>
    rvest::html_attr("href")
  state_links <- state_links[stringr::str_detect(state_links, paste0("^", state, ".*\\.php$"))]
  state_links <- state_links[!is.na(state_links)]

  # For each region...
  for (region_link in state_links) {
    # Create and check url
    region_url <- paste0(base_url, region_link)
    if (!RCurl::url.exists(region_url)) {
      warning(paste0("URL ", region_url, " does not exist."))
      next
    }

    # Extract list text from html
    region_page <- rvest::read_html(region_url)
    page_text <- rvest::html_elements(region_page, "li") |>
      rvest::html_text2()

    # Define regex patterns
    address_pattern <- "\\d+\\s[^,]+,\\s[^,]+,\\s[A-Z]{2}\\s\\d{5}"
    farm_pattern <- "^.+?\\s?(-|\\n)"

    # Extract farm names and addresses
    farm_addresses <- stringr::str_extract(page_text, address_pattern)
    farm_names <- stringr::str_extract(page_text, farm_pattern)

    # Filter for found addresses
    farm_names <- farm_names[!is.na(farm_addresses)]
    farm_addresses <- farm_addresses[!is.na(farm_addresses)]

    # Remove hyphen at end of name
    farm_names <- stringr::str_trim(stringr::str_remove(farm_names, "-$"))

    # Check if not same length
    if (length(farm_names) != length(farm_addresses)) {
      warning(paste0("Oopsies for ", region_link))
      next
    }

    # Extract region name
    region <- stringr::str_remove(region_link, "^[[:alpha:]]{2}") |>
      stringr::str_remove(".php")

    # Add to vectors
    farms <- c(farms, farm_names)
    addresses <- c(addresses, farm_addresses)
    states <- c(states, rep(state, length(farm_names)))
    regions <- c(regions, rep(region, length(farm_names)))
  }
}

# Create data frame
farms_df <- data.frame(farm = farms, address = addresses, state = states, region = regions)
