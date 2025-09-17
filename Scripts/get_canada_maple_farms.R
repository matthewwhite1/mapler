# Initialize empty vectors
farms <- c()
addresses <- c()
states <- c()
regions <- c()

# Define base url
base_url <- "https://www.maplesyrupfarms.org/"

# Define province names
# Besides Newfoundland, Prince Edward Island, Quebec, and Yukon (added later)
provinces <- c("CNAL", "CNBC", "CNMB", "CNNB", "CNNS", "CNON", "CNSK")

# For each province...
for (province in provinces) {
  # Define province url and read it
  province_url <- paste0(base_url, province, ".php")
  page <- rvest::read_html(province_url)

  # Get state regions
  province_links <- rvest::html_elements(page, "a") |>
    rvest::html_attr("href")
  province_links <- province_links[stringr::str_detect(province_links, paste0("^", province, ".*\\.php$"))]
  province_links <- province_links[!is.na(province_links)]
  province_links <- c(province_links, "CNNF.php", "CNPE.php", "CNQC.php", "CNYK.php")

  # For each region...
  for (region_link in province_links) {
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
    address_pattern <- "\\d+.+,.+,\\s[:alpha:]{2}\\s.{3}\\s.{3}"
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

    # Extract province
    prov <- stringr::str_extract(region_link, "^[[:alpha:]]{4}")

    # Extract region name
    region <- stringr::str_remove(region_link, "^[[:alpha:]]{4}") |>
      stringr::str_remove(".php")

    # Add to vectors
    farms <- c(farms, farm_names)
    addresses <- c(addresses, farm_addresses)
    states <- c(states, rep(prov, length(farm_names)))
    regions <- c(regions, rep(region, length(farm_names)))
  }
}

# Create data frame with removed duplicates
canada_farms_df <- data.frame(farm = farms, address = addresses, state = states, region = regions) |>
  dplyr::distinct(farm, .keep_all = TRUE)

# Write to farms combined with output from other farms script
write.csv(rbind(farms_df, canada_farms_df), "Data_Clean/farms.csv")
