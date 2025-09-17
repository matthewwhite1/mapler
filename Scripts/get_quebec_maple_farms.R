base_url_1 <- "https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks"
page_1 <- rvest::read_html(base_url_1)
farm_links_1 <- rvest::html_elements(page_1, "a") |>
  rvest::html_attr("href")
farm_links_1 <- farm_links_1[31:50]

base_url_2 <- "https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks?supplier=36&page=1&page=1"
page_2 <- rvest::read_html(base_url_2)
farm_links_2 <- rvest::html_elements(page_2, "a") |>
  rvest::html_attr("href")
farm_links_2 <- farm_links_2[31:50]

base_url_3 <- "https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks?supplier=36&page=2&page=2"
page_3 <- rvest::read_html(base_url_3)
farm_links_3 <- rvest::html_elements(page_3, "a") |>
  rvest::html_attr("href")
farm_links_3 <- farm_links_3[31:50]

base_url_4 <- "https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks?supplier=36&page=3&page=3"
page_4 <- rvest::read_html(base_url_4)
farm_links_4 <- rvest::html_elements(page_4, "a") |>
  rvest::html_attr("href")
farm_links_4 <- farm_links_4[31:50]

base_url_5 <- "https://www.bonjourquebec.com/en-us/to-see-and-do/delicious-discoveries/sugar-shacks?supplier=36&page=4&page=4"
page_5 <- rvest::read_html(base_url_5)
farm_links_5 <- rvest::html_elements(page_5, "a") |>
  rvest::html_attr("href")
farm_links_5 <- farm_links_5[31:35]

# Combine
farm_links <- c(farm_links_1, farm_links_2, farm_links_3, farm_links_4, farm_links_5) |>
  stringr::str_replace("^", "https://www.bonjourquebec.com")

# For each farm...
names <- c()
addresses <- c()
for (i in seq_along(farm_links)) {
  farm_url <- farm_links[i]
  farm_page <- rvest::read_html(farm_url)
  farm_name <- rvest::html_elements(farm_page, "h1.title.mb-3.fiche-title") |>
    rvest::html_text2()
  farm_address <- rvest::html_elements(farm_page, "div.mb-4.contact-adresse") |>
    rvest::html_text2()
  names <- c(names, farm_name)
  addresses <- c(addresses, farm_address)
}

# Remove country for geocoding
addresses <- stringr::str_remove(addresses, ", Canada")

# Create data frame
quebec_farms <- data.frame(farm = names, address = addresses, state = rep(NA, 85), region = rep(NA, 85))

# Write csv
write.csv(quebec_farms, "Data_Clean/quebec_farms.csv")
