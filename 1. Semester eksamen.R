pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "testthat", "MASS", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis", "boot", "glmnet", "pls", "rvest")

# Superstats crawl

superstats_program <- list()

for (y in 2023:2025) {
  
  url <- paste0("https://superstats.dk/program?season=", y)
  print(url)
  
  alle_tabeller <- read_html(url, encoding = "UTF-8") |> 
    html_nodes("div#club table") |> 
    html_table(header = FALSE, convert = FALSE)
  
  superstats_program[[as.character(y)]] <- alle_tabeller
  
}

# Kør denne linje for at se data fra Superstats
superstats_program

# Laver alt data til én dataframe
superstats_dataframe <- bind_rows(superstats_program, .id = "runde")



### ---------------------------------------------------------------------------


# Danske helligdage fra Nager.Date

helligdage_list <- list()

for (y in 2023:2025) {
  
  url_helligdage <- paste0("https://date.nager.at/api/v3/PublicHolidays/", y, "/DK")
  print(url_helligdage)
  
  res <- httr::GET(url_helligdage)
  
  if (httr::status_code(res) == 200) {
    json_content <- httr::content(res, as = "text", encoding = "UTF-8")
    df <- jsonlite::fromJSON(json_content)
    
    helligdage_list[[as.character(y)]] <- df
  }
}


# Kør for at se alle helligdagene
helligdage_list

# Saml alle år til ét dataframe
helligdage_df <- bind_rows(helligdage_list)

helligdage_df






