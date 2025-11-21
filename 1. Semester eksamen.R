pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "testthat", "MASS", "leaps", "caret",
               "RSQLite", "class", "nasaweather", "fueleconomy", "viridis",
               "boot", "glmnet", "pls", "rvest", "DBI", "RSQLite",
               "lubridate", "rlist", "rjstat", "rjson", "Rcrawler", "usethis")

# Indhentning af datafiler fra VFF

# RDS filer
readRDS("data/fcidk.rds")
readRDS("data/vffkort01.rds")

View(fcidk)
View(vffkort01)


# SQL fil
con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")

dbListTables(con)

db_vff <- dbReadTable(con, "db_vff")
db_fcidk<- dbReadTable(con, "db_fcidk")

view(db_vff)
view(db_fcidk)


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

# Saml alle år til ét dataframe
helligdage_df <- bind_rows(helligdage_list)

# Opdater datasættet
helligdage_df <- helligdage_df |> 
  dplyr::select(date, localName) |> 
  rename(
    dato = date,
    helligdag = localName
  )  
view(helligdage_df)
str(helligdage_df)

#### DMI data ------------------------------------------------------------------

# Base URL og API-nøgle
dmi_base_url <- "https://dmigw.govcloud.dk/v2/"
dmi_info_url <- "metObs/collections/observation/items?"
api_key <- Sys.getenv("MY_API_KEY")


# Forbedret funktion med fejlhåndtering
hent_dmi_data <- function(station_id, parameter_id, start_date, end_date, limit, season_label = NULL) {
  

  # Byg query
  query <- paste0(
    "stationId=", station_id,
    "&parameterId=", parameter_id,
    "&datetime=", start_date, "Z/", end_date, "Z",
    "&limit=", format(limit, scientific = FALSE)
  )
  
  # Byg full URL med API-nøgle
  full_url <- paste0(dmi_base_url, dmi_info_url, query, "&api-key=", api_key)
  
  # API-kald
  response <- GET(full_url)
  
  # Tjek status
  if (status_code(response) != 200) {
    stop("API fejl for ", parameter_id, " (", season_label, "): Status ", status_code(response))
  }
  
  # Parse response 
  content_json <- jsonlite::fromJSON(rawToChar(response$content), flatten = FALSE)
  
  # Tjek om der er data
  if (is.null(content_json$features) || length(content_json$features) == 0) {
    warning("Ingen data fundet for ", parameter_id, " (", season_label, ")")
    return(tibble())
  }
  
  props <- content_json$features$properties
  
  # Udtræk data
  df_selected <- tibble(
    observationstidspunkt = props$observed,
    værdi = as.numeric(props$value),
    parameter = parameter_id,
    season = season_label
  )
  
  return(df_selected)
}


# Liste over sæsoner og deres datoer
saesoner <- tibble::tibble(
  season_label = c("19/20", "20/21", "21/22", "22/23", "23/24"),
  start_date   = c("2019-07-31T00:00:00", "2020-07-31T00:00:00", "2021-07-31T00:00:00", "2022-07-17T00:00:00", "2023-07-28T00:00:00"),
  end_date     = c("2020-05-15T23:59:00", "2021-05-15T23:59:00", "2022-05-15T23:59:00", "2023-05-29T23:59:00", "2024-05-25T23:59:00")
)

## Vind parameter
vind_list <- list()

for (i in 1:nrow(saesoner)) {
  
  season <- saesoner$season_label[i]
  start  <- saesoner$start_date[i]
  slut   <- saesoner$end_date[i]
  
  cat("Henter sæson:", season, "\n")
  
  vind_list[[season]] <- hent_dmi_data(
    station_id    = "06060",
    parameter_id  = "wind_speed_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000,
    season_label  = season
  )
}

vind <- bind_rows(vind_list, .id = "sæson")

## Temperatur parameter
temp_list <- list()

for (i in 1:nrow(saesoner)) {
  
  season <- saesoner$season_label[i]
  start  <- saesoner$start_date[i]
  slut   <- saesoner$end_date[i]
  
  cat("Henter TEMP for sæson:", season, "\n")
  
  temp_list[[season]] <- hent_dmi_data(
    station_id    = "06060",
    parameter_id  = "temp_mean_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000,
    season_label  = season
  )
}

temp <- dplyr::bind_rows(temp_list, .id = "sæson")

## Nedbør parameter
nedboer_list <- list()

for (i in 1:nrow(saesoner)) {
  
  season <- saesoner$season_label[i]
  start  <- saesoner$start_date[i]
  slut   <- saesoner$end_date[i]
  
  cat("Henter NEDBØR for sæson:", season, "\n")
  
  nedboer_list[[season]] <- hent_dmi_data(
    station_id    = "06060",
    parameter_id  = "precip_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000,
    season_label  = season
  )
}

nedboer <- dplyr::bind_rows(nedboer_list, .id = "sæson")


# Samler det hele
vejr_all <- dplyr::bind_rows(
  vind  |> dplyr::mutate(type = "vind"),
  temp  |> dplyr::mutate(type = "temp"),
  nedboer |> dplyr::mutate(type = "nedbør")
)

View(vejr_all)

# Omdatter til wide format med pivot, altså så alle værdier får deres egen kolonner
vejr_wide <- vejr_all %>%
  dplyr::select(sæson, observationstidspunkt, type, værdi) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = værdi
  )

view(vejr_wide)