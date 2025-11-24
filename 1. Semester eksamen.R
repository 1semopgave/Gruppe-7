pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "testthat", "MASS", "leaps", "caret",
               "RSQLite", "class", "nasaweather", "fueleconomy", "viridis",
               "boot", "glmnet", "pls", "rvest", "DBI", "RSQLite",
               "lubridate", "rlist", "rjstat", "rjson", "Rcrawler", "usethis")


# Indhentning af datafiler fra VFF ----------------------------------------

  # RDS filer
fcidk <- readRDS("data/fcidk.rds")
vffkort01 <- readRDS("data/vffkort01.rds")

View(fcidk)
View(vffkort01)

  # SQL fil
con <- dbConnect(SQLite(), "data/fodbolddata.sqlite")
dbListTables(con)

db_vff <- dbReadTable(con, "db_vff")
db_fcidk<- dbReadTable(con, "db_fcidk")

view(db_vff)
view(db_fcidk)


# Superstats crawl --------------------------------------------------------
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
view(superstats_dataframe)

#Rensning af data fra Superstats
superstats_dataframe <- superstats_dataframe[ , -c(7, 8)]

superstats_dataframe <- superstats_dataframe[
  !(startsWith(superstats_dataframe$X1, "Runde") &
      startsWith(superstats_dataframe$X2, "Runde")), ]

superstats_dataframe <- superstats_dataframe |>
  rename(Runde = runde,Ugedag = X1,Dato = X2,Hold = X3,Resultat = X4,Tilskuertal = X5)


# Helligdage fra Nager.Date -----------------------------------------------
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

helligdage_df <- helligdage_df |> 
  dplyr::select(date, localName) |> 
  rename(
    dato = date,
    helligdag = localName
  )  |> 
  dplyr::mutate(
    dato = as.Date(dato)
  ) |> 
  dplyr::filter(helligdag != "Banklukkedag")

view(helligdage_df)


# DMI data ----------------------------------------------------------------
  # Base URL og API-nøgle
dmi_base_url <- "https://dmigw.govcloud.dk/v2/"
dmi_info_url <- "metObs/collections/observation/items?"
api_key <- Sys.getenv("MY_API_KEY")

# Funktion
  hent_dmi_data <- function(station_id, parameter_id, start_date, end_date, limit, season_label = NULL) {
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

# År der skal hentes
år <- 2023:2025
karup <- "06060"
vejr_list <- list()

for (y in år) {
  start <- paste0(y, "-01-01T00:00:00")
  slut  <- paste0(y, "-12-31T23:59:59")
  
  cat("Henter Vind, Temp og Nedbør for år:", y, "\n")
  
  # Vind
  vind_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "wind_speed_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 200000,
    season_label  = as.character(y)
  ) |> 
    dplyr::mutate(type = "vind")
  
  # Temperatur
  temp_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "temp_mean_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 200000,
    season_label  = as.character(y)
  ) |> 
    dplyr::mutate(type = "temp")
  
  # Nedbør
  nedboer_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "precip_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 200000,
    season_label  = as.character(y)
  ) |> 
    dplyr::mutate(type = "nedbør")
  
  # Saml for året
  vejr_list[[as.character(y)]] <- dplyr::bind_rows(
    vind_y,
    temp_y,
    nedboer_y
  )
}

vejr_all <- dplyr::bind_rows(vejr_list, .id = "år")

# Omdanner til wide format med pivot, altså så alle værdier får deres egen kolonner
vejr_wide <- vejr_all |> 
  dplyr::select(år, observationstidspunkt, type, værdi) |> 
  tidyr::pivot_wider(
    names_from = type,
    values_from = værdi
  )

view(vejr_wide)