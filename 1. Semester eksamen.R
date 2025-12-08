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
con <- dbConnect(SQLite(), "fodbolddata.sqlite")
dbListTables(con)

db_vff <- dbReadTable(con, "db_vff")
db_fcidk<- dbReadTable(con, "db_fcidk")

view(db_vff)
view(db_fcidk)


# Superstats crawl --------------------------------------------------------
superstats_program <- list()

for (y in 2003:2025) {
  
  url <- paste0("https://superstats.dk/program?season=", y)
  print(url)
  
  alle_tabeller <- read_html(url, encoding = "UTF-8") |> 
    html_nodes("div#club table") |> 
    html_table(header = FALSE, convert = FALSE)
  
  alle_tabeller <- bind_rows(alle_tabeller) |>
    mutate(Season = y)  
  
  superstats_program[[as.character(y)]] <- alle_tabeller
}

# Kør denne linje for at se data fra Superstats
superstats_program

# Superstats gemmes til RDS -----------------------------------------------
saveRDS(superstats_program, file = "data/superstats_program.rds")

# Load RDS
superstats_data <- readRDS("data/superstats_program.rds")


# Laver alt data til én dataframe
superstats_dataframe <- bind_rows(superstats_program, .id = "runde")
view(superstats_dataframe)

#Opretter ny variabel kaldet runde, og fjerner den gamle der havde forkert data
superstats_dataframe <- superstats_dataframe %>%
  dplyr::select(-dplyr::any_of("runde")) %>%
  mutate(runde_new = ifelse(str_detect(X1, "^Runde"), X1, NA)) %>%
  tidyr::fill(runde_new, .direction = "down") %>%
  filter(!str_detect(X1, "^Runde")) %>%
  rename(runde = runde_new)

#Rensning af data fra Superstats
superstats_dataframe <- dplyr::select(superstats_dataframe, -X6, -X7)

#Omdøber variablerne
superstats_dataframe <- superstats_dataframe |>
  rename(Runde = runde,Ugedag = X1,Dato = X2,Hold = X3,Resultat = X4,Tilskuertal = X5)

#Filtrere så vi kun kan se VFF hjuemmekampe
superstats_dataframe <- superstats_dataframe |>
  dplyr::filter(stringr::str_starts(Hold, "VFF"))

#Laver 3 variabler der viser mål til VFF(hjemme) og mål til modstander(ude9)
#Og en 3. variabel der viser om vff har fået en sejr(1) eller uafgjordt eller tabt(0)
superstats_dataframe <- superstats_dataframe |>
  tidyr::separate(Resultat, into = c("mål_hjemme", "mål_ude"), sep = "-", remove = FALSE) |>
  dplyr::mutate(
    mål_hjemme = as.numeric(mål_hjemme),
    mål_ude = as.numeric(mål_ude),
    vff_sejr = ifelse(mål_hjemme > mål_ude, 1, 0)
  )

#Laver en variabel der viser hvor mange sejr VFF har haft de seneste 3 kampe op til en given kamp
superstats_dataframe <- superstats_dataframe |>
  mutate(
    sejre_seneste_3 =
      lag(vff_sejr, 1) +
      lag(vff_sejr, 2) +
      lag(vff_sejr, 3)
  )
view(superstats_dataframe)
#__________________________________
#Antal mål VFF har scoret i de seneste tre hjemmekampe før kampdag
superstats_dataframe <- superstats_dataframe |> 
  mutate(
    maal_seneste_3 = lag(mål_hjemme, 1) +
      lag(mål_hjemme, 2) +
      lag(mål_hjemme, 3)
  )

#Antal point VFF har fået de seneste tre kampe før kampdag
#3 point ved sejr, 1 point ved uafgjort, 0 ved nederlag

superstats_dataframe <- superstats_dataframe |> 
  mutate(
    point = case_when(
      mål_hjemme > mål_ude ~ 3,
      mål_hjemme == mål_ude ~ 1,
      TRUE ~ 0
    ),
    point_seneste_3 = lag(point, 1) +
      lag(point, 2) +
      lag(point, 3)
  )

# Her laver vi datovariabler
superstats_dataframe <- superstats_dataframe |>
filter(str_detect(Dato, "^\\d{2}/\\d{2}")) |>
  mutate(
    måned      = as.integer(substr(Dato, 4, 5)),
    År         = if_else(måned >= 7, Season - 1, Season),
    dag_maaned = substr(Dato, 1, 5),    # "dd/mm"
    tid        = substr(Dato, 7, 11),   # "HH:MM"
    Dato_text  = paste0(dag_maaned, "/", År, " ", tid),
    datetime   = dmy_hm(Dato_text, tz = "Europe/Copenhagen"),
    dato       = as.Date(datetime),
    # Nedstående laves der en kolonne med rundenummer samt sæson
    runde_nr = as.integer(str_extract(Runde, "\\d+")),
    season   = case_when(
      month(dato) >= 7 ~ paste0(year(dato), "/", year(dato) + 1),
      TRUE             ~ paste0(year(dato) - 1, "/", year(dato))
    )
  )
    

superstats_dataframe

# Vi cleaner vores dataframe, så vi kun har de nødvendige variabler med
superstats_clean <- superstats_dataframe |>
  dplyr::select(
    Ugedag, Hold, mål_hjemme, mål_ude,
    Tilskuertal, Runde, runde_nr, season,
    vff_sejr, sejre_seneste_3, maal_seneste_3,
    point, point_seneste_3,
    datetime, dato
  )


# Helligdage fra Nager.Date -----------------------------------------------
helligdage_list <- list()

for (y in 2003:2025) {
  
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

helligdage <- helligdage_df |> 
  dplyr::select(date, localName) |> 
  rename(
    dato = date,
    helligdag = localName
  )  |> 
  dplyr::mutate(
    dato = as.Date(dato)
  ) |> 
  dplyr::filter(helligdag != "Banklukkedag")

view(helligdage)

# DMI gemmes til RDS -----------------------------------------------
saveRDS(helligdage, file = "data/helligdage.rds")

# Load RDS
helligdage <- readRDS("data/helligdage.rds")

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
år <- 2003:2025
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

# DMI gemmes til RDS -----------------------------------------------
saveRDS(vejr_all, file = "data/vejr_all.rds")

# Load RDS
vejr_all <- readRDS("data/vejr_all.rds")

# Omdanner til wide format med pivot, og ændre observationstidspunkt til httm
vejr_wide <- vejr_all |>
  dplyr::select(år, observationstidspunkt, type, værdi) |>
  dplyr::mutate(
    # Konverter ISO-tid til datetime i UTC
    datotid_utc = lubridate::ymd_hms(observationstidspunkt, tz = "UTC"),
    # Konverter til dansk tid
    datetime = lubridate::with_tz(datotid_utc, tzone = "Europe/Copenhagen")
  ) |>
  tidyr::pivot_wider(
    names_from = type,
    values_from = værdi
  ) |>
  # Her vælger vi kun at  beholder datetime
  dplyr::select(datetime, dplyr::everything(), -år, -observationstidspunkt, -datotid_utc)

view(vejr_wide)


# Joining af datasæt ------------------------------------------------------

# Her joiner vi data fra superstats med helligdage
superstats_helligdage <- superstats_clean |>
  left_join(helligdage, by = "dato") |>
  mutate(
    helligdag_dummy = if_else(is.na(helligdag), 0, 1)
  ) |>
  dplyr::select(-helligdag) 

# Herefter joiner vi overstående med data fra DMI, så vi får et stort dataframe
kamp_vejr_hellig <- superstats_helligdage |> 
  mutate(datetime = floor_date(datetime, "hour")) |> 
  left_join(vejr_wide, by = "datetime")

view(kamp_vejr_hellig)

# Nu joiner vi overstående med vffkort01
fuld_datasæt <- kamp_vejr_hellig |>
  left_join(
    vffkort01,
    by = c("season" = "sæson", "runde_nr" = "runde")
  )

view(fuld_datasæt)
