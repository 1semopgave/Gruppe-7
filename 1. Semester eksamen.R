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

for (y in 2003:2026) {
  
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
    ), 
      datetime_hour = floor_date(datetime, unit = "hour")
  )
    

superstats_dataframe

# Vi cleaner vores dataframe, så vi kun har de nødvendige variabler med
superstats_clean <- superstats_dataframe |>
  dplyr::select(
    Ugedag, Hold, mål_hjemme, mål_ude,
    Tilskuertal, Runde, runde_nr, season,
    vff_sejr, sejre_seneste_3, maal_seneste_3,
    point, point_seneste_3,
    datetime, datetime_hour, dato
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

# Helligdage gemmes til RDS -----------------------------------------------
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
    limit         = 50000,
    season_label  = as.character(y)
  ) |> 
    dplyr::mutate(type = "vind")
  
  # Temperatur
  temp_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "temp_mean_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000,
    season_label  = as.character(y)
  ) |> 
    dplyr::mutate(type = "temp")
  
  # Nedbør
  nedboer_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "precip_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000,
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
    datetime = lubridate::with_tz(datotid_utc, tzone = "Europe/Copenhagen"),
    datetime_hour = floor_date(datetime, unit = "hour")
  ) |>
  tidyr::pivot_wider(
    names_from = type,
    values_from = værdi
  ) |>
  # Her vælger vi kun at  beholder datetime
  dplyr::select(datetime, datetime_hour, dplyr::everything(), -år, -observationstidspunkt, -datotid_utc)

view(vejr_wide)



# Joining af datasæt ------------------------------------------------------
    # Ny projekt-database 
con_sql <- dbConnect(SQLite(), "data/vff_eksamen.sqlite")

# Skriv de rensede dataframes ind som tabeller i databasen
dbWriteTable(con_sql, "superstats", superstats_clean, overwrite = TRUE)
dbWriteTable(con_sql, "helligdage", helligdage,      overwrite = TRUE)
dbWriteTable(con_sql, "vejr",       vejr_wide,       overwrite = TRUE)
dbWriteTable(con_sql, "vffkort",    vffkort01,       overwrite = TRUE)

dbListTables(con_sql) 

# SQL-join med SELECT, FROM, LEFT JOIN, WHERE, GROUP BY, HAVING
sql_join <- "
SELECT 
  s.season,
  s.runde_nr,
  s.Runde,
  s.Ugedag,
  s.Hold,
  s.mål_hjemme,
  s.mål_ude,
  s.Tilskuertal,
  s.vff_sejr,
  s.sejre_seneste_3,
  s.maal_seneste_3,
  s.point,
  s.point_seneste_3,
  s.datetime,
  s.dato,

    -- helligdag -> dummy
    CASE 
      WHEN h.dato IS NULL THEN 0 
      ELSE 1 
    END AS helligdag_dummy,

    -- vejr
    v.vind,
    v.temp,
    v.nedbør,

    -- VFF billetdata
    k.d10,
    k.d7,
    k.d3,
    k.d10_tilskuere,
    k.d7_tilskuere,
    k.d3_tilskuere

FROM superstats AS s

LEFT JOIN helligdage AS h
  ON s.dato = h.dato

LEFT JOIN vejr AS v
  ON s.datetime_hour = v.datetime_hour

LEFT JOIN vffkort AS k
  ON s.season   = k.sæson
 AND s.runde_nr = k.runde

-- behold kun VFF-hjemmekampe
WHERE s.Hold LIKE 'VFF-%'

GROUP BY 
  s.season,
  s.runde_nr,
  s.datetime 
  
  HAVING COUNT(s.datetime) >= 1
"

# Hent joinet datasæt tilbage til R
fuld_datasæt <- dbGetQuery(con_sql, sql_join)
dbDisconnect(con_sql)

# Konvertere
fuld_datasæt <- fuld_datasæt |>
  mutate(
    
    # ---- POSIXCT ----
    datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "Europe/Copenhagen"),
    
    # ---- Datetime ----
    dato = as.Date(dato, origin = "1970-01-01"),
    
    # ---- Factor variabler ----
    Ugedag = as.factor(Ugedag),
    Hold = as.factor(Hold),
    season = as.factor(season),
    Runde = as.factor(Runde),
    
    # ---- Numeric variabler ----
    Tilskuertal = as.numeric(gsub("\\.", "", Tilskuertal)),
    mål_hjemme = as.numeric(mål_hjemme),
    mål_ude = as.numeric(mål_ude),
    vff_sejr = as.numeric(vff_sejr),
    sejre_seneste_3 = as.numeric(sejre_seneste_3),
    maal_seneste_3 = as.numeric(maal_seneste_3),
    point = as.numeric(point),
    point_seneste_3 = as.numeric(point_seneste_3),
    helligdag_dummy = as.integer(helligdag_dummy),
    
    d10 = as.numeric(d10),
    d7  = as.numeric(d7),
    d3  = as.numeric(d3),
    d10_tilskuere = as.numeric(d10_tilskuere),
    d7_tilskuere  = as.numeric(d7_tilskuere),
    d3_tilskuere  = as.numeric(d3_tilskuere)
  )


#__________Esktra tilføjelse af variabel_____
kamp_vejr_window <- list()

for(i in 1:nrow(fuld_datasæt)) {
  
  kamp_tid <- fuld_datasæt$datetime[i]
  
  start_window <- kamp_tid - hours(3)
  slut_window  <- kamp_tid + hours(2)
  
  # filtrér vejrdata i vinduet
  vejr_subset <- vejr_wide |>
    filter(datetime >= start_window,
           datetime <= slut_window)
  
  # beregn gennemsnit
  kamp_vejr_window[[i]] <- tibble(
    datetime = kamp_tid,
  gns_vind   = round(mean(vejr_subset$vind,   na.rm = TRUE), 1),
  gns_temp   = round(mean(vejr_subset$temp,   na.rm = TRUE), 1),
  gns_nedbør = round(mean(vejr_subset$nedbør, na.rm = TRUE), 1)
  )
}

# slå alle rækker sammen
kamp_vejr_window <- bind_rows(kamp_vejr_window)

#Sæt på fulde datasæt, og fjerne 2002, da vi ingen dmi data har
fuld_datasæt <- fuld_datasæt |>
  filter(lubridate::year(dato) != 2002) |>
  left_join(kamp_vejr_window, by = "datetime") |>
  na.omit()


view(fuld_datasæt)

#Gruppering af hold i A, B, C - brug for det senere i vores opgave 
fuld_datasæt <- fuld_datasæt %>%
  mutate(
    hold_kategori = case_when(
      hold %in% c("VFF-FCK", "VFF-BIF", "VFF-FCM", "VFF-AGF") ~ "A",
      hold %in% c("VFF-AaB", "VFF-OB", "VFF-SIF", "VFF-RFC", "VFF-Esbjerg") ~ "B",
      TRUE ~ "C"
    ),
    hold_kategori = factor(hold_kategori, levels = c("A", "B", "C"))
  )
#Fjerne variablet hold fra vores datatabel, da vi har hold_kategorie plus fjerne alle NA. 
fuld_datasæt <- fuld_datasæt |>
  dplyr::select (-hold)

fuld_datasæt <- na.omit(fuld_datasæt)
view(fuld_datasæt)


#Sætter seed, laver 70% trænigsdata, fjerner alle na i datasættet.
set.seed(7)


train_ind <- sample(seq_len(nrow(fuld_datasæt)), size = 0.7 * nrow(fuld_datasæt))

train <- fuld_datasæt[train_ind, ]
test  <- fuld_datasæt[-train_ind, ]



# Byg modellen med relevante variabler
lm_mod <- lm(
  Tilskuertal ~ Ugedag +mål_hjemme + mål_ude + sejre_seneste_3 + maal_seneste_3 + point + datetime + 
    helligdag_dummy + gns_temp + gns_vind + gns_nedbør + d10_tilskuere + d7_tilskuere + d3_tilskuere +
    hold_kategori,
  train)
sm_01_træning <- summary(lm_mod)
mean(sm_01_træning$residuals^2) #MSE for ikke flexible model på træningsdata


pred01_test<- predict (lm_mod, test)
sm_01_test <- summary (pred01_test)

mean ((pred01_test-test$Tilskuertal)^2)  #lidt i tvil om jeg jeg valgt de rigtig vairable /kolon)


#med polynomiumsgrad 

lm_mod2 <- lm(
  Tilskuertal ~ 
    poly(mål_hjemme, 2, raw = TRUE) +
    poly(mål_ude, 2, raw = TRUE) +
    poly(sejre_seneste_3, 2, raw = TRUE) +
    poly(maal_seneste_3, 2, raw = TRUE) +
    poly(point, 2, raw = TRUE) +
    poly(gns_temp, 2, raw = TRUE) +
    poly(gns_vind, 2, raw = TRUE) +
    poly(gns_nedbør, 2, raw = TRUE) +
    poly(d10_tilskuere, 2, raw = TRUE) +
    poly(d7_tilskuere, 2, raw = TRUE) +
    poly(d3_tilskuere, 2, raw = TRUE) +
    Ugedag + datetime + helligdag_dummy + hold_kategori,
  data = train
)

