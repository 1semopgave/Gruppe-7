pacman::p_load("dplyr", "tidyverse", "car", "magrittr", "nycflights13", "gapminder",
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
superstats_program <- readRDS("data/superstats_program.rds")


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

# Lav hold-kategori (A/B/C)
superstats_dataframe <- superstats_dataframe |>
  mutate(
    hold_kategori = case_when(
      Hold %in% c("VFF-FCK", "VFF-BIF", "VFF-FCM", "VFF-AGF") ~ "A",
      Hold %in% c("VFF-AaB", "VFF-OB", "VFF-SIF", "VFF-RFC", "VFF-Esbjerg") ~ "B",
      TRUE ~ "C"
    ),
    hold_kategori = factor(hold_kategori, levels = c("A", "B", "C"))
  )

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


# Filtrer 2026 fra, da kampene ikke er spillet endnu    
superstats_dataframe <- superstats_dataframe |>
  filter(År != 2026)
superstats_dataframe


# Tilskuertal til numeric 
superstats_dataframe <- superstats_dataframe |>
  mutate(
    Tilskuertal = as.numeric(gsub("\\.", "", Tilskuertal))
  ) |> # Laver en variabel som siger tilskuerantallet til sidste hjemmekamp mod modstanderen
  mutate(
    modstander = stringr::str_remove(Hold, "^VFF-"),  
    modstander = stringr::str_squish(modstander)     
  ) |>
  arrange(modstander, dato, datetime) |>
  group_by(modstander) |>
  mutate(
    tilskuere_sidste_modstander = lag(Tilskuertal)
  ) |>
  ungroup() |>
  arrange(datetime)

view(superstats_dataframe)


# Vi cleaner vores dataframe, så vi kun har de nødvendige variabler med
superstats_clean <- superstats_dataframe |>
  dplyr::select(
    Ugedag, Hold, hold_kategori, mål_hjemme, mål_ude,
    Tilskuertal, tilskuere_sidste_modstander, Runde, runde_nr, season,
    vff_sejr, sejre_seneste_3, maal_seneste_3,
    point, point_seneste_3,
    datetime, datetime_hour, dato, 
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
  s.tilskuere_sidste_modstander,
  s.hold_kategori,

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

# Sæt på fulde datasæt, og fjerne 2002, da vi ingen dmi data har
fuld_datasæt <- fuld_datasæt |>
  filter(lubridate::year(dato) != 2002) |>
  left_join(kamp_vejr_window, by = "datetime") |>
  na.omit()

view(fuld_datasæt)

#Opsætning af datasæt med variabler tilpasset tidshorisonterne
fuld_datasæt <- fuld_datasæt |>
  mutate(
    måned = factor(lubridate::month(dato))
  )

#1 måned før
`1mdr_data` <- fuld_datasæt |>
  dplyr::select(
    Tilskuertal,
    hold_kategori,
    Ugedag,
    helligdag_dummy,
    måned,
    tilskuere_sidste_modstander
  ) |>
  na.omit()

#10 dage før
`10d_data` <- fuld_datasæt |>
  dplyr::select(
    Tilskuertal,
    d10,
    d10_tilskuere,
    hold_kategori,
    Ugedag,
    helligdag_dummy,
    måned,
    tilskuere_sidste_modstander
  ) |>
  na.omit()

#7 dage før
`7d_data` <- fuld_datasæt |>
  dplyr::select(
    Tilskuertal,
    d7,
    d7_tilskuere,
    hold_kategori,
    Ugedag,
    helligdag_dummy,
    måned,
    tilskuere_sidste_modstander
  ) |>
  na.omit()

#3 dage før
`3d_data` <- fuld_datasæt |>
  dplyr::select(
    Tilskuertal,
    d3,
    d3_tilskuere,
    hold_kategori,
    Ugedag,
    helligdag_dummy,
    gns_temp,
    gns_nedbør,
    gns_vind,
    tilskuere_sidste_modstander
  ) |>
  na.omit()


#____________
run_models <- function(data) {

  set.seed(7)
  
  n <- nrow(data)
  train <- sample(seq_len(n), size = floor(0.70 * n))
  test  <- setdiff(seq_len(n), train)
  
  # ---------- Subset selection + K-fold ----------
  k <- 5
  folds <- sample(rep(1:k, length = n))
  
  cv.errors <- matrix(
    NA, k, ncol(data) - 1
  )
  
  for (j in 1:k) {
    best.fit <- regsubsets(
      Tilskuertal ~ .,
      data = data[folds != j, ],
      nvmax = ncol(data) - 1
    )
    
    for (i in 1:(ncol(data) - 1)) {
      mat <- model.matrix(Tilskuertal ~ ., data[folds == j, ])
      coefi <- coef(best.fit, id = i)
      pred <- mat[, names(coefi)] %*% coefi
      cv.errors[j, i] <- mean(
        (data$Tilskuertal[folds == j] - pred)^2
      )
    }
  }
  
  mean_mse <- colMeans(cv.errors)
  best_size <- which.min(mean_mse)
  rmse_subset <- sqrt(mean_mse[best_size])
  
  # ---------- LOOCV ----------
  glm_fit <- glm(Tilskuertal ~ ., data = data)
  loocv <- cv.glm(data, glm_fit, K = n)
  rmse_loocv <- sqrt(loocv$delta[1])
  
  # ---------- Ridge & Lasso ----------
  x <- model.matrix(Tilskuertal ~ ., data)[, -1]
  y <- data$Tilskuertal
  
  lambda_grid <- 10^seq(10, -2, length = 100)
  
  ridge_cv <- cv.glmnet(x[train, ], y[train], alpha = 0)
  ridge_pred <- predict(
    ridge_cv,
    s = ridge_cv$lambda.min,
    newx = x[test, ]
  )
  rmse_ridge <- sqrt(mean((ridge_pred - y[test])^2))
  
  lasso_cv <- cv.glmnet(x[train, ], y[train], alpha = 1)
  lasso_pred <- predict(
    lasso_cv,
    s = lasso_cv$lambda.min,
    newx = x[test, ]
  )
  rmse_lasso <- sqrt(mean((lasso_pred - y[test])^2))
  
  return(list(
    rmse_subset = rmse_subset,
    rmse_loocv  = rmse_loocv,
    rmse_ridge  = rmse_ridge,
    rmse_lasso  = rmse_lasso
  ))
}

#_____________
result_1mdr <- run_models(`1mdr_data`)
result_10d  <- run_models(`10d_data`)
result_7d   <- run_models(`7d_data`)
result_3d   <- run_models(`3d_data`)
#_________
results <- rbind(
  `1 måned` = unlist(result_1mdr),
  `10 dage` = unlist(result_10d),
  `7 dage`  = unlist(result_7d),
  `3 dage`  = unlist(result_3d)
)

results
