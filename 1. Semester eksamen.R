pacman::p_load("dplyr", "tidyverse", "car", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "testthat", "MASS", "leaps", "caret",
               "RSQLite", "class", "nasaweather", "fueleconomy", "viridis",
               "boot", "glmnet", "pls", "rvest", "DBI", "RSQLite",
               "lubridate", "rlist", "rjstat", "rjson", "Rcrawler", "usethis")


# Indhentning af datafiler fra VFF ----------------------------------------

  # Henter RDS filerne ned
fcidk <- readRDS("data/fcidk.rds")
vffkort01 <- readRDS("data/vffkort01.rds")

View(fcidk)
View(vffkort01)

# Superstats crawl --------------------------------------------------------
superstats_program <- list()

for (y in 2003:2026) {
# Her laver vi et loop, så vi henter alle sæsoner ned fra 2003 til 2026  
  url <- paste0("https://superstats.dk/program?season=", y)
  print(url)
  
  alle_tabeller <- read_html(url, encoding = "UTF-8") |> 
    html_nodes("div#club table") |> 
    html_table(header = FALSE, convert = FALSE)
  
  alle_tabeller <- bind_rows(alle_tabeller) |>
    mutate(sæson = y)  
  
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

# Opretter ny runde variabel, og fjerner den gamle der havde forkert data
superstats_dataframe <- superstats_dataframe |> 
  dplyr::select(-dplyr::any_of("runde")) |> 
  mutate(runde_new = ifelse(str_detect(X1, "^Runde"), X1, NA)) |> 
  tidyr::fill(runde_new, .direction = "down") |> 
  filter(!str_detect(X1, "^Runde")) |> 
  rename(runde = runde_new)


# Rensning af data samt ændring i variable navne
superstats_dataframe <- dplyr::select(superstats_dataframe, -X6, -X7) |>
  rename(runde = runde, ugedag = X1, dato = X2, hold = X3, resultat = X4, tilskuertal = X5) |> 
  # Fjerner de kampe som ikke er spillet endnu
  filter(!is.na(tilskuertal) & tilskuertal != "")


# Filtrere så vi kun kan se VFF hjuemmekampe
superstats_dataframe <- superstats_dataframe |>
  dplyr::filter(stringr::str_starts(hold, "VFF"))

# Lav hold-kategori (A/B/C)
superstats_dataframe <- superstats_dataframe |>
  mutate(
    hold_kategori = case_when(
      hold %in% c("VFF-FCK", "VFF-BIF", "VFF-FCM", "VFF-AGF") ~ "A",
      hold %in% c("VFF-AaB", "VFF-OB", "VFF-SIF", "VFF-RFC", "VFF-Esbjerg") ~ "B",
      TRUE ~ "C"
    ),
    hold_kategori = factor(hold_kategori, levels = c("A", "B", "C"))
  )

# Danner nye variabler 
superstats_dataframe <- superstats_dataframe |>
  separate(resultat, into = c("mål_hjemme", "mål_ude"), sep = "-", remove = FALSE) |>
  # Vi splitter resultat til mål hjemme og mål ude, hvorefter vi danner en variable om VFF har vundet eller tabt
  mutate(
    mål_hjemme = as.numeric(mål_hjemme),
    mål_ude    = as.numeric(mål_ude),
    vff_sejr   = if_else(mål_hjemme > mål_ude, 1, 0),
    point      = case_when(
      mål_hjemme > mål_ude ~ 3,
      mål_hjemme == mål_ude ~ 1,
      TRUE ~ 0
    )
  ) |>
  mutate(
    sejre_seneste_3 = lag(vff_sejr, 1) + lag(vff_sejr, 2) + lag(vff_sejr, 3),
    mål_seneste_3   = lag(mål_hjemme, 1) + lag(mål_hjemme, 2) + lag(mål_hjemme, 3),
    point_seneste_3 = lag(point, 1) + lag(point, 2) + lag(point, 3)
  )
  # Vi har her dannet variabler der viser hvor mange sejre, mål og point VFF har fået i de seneste 3 hjemmekampe


# Her laver vi dato, tid og sæson variabler
superstats_dataframe <- superstats_dataframe |>
  filter(str_detect(dato, "^\\d{2}/\\d{2}")) |>
  mutate(
    måned = as.integer(substr(dato, 4, 5)),
    år    = if_else(måned >= 7, sæson - 1, sæson),
    datetime = lubridate::dmy_hm(
      paste0(substr(dato, 1, 5), "/", år, " ", substr(dato, 7, 11)),
      tz = "Europe/Copenhagen"
    ),
    dato = as.Date(datetime),
    runde_nr = as.integer(str_extract(runde, "\\d+")),
    datetime_hour = floor_date(datetime, unit = "hour"),
    sæson   = case_when(
      month(dato) >= 7 ~ paste0(year(dato), "/", year(dato) + 1),
      TRUE             ~ paste0(year(dato) - 1, "/", year(dato))
  )
)

# Her danner vi en ny variable, hvor vi kan se tilskuerantallet til sidste hjemmekamp mod den givende modstander
superstats_dataframe <- superstats_dataframe |>
  mutate(
    tilskuertal = as.numeric(gsub("\\.", "", tilskuertal)),
    modstander  = str_squish(str_remove(hold, "^VFF-"))
  ) |>
  arrange(modstander, datetime) |>
  group_by(modstander) |>
  mutate(tilskuere_sidste_modstander = lag(tilskuertal)) |>
  ungroup() |>
  arrange(datetime)

# Vi cleaner vores dataframe, så vi kun har de nødvendige variabler med
superstats_clean <- superstats_dataframe |>
  dplyr::select(
    ugedag, hold, hold_kategori, mål_hjemme, mål_ude,
    tilskuertal, tilskuere_sidste_modstander, runde_nr, sæson,
    vff_sejr, sejre_seneste_3, mål_seneste_3,
    point, point_seneste_3,
    datetime, datetime_hour, dato 
  )

view(superstats_clean)


# Helligdage fra Nager.Date -----------------------------------------------

#Først laver vi en container, som vi kan smide vores data ned i
helligdage_list <- list()

# Her laver vi en løkke, som henter data fra nager ned for hvert år
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

# Her udvælger kun date og localName variablerne. Derefter laver vi navnene om, samt fjerne banklukkedag i vores datasæt
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
  hent_dmi_data <- function(station_id, parameter_id, start_date, end_date, limit = 50000) {
  query <- paste0(
    "stationId=", station_id,
    "&parameterId=", parameter_id,
    "&datetime=", start_date, "Z/", end_date, "Z",
    "&limit=", format(limit, scientific = FALSE)
  )
  
  # Bygger full URL med API-nøgle
  full_url <- paste0(dmi_base_url, dmi_info_url, query, "&api-key=", api_key)
  
  # API-kald
  response <- GET(full_url)
  
  # Tjekker status
  if (status_code(response) != 200) {
    stop("API fejl for ", parameter_id, ": Status ", status_code(response))
  }
  
  # Parse response 
  content_json <- jsonlite::fromJSON(rawToChar(response$content), flatten = FALSE)
  
  # Tjekker om der er data
  if (is.null(content_json$features) || length(content_json$features) == 0) {
    warning("Ingen data fundet for ", parameter_id)
    return(tibble())
  }
  
  props <- content_json$features$properties
  
  # Udtræk data
  df_selected <- tibble(
    observationstidspunkt = props$observed,
    værdi = as.numeric(props$value),
    parameter = parameter_id
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
    limit         = 50000
  ) |> 
    dplyr::mutate(type = "vind")
  
  # Temperatur
  temp_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "temp_mean_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000
  ) |> 
    dplyr::mutate(type = "temp")
  
  # Nedbør
  nedbør_y <- hent_dmi_data(
    station_id    = karup,
    parameter_id  = "precip_past1h",
    start_date    = start,
    end_date      = slut,
    limit         = 50000
  ) |> 
    dplyr::mutate(type = "nedbør")
  
  # Saml for året
  vejr_list[[as.character(y)]] <- dplyr::bind_rows(
    vind_y,
    temp_y,
    nedbør_y
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
  # Her vælger vi kun at beholder datetime
  dplyr::select(datetime_hour, dplyr::everything(), -datetime, -år, -observationstidspunkt, -datotid_utc)

view(vejr_wide)



# Joining af datasæt ------------------------------------------------------
    # Opretter en projekt-database 
con_sql <- dbConnect(SQLite(), "data/vff_eksamen.sqlite")

# Sætter de rensede dataframes ind som tabeller i databasen
dbWriteTable(con_sql, "superstats", superstats_clean, overwrite = TRUE)
dbWriteTable(con_sql, "helligdage", helligdage,      overwrite = TRUE)
dbWriteTable(con_sql, "vejr",       vejr_wide,       overwrite = TRUE)
dbWriteTable(con_sql, "vffkort",    vffkort01,       overwrite = TRUE)

dbListTables(con_sql) 

# SQL-join
sql_join <- "
SELECT 
  s.sæson,
  s.runde_nr,
  s.ugedag,
  s.hold,
  s.mål_hjemme,
  s.mål_ude,
  s.tilskuertal,
  s.vff_sejr,
  s.sejre_seneste_3,
  s.mål_seneste_3,
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
    k.d10_tilskuere,
    k.d7_tilskuere,
    k.d3_tilskuere

FROM superstats AS s

LEFT JOIN helligdage AS h
  ON s.dato = h.dato

LEFT JOIN vejr AS v
  ON s.datetime_hour = v.datetime_hour

LEFT JOIN vffkort AS k
  ON s.sæson   = k.sæson
 AND s.runde_nr = k.runde

-- behold kun VFF-hjemmekampe
WHERE s.Hold LIKE 'VFF-%'

GROUP BY 
  s.sæson,
  s.runde_nr,
  s.datetime 
  
  HAVING COUNT(s.datetime) >= 1
"

# Hent joinet datasæt tilbage til R
fuld_datasæt <- dbGetQuery(con_sql, sql_join)
dbDisconnect(con_sql)

str(fuld_datasæt)

# Konvertere til de rigite variabler
fuld_datasæt <- fuld_datasæt |>
  mutate(
    # ---- POSIXCT ----
  datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "Europe/Copenhagen"),
    # ---- Datetime ----
  dato = as.Date(dato, origin = "1970-01-01"),
    # ---- Factor variabler ----
  ugedag = as.factor(ugedag),
  hold = as.factor(hold),
  sæson = as.factor(sæson),
    # ---- Numeric variabler ----
  tilskuertal = as.numeric(gsub("\\.", "", tilskuertal)),
  mål_hjemme = as.numeric(mål_hjemme),
  mål_ude = as.numeric(mål_ude),
  vff_sejr = as.numeric(vff_sejr),
  sejre_seneste_3 = as.numeric(sejre_seneste_3),
  mål_seneste_3 = as.numeric(mål_seneste_3),
  point = as.numeric(point),
  point_seneste_3 = as.numeric(point_seneste_3),
  helligdag_dummy = as.integer(helligdag_dummy),
  tilskuere_sidste_modstander = as.numeric(tilskuere_sidste_modstander),
    
  d10_tilskuere = as.numeric(d10_tilskuere),
  d7_tilskuere  = as.numeric(d7_tilskuere),
  d3_tilskuere  = as.numeric(d3_tilskuere)
  )

view(fuld_datasæt)


#__________Esktra tilføjelse af variabeler______________________________________

kamp_vejr_window <- list()

# Vi danner en variable med gennesnittet af vejret 3 timer før kampstart til slutfløjt
for(i in 1:nrow(fuld_datasæt)) {
  
  kamp_tid <- fuld_datasæt$datetime[i]
  start_window <- kamp_tid - hours(3)
  slut_window  <- kamp_tid + hours(2)
  
  # filtrér vejrdata i vinduet
  vejr_subset <- vejr_wide |>
    filter(datetime_hour >= start_window,
           datetime_hour <= slut_window)
  
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

# Laver måned variabel, samt weekend og tidspunkt dummies
fuld_datasæt <- fuld_datasæt |>
  dplyr::mutate(
    måned = factor(lubridate::month(dato)),
    weekend = if_else(ugedag %in% c("Lør", "Søn"), 1, 0),
    tidspunkt = factor(if_else(lubridate::hour(datetime) < 17, "Eftermiddag", "Aften"))
  )

view(fuld_datasæt)

# Datasæt for de 4 tidsperioder -------------------------------------------
  # 1 måned før
data_1_mdr <- fuld_datasæt |>
  dplyr::select(tilskuertal, hold_kategori, weekend, helligdag_dummy,
  tilskuere_sidste_modstander, tidspunkt, måned
  ) |>
  na.omit()
summary(lm(tilskuertal ~ ., data = data_1_mdr))

  # 10 dage før
data_10d <- fuld_datasæt |>
  dplyr::select(tilskuertal, d10_tilskuere, hold_kategori, weekend,
  helligdag_dummy, måned, tilskuere_sidste_modstander, tidspunkt
  ) |>
  na.omit()
summary(lm(tilskuertal ~ ., data = data_10d))

  # 7 dage før
data_7d <- fuld_datasæt |>
  dplyr::select(tilskuertal, d7_tilskuere, hold_kategori, weekend,
  helligdag_dummy, måned, tilskuere_sidste_modstander, tidspunkt,
  sejre_seneste_3, mål_seneste_3
  ) |>
  na.omit()
summary(lm(tilskuertal ~ ., data = data_7d))

  # 3 dage før
data_3d <- fuld_datasæt |>
  dplyr::select(tilskuertal, d3_tilskuere, hold_kategori, weekend,
  helligdag_dummy, gns_temp, gns_nedbør, gns_vind, tilskuere_sidste_modstander,
  tidspunkt, sejre_seneste_3, mål_seneste_3
  ) |>
  na.omit()
summary(lm(tilskuertal ~ ., data = data_3d))


# Modeller ----------------------------------------------------------------
kør_alle_modeller <- function(data, seed = 7, k = 5) {
  set.seed(seed)
  
  # ---- Split (train/test) ----
  n <- nrow(data)
  train_opdeling <- sample(seq_len(n), size = floor(0.70 * n))
  test_opdeling  <- setdiff(seq_len(n), train_opdeling)
  
  train <- data[train_opdeling, ]
  test  <- data[test_opdeling, ]
  
  # ---- Stor LM (test RMSE) ----
  lm_fit <- lm(tilskuertal ~ ., data = train)
  lm_pred <- predict(lm_fit, newdata = test)
  rmse_lm_test <- sqrt(mean((test$tilskuertal - lm_pred)^2))
  
  # ---- Ridge / Lasso (CV på train + test RMSE) ----
  x_train <- model.matrix(tilskuertal ~ ., train)[, -1]
  y_train <- train$tilskuertal
  x_test  <- model.matrix(tilskuertal ~ ., test)[, -1]
  y_test  <- test$tilskuertal
  
  # ridge
  set.seed(seed)
  ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = k)
  bestlam_ridge <- ridge_cv$lambda.min
  rmse_ridge_cv <- sqrt(min(ridge_cv$cvm))
  ridge_pred <- predict(ridge_cv, s = bestlam_ridge, newx = x_test)
  rmse_ridge_test <- sqrt(mean((y_test - ridge_pred)^2))
  
  # lasso
  set.seed(seed)
  lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = k)
  bestlam_lasso <- lasso_cv$lambda.min
  rmse_lasso_cv <- sqrt(min(lasso_cv$cvm))
  lasso_pred <- predict(lasso_cv, s = bestlam_lasso, newx = x_test)
  rmse_lasso_test <- sqrt(mean((y_test - lasso_pred)^2))
  
  
  # ---- Best subset (K-fold CV på train + test RMSE) ----
  form <- tilskuertal ~ .
  predict_regsubsets <- function(object, newdata, form, id) {
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    as.numeric(mat[, xvars, drop = FALSE] %*% coefi)
  }
  
  
  set.seed(seed)
  folds <- sample(rep(1:k, length = nrow(train)))
  
  cv.errors <- matrix(NA, k, ncol(train) - 1)
  
  for (j in 1:k) {
    best.fit <- leaps::regsubsets(form, data = train[folds != j, ], nvmax = ncol(train) - 1)
    for (i in 1:(ncol(train) - 1)) {
      pred <- predict_regsubsets(best.fit, train[folds == j, ], form = form, id = i)
      cv.errors[j, i] <- mean((train$tilskuertal[folds == j] - pred)^2)
    }
  }
  
  mean_mse <- colMeans(cv.errors)
  best_size <- which.min(mean_mse)
  rmse_subset_cv <- sqrt(min(mean_mse))
  
  reg_best <- leaps::regsubsets(form, data = train, nvmax = ncol(train) - 1)
  subset_pred <- predict_regsubsets(reg_best, test, form = form, id = best_size)
  rmse_subset_test <- sqrt(mean((test$tilskuertal - subset_pred)^2))
  
  subset_coef <- coef(reg_best, id = best_size)
  
  list(
    rmse_lm_test     = rmse_lm_test,
    rmse_ridge_cv    = rmse_ridge_cv,
    rmse_ridge_test  = rmse_ridge_test,
    rmse_lasso_cv    = rmse_lasso_cv,
    rmse_lasso_test  = rmse_lasso_test,
    rmse_subset_cv   = rmse_subset_cv,
    rmse_subset_test = rmse_subset_test,
    best_size        = best_size,
    subset_coef      = subset_coef
  )
}

res_1mdr <- kør_alle_modeller(data_1_mdr)
res_10d  <- kør_alle_modeller(data_10d)
res_7d   <- kør_alle_modeller(data_7d)
res_3d   <- kør_alle_modeller(data_3d)


rmse_table <- tibble::tibble(
  tidshorisont = c("1 måned", "10 dage", "7 dage", "3 dage"),
  LM_test_rmse = c(res_1mdr$rmse_lm_test,     res_10d$rmse_lm_test,     res_7d$rmse_lm_test,     res_3d$rmse_lm_test),
  Ridge_CV_    = c(res_1mdr$rmse_ridge_cv,    res_10d$rmse_ridge_cv,    res_7d$rmse_ridge_cv,    res_3d$rmse_ridge_cv),
  Ridge_test   = c(res_1mdr$rmse_ridge_test,  res_10d$rmse_ridge_test,  res_7d$rmse_ridge_test,  res_3d$rmse_ridge_test),
  Lasso_CV     = c(res_1mdr$rmse_lasso_cv,    res_10d$rmse_lasso_cv,    res_7d$rmse_lasso_cv,    res_3d$rmse_lasso_cv),
  Lasso_test   = c(res_1mdr$rmse_lasso_test,  res_10d$rmse_lasso_test,  res_7d$rmse_lasso_test,  res_3d$rmse_lasso_test),
  Subset_CV    = c(res_1mdr$rmse_subset_cv,   res_10d$rmse_subset_cv,   res_7d$rmse_subset_cv,   res_3d$rmse_subset_cv),
  Subset_test  = c(res_1mdr$rmse_subset_test, res_10d$rmse_subset_test, res_7d$rmse_subset_test, res_3d$rmse_subset_test),
  best_size    = c(res_1mdr$best_size,        res_10d$best_size,        res_7d$best_size,        res_3d$best_size)
)

rmse_table


  

# Best/ worst case scenario -----------------------------------------------
best_case_7d <- data.frame(
  d7_tilskuere = quantile(`7d_data`$d7_tilskuere, 0.90),
  hold_kategori = factor("A", levels = c("A","B","C")),
  Ugedag = factor("Søn", levels = levels(`7d_data`$Ugedag)),
  helligdag_dummy = 0,
  måned = factor(2, levels = levels(`7d_data`$måned)),
  tilskuere_sidste_modstander =
    quantile(`7d_data`$tilskuere_sidste_modstander, 0.90)
)

pred_best_7d <- predict(lm_7d, newdata = best_case_7d)
pred_best_7d



`7d_data`$tilskuere_sidste_modstander




fuld_datasæt |>
  dplyr::filter(Hold == "VFF-BIF") |>
  dplyr::arrange(desc(dato)) |>
  dplyr::select(dato, Tilskuertal)





# ------------------------------------------------------------
# Prædiktion (1 måned) på Viborg FF vs Brøndby IF
# ------------------------------------------------------------

# Sikr korrekt datatype
`1mdr_data` <- `1mdr_data` |>
  dplyr::mutate(
    hold_kategori = factor(hold_kategori, levels = c("A", "B", "C"))
  )

# Find min og max historisk tilskuertal mod Brøndby
broendby_tilskuere_range <- fuld_datasæt |>
  dplyr::filter(Hold == "VFF-BIF") |>
  dplyr::summarise(
    min_tilskuere = min(Tilskuertal, na.rm = TRUE),
    max_tilskuere = max(Tilskuertal, na.rm = TRUE)
  )

# Best/worst-case newdata for Brøndby kampen
broendby_scenarios <- tibble(
  scenario = c("Worst-case", "Best-case"),
  hold_kategori = factor(c("A", "A"), levels = levels(`1mdr_data`$hold_kategori)),
  weekend = c(1, 1),
  helligdag_dummy = c(0, 0),
  måned = factor(c(2, 2), levels = levels(`1mdr_data`$måned)),
  tilskuere_sidste_modstander = c(
    broendby_tilskuere_range$min_tilskuere,
    broendby_tilskuere_range$max_tilskuere
  )
)

# Træn 1-måned-modellen
best_model_1mdr <- lm(
  Tilskuertal ~
    hold_kategori +
    weekend +
    helligdag_dummy +
    måned +
    tilskuere_sidste_modstander,
  data = `1mdr_data`
)

# Prædiktion for Brøndby
broendby_scenarios$pred_tilskuertal <- predict(
  best_model_1mdr,
  newdata = broendby_scenarios
)

broendby_scenarios

