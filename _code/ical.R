library(tidyverse)
library(lubridate)
library(calendar)
library(purrr)

# defining the planning table
year <- 2026

planning <- tibble::tribble(
    ~summary,                                              ~wday,     ~day, ~start_hm, ~end_hm,
    "IMSB cours n°01 : Introduction à l'inférence bayésienne",  "Lundi",     19, "14:00",   "17:00",
    "IMSB cours n°02 : Modèle Beta-Binomial",                   "Mardi",     20, "09:00",   "12:00",
    "IMSB cours n°03 : Introduction à brms, modèle linéaire",   "Mardi",     20, "14:00",   "17:00",
    "IMSB cours n°04 : Modèle de régression linéaire (suite)",  "Mercredi",  21, "09:00",   "12:00",
    "IMSB cours n°05 : Markov Chain Monte Carlo",               "Mercredi",  21, "14:00",   "17:00",
    "IMSB cours n°06 : Modèle linéaire généralisé",             "Jeudi",     22, "09:00",   "12:00",
    "IMSB cours n°07 : Comparaison de modèles",                 "Jeudi",     22, "14:00",   "17:00",
    "IMSB cours n°08 : Modèles multi-niveaux (généralisés)",    "Vendredi",  23, "09:00",   "12:00",
    "IMSB cours n°09 : Examen final",                           "Vendredi",  23, "14:00",   "17:00"
    )

# Europe/Paris times
make_dt <- function (day_num, hm) {
    
    ymd_hm(sprintf("%04d-01-%02d %s", year, day_num, hm), tz = "Europe/Paris")
    
}

# reshaping in dataframe
planning <- planning |>
    mutate(
        dt_start = make_dt(day, start_hm),
        dt_end   = make_dt(day, end_hm)
        )

# building events row-by-row and binding to a single data.frame
ics_df <- pmap_dfr(
    planning[, c("dt_start", "dt_end", "summary")],
    ~ic_event(start_time = ..1, end_time = ..2, summary = ..3)
    )

# optional: adding a course-wide DESCRIPTION or LOCATION
ics_df$DESCRIPTION <- "Formation interdisciplinaire de modélisation statistique bayésienne, M2R, Université Grenoble-Alpes"
ics_df$LOCATION <- "Université Grenoble Alpes"

# writing the .ics file
# ic_write(ics_df, "files/planning.ics", zulu = FALSE)
ic_write(ics_df, "files/planning.ics")
