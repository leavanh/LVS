## Diese Datei erzeugt data3
# Dem Szenario 3 (Wildtiere) folgend werden Messungen gelöscht

## data3 erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

data3 <- data_noNA

## Messungen löschen

# welche kommen in Frage?

time_intervall <- interval(
  as.POSIXct("1900-01-01 01:00:00", tz = "UTC"),
  as.POSIXct("1900-01-01 03:59:59", tz = "UTC")
)

# neue Summen berechnen

min_data2 <- min_data2 %>%
  mutate(
    lvs_false_min = count_people_min - lvs_true_min, # nur Beacon unterschätzen
    ratio_min = lvs_true_min/count_people_min)

min_data2 <- min_data2 %>%
  group_by(date) %>%
  mutate(lvs_true = sum(lvs_true_min),
         lvs_false = sum(lvs_false_min),
         count_people = lvs_true + lvs_false,
         ratio = lvs_true/(count_people))

date_data2 <- distinct(subset(min_data2, 
                              select = -c(time, lvs_true_min,
                                          lvs_false_min, count_people_min,
                                          ratio_min)))
