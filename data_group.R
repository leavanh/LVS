## Diese Datei erzeugt data_group
# Dem Szenario 2 (Gruppen) folgend werden neue Messungen generiert

## Pakete laden

library("tidyverse")
library("lubridate")

## Daten laden

data <- readRDS(file = "Daten/data.RDS")
date_data <- readRDS(file = "Daten/date_data.RDS")
min_data <- readRDS(file = "Daten/min_data.RDS")

# NAs (fehlende Werte) rauswerfen

date_data_noNA <- date_data[!is.na(date_data$count_people),]
data_noNA <- data[!is.na(data$lvs),]
min_data_noNA <- min_data[!is.na(min_data$count_people_min),]

# Da die Daten zufällig erzeugt werden benutzen wir einen seed
# so kommt bei jedem Durchlauf das selbe Ergebnis raus

set.seed(42)


## data_group erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

min_data_group <- min_data_noNA

## Neue Personenanzahl berechnen

group_size <- min_data_noNA$count_people_min # Gruppengrößenvektor erhalten

# Anzahl der hinzuzufügenden Personen berechnen
# new_person_prob gibt die Wahrscheinlichkeit an für jede Person in der 
# Gruppe eine weitere hinzuzufügen
# bei Gruppen größer gleich 50 ist die Wahrscheinlichkeit >= 1 (Vorsicht!)

new_person_prob <- 0.20 + 0.016 * group_size
new_group_size <- group_size + rbinom(length(group_size), 
                                      group_size, new_person_prob)

## zu data_group hinzufügen

min_data_group$count_people_min <- new_group_size

# neue Summen berechnen

min_data_group <- min_data_group %>%
  mutate(
    lvs_false_min = count_people_min - lvs_true_min, # nur nicht-LVS unterschätzen
    ratio_min = lvs_true_min/count_people_min)

min_data_group <- min_data_group %>%
  group_by(date) %>%
  mutate(lvs_true = sum(lvs_true_min),
         lvs_false = sum(lvs_false_min),
         count_people = lvs_true + lvs_false,
         ratio = lvs_true/(count_people)) %>%
  ungroup()

date_data_group <- distinct(subset(min_data_group, 
                             select = -c(time, lvs_true_min,
                                         lvs_false_min, count_people_min,
                                         ratio_min, cloud_cover)))

# abspeichern

saveRDS(min_data_group, file = "Daten/min_data_group.RDS")
saveRDS(date_data_group, file = "Daten/date_data_group.RDS")                                    