## Diese Datei erzeugt data_group
# Dem Szenario 2 (Gruppen) folgend werden neue Messungen generiert

## data_group erzeugen (ohne NAs da wir diese Tage eh nicht brauchen)

min_data_group <- min_data_noNA

## Neue Personenanzahl berechnen
# Gruppengrößenvektor erhalten

group_size <- min_data_noNA$count_people_min

# Anzahl der hinzuzufügenden Personen berechnen
# new_person_prob gibt die Wahrscheinlichkeit an für jede Person in der 
# Gruppe eine weitere hinzuzufügen
# bei Gruppen größer gleich 50 ist die Wahrscheinlichkeit = 1 (Vorsicht!)

new_person_prob <- 0.20 + 0.016 * group_size
new_group_size <- group_size + rbinom(length(group_size), 
                                      group_size, new_person_prob)

## zu data_group hinzufügen

min_data_group$count_people_min <- new_group_size

# neue Summen berechnen

min_data_group <- min_data_group %>%
  mutate(
    lvs_false_min = count_people_min - lvs_true_min, # nur Beacon unterschätzen
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
                                         ratio_min)))
                                    