


### Erst nur Unterschätzung: alle überschätzten Sachen raus



## alle manuell eingetragenen Überschätzungswerte aus den Studentendaten raus:

# neuer, bereinigter Datensatz

zlg_beide_bereinigt <- zlg_beide

zlg_ckpt_beide_bereinigt <- zlg_ckpt_beide


# spielende Kinder aus manueller Zählung entfernen
# Zeilen mit Kinder auswählen und Werte auf Null setzen

zlg_beide_bereinigt[135:140, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[c(33,34), -c(1,3)] <- 0

# Hund entfernen

zlg_beide_bereinigt[213, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[61, -c(1,3)] <- 0

# "Gerät erkennt erst "LVS erst nicht erkannt, dann doch"
# -> passt nicht zu den Daten vom Checkpoint


# Gerät piepst drei mal bei zwei Personen

zlg_beide_bereinigt[448, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[84, -c(1,3)] <- 2


# Gerät piepst durchgängig, da eine Person sich Schuhe auszieht

zlg_beide_bereinigt[482:484, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[94:97, -c(1,3)] <- 0

# Gerät piepst zwei mal, Person macht vor Gerät Foto

zlg_beide_bereinigt[488, -c(1,11)] <- 0

zlg_ckpt_beide_bereinigt[98, -c(1,3)] <- 0





## Über gesamten Zeitraum, bereinigter Datensatz: 

## Studentenmessungen vs Checkpointmessungen

messungvergleich <- data.frame(type = c("Studenten", "Checkpoint"),
                               sum =as.numeric(
                                      c(sum(zlg_beide_bereinigt$gesamt),
                                        sum(zlg_ckpt_beide_bereinigt$erfasst))))
messungvergleich

ggplot(data = messungvergleich) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Checkpoint- und Studentendaten",
       x = "Art der Messung", y = "Anzahl")


## Anteil anderer Kontakte an Gesamtmessungen der Studenten

# Passenden Dataframe mit Summen für Plot erstellen

zlg_beide_bereinigt_summen <- data.frame(type = colnames(zlg_beide_bereinigt)[-c(1,11)], 
                                        sum = apply(zlg_beide_bereinigt[,-c(1,11)], 2, sum), 
                                        row.names = NULL)
zlg_beide_bereinigt_summen$type <- as.character(zlg_beide_bereinigt_summen$type)

zlg_beide_bereinigt_summen <- rbind(zlg_beide_bereinigt_summen, 
                                    c("checkpoint", as.numeric(sum(zlg_ckpt_beide_bereinigt$erfasst))))

zlg_beide_bereinigt_summen$type <- as.factor(zlg_beide_bereinigt_summen$type)
zlg_beide_bereinigt_summen$sum <- as.numeric(zlg_beide_bereinigt_summen$sum)

zlg_beide_bereinigt_summen

# Anteil anderer Kontakte an Gesamtmessungen

ggplot(data = filter(zlg_beide_bereinigt_summen, type %in% c("aK_gesamt", "gesamt"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity")




## Anteil Erfassungen vs Nichterfassungen zu allen Daten

# Dataframe mit Summen erstellen

zlg_beide_summen <- data.frame(type = colnames(zlg_beide)[-c(1,11)], 
                               sum = apply(zlg_beide[,-c(1,11)], 2, sum), 
                               row.names = NULL)
zlg_beide_summen$type <- as.character(zlg_beide_summen$type)

zlg_beide_summen <- rbind(zlg_beide_summen, 
                          c("checkpoint", as.numeric(sum(zlg_ckpt_beide$erfasst))))

zlg_beide_summen$type <- as.factor(zlg_beide_summen$type)
zlg_beide_summen$sum <- as.numeric(zlg_beide_summen$sum)

zlg_beide_summen

# Plot

ggplot(data = filter(zlg_beide_summen, type %in% c("gesamt", "checkpoint"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity")






# Erfassung je Gruppe berechnen (für kompletten Datensatz)

erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
                      summarise(anzahl = sum(erfasst), 
                                mittel = mean(erfasst)) %>% 
                        mutate(type = "erfasst")

nicht_erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
                            summarise(anzahl = sum(nicht_erfasst),
                                      mittel = mean(nicht_erfasst)) %>% 
                              mutate(type = "nicht_erfasst")

erfassung_je_gruppe <- rbind(erfasst_je_gruppe, nicht_erfasst_je_gruppe) %>%
                        rename(grösse = gesamt)



# Plot Anzahl der (Nicht-)Erfassungen nach Gruppengröße

ggplot(data = erfassung_je_gruppe) +
  geom_bar(aes(x = grösse, y = anzahl, fill = type), 
           stat = "identity", position = "dodge") +
  labs(title = "absolute Anzahl der (Nicht-)Erfassungen nach Gruppengröße")

# Plot Verhältnis der (Nicht-)Erfassungen nach Gruppengröße
# (Warnmeldung, weil keine Gruppen von 6 bzw. 7) 

ggplot(data = erfassung_je_gruppe) +
  geom_bar(aes(x = grösse, y = anzahl, fill = type), 
           stat = "identity", position = "fill") +
  labs(title = "Verhältnis der (Nicht-)Erfassungen nach Gruppengröße",
       y = "Anteil")


# Boxplot: je Gruppengröße Anteil nicht erfasster

ggplot(data = zlg_beide, aes(x = as.factor(gesamt), y = nicht_erfasst/gesamt)) +
  geom_boxplot()

ggplot(data = zlg_beide, aes(x = as.factor(gesamt), y = nicht_erfasst)) +
  geom_boxplot()

# Violineneplot je Gruppengröße Anteil nicht erfasster

ggplot(data = zlg_beide, aes(x = as.factor(gesamt), y = nicht_erfasst/gesamt)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Boxplot mit einzelnen Punkten Anteil Nichterfassungen je Gruppengröße

ggplot(data = zlg_beide,
       aes(x = as.factor(gesamt), y = nicht_erfasst/gesamt, 
           color = as.factor(gesamt))) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.3) +
  labs(title = "Anteil der Nichterfassungen je Summe Messungen in einer Minute",
       x = "Gruppengröße", y = "Anteil Nichterfassungen") +
  theme(legend.position = "none")










