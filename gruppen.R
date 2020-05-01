


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

## Studentenmessungen vs Checkpointmessungen

ggplot(data = filter(zlg_beide_bereinigt_summen, type %in% c("checkpoint", "gesamt"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Checkpoint- und Studentendaten",
       x = "Art der Messung", y = "Anzahl")

# als Anteil in Prozent

zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "checkpoint"] / 
  zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "gesamt"]



# Anteil anderer Kontakte an Gesamtmessungen

ggplot(data = filter(zlg_beide_bereinigt_summen, type %in% c("aK_gesamt", "SG_gesamt"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Skitourengeher und anderer Kontakte",
       x = "Art der Messung", y = "Anzahl")

# als Anteil in Prozent

zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "aK_gesamt"] / 
  zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "gesamt"]






# Dataframe mit Summen für alle Daten erstellen

zlg_beide_summen <- data.frame(type = colnames(zlg_beide)[-c(1,11)], 
                               sum = apply(zlg_beide[,-c(1,11)], 2, sum), 
                               row.names = NULL)
zlg_beide_summen$type <- as.character(zlg_beide_summen$type)

zlg_beide_summen <- rbind(zlg_beide_summen, 
                          c("checkpoint", as.numeric(sum(zlg_ckpt_beide$erfasst))))

zlg_beide_summen$type <- as.factor(zlg_beide_summen$type)
zlg_beide_summen$sum <- as.numeric(zlg_beide_summen$sum)

zlg_beide_summen


## Summen als Plot nebeneinander

zlg_beide_summen <- 
mutate(zlg_beide_summen, 
       erfassung = as.factor(c("erfasst", "nicht_erfasst", "erfasst", 
                               "nicht_erfasst", "erfasst", "nicht_erfasst", 
                               "beide", "beide", "beide", "beide")),
       kontakt = c("SG", "SG", "aK", "aK", "beide", "beide", "SG", "aK",
                   "beide", "beide"))

ggplot(zlg_beide_summen[1:4,], 
       aes(x = factor(kontakt, levels = c("SG", "aK")),
           y = sum, fill = erfassung)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Erfassungen nach Art des Kontakts",
       x = NULL,
       y = "absolute Häufigkeit") +
  scale_x_discrete(labels=c("SG" = "Skitourengänger", 
                            "aK" = "andere Kontakte")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size= 15)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst")) +
  geom_text(aes(label=sum), position=position_dodge(width=0.9), vjust=-0.25)

ggsave("Plots/Erfassungen_nach_Kontakt.png")


# Erfassungen vs Nichterfassungen zu allen Daten

ggplot(data = filter(zlg_beide_summen, type %in% c("nicht_erfasst", "erfasst"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Erfassungen und Nichterfassungen",
       x = "Art der Messung", y = "Anzahl")

# Anteil Nichterfassungen in Prozent

zlg_beide_summen$sum[zlg_beide_summen$type == "nicht_erfasst"] / 
  zlg_beide_summen$sum[zlg_beide_summen$type == "gesamt"]






# Erfassung je Gruppe berechnen (für kompletten Datensatz)

erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
                      summarise(anzahl = sum(erfasst), 
                                mittel = mean(erfasst),
                                häufigkeit = n()) %>% 
                        mutate(type = "erfasst")

nicht_erfasst_je_gruppe <- zlg_beide %>% group_by(gesamt) %>% 
                            summarise(anzahl = sum(nicht_erfasst),
                                      mittel = mean(nicht_erfasst),
                                      häufigkeit = n()) %>% 
                              mutate(type = "nicht_erfasst")

erfassung_je_gruppe <- rbind(erfasst_je_gruppe, nicht_erfasst_je_gruppe) %>%
                        rename(grösse = gesamt)

erfassung_je_gruppe

# Plot Anzahl der (Nicht-)Erfassungen nach Gruppengröße

anz_gruppe <- 
ggplot(data = erfassung_je_gruppe, aes(x = grösse, y = anzahl, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der (Nicht-)Erfassungen nach Gruppengröße",
       x = "Gruppengröße",
       y = "Anzahl") +
  scale_x_continuous(breaks = 1:8) +
  theme(
        axis.line = element_line(colour = "black"),
        text = element_text(size= 15)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

ggsave("Plots/Anzahl_nach_Gruppe.png", anz_gruppe)

# Plot Verhältnis der (Nicht-)Erfassungen nach Gruppengröße
# (Warnmeldung, weil keine Gruppen von 6 bzw. 7) 

verh_gruppe <- 
ggplot(data = erfassung_je_gruppe) +
  geom_bar(aes(x = grösse, y = anzahl, fill = type), 
           stat = "identity", position = "fill") +
  labs(title = "Verhältnis der (Nicht-)Erfassungen nach Gruppengröße",
       x = "Gruppengröße",
       y = "Anteil") +
  scale_x_continuous(breaks = 1:8) +
  theme(
    axis.line = element_line(colour = "black"),
    text = element_text(size= 15)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

ggsave("Plots/Anteil_nach_Gruppe.png", verh_gruppe)


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

# nur Punkte, ohne Boxplot

ggplot(data = zlg_beide,
       aes(x = as.factor(gesamt), y = nicht_erfasst/gesamt, 
           color = as.factor(gesamt))) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  labs(title = "Anteil der Nichterfassungen je Summe Messungen in einer Minute",
       x = "Gruppengröße", y = "Anteil Nichterfassungen") +
  theme(legend.position = "none")










