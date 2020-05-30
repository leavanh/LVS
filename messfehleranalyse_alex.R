
### In dieser Datei werden die Plots für die deskriptive Messfehleranalyse 
### erzeugt

source("prepare_data_1920.R", encoding = "UTF-8")

# Von den Studenten gezählte Personen, sortiert nach Erfassungsart am 27.02

sums_27$type2 <- factor(sums_27$type, as.character(sums_27$type)) 
ggplot(sums_27, aes(x = type2, y = sum)) +
  geom_bar(aes(x=type2), data=sums_27, stat="identity") +
  labs(title = "Personen am 27.02.2019 (Manuelle Messung)",
       x = "Personen",
       y = "absolute Häufigkeit") +
  scale_x_discrete(labels=c("aK_gesamt" = "Erfasste und \n nicht erfasste \n andere Kontakte", 
                            "erfasst" = "Erfasst \n andere Kontakte und \n Skitourengeher",
                            "Erfasst_aK" = "Erfasste \n andere Kontakte", "Erfasst_SG"= "Erfasste \n Skitourengeher", 
                            "gesamt" = "Erfasste und \n nicht erfasste Skitourengeher \n und andere Kontakte", 
                            "nicht_erfasst" = "Nicht erfasste \n Skitourengeher \n andere Kontakte",
                            "Nicht_erfasst_aK" = "Nicht erfasste \n andere Kontakte", 
                            "Nicht_erfasst_SG" = "Nicht erfasste \n Skitourengeher", 
                            "SG_gesamt" = "Erfasste und \n nicht erfasste \n Skitourengeher")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

# Vergleich nach Art des Kontakts am 27.02.

plot_zlg27 <-ggplot(sums_27, aes(x = type2, y = sum)) +
  geom_bar(aes(x=type2), data=sums_27, stat="identity") +
  labs(title = "Personen am 27.02.2019 (Manuelle Messung)",
       x = "Personen",
       y = "absolute Häufigkeit") +
  scale_x_discrete(labels=c("aK_gesamt" = "Erfasste und \n nicht erfasste \n andere Kontakte", 
                            "erfasst" = "Erfasst \n andere Kontakte und \n Skitourengeher",
                            "Erfasst_aK" = "Erfasste \n andere Kontakte", "Erfasst_SG"= "Erfasste \n Skitourengeher", 
                            "gesamt" = "Erfasste und \n nicht erfasste Skitourengeher \n und andere Kontakte", 
                            "nicht_erfasst" = "Nicht erfasste \n Skitourengeher \n andere Kontakte",
                            "Nicht_erfasst_aK" = "Nicht erfasste \n andere Kontakte", 
                            "Nicht_erfasst_SG" = "Nicht erfasste \n Skitourengeher", 
                            "SG_gesamt" = "Erfasste und \n nicht erfasste \n Skitourengeher"),
                   limit = c( "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_zlg27

# Von den Studenten gezählte Personen, sortiert nach Erfassungsart am 28.02

sums_28$type2 <- factor(sums_28$type, as.character(sums_28$type)) 
ggplot(sums_28, aes(x = type2, y = sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Anzahl nach Art der Erfassungen am 28.02.",
       x = "Art der Erfassung",
       y = "absolute Anzahl") +
  scale_x_discrete(labels=c("aK_gesamt" = "Erfasste und \n nicht erfasste \n andere Kontakte", 
                            "erfasst" = "Erfasst \n andere Kontakte und \n Skitourengeher",
                            "Erfasst_aK" = "Erfasste \n andere Kontakte", "Erfasst_SG"= "Erfasste \n Skitourengeher", 
                            "gesamt" = "Erfasste und \n nicht erfasste Skitourengeher \n und andere Kontakte", 
                            "nicht_erfasst" = "Nicht erfasste \n Skitourengeher \n andere Kontakte",
                            "Nicht_erfasst_aK" = "Nicht erfasste \n andere Kontakte", 
                            "Nicht_erfasst_SG" = "Nicht erfasste \n Skitourengeher", 
                            "SG_gesamt" = "Erfasste und \n nicht erfasste \n Skitourengeher")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Vergleich nach Art des Kontakts 28.02.

plot_zlg28 <- ggplot(sums_28, aes(x = type2, y = sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Anzahl nach Art der Erfassungen am 28.02.",
       x = "Art der Erfassung",
       y = "absolute Anzahl") +
  scale_x_discrete(labels=c("aK_gesamt" = "Erfasste und \n nicht erfasste \n andere Kontakte", 
                            "erfasst" = "Erfasst \n andere Kontakte und \n Skitourengeher",
                            "Erfasst_aK" = "Erfasste \n andere Kontakte", "Erfasst_SG"= "Erfasste \n Skitourengeher", 
                            "gesamt" = "Erfasste und \n nicht erfasste Skitourengeher \n und andere Kontakte", 
                            "nicht_erfasst" = "Nicht erfasste \n Skitourengeher \n andere Kontakte",
                            "Nicht_erfasst_aK" = "Nicht erfasste \n andere Kontakte", 
                            "Nicht_erfasst_SG" = "Nicht erfasste \n Skitourengeher", 
                            "SG_gesamt" = "Erfasste und \n nicht erfasste \n Skitourengeher"),
                   limit = c( "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_zlg28











## Anzahl Studentenmessungen vs Checkpointmessungen

ggplot(data = filter(zlg_beide_bereinigt_summen, type %in% c("checkpoint", "gesamt"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Checkpoint- und Studentendaten",
       x = "Art der Messung", y = "Anzahl")

# als Anteil in Prozent

zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "checkpoint"] / 
  zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "gesamt"]



# Anteil "andere Kontakte" an Gesamtzahl der von Studenten gemessenen Personen

zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "aK_gesamt"] / 
  zlg_beide_bereinigt_summen$sum[zlg_beide_bereinigt_summen$type == "gesamt"]








## Tabelle der Erfassungen nach Art

zlg_beide_summen <- 
  mutate(zlg_beide_summen, 
         erfassung = as.factor(c("erfasst", "nicht_erfasst", "erfasst", 
                                 "nicht_erfasst", "erfasst", "nicht_erfasst", 
                                 "beide", "beide", "beide", "beide")),
         kontakt = c("SG", "SG", "aK", "aK", "beide", "beide", "SG", "aK",
                     "beide", "beide"))



# Erfassungen vs Nichterfassungen zu allen Daten

ggplot(data = filter(zlg_beide_summen, type %in% c("nicht_erfasst", "erfasst"))) +
  geom_bar(aes(x = type, y = sum), stat = "identity") +
  labs(title = "Vergleich der Erfassungen und Nichterfassungen",
       x = "Art der Messung", y = "Anzahl")

# Anteil Nichterfassungen in Prozent

zlg_beide_summen$sum[zlg_beide_summen$type == "nicht_erfasst"] / 
  zlg_beide_summen$sum[zlg_beide_summen$type == "gesamt"]



# Unterscheidung zwischen Kontakten und Erfassung

ggplot(zlg_beide_summen[1:4,], 
       aes(x = factor(kontakt, levels = c("SG", "aK")),
           y = sum, fill = erfassung)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(#title = "Manuelle Zählung am 27.02- und 28.02.2019",
    x = NULL,
    y = "Absolute Häufigkeit") +
  scale_x_discrete(labels=c("SG" = "Skitourengänger", 
                            "aK" = "Andere Kontakte")) +
  theme(legend.position="top",
        text = element_text(size= 15)) +
  #scale_fill_discrete(name = NULL, labels = c("Erfasst", "Nicht erfasst")) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("#993300", "antiquewhite4")) 
#geom_text(aes(label=sum), position=position_dodge(width=0.9), vjust=-0.25)


### Erfassungen in 3 min intervall
erf_zeit_27_plot <- 
  ggplot(zlg_27_grouped_3min_erf, 
         aes(x = as.POSIXct(breaks), y = sum,
             fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 27.02.",
       x = NULL,
       y = "Absolute \n Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) 
  #scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))


erf_zeit_28_01_plot <- 
  ggplot(zlg_28_01_grouped_3min_erf, 
         aes(x = as.POSIXct(breaks), y = sum,
             fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 28.02. mittags",
       x = NULL,
       y = "Absolute \n Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) 
  #scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

erf_zeit_28_02_plot <- 
  ggplot(zlg_28_02_grouped_3min_erf, 
         aes(x = as.POSIXct(breaks), y = sum,
             fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 28.02. nachmittags",
       x = NULL,
       y = "Absolute \n Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) 
  #scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

erf_zeit_grid <- 
  grid.arrange(erf_zeit_27_plot, erf_zeit_28_01_plot, erf_zeit_28_02_plot,
               nrow = 3)
ggsave("Plots/kontakte_je_zeitraum.png", erf_zeit_grid)

# Studentische zählung, erfassung und nicht erfassung von Gruppen
ggplot(data = erfassung_je_gruppe, 
       aes(x = grösse, y = anzahl, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Manuelle Zählung am 27.02- und 28.02.2019 \n nach Gruppengröße",
       x = "Gruppengröße",
       y = "Anzahl") +
  scale_x_continuous(breaks = 1:8) +
  theme(
    axis.line = element_line(colour = "black"),
    text = element_text(size= 15)) +
  #scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


#Verhältnis der Gruppengröße
ggplot(data = erfassung_je_gruppe) +
  geom_bar(aes(x = grösse, y = anzahl, fill = type), 
           stat = "identity", position = "fill") +
  labs(#title = "Verhältnis der (Nicht-)Erfassungen nach Gruppengröße",
    x = "Gruppengröße",
    y = "Anteil") +
  scale_x_continuous(breaks = 1:8) +
  theme(text = element_text(size = 15), legend.position="top") +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("#993300", "antiquewhite4")) 
