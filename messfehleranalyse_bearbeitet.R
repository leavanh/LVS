### Grafiken anpassen 


## Grafiken zu Messfehleranalyse.R

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

# Von den Studenten gezählte Personen, sortiert nach Erfassungsart am 27.02, ohne Berechnungen
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


# Von den Studenten gezählte Personen, sortiert nach Erfassungsart am 27.02, ohne Berechnungen
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
## Skitourengänger und andere Kontakte Gesamt nach Uhrzeit
# Erfasste und nicht erfasste Skitourengeher, sowie andere Kontakte nach Uhrzeit am 27.02
#Dunkelgrau=Erfasste und nicht erfasste Skitourengeher
#Hellgrau=Erfasste und nicht erfasste andere Kontakte
ggplot(data = zlg_27, aes(x = time)) +
  geom_bar(aes(y = SG_gesamt), stat = "identity", 
           position = "dodge", fill = "dimgray") +
  geom_bar(aes(y = aK_gesamt), stat = "identity", 
           fill = "azure3", position = "dodge") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Skitourengänger und andere Kontakte am 27.02.2019",
       x = "Uhrzeit",
       y = "absolute Anzahl")

## Skitourengänger und andere Kontakte Gesamt nach Uhrzeit
# Erfasste und nicht erfasste Skitourengeher, sowie andere Kontakte nach Uhrzeit am 28.02
#Dunkelgrau=Erfasste und nicht erfasste Skitourengeher
#Hellgrau=Erfasste und nicht erfasste andere Kontakte
ggplot(data = zlg_28, aes(x = time)) +
  geom_bar(aes(y = SG_gesamt), stat = "identity", 
           position = "dodge", fill = "dimgray") +
  geom_bar(aes(y = aK_gesamt), stat = "identity", 
           fill = "azure3", position = "dodge") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Skitourengänger und andere Kontakte am 28.02.2019",
       x = "Uhrzeit",
       y = "absolute Anzahl")

## Vergleich erfasst vs nicht erfasst
#Für jede Minute
#Nicht erfasste Skitourengeher sowie andere Kontakte am 27.02.2019
ggplot(data = zlg_27) +
  geom_bar(aes(x = time, y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = "Uhrzeit d. nicht erfasste Skitourengeher \n sowie andere Kontakte am 27.02.2019",
       x = "Uhrzeit",
       y = "Anzahl an nicht erfassten \n Personen (SG und aK)") 

#Nicht erfasste Skitourengeher sowie andere Kontakte am 28.02.2019
ggplot(data = zlg_28) +
  geom_bar(aes(x = time, y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = "Uhrzeit d. nicht erfasste Skitourengeher \n sowie andere Kontakte am 28.02.2019",
       x = "Uhrzeit",
       y = "Anzahl an nicht erfassten \n Personen (SG und aK)")

#Nicht erfasste Skitourengeher sowie andere Kontakte am 27.02.2019 und 28.02.2019
ggplot(data = zlg_beide) +
  geom_bar(aes(x = time, y = nicht_erfasst, fill = date), 
           stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  scale_y_continuous(limits = c(0, 2.1)) +
  labs(title = "Uhrzeit d. nicht erfasste Skitourengeher \n sowie andere Kontakte am 27.02 und 28.02",
       x = "Uhrzeit",
       y = "Anzahl an nicht erfassten \n Personen (SG und aK)")

# Nichterfassungen nach Uhrzeit in 3-Minuten-Intervallen

#Nichterfassung vom 27.02.2019 im 3-Min-Intervall
ggplot(data = zlg_27_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = "Nichterfassungen am 27.02.2019 \n (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Anzahl nicht erfasste Skitourengeher \n und andere Kontakte")

#Nichterfassung vom 28.02.2019 im 3-Min-Intervall
ggplot(data = zlg_28_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = "Nichterfassungen am 28.02.2019 \n (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Anzahl nicht erfasste Skitourengeher \n und andere Kontakte")

#Nichterfassung vom 27.02.2019 und 28.02.2019 im 3-Min-Intervall
ggplot(data = zlg_beide_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), 
           stat = "identity") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = "Nichterfassungen am 27.02 und 28.02 \n (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Anzahl nicht erfasste Skitourengeher \n und andere Kontakte")

# Erfassungen und Nichterfassungen nebeneinanderstellen
ggplot(data = zlg_beide) +
  geom_bar(aes(x = time, y = erfasst), 
           color = "blue", stat = "identity", position = "dodge") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  geom_bar(aes(x = time, y = nicht_erfasst), 
           color = "red", stat = "identity", position = "dodge") +
  labs(title = "Nichterfassungen am 27.02 und 28.02 \n (in 3-Minuten-Intervallen) \n rot=Nicht Erfasst und Blau=Erfasst",
       x = "Uhrzeit",
       y = "Anzahl erfasster und nicht erfasste Skitourengeher \n und andere Kontakte")

#----------------------------------------------------------------------------------------------------------------------------------------------

###Plots zum Vergleich von Checkpoints und Studentenzählungen
#Messungen der Checkpoints am 27.02.2019 ab 10.30 Uhr bis 12:55 Uhr
barplot_ckpt_27 <- ggplot(ckpt_27, aes(type)) +
  geom_bar() +
  labs(title = "Automatische Messungen der Checkpoints am 27.02.2019",
       x = "Art der Messung",
       y = "Absolute Häufigkeit")

barplot_ckpt_27
 
#Messungen der Checkpoints am 28.02.2019 ab 10.38 Uhr bis 16:39 Uhr
barplot_ckpt_28 <- ggplot(ckpt_28, aes(type)) +
  geom_bar() +
  labs(title = "Automatische Messungen der Checkpoints am 28.02.2019",
       x = "Art der Messung",
       y = "Absolute Häufigkeit") 

barplot_ckpt_28

##Vergleich von Checkpoints vs Studentenzählungen
if (!require("gridExtra")) install.packages("gridExtra")
library("gridExtra")
library(grid)

#Am 27.02
grid.arrange(plot_zlg27, barplot_ckpt_27, nrow = 1)

#Am 28.02
grid.arrange(plot_zlg28, barplot_ckpt_28, nrow = 1)






