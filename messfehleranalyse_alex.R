
ggplot(zlg_beide_summen[1:4,], 
       aes(x = factor(kontakt, levels = c("SG", "aK")),
           y = sum, fill = erfassung)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Manuelle Zählung am 27.02- und 28.02.2019",
       x = NULL,
       y = "Absolute Häufigkeit") +
  scale_x_discrete(labels=c("SG" = "Skitourengänger", 
                            "aK" = "Andere Kontakte")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size= 15)) +
  #scale_fill_discrete(name = NULL, labels = c("Erfasst", "Nicht erfasst")) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) +
  geom_text(aes(label=sum), position=position_dodge(width=0.9), vjust=-0.25)

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

#Checkpointerfassungen am 27-02 und 28-02-2019
zlg_ckpt_beide_bereinigt %>%
  ggplot(aes(date)) +
  geom_col(aes(y = erfasst, fill = "cadetblue")) +
  #geom_col(aes(y = lvs_true, fill = "blue")) +
  scale_fill_identity(name = "Messung",
                      breaks = c("cadetblue"),
                      labels = c("Erfasst"),
                      guide = "legend") +
  labs(title = "Checkpoint Zählung am 27.02- und 28.02.2019",
       x = "Datum",
       y = "Absolute Häufigkeit") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

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

#Verhältnis der Gruppengröße
ggplot(data = erfassung_je_gruppe) +
  geom_bar(aes(x = grösse, y = anzahl, fill = type), 
           stat = "identity", position = "fill") +
  labs(title = "Verhältnis der (Nicht-)Erfassungen \n nach Gruppengröße",
       x = "Gruppengröße",
       y = "Anteil") +
  scale_x_continuous(breaks = 1:8) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(name = NULL, labels = c("Erfasst", "Nicht Erfasst"), 
                    values = c("cadetblue", "antiquewhite4")) 
