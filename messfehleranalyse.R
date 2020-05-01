

# Zeitlimit der manuellen Messungen für die Plots später

time_limits <- as.POSIXct(strptime(c("1899-12-31 10:00:00","1899-12-31 17:00:00"), 
                                   format = "%Y-%m-%d %H:%M", tz = "UTC"))



## Summen der einzelnen Reihen in der manuellen Zählung: 
# SG, aK, erfasst, nicht erfasst

(summen_27 <- apply(zlg_27[,-1], 2, sum))
(summen_28 <- apply(zlg_28[,-1], 2, sum))

# Summen als separate Tabelle

sums_27 <- data.frame(cbind(names(summen_27), as.numeric(summen_27)))
names(sums_27) <- c("type", "sum")
sums_27$sum <- as.numeric(as.character(sums_27$sum))

sums_28 <- data.frame(cbind(names(summen_28), as.numeric(summen_28)))
names(sums_28) <- c("type", "sum")
sums_28$sum <- as.numeric(as.character(sums_28$sum))

# alle Summen geplottet

ggplot(sums_27, aes(x = type, y = sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Anzahl nach Art der Erfassungen am 27.02.",
       x = "Art der Erfassung",
       y = "absolute Anzahl")

ggplot(sums_28, aes(x = type, y = sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Anzahl nach Art der Erfassungen am 28.02.",
       x = "Art der Erfassung",
       y = "absolute Anzahl")



## Erfassungen und Nicht-Erfassungen Gesamt nebeinanderstellen nach Uhrzeit

zlg_27_Erfassung <- zlg_27 %>% gather(key = "Erfassung", value = "Anzahl", c(erfasst, nicht_erfasst))

Plot_Erfassung_27 <-   ggplot(data = zlg_27_Erfassung, aes(x = time, y = Anzahl, fill = Erfassung)) +
                      geom_bar(stat = "identity", position = "dodge") +
                      theme(legend.title = element_blank())


zlg_28_Erfassung <- zlg_28 %>% gather(key = "Erfassung", value = "Anzahl", c(erfasst, nicht_erfasst))

Plot_Erfassung_28 <- ggplot(data = zlg_28_Erfassung, aes(x = time, y = Anzahl, fill = Erfassung)) +
                        geom_bar(stat = "identity", position = "dodge") +
                        theme(legend.title = element_blank()) +
                        scale_x_datetime(limits = time_limits)


## Skitourengänger und andere Kontakte Gesamt nach Uhrzeit

Plot_Typ_Person_27 <- ggplot(data = zlg_27, aes(x = time)) +
                        geom_bar(aes(y = SG_gesamt), stat = "identity", 
                                 position = "dodge", fill = "blue") +
                        geom_bar(aes(y = aK_gesamt), stat = "identity", 
                                 fill = "green", position = "dodge")


Plot_Typ_Person_28 <- ggplot(data = zlg_28, aes(x = time)) +
  geom_bar(aes(y = SG_gesamt), stat = "identity", 
           position = "dodge", fill = "blue") +
  geom_bar(aes(y = aK_gesamt), stat = "identity", 
           fill = "green", position = "dodge")




## Vergleich erfasst vs nicht erfasst




# Plot: zu welchen Uhrzeiten passieren die Nichterfassungen


# erstmal nur für den 28. -> beide Tage zusammenlegen???

# für jede Minute

ggplot(data = zlg_27) +
  geom_bar(aes(x = time, y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(limits = time_limits)

ggplot(data = zlg_28) +
  geom_bar(aes(x = time, y = nicht_erfasst), stat = "identity") +
  scale_x_datetime(limits = time_limits)

ggplot(data = zlg_beide) +
  geom_bar(aes(x = time, y = nicht_erfasst, fill = date), 
           stat = "identity") +
  scale_x_datetime(limits = time_limits)

# gruppiert in 3-Minuten-Intervalle

zlg_27_breaks_3min <- mutate(zlg_27, breaks = cut(zlg_27$time, breaks = "3 min"))
zlg_27_grouped_3min <- zlg_27_breaks_3min %>% group_by(breaks) %>% 
                        summarise(
                          erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
                        )

zlg_28_breaks_3min <- mutate(zlg_28, breaks = cut(zlg_28$time, breaks = "3 min"))
zlg_28_grouped_3min <- zlg_28_breaks_3min %>% group_by(breaks) %>% 
                      summarise(
                       erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
                      )

zlg_28_01_breaks_3min <- mutate(zlg_28_01, breaks = cut(zlg_28_01$time, breaks = "3 min"))
zlg_28_01_grouped_3min <- zlg_28_01_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )

zlg_28_02_breaks_3min <- mutate(zlg_28_02, breaks = cut(zlg_28_02$time, breaks = "3 min"))
zlg_28_02_grouped_3min <- zlg_28_02_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )


zlg_beide_breaks_3min <- mutate(zlg_beide, breaks = cut(zlg_beide$time, breaks = "3 min"))
zlg_beide_grouped_3min <- zlg_beide_breaks_3min %>% group_by(breaks) %>% 
  summarise(
    erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst)
  )


zlg_27_grouped_3min_erf <- gather(zlg_27_grouped_3min, 
                                  key = "erfassung", value = "sum", 
                                  erfasst, nicht_erfasst)

zlg_28_01_grouped_3min_erf <- gather(zlg_28_01_grouped_3min, 
                                  key = "erfassung", value = "sum", 
                                  erfasst, nicht_erfasst)

zlg_28_02_grouped_3min_erf <- gather(zlg_28_02_grouped_3min, 
                                  key = "erfassung", value = "sum", 
                                  erfasst, nicht_erfasst)


# Nichterfassungen nach Uhrzeit in 3-Minuten-Intervallen


erf_zeit_27_plot <- 
ggplot(zlg_27_grouped_3min_erf, 
       aes(x = as.POSIXct(breaks), y = sum,
           fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 27.02.",
       x = NULL,
       y = "absolute Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size= 10)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))


erf_zeit_28_01_plot <- 
  ggplot(zlg_28_01_grouped_3min_erf, 
         aes(x = as.POSIXct(breaks), y = sum,
             fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 28.02. mittags",
       x = NULL,
       y = "absolute Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size= 10)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

erf_zeit_28_02_plot <- 
  ggplot(zlg_28_02_grouped_3min_erf, 
         aes(x = as.POSIXct(breaks), y = sum,
             fill = erfassung)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kontakte nach Uhrzeit am 28.02. nachmittags",
       x = NULL,
       y = "absolute Häufigkeit") +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(1,3,5,7,10),
                     limits = c(0,10)) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size= 10)) +
  scale_fill_discrete(name = NULL, labels = c("erfasst", "nicht erfasst"))

erf_zeit_grid <- 
grid.arrange(erf_zeit_27_plot, erf_zeit_28_01_plot, erf_zeit_28_02_plot,
             nrow = 3)


ggsave("Plots/Erfassung_nach_Uhrzeit_grid.png", erf_zeit_grid)





ggplot(data = zlg_27_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), stat = "identity") +
  labs(title = "Nichterfassungen am 27.02. (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")


ggplot(data = zlg_28_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), stat = "identity") +
  labs(title = "Nichterfassungen am 28.02. (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")

ggplot(data = zlg_beide_grouped_3min) + 
  geom_bar(aes(x = as.POSIXct(breaks), y = nicht_erfasst), 
           stat = "identity") +
  labs(title = "Nichterfassungen a beiden Tagen (in 3-Minuten-Intervallen)",
       x = "Uhrzeit",
       y = "Absolute Häufigkeit")  

# Erfassungen und Nichterfassungen nebeneinanderstellen

ggplot(data = zlg_beide) +
  geom_bar(aes(x = time, y = erfasst), 
           color = "blue", stat = "identity", position = "dodge") +
  geom_bar(aes(x = time, y = nicht_erfasst), 
           color = "red", stat = "identity", position = "dodge")



# Wie viele Nichterfassungen gab es insgesamt je Gruppengröße zur gleichen Minute

zlg_28 %>% group_by(erfasst) %>% 
  summarise(nicht_erfasst = sum(nicht_erfasst))

zlg_28 %>% group_by(Erfasst_SG) %>% 
  summarise(nicht_erfasst_SG = sum(Nicht_erfasst_SG))

zlg_28 %>% group_by(Erfasst_aK) %>% 
  summarise(nicht_erfasst_aK = sum(Nicht_erfasst_aK))


zlg_gruppenvgl_abs <- zlg_beide %>% group_by(erfasst) %>% 
                    summarise(nicht_erfasst = sum(nicht_erfasst))
zlg_gruppenvgl_abs


ggplot(data = zlg_gruppenvgl_abs) +
  geom_bar(aes(x = erfasst, y = nicht_erfasst), stat = "identity")


zlg_beide %>% group_by(Erfasst_SG) %>% 
  summarise(durchschn_nicht_erfasst_SG = sum(Nicht_erfasst_SG))

zlg_beide %>% group_by(Erfasst_aK) %>% 
  summarise(durchschn_nicht_erfasst_aK = sum(Nicht_erfasst_aK))



# Wie viele Nichterfassungen gab es durchschn. je Gruppengröße zur gleichen Minute

zlg_gruppenvgl_durchschn <- zlg_beide %>% group_by(erfasst) %>% 
                              summarise(
                                durchschn_nicht_erfasst = mean(nicht_erfasst)
                                )
zlg_gruppenvgl_durchschn

ggplot(data = zlg_gruppenvgl_durchschn) +
  geom_bar(aes(x = erfasst, y = durchschn_nicht_erfasst), stat = "identity")
# n = Anzahl wie oft jeweilige Gruppengröße vorkommt in/über die Balken?









