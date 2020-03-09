getwd()
## Daten einlesen

data <- readRDS("/home/zorana/Schreibtisch/LVS/LVS-IR/Daten/data.RDS")

## library

#install.packages("ggplot2")
library("ggplot2")
install.packages("ggthemes")
library(ggthemes)
install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
library(gridExtra)

## Test
str(data)
type_freq <- table(data$type)
type_freq
type_prop <- prop.table(type_freq)
type_prop

table(data$holiday)

table(data$day_weekend)

# Häufigkeitstabelle von Beacon/Infrared pro Wochentag
table_1 <- table(data$type, data$day)
table_1

#----------------------------------------------------------------------------------------------------------------------

## 1 Variable

# Absolute Häufigkeit von Beacon und Infrared 
#(Anzahl Infrarotstrahlungen viel höher als LVS)
ggplot(data, aes(type)) +
  geom_bar() +
  xlab("Art der Messung") +
  ylab("Absolute Häufigkeit")

# Häufigkeit von Nord und Sued Position 
#(Anzahl Messungen viel höher auf der Sued Seite)
ggplot(data, aes(position)) +
  geom_bar() +
  xlab("Position") +
  ylab("Absolute Häufigkeit")

#-------------------------------------------------------------------------------------------------------------------

## 2 Variablen

# Häufigkeit von Beacon/Infrared am jeweiligen Wochentag
#(Messungen am Wochenende am hoechsten, Montags am niedrigsten)
ggplot(data, aes(day, fill=type)) +
  geom_bar(position = "dodge") +
  xlab("Tag") +
  ylab("Absolute Häufigkeit")

# Versuch die x-Achse nach Wochentagen zu sortieren

p <- data %>%
  mutate(day = fct_relevel(day, 
                           "Montag", "Dienstag", "Mittwoch", 
                           "Donnerstag", "Freitag")) %>%
  ggplot( aes(day, fill=position)) +
  geom_bar(stat="identity") +
  xlab("")
p

# Häufigkeit von Beacon/Infrared an Nord/Sued Seite 

ggplot(data, aes(type, fill = position)) +
  geom_bar() +
  xlab("Art der Messung") +
  ylab("Absolute Häufigkeit")


# Schneehoehe an Nord/Sued Seite
# 25%-Quantil deutlich hoeher als bei Nordseite; 50%- und 75%-Quantil etwas hoeher als bei Nordseite

b <- ggplot(data, aes(position, snowhight))
b + geom_boxplot(aes(fill=factor(position))) + 
  labs(title="Schneehöhe an der Nord- bzw. Südseite")


# Schneehöhe von Jan.-April 
#(Am höchsten Mitte Februar, am niedrigsten Mitte Dezember)

ggplot(data, aes(date, snowhight)) +
  geom_jitter() +
  labs(
    y="Schneehöhe", 
    x="Monat", 
    title="Jittered Points")

# Temperaturverlauf pro Monat
#(Zu erwartendes Ergebnis: linearer Zusammenhang)
ggplot(data, aes(date, temperature)) +
  geom_jitter() +
  labs(
    y="Temperatur", 
    x="Monat", 
    title="Jittered Points")

# Temperaturverlauf an Wochentag
# (Unsinn:keine Aussage moeglich)
ggplot(data, aes(x=day, y=temperature)) +
  geom_boxplot()

#-------------------------------------------------------------------------------------------------------------------

## 3 Variablen

# Boxplot zum Temperaturvgl. Beacon/Infrared auf der Nord/Sued Seite
ggplot(data, aes(x = position, y = temperature, fill = type, color=type))+
  facet_wrap(~type) + 
  geom_point( position = position_jitterdodge(dodge.width=0.4))+
  geom_boxplot(fill="white",outlier.colour = NA, 
               position = position_dodge(width=0.4))+
  labs(title = "Temperaturvergleich von beacon/infrared auf Nord/Süd Seite", x= "Position", y="Temperatur")

# Boxplot zum Schneehoehenvgl. auf Nord/Sued Seite von Mitte Dez.-Mitte Ap.
ggplot(data, aes(x = date, y = snowhight, fill = position, color=position))+
  facet_wrap(~position) + 
  geom_point( position = position_jitterdodge(dodge.width=0.4))+
  geom_boxplot(fill="white",outlier.colour = NA, 
               position = position_dodge(width=0.4))+
  labs(title = "Schneehöhenvergleich auf Nord/Süd Seite", x= "Monat", y="Schneehöhe in cm")

# Titel wie oben nur anderer Plot
plot1 <- ggplot(data, aes(x=date, y=snowhight)) + 
  geom_point(aes(col=position)) + 
  geom_smooth(method="loess", se=FALSE) +   #gam fuer n>1000, loess fuer n<1000
  labs(
    y="Schneehöhe in cm", 
    x="Monat", 
    title="Schneehöhe von Mitte Dezember bis Mitte April")

plot2 <- ggplot(data, aes(date, fill=position)) +
  geom_bar() +
  xlab("Monat") +
  ylab("Absolute Häufigkeit")

# Plot1 und 2 zusammengefügt
grid.arrange(plot1, plot2, ncol=2)
