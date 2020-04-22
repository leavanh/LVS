#Für Excel-Tabelle

write.csv(concurvity(date_model, full = TRUE), "concurvity_true.csv")

write.csv(concurvity(date_model, full = FALSE), "concurvity_false.csv")

#---------------------------------------------------------------------------------------------------
#Paket laden
if (!require("gridExtra")) install.packages("gridExtra")
library("gridExtra")

##Histogramm für temperature, Niedrigster Wert -7,9 ; Höchster Wert +9,7
a1 <- ggplot(date_data, aes(temperature)) +
  geom_bar() +
  xlab("Temperatur") +
  ylab("Absolute Häufigkeit")

# Variable temperature transformieren in 6 Kategorien
date_data$temperature2<-cut(date_data$temperature, seq(-8,10,3),
                            right=FALSE, labels=c(1:6))


date_data$temperature3 <- as.integer(date_data$temperature2) 

# Barplot zu Variable temperature3

a2 <- ggplot(date_data, aes(temperature2)) +
  geom_bar() +
  xlab("Temperatur (in 3 Grad Intervallen) mit K=6") +
  ylab("Absolute Häufigkeit")


grid.arrange(a1, a2, nrow = 1)

#---------------------------------------------------------------------------------------------------

##Histogramm für solar_radiation, Min=14.00 und Max=792.00

q1 <- ggplot(date_data, aes(solar_radiation)) +
  geom_bar() +
  xlab("Sonnenstrahlung in W/m^2") +
  ylab("Absolute Häufigkeit") +
  title("")

#Variable solar_radiation transformieren in 4 Kategorien

date_data$solar_radiation2<-cut(date_data$solar_radiation, seq(0,800,200),
                                right=FALSE, labels=c(1:4))

date_data$solar_radiation3 <- as.integer(date_data$solar_radiation2)

q2 <- ggplot(date_data, aes(solar_radiation2)) +
  geom_bar() +
  xlab("Sonnenstrahlung in 4 Kategorien") +
  ylab("Absolute Häufigkeit")

grid.arrange(q1, q2, nrow = 1)

#Variable solar_radiation transformieren in 3 Kategorien

date_data$solar_radiation2<-cut(date_data$solar_radiation, seq(0,801,267),
                                right=FALSE, labels=c(1:3))

date_data$solar_radiation3 <- as.integer(date_data$solar_radiation2)

#Barplot für solar_radiation in 3 Kategorien geteilt
r2 <- ggplot(date_data, aes(solar_radiation2)) +
  geom_bar() +
  xlab("Sonnenstrahlung in 3 Kategorien") +
  ylab("Absolute Häufigkeit")

grid.arrange(q1, r2, nrow = 1)

#---------------------------------------------------------------------------------------------------

#Variable solar_radiation transformieren in Differenz zum Vortag

date_data$solar_diff <- date_data$solar_radiation - lag(date_data$solar_radiation, 
                                                        default = first(date_data$solar_radiation),
                                                        by = date_data$date)

#---------------------------------------------------------------------------------------------------

##Plot für Concurvity (worst)

vis.concurvity <- function(date_model, type="worst"){
  cc <- concurvity(date_model, full=FALSE)[[type]]
  
  diag(cc) <- NA
  cc[lower.tri(cc)]<-NA
  
  layout(matrix(1:2, ncol=2), widths=c(5,2))
  opar <- par(mar=c(8, 8, 3, 0) + 0.1)
  # main plot
  image(z=cc, x=1:ncol(cc), y=1:nrow(cc), ylab="", xlab="",
        axes=FALSE, asp=1, zlim=c(0,1))
  axis(1, at=1:ncol(cc), labels = c("Para", "s(Temperatur)", "s(Schneehöhe Diff.)", "s(Sonnenstrahlung)",
                                    "s(Datum)", "s(Lawinenstufe)", "s(Wochentag)"), las=2)
  axis(2, at=1:nrow(cc), labels = c("Para", "s(Temperatur)", "s(Schneehöhe Diff.)", "s(Sonnenstrahlung)",
                                    "s(Datum)", "s(Lawinenstufe)", "s(Wochentag)"), las=2)
  # legend
  opar <- par(mar=c(8, 0, 3, 5) + 0.1)
  image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
        x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
  axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)
  par(opar)
}

vis.concurvity(date_model)
