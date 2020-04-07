

## Daten aus 19/20 einlesen

# dazu aus den beiden .txt-Files rauskopiert und in Exceldatei eingefügt

# welcher war Nord, welcher Süd?

data_1920_01 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_01", col_names = c("type", "date", "time"))
data_1920_02 <- read_excel("Daten/Daten_1920.xlsx", sheet = "1920_02", col_names = c("type", "date", "time"))

data_1920 <- rbind(data_1920_01, data_1920_02)

# Reihen, die nicht Beacon oder Infrared enthalten, rauslöschen

all_checkpoint_stats_1920 <- subset(data_1920,
                               type %in% c("Beacon", "Infrared"))

# Zeit und Datum in passendes Foramt umwandeln

all_checkpoint_stats_1920$date <- as.POSIXct(all_checkpoint_stats_1920$date, format = "%d %B %Y", tz = "UTC")
all_checkpoint_stats_1920$time <- as.POSIXct(all_checkpoint_stats_1920$time, format = "%d %B %Y", tz = "UTC")


# manuelle Zählung einlesen

zaehlung_lvs_check_27_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", sheet = 1, skip = 1,
                                       col_names = c("time", "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK"))
zaehlung_lvs_check_28_02 <- read_excel("Daten/Zaehlung_LVS-check.xlsx", sheet = 2, skip = 1,
                                       col_names = c("time", "Erfasst_SG", "Nicht_erfasst_SG", "Erfasst_aK", "Nicht_erfasst_aK"))

# Checkpoint-Daten an den Tagen der manuellen Zählung

checkpoint_stats_27_02 <- filter(all_checkpoint_stats_1920, date == as.POSIXct("2020-02-27", tz = "UTC"))
checkpoint_stats_28_02 <- filter(all_checkpoint_stats_1920, date == as.POSIXct("2020-02-28", tz = "UTC"))


