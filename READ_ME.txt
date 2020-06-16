Der R-Code besteht aus verschiedenen R-Dateien. 
Um die Grafiken und Ergebnisse zu erhalten einfach run.R durchlaufen lassen. 

In run.R werden durch den source()-Befehl andere R- Dateien aufgerufen (zum Beispiel eine Funktion, die die Modelle
erzeugt oder eine Datei in der die Grafiken erzeugt werden). Möchte man also etwas an zum Beispiel dem Tagesmodell ändern
schaut man in run.R um welche Datei es sich wohl handelt (day_model.R in diesem Fall) und ruft diese auf und ändert sie.

Neben run.R und den dort aufgerufen Dateien gibt es noch read_data.R und .......
read_data.R:
Dort werden die Rohdaten eingelesen, zusammengeführt und in übersichtliche DataFrames gepackt. Diese werden dann im Ordner
Daten abgespeichert. In run.R werden die Daten dann am Anfang aufgerufen. read_data.R wird nicht mit source() aufgerufen,
da es dann bei jedem mal run.R durchlaufen neu durchlaufen müsste und dies etwas Zeit braucht. Da der Datensatz sich ja 
eigentlich eh nicht ändert haben wir es für euch durchlaufen lassen und ihr müsst die Datei an sich nicht öffnen. 
Falls ihr jedoch wissen wollt, wie manche Variablen zustande kommen könnt ihr in read_data rein schauen. 
(Auch um die neuen Daten einzulesen empfehlen wir euch einen ähnlichen Aufbau. Also eine neue Datei read_data_1920.R
in die ihr den Code aus read_data.R kopiert und nur einige Stellen abändert. Die erzeugten DataFrames speichert ihr dann
unter anderem Namen und ruft sie anschließend in run.R auf. Wenn ihr den run.R Code dann einfach durchlaufen lassen wollt
nennt ihr in R die neuen Daten einfach wie die alten, also data, date_data und min_data. Dann müsst ihr nur noch
Kleinigkeiten anpassen wo sich z.B. Variablen geändert haben.)