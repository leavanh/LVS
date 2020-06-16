Der R-Code besteht aus verschiedenen R-Dateien.
Um die Grafiken und Ergebnisse zu erhalten einfach run.R durchlaufen lassen.

Wichtiges im Vorraus:
Falls Probleme bei Umlauten entstehen schauen ob "UTF-8" als Encoding eingestellt ist. 
Als working directory muss der Ort der run-Datei eingestellt sein.
Manchmal ist Code auskommentiert, bei Bedarf einfach das Symbol "#" davor löschen.

Jetzt zu den einzelnen Dateien:

run.R: 

In run.R werden durch den source()-Befehl andere R- Dateien aufgerufen 
(zum Beispiel eine Funktion, die die Modelle laufen lässt oder eine Datei in der die 
Grafiken erzeugt werden). Möchte man also etwas an zum Beispiel dem Tagesmodell ändern
schaut man in run.R um welche Datei es sich handelt (day_model.R in diesem Fall) 
und ruft diese auf und ändert in dieser die Sachen ab.

Neben run.R und den dort aufgerufen Dateien gibt es noch read_data_1819.R und 
data_general.R, data_group.R, data_night.R, data_temp.R.
Alle diese Dateien erzeugen DataFrames die im Ordner "Daten" gespeichert werden. Da
das Erzeugen teilweise etwas Zeit kostet und auch nur einmal anfangs nötig ist
werden diese Dateien nicht in run.R aufgerufen. Wir haben diese Dateien bereits durch-
laufen lassen und die richtigen DataFrames in "Daten" gespeichert. Ihr müsst das also
nicht mehr machen (ihr braucht wirklich nur run.R öffnen). Falls ihr jedoch wissen 
wollt, wie manche Variablen zustande kommen oder was genau die Szenarien sind so könnt 
ihr in diese Dateien rein schauen.

read_data_1819.R:
Dort werden die Rohdaten eingelesen, zusammengeführt und in übersichtliche DataFrames 
gepackt. In run.R werden die Daten dann am Anfang aufgerufen.
(Auch um die neuen Daten einzulesen empfehlen wir euch einen ähnlichen Aufbau. Also eine neue Datei read_data_1920.R
in die ihr den Code aus read_data_1819.R kopiert und nur einige Stellen abändert. Die erzeugten DataFrames speichert ihr dann
unter anderem Namen und ruft sie anschließend in run.R auf. Wenn ihr den run.R Code dann einfach durchlaufen lassen wollt
nennt ihr in R die neuen Daten einfach wie die Alten, also data, date_data und min_data. Dann müsst ihr nur noch
Kleinigkeiten anpassen wo sich z.B. Variablen geändert haben.)

data_general.R, data_group.R, data_night.R, data_temp.R:
Erzeugen die neuen Datensätze zum jeweiligen Szenario.
