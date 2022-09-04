# Daten für das Projekt
Diese Datei soll übersichtlich darstellen, wie die Originaldaten bearbeitet und angepasst wurden. Im Prozess wird neben den Commits hier beschrieben, was in welchem Schritt geändert wurde. 

## Schritt 1
Originaldaten der XBox One Game Sales werden von Kaggle (https://www.kaggle.com/datasets/sidtwr/videogames-sales-dataset?select=XboxOne_GameSales.csv) heruntergeladen und in dem passenden erstellten Ordner der CSV-Daten eingecheckt. Zusätzlich wird diese Original-CSV in das .ods-Format zum Bearbeiten der Daten in Open Office Calc umgewandelt und wiederum im passenden Ordner eingecheckt. 

## Schritt 2
Verkleinerung der Originaldaten in CSV und .ods zum Testen und übersichtlicheren Programmierung der Visualisierungen. Später muss in diesen nur noch der entsprechende Link getauscht werden. Weiterhin wurde die Spalte "Year" gelöscht, da sie irrelevant für die Visualisierungen ist. 

## Schritt 3 
Anlegen einer .ods-Datei mit den modifizierten Daten für das Projekt. Löschen der Spalte "Year", da sie irrelevant für die Visualisierung ist. Löschen der Positionen/Spiele, die in allen Kategorien der Verkäufe einen Wert von 0 haben. Danach löschen der Positionen/Spiele, die keinen Publisher oder "Unknown" angegeben haben, da dies für die Zielgruppe irrelevant und diese nicht vergleichbar sind. Löschen der Positionen/Spiele, deren Publisher nur einmalig enthalten ist, da sie irrelevant für die Visualisierungen und die Zielgruppe sind. Schließlich konvertieren in CSV und beide Dateien in die jeweiligen Ordner einchecken. 