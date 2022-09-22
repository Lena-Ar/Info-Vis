# Daten für das Projekt
Diese Datei soll übersichtlich darstellen, wie die Originaldaten bearbeitet und angepasst wurden. Im Prozess wird neben den Commits hier beschrieben, was in welchem Schritt geändert wurde. 

## Schritt 1
Originaldaten der XBox One Game Sales werden von Kaggle (https://www.kaggle.com/datasets/sidtwr/videogames-sales-dataset?select=XboxOne_GameSales.csv) heruntergeladen und in dem passenden erstellten Ordner der CSV-Daten eingecheckt. Zusätzlich wird diese Original-CSV in das .ods-Format zum Bearbeiten der Daten in Open Office Calc umgewandelt und wiederum im passenden Ordner eingecheckt. 

## Schritt 2
Verkleinerung der Originaldaten in CSV und .ods zum Testen und übersichtlicheren Programmierung der Visualisierungen. Später muss in diesen nur noch der entsprechende Link getauscht werden. Weiterhin wurde die Spalte "Year" gelöscht, da sie irrelevant für die Visualisierungen ist. 

## Schritt 3 
Anlegen einer .ods-Datei mit den modifizierten Daten für das Projekt. Löschen der Spalte "Year", da sie irrelevant für die Visualisierung ist. Löschen der Positionen/Spiele, die in allen Kategorien der Verkäufe einen Wert von 0 haben. Danach löschen der Positionen/Spiele, die keinen Publisher oder "Unknown" angegeben haben, da dies für die Zielgruppe irrelevant und diese nicht vergleichbar sind. Löschen der Positionen/Spiele, deren Publisher nur einmalig enthalten ist, da sie irrelevant für die Visualisierungen und die Zielgruppe sind. Schließlich konvertieren in CSV und beide Dateien in die jeweiligen Ordner einchecken. 

## Schritt 4 
In der modifizierten CSV- und ODS-Datei den Namen des Publishers "Namco Bandai Games" austauschen durch "Bandai Namco Games", da beide denselben Publisher repräsentieren. "Namco Bandai Games" wurde 2014 zu "Bandai Namco Games" umbenannt. So werden die Daten für das Projekt einheitlicher und eindeutiger. 

## Schritt 5
Erstellung einer neuen CSV-Datei, die die Abhängigkeiten für die gewünschte Baumstruktur für die Konvertierung in JSON enthält. Aufgrund des gewählten Anwendungsfalles sind nur Publisher, Genres und Titel der Spiele nötig.

## Schritt 6
Konvertierung der CSV-Datei in JSON mittels des Online-Tools https://www.convertcsv.com/csv-to-json.htm . Entstehenden Code in ein neues JSON-Dokument eintragen und im passenden Ordner einchecken. Zusätzlich ein Test-File mit nur 20 Spielen erstellen zum übersichtlichen Entwickeln der Baumhierarchie. Späterer, einfacher Austausch mit der großen JSON-Datei.

## Schritt 7 
Revision des JSON-Codes und Verbesserung um flasch gesetzte Abhängigkeiten in dieser Datei und der zugrunde liegenden CSV-Datei. Löschen der leeren Felder/Knoten. Hinzufügen eines Wurzelknotens.

## Schritt 8
Zweite Revision des JSON-Codes. Ergänzen der Abkürzungen der Publisher hinter den jeweiligen Genres zur eindeutigen Identifizierung für die Baumdarstellung, da pro Knoten nur ein eingehender Pfad gewünscht ist. Sonst käme es zum Überkreuzen von Pfaden und entsprechend mehreren eingehenden Pfaden pro Knoten bzw. mehreren Eltern pro Kind. 