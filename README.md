# easyArticel
Skripts zum automatisieren und vereinfachen der Bestellungen mit der foodsoft für foodcoops

im R File ist der Dateiname der zu lesenden pdf anzupassen. pdf sollte im gleichen ordner liegen.
Funktioniert mit Bersta Artikeln Gemüse.

Da Ding mach folgendes:

-liest mal die Daten aus (nona :-)
-filtert nach Herkunft Österreich, Waldviertel
-kombiniert die Artikelnummer zum Namen dazu (ersetzt Menge und Einheit aus dem pdf)
-rechnet für bestimmte Artikel (wie bis jetzt gehabt von Einheit Kilo auf 500 g um, gleiches für 1 kg nach 250 g)
-rundet auf 5 cent auf

Es werden 2 csvs erstellt nach den Vorgaben der foodsoft siehe:

https://app.foodcoops.at/allmunde/suppliers/5/articles/upload

Das csv. "kurz" enthält nur die Daten für die Bestellung (also nur die kleineren Einheiten).
das csv. "alle" enthält alle Artikel 
