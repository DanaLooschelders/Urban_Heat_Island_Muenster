#### Klima�bung SS 2019 ####

# Allgemeine Hinweise:
# Dieses Script dient als Hilfestellung f�r die Bearbeitung der Daten aus der Klima�bung und kann beliebig angepasst werden
# Autoren: H. Lokys, B. Paas, C. Schaller; Universit�t M�nster
# Bei Fragen wendet euch an die Tutor*innen (Mira & Jonathan) oder den Kursleiter Bastian Paas (bastian.paas@uni-muenster.de)

#### Vorbereitungen ####

# 1. Ordner erstellen in dem Ihr arbeiten m�chtet. In diesem Odner m�ssen Schreib- und Leserechte bestehen! 
# Speichert die Daten am besten NICHT auf dem Desktop.
#
# 2. Die Datei "usePackage.R" aus dem Learnweb herunterladen und in den Arbeitsordner abspeichern.
# Rechtsklick auf die Datei und "Ziel speichern unter" w�hlen.
# ACHTUNG: die Endung der Datei muss ".R" sein. Sollte es Fehlermeldungen geben �berpr�fen, ob evtl. ".R.txt" als Endung dasteht.
# Stichwort Versteckte Endungen (Kann unter Windows behoben werden im Explorer unter "Organisieren"/"Ordner und Suchoptionen"/"Ansicht"/ 
# -> Empfehlung: Hier den Haken ENTFERNEN f�r "Erweiterungen bei bekannten Dateitypen ausblenden")
#
# 3. Die Datei "Mutterdatai.csv" aus dem Learnweb herunterladen und in den Arbeitsordner abspeichern.
# Rechtsklick auf die Datei und "Ziel speichern unter" w�hlen. Die Endung der Datei muss ".csv" sein.

#### R konfigurieren ####

# 1. working directory festlegen
# Hierbei wird festgelegt in welchem Verzeichnis R arbeitet.
# Der Einfachheit halber werden sowohl Rohdaten, als auch Ergebnisse in einem Verzeichnis abgelegt
# Der Dateipfad ist nicht festgelegt und kann von euch frei gew�hlt werden. Logischerweise ist es sinnvoll
# damit die �bung funktioniert, dass die working directory
# der Dateipfad zu eurem Arbeitsordner ist, in dem die Dateien, die aus dem learnweb heruntergeladen wurden, liegen
# Wichtig: Es m�ssen Lese- und Schreibrechte in dem Ordner bestehen
# Bei der Angabe des Dateipfads kann die Pfadangabe aus dem Explorer kopiert werden..
# jedoch muss darauf geachtet werden, dass "\" durch "\\" oder "/" ersetzt wird

setwd("E:\\")

# 2. Selbstgeschriebene Funktionen laden
# Zur vereinfachung haben wir einige Funtionen f�r euch vorbereitet.
# speichert diese bitte unter dem gew�hlten Dateipfad ab.
# in R laden wir die Funktionen mit folgenden Befehlen:

source("usePackage.R")

# 3. Pakete laden
# Es werden alle Pakete geladen, die f�r die Bearbeitung ben�tigt werden.
# Eine andere M�glichkeit pakete zu laden sind die Funktionen require() oder library(), die wir hier aber nicht verwenden


usePackage('plotrix') #wird u.a. zum erstellen von Windrosen ben�tigt
usePackage('openair') #wird u.a. zum erstellen von Windrosen ben�tigt

# 4. Eigene Funktionen definieren, die wir sp�ter ben�tigen
# Umrechnung von Grad in Bogenma� und umgekehrt:

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}



#### Daten einladen ####

filename<-"Mutterdatei.csv" #
metdata<-read.table(filename,sep=",", header=TRUE, stringsAsFactors = FALSE,na.strings = "NA",
                    colClasses=c("character",rep("numeric",23)))
# read.table liest die daten der datei filename ein
# sep="," bedeutet, dass die Daten mit Komma getrennt sind
# header=TRUE gibt an, dass wir eine Kopfzeile einlesen
# StringsAsFactors=FALSE verhindert, dass R die Spalten automaisch formatiert
# na.strings="NA" teilt R mit, dass fehlende Werte in der Ausgangsdatei als "NA" gespeichert wurden

# zum Pr�fen ob alle Daten korrekt eingelesen wurden die folgende Zeile auskommentieren:

# str(metdata)


# Erste Spalte in ein von R erkanntes Zeitformat �berf�hren

metdata$TIMESTAMP<-as.POSIXct(metdata$TIMESTAMP)



#### Arbeit mit den Daten ####

# Wichtige Funktionen f�r die Arbeit mit den Daten

# colSums Spaltensummen
# aggregate teilt Daten in Subsets(z.B. 10 min) und rechnet dann einen Wert f�r dieses Subset aus (z.B. 10 Minuten Mittelwert)
# cut teilt Daten in Intervalle ein und benennt alle Werte die in dieses Intervall fallen einheitlich

# # Beispiel f�r aggregate und cut (kann getestet werden indem hier auskommentiert wird):
# # Erzeuge 20 Fantasie-Zufallswerte
# temperature <- rnorm(20, mean=10)
# # Erzeuge fortlaufende Minuten ab dem 28.11.15, 9:20
# times <- seq(ISOdatetime(2015,11,28,9,20,0), by = "min", length.out = 20)
# # Erzeuge 10-Minuten-Mittel
# result <- aggregate(list(temperature = temperature),
#                     list(time = cut(times, "10 min")),
#                     FUN=mean)
# #                time temperature
# # 2015-11-28 09:20:00    10.09235
# # 2015-11-28 09:30:00    10.11168

# damit wir die Daten mit den Daten vom FMO vergleichen k�nnen (Stunden Daten), m�ssen wir unsere Daten in Stundenmittel �berf�hren
# Hierf�r wird zun�chst eine Hilfsvariable erstellt, die unsere 10 Minuten Werte einer Stunde zuweist:

Hours<-cut(metdata$TIMESTAMP, "1 hour",right = TRUE) 
# hierbei wird unser Zeitstempel TIMESTAMP in Stundenintervalle geteilt. 
# Alle 10 Minuten die in eine Stunde fallen, bekommen die volle Stunde zugewiesen (z.B. 16:20 Uhr --> 16:00 Uhr).
# Da der Logger immer das Ende des Intervalls aufzeichnet, m�ssen wir die Option right=TRUE verwenden. 
# Hiermit wird das Intervall von 16:50-17:00, welches der Logger als 17:00 auzeichnet der Kategorie 16:00 zugewiesen.

# Da die Windgeschwindigkeit in unterschiedliche Richtungen zeigen kann, m�ssen wir sie vektoriell mitteln
# hierzu erschaffen wir uns die Hilfsgr��en u und v wobei gilt
# u = Windgeschw. * COS(deg2rad(270 - Windrichtung) Westkomponente 
# v = Windgeschw. * SIN(deg2rad(270 - Windrichtung) S�dkomponente
# die Funtion deg2rad rechnet unsere Windrichtung von Grad (Englisch degree) zu Radiant (Bogenma�) um


# Westkomponente bestimmen:

u_USA<-metdata$WS_USA*cos(deg2rad(270-metdata$WindDir_USA)) #Ultraschallanemometer
u_Prop<-metdata$WS_Prop*cos(deg2rad(270-metdata$WindDir_Prop)) #Propelleranemometer


# S�dkomponente bestimmen:

v_USA<-metdata$WS_USA*sin(deg2rad(270-metdata$WindDir_USA)) #Ultraschallanemometer
v_Prop<-metdata$WS_Prop*sin(deg2rad(270-metdata$WindDir_Prop)) #Propelleranemometer


# Jetzt f�gen wir diese Spalten an das dataframe mit all unseren Daten an

metdata$u_USA<-u_USA
metdata$v_USA<-v_USA
metdata$u_Prop<-u_Prop
metdata$v_Prop<-v_Prop


# Nun kann mit Hilfe der Variable "Hours" der Stundenmittelwert bestimmt werden
# wir berechnen die Werte nur f�r Gr��en die auch am FMO gemessen werden (nachschauen in der Tabelle vom FMO):

metdata_1hmean<-aggregate(metdata[,c(4,7,25:28)],by=list(Hours),FUN=mean) 
#aggregate f�hrt dabei die Funktion FUN (hier Mittelwert bilden - mean) auf den datensatz "metdata" aus.
#Dieser wird dabei in Abschnitte geteilt, die durch unseren Hilfsvektor "Hours" definiert sind. 
#Diese Operation f�heren wir auf alle Spalten, au�er unseren zeitstempel durch (metdata[,2:nCol(metdata)]).


# Der Mittelwert ergibt nicht f�r alle Gr��en Sinn (z.B. Niederschlag). Deshalb berechnen wir auch noch die Summe �ber eine Stunde:

metdata_1hsum<-aggregate(metdata[,3],by=list(Hours),FUN=sum)


# ob alles so funktioniert hat wie wir es uns vorstellen k�nnen wir folgenderma�en �berpr�fen (auskommentieren):

#str(metdata_1hmean)


#damit unsere erste Spalte in den Data frames "metdata_1hmean" und metdata_1hsum" nicht Group1 hei�t
#sondern wieder einen Namen tr�gt der mit dem zeitstempel zu tun hat, benennen wir sie um:

names(metdata_1hmean)[1] <- "TIMESTAMP"
names(metdata_1hsum)[1] <- "TIMESTAMP"
names(metdata_1hsum)[2] <- "Rain_Tot" #bei nur einem Wert vergibt aggregate keine Spaltennamen


# Windgeschwindigkeit berechnen nach der Formel 
# Windgeschw.= wurzel(u^2+v^2)
# sqrt zieht die Wurzel. Hierf�r greifen wir auf die ein Stunden Blockmittel zur�ck, die wir im dataframe metdata_1hmean gespeichert haben:

WS_mean_USA<-sqrt(metdata_1hmean$u_USA^2+metdata_1hmean$v_USA^2)
WS_mean_Prop<-sqrt(metdata_1hmean$u_Prop^2+metdata_1hmean$v_Prop^2)


# Nach dem Berechnen der mittleren Windgeschwindigkeit berechnen wir au�erdem noch die mittlere Windrichtung.
# dies geschieht entsprechend seite 17 aus dem Skript zur Klima�bung nach Quadranten unterschiedlich.
# Deshalb verwenden wir die R funktion atan2 
# die Brechenung geschieht analog zu Excel mit der Formel alpha=(Arctan2(v;u)+PI())*180/PI(),
# die Umrechnung von radiant in Bogenma� (*180/pi) haben wir in unserer Funktion rad2deg gespeichert

# [!] Vorsicht: Die Excel-Funktion ARCTAN2(x,y) erwartet zuerst die x-, dann die y-Komponente. Die R-Funktion atan2(y,x) hingegen
#               erwartet zuerst die y-, dann die x-Komponente!

Winddir_mean_USA<-rad2deg(atan2(metdata_1hmean$u_USA,metdata_1hmean$v_USA)+pi)
Winddir_mean_Prop<-rad2deg(atan2(metdata_1hmean$u_Prop,metdata_1hmean$v_Prop)+pi)


# Alternativ ist auch folgende Rechnung moeglich:

Winddir_mean_USA_Variante2 <- (270-rad2deg(atan2(metdata_1hmean$v_USA,metdata_1hmean$u_USA)))%%360
Winddir_mean_Prop_Variante2 <- (270-rad2deg(atan2(metdata_1hmean$v_Prop,metdata_1hmean$u_Prop)))%%360
# Anm.: Die Zeichenfolge %% steht f�r den Modulo, d.h. f�r den Rest bei Division durch den danachfolgenden Wert.
#       Das Verhalten entspricht der Excel-Funktion REST().


# Windrichtung und Windgeschwindigkeit in die Tabellen eintragen

metdata_1hmean$WS_USA<-WS_mean_USA
metdata_1hmean$WindDir_USA<-Winddir_mean_USA

metdata_1hmean$WS_Prop<-WS_mean_Prop
metdata_1hmean$WindDir_Prop<-Winddir_mean_Prop


#### Grafiken erstellen ####

# beispielhafte Grafiken

# merke: plot() erstellt immer eine neue Grafik. lines() oder points() plottet in die bestehende Grafik hinein


# Temperatur

plot(metdata$TIMESTAMP,metdata$AirTC_Max, xlab="Datum", ylab=expression(paste("Temperatur [",~degree*C,"]", sep="")),
     main="Temperatur (10 Minuten Maximum)")
# Die Arte der Punkte kann mit der option pch ver�ndert werden. Verschiedene symbole k�nnt ihr unter ?pch nachsehen.

plot(metdata$TIMESTAMP,metdata$AirTC_Max,pch=20, xlab="Datum", ylab=expression(paste("Temperatur [",~degree*C,"]", sep="")),
     main="Temperatur (10 Minuten Maximum)")
# mit ylab geben wir an wie die y-Achse bezeichnet werden soll.Damit wir hier das Grad Symbol f�r die Angabe in Celsius erzeugen k�nnen,
# verwenden wir expression. Mit expression wandelt R bestimmte Zeichen um (z.B. griechiche Buchstaben).
# Damit wir zum Grad Zeichen auch noch Text verwenden k�nnen kombinieren wir das Ganze mit paste.
# paste erm�glich die Kombination von Text und Variablen/expressions.


# wir k�nnten die Zeitreihe der Temperatur anstatt als Punkte auch als Linie darstellen. Hierzu verwenden wir die Option type="l"

plot(metdata$TIMESTAMP,metdata$AirTC_Max, type="l", xlab="Datum", ylab=expression(paste("Temperatur [",~degree*C,"]", sep="")),
     main="Temperatur (10 Minuten Maximum)")

# Was haltet ihr f�r sinnvoller?


# die X-Achsen Beschriftung �ndern
# hiermit k�nnt ihr experimentieren, bis ihr eine gut lesbare Darstellung findet

plot(metdata$TIMESTAMP,metdata$AirTC_Max,type="l", xlab="Datum", ylab=expression(paste("Temperatur [",~degree*C,"]", sep="")),
     main="Temperatur (10 Minuten Maximum)",xaxt = "n") 
# xaxt="n" gibt an, dass keine x-Achsen Ticks geplottet werden. Diese f�gen wir dann per Hand mit der n�chsten Zeile hinzu:

axis.POSIXct(1, at=seq(min(metdata$TIMESTAMP), max(metdata$TIMESTAMP), by="1 day"), format="%d-%m-%Y")


# Niederschlag
# type="h" gibt an, dass die Daten als vertikale Linien (�hnlich einem Histogramm) geplottet werden

plot(metdata$TIMESTAMP,metdata$Rain_Tot,type="h", xlab="Datum", ylab="Niederschlagsintensit�t [mm/10min]", main="Niederschlag")


# f�r den Niederschlag ist auch der kumulierte Nierdschlag interessant, deshalb verwenden wir die Funktion cumsum

Rain_cumsum<-cumsum(metdata$Rain_Tot)
# hier haben wir ein Problem wegen der NA Werte!
# Was machen wir mit den NA? Nehmen wir an der Niederschlag war hier gleich Null? Dann k�nnen wir uns folgenderma�en helfen:
# wir schreiben eine Funktion die f�r die Bildung der cumsum alle NA in 0 umwandelt
cum.na <- function(x) {
  x[which(is.na(x))] <- 0
  return(cumsum(x))
}

Rain_cumsum <-cum.na(metdata$Rain_Tot) 

# anh�ngen der erzeugten Werte des kumulierten Niederschlags an den dataframe "metdata"
metdata$Rain_cumsum<-Rain_cumsum


# zun�chst plotten wir wieder die Niederschlagsintesit�t. Hierbei w�hlen wir das Maximum der Y Achse (ylim) entsprechend dem Maximum
# des Kumulierten Niederschlags, damit der ganze plot sp�ter zu sehen ist.
# R Richtet sich n�mlich bei einem Plot immer nach dem Ausma� der ersten geplotteten Daten, wenn man es nicht spezifiziert.

plot(metdata$TIMESTAMP,metdata$Rain_Tot,type="h", xlab="Datum", ylab="Niederschlag [mm]", main="Niederschlag",ylim=c(0,max(Rain_cumsum)))


# nachdem wir die Niederschlagsintensit�t geplottet haben m�ssen wir nun noch den kumulierten Niederschlag einzeichen. 
# Damit der alte Plot erhalten bleibt verwenden wir "lines" an Stelle von "plot"(plot w�rde wieder ein neues Fenster erzeugen);
# "lines" plottet in die bestehende Grafik hinein

lines(metdata$TIMESTAMP,metdata$Rain_cumsum, col="red")


# Da wir nun zwei Linien im Plot haben, ben�tigen wir noch eine Legende
# "topleft" f�hrt dazu, dass die Legende nach links oben gezeichnet wird.
# Als n�chstes geben wir an welcher Text in der Legende stehen soll.
# Hierbei gehen wir chronologisch die Plots durch, die wir erzeugt haben, also zuerst die Intensit�t, dann der Kumulietre Niederschlag.
# "lty" gibt an welche Art von Linie in der Legende erscheinen soll. 1 erzeugt hier eine normale Linie. 
# Zuletzt m�ssen wir noch spezifizieren, welche Farben die Linien haben sollen. 
# Dies gescheiht nat�rlich analog zu den Farben die wir im Plot verwendet haben.

legend("topleft", c("Niederschlagsintensit�t /10min", "Kumulierter Niederschlag"), lty=1, col=c("black", "red"))
# hier kann es auch sinnvoll sein die Niederschlagsintensit�t und den Kumulierten Niederschlag nicht auf einer,
# sondern auf 2 Achsen abzubinden. Hierzu empfohlen:
# https://kieranhealy.org/blog/archives/2016/01/16/two-y-axes/ und https://github.com/kjhealy/two-y-axes/blob/master/two-y-axes.r


# Windrosen

# Die Winddaten k�nnen z.B. f�r Windgeschwindigkeiten analog zu den Temperaturdaten gezeigt werden. 
# Bei den Windrichtungen ist es weniger sinvoll. Deshalb erzeugen wir hier eine Windrose


# Einfache Windrosen mit openair:

windRose(metdata,wd="WindDir_USA",ws= "WS_USA")


# Hiermit k�nnen wir jedoch schlecht 2 Stationen miteinander vergleichen. Deshalb nutzen wir das Paket plotrix

# Um die Windrose zu erzeugen m�ssen wir die Daten zun�chst nach Windrichtungen gruppieren
# Hierzu erzeugen wir uns zun�chst die Grenzen f�r die einzelnen Kategorien. 
# Da 360 Grad 2 PI entsprechen, teilen wir den Kreis in 16 Abschritte von 0 bis 2 pi.
#
# Jetzt liegen die grenzen der kategorien jedoch genau auf unseren Windrichtungen. 
# Deshalb verschieben wir alle Kategorien noch um pi/16. Nun reicht jede Kategorie von pi/16 vor der dargestellten Windrichtung,
# bis pi/16 danach.

grenzen<-c(0,rad2deg(seq(0,2*pi,by=pi/8)+pi/16))


# Der letzte Wert wurde nun �ber 360 Grad hinausgeschoben. deshalb setzen wir ihn auf 360 Grad zur�ck.
# Eigentlich geht unsere Kategorie f�r "Nord" ja von 348.75 bis 11.25 Grad

grenzen[length(grenzen)]<-360


# Die Windrichtungsdaten werden in die 16 Kategorien eingeteilt. Dies geschieht mit der Funktion cut
# cut erzeugt einen Faktor (kategoriale Variable) andhand der "grenzen" die wir vorher definiert haben.
# Die Nordkategorie tritt zweimal auf und wird mit N1 und N2 bezeichnet, da die Kategorie von 360-pi/16 bis 0+pi/16 gehen soll

WD_cat_USA<-cut(metdata$WindDir_USA, breaks=grenzen, labels=c("N1", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                                                              "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N2"))


# Aufgrund der Nordbesonderheit benennnen wir die level des Faktors nochmal um, so dass N1 und N2 zu N werden

levels(WD_cat_USA)<- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW","N")


# Zur Darstellung der H�ufigkeiten lassen wir uns nun die h�ufigkeiten der Kategorien berechen
# R verf�gt hierzu �ber die funktion table, welche eine H�ufigkeitstabelle erstellt

haeuf_USA<-table(WD_cat_USA)


# neben den absoluten h�ufigkeiten sind auch die relativen H�ufigkeiten interessant. 
# Somit teilen wir die absoluten H�ufigkeiten durch die Gesamtzahl der Messungen.

perc_USA<-haeuf_USA/sum(haeuf_USA)


# F�r die Darstellung werden noch einige Parameter zur Erzeugung des Plots ben�tigt
# Erstellung der abgebildeten Gradzahlen von 0 bis 2pi.
# Damit Norden nicht zweimal vorkommt, wird die sequenz nur bis 2*pi-pi/8 erzeugt:

Grad<-c(rad2deg(seq(0,2*pi-pi/8,by=pi/8)))


# Damit die Windrichtungen auch mit einem Label versehen werden k�nnen, wird es hier erzeugt:

Richtungen <- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")


# Die Darstellung erfolgt mithilfe des Pakets plotrix
# Die Funktion polar.plot erzeugt eine runde Darstellung in Grad. Das Netz im Hintergrund wird definiert durch polar.pos,
# die Daten geben wir an erster Stelle ein.

# Darstellung der Windrose mit relativen H�ufigkeiten:

polar.plot(as.numeric(perc_USA),polar.pos=Grad,main="Windrose relative H�ufigkeiten",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=Grad,labels=Richtungen, line.col="blue",
           radial.lim=c(0,max(perc_USA)),
           boxed.radial=TRUE)


# Darstellung der Windrose mit absoluten H�ufigkeiten:

polar.plot(as.numeric(haeuf_USA),polar.pos=Grad,main="Windrose absolute H�ufigkeiten",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=Grad,labels=Richtungen, line.col="blue",
           radial.lim=c(0,max(haeuf_USA)),poly.col="blue",
           boxed.radial=TRUE)


# um mehrere dieser Grafiken in einem Plot darzustellen, erstellen wir eine Matrix aus Vektoren, mit Daten zweier Stationen;
# in diesem Beispiel sind in der Beispielmatrix "multiwindrose" ein Vektor mit den Daten der 'echten' H�ufigkeiten hinterlegt;
# der zweite Vektor enth�lt fiktive Daten mit den H�ufigkeiten, auf die der Betrag "50" addiert ist.
# Im echten Leben macht es Sinn, hier die Daten (H�ufigkeiten) eines zweiten Standorts einzutragen, mit dem verglichen werden soll
# (z.B. die Standorte FMO oder GEO1 Dach):

multiwindrose<-rbind(as.numeric(haeuf_USA)+50,as.numeric(haeuf_USA))

polar.plot(multiwindrose,polar.pos=Grad,main="Windrose absolute H�ufigkeiten",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=Grad,labels=Richtungen, line.col=c(2,3),
           radial.lim=c(0,max(haeuf_USA)),
           boxed.radial=FALSE)
legend(-200,500,c("Ort A +50", "Ort A"),col=c(2,3),lty=1)



#### Erstellte Dataframes mit neuen Variablen und Spalten abspeichern ####


# Schreiben der 10-Minuten Daten in eine .CSV Datei (Datum im Dateinamen anpassen!)

write.table(metdata, file="metdata_bearbeitet_YYYYmmdd.csv",
            sep=";", dec=",", row.names=FALSE)


# Schreiben der 1-Stunden Daten in eine .CSV Datei (Datum im Dateinamen anpassen!)

write.table(metdata_1hmean, file="metdata_1hmean_bearbeitet_YYYYmmdd.csv",
            sep=";", dec=",", row.names=FALSE