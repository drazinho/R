
#####Wichtiger Hinweis: Bitte vor dem Ausführen des Skript den im Digital Repository beigelegten Rohdatensatz
    # München und das R-Makro namens process herunterladen. Das R-Makro process bitte auch ausführen. Ggf. fanach bitte 
    # das Skript zwei Mal hintereinander ganz ausführen, bzw. so viele Male bis in der Umgebung (Environment)
    # bei daten2 und daten3 1615 obs. (Beobachtungen) stehen.

#############Anfang des Skripts#############

# Erste Datenpakete laden
library(mosaic)
#1 Datenmanagement 

## Datensatz einlesen

#Rohdatensatz umbennenn:
datensatzroh <- Muenchen

## Erster Einblick in den Rohdatensatz:

inspect(datensatzroh)

## Relevante Variablen filtern

datensatz <- select(datensatzroh, G4, D1, D2)

## Relevante Variablen laut Variablenbeschreibung umbennen

names(datensatz)[names(datensatz) == "G4"] <- "Muk" # monatliche Ausgaben für Mode und Kleidung
names(datensatz)[names(datensatz) == "D1"] <- "Alter" # Alter in Jahren
names(datensatz)[names(datensatz) == "D2"] <- "Geschlecht" # Geschlechter

summary(datensatz$Geschlecht)

## Beobachtungen den Geschlechtern zuweisen*

datensatz$Geschlecht[daten$Geschlecht == "1"] <- "männlich" 
datensatz$Geschlecht[daten$Geschlecht == "2"] <- "weiblich"
datensatz$Geschlecht[daten$Geschlecht == "3"] <- "divers"


inspect(datensatz) # 39 fehlende Beobachtungen

daten <- na.omit(datensatz) # nach Bereinigung de fehlenden Beobachtung: N = 1640


# 2 Deskriptivstatistik

## 2.1 Geschlecht

bargraph(~Geschlecht, data = daten) # Balkendiagramm der Geschlechter
tally(~Geschlecht, data = daten) # absoluter Anteil der Geschlechter
tally(~Geschlecht, format = "percent", data = daten) # relativer Anteil der Geschlechter

daten2 <- daten[daten$Geschlecht !='divers',] # Divers aus Datensatz entfernen

bargraph(~Geschlecht, data = daten2) # Balkendiagramm der Geschlechter ohne divers
tally(~Geschlecht, data = daten2) # absoluter Anteil der Geschlechter ohne divers
tally(~Geschlecht, format = "percent", data = daten2) # relativer Anteil der Geschlechter ohne divers

## 2.2 Alter

bwplot(~Alter, data = daten2) # Boxplot von  Muk
favstats(~Alter, data = daten2) #Lagemaße von Muk
histogram(~Alter, data = daten2) # visulle Darstellung als Histogramm

## 2.3 MuK 

bwplot(~Muk, data = daten2) # Boxplot von  Muk
favstats(~Muk, data = daten2) #Lagemaße von Muk
histogram(~Muk, data = daten2) # visulle Darstellung als Histogramm

# 3  Normalverteilung der Variablen nach Aldor-Noiman et al. (2013).

## 3.1 Muk

ks_Muk <- ks.test(daten2$Muk, "pnorm") #ohne spezifische theoretische Verteilung, mit Standardnormalverteilung getestet: nicht normalverteilt, da p < .05-
print(ks_Muk)

shapiro.test(daten2$Alter) # Shapiro-Wilk-Test zur Sicherheit: nicht normalverteilt, da p < .05

# Skewness und Kurtosis

library(moments) #Paket für Skewness und Kurtosis laden
skewness(daten2$Muk) #Positiv, tendiert eher zu linkssteil 
kurtosis(daten2$Muk) # Schmalere und steilere Spitze als bei Normalverteilung 

#QQ-Plot zur Visualsierung der Verteilung von Muk:

qqnorm(daten2$Muk, main = "Q-Q-Plot von Muk" )
qqline(daten2$Muk) #Referenzlinie 

# Nicht normalverteilt, da Werte entfernt von Referenzlinie sind. Linkssteile Verteilung

# Histogramm von "Muk" mit Dichteschätzung als Kurve und Balkenanpassung für bessere Übersichtlichkeit
hist(daten2$Muk, col = "lightblue", freq = FALSE, main = "Histogramm von Muk", breaks = 20)
lines(density(daten2$Muk), col = "red", lwd = 2)

# Es scheint hier eine linkssteile Verteilung von Muk vorzuliegen.

## 3.2 Alter

# Kolmogorov-Smirnov-Test für Alter, da große Stichprobe vorliegt


ks_Alter <- ks.test(daten2$Alter, "pnorm") #ohne spezifische theoretische Verteilung, mit Standardnormalverteilung getestet: nicht normalverteilt, da p < .05-
print(ks_Alter)

shapiro.test(daten2$Alter) # Shapiro-Wilk-Test zur Sicherheit: nicht normalverteilt, da p < .05

skewness(daten2$Alter) #Positiv nahe 0, tendiert eher zu linkssteil bzw. eher normalverteilt
kurtosis(daten2$Alter) # Schmalere und steilere Spitze als Normalverteilung in der Spitze

# QQ-Plot zur Visualsierung der Verteilung von Alter:

qqnorm(daten2$Alter, main = "Q-Q-Plot von Alter") 
qqline(daten2$Alter) #Referenzlinie generieren

# Nicht normalverteilt, da Werte entfernt von Referenzlinie sind

#Histogramm von Alter mit Dichteschätzung als Kurve
hist(daten2$Alter, col = "lightblue", freq = FALSE, main = "Histogramm von Alter")
lines(density(daten2$Alter), col = "red", lwd = 1)

# Alter weist eine multimodale Verteilung auf (drei Gipfel)

## 3.3 Prüfung der Voraussetzungen für t-Test bzgl. Normalverteilungsannahme

library(psych) #benötiges Paket dafür laden

describeBy(daten2$Muk, daten2$Geschlecht, mat = TRUE)

# Interpretation von Ausgabe: Zwar N mind. 30, aber Skewness und Kurtosis für beide Gruppen ist größer als |-1.65| -> Mann-Whitney-U-Test anwenden.

# 4 Inferenzstatistik

## 4.1 H1

# Visueller Vergleich der Gruppenanteile von MUK beider Geschlechter mittels Boxplots

# Erstelle Boxplots für den Anteil von Muk nach Geschlechtern m. & w. mit angepasster y-Achsen-Skalierung
boxplot(Muk ~ Geschlecht, data = data_for_boxplot, 
        xlab = "Geschlecht", ylab = "Anteil von Muk",
        main = "Boxplots für Anteil von Muk nach Geschlechtern",
        ylim = c(0, 2000))  #y-Achsen Grenzen 

# Benutzerdefinierte Achsenmarkierungen generieren
axis(2, at = seq(0, 2000, by = 250))  # Achsenmarkierungen bei jedem 250er Schritt bis 2000 zur besseren Übersicht

# Erstelle einen Dataframe mit den relevanten Daten
data_for_boxplot <- data.frame(Muk = daten2$Muk, Geschlecht = daten2$Geschlecht)

# Erstelle Boxplots für den Anteil von "Muk" nach Geschlecht
boxplot(Muk ~ Geschlecht, data = data_for_boxplot, xlab = "Geschlecht", ylab = "Anteil von Muk",
main = "Anteile von MUK nach Geschlechtern")


library(stats) #benötigtes Paket hierfür wird geladen

mwut_Ergebnis <- wilcox.test(daten2$Muk ~ daten2$Geschlecht, exact=FALSE, correct=FALSE, conf.int=TRUE) #MWUT durchführen
print(mwut_Ergebnis) # p-Wert über .05 -> H1 wird nicht angenommen.

## 4.2 H2

cor.test(daten2$Alter, daten2$Muk, method = "kendall", alternative = "less") # Zusammenhangsanalyse nach Kendall, da UV und AV nicht-normalverteilt sind

   
# tau = -.03 (p < .05), H1 angenommen: Mit zunehmendem Alter geben Menschen monatlich weniger Geld für Mode aus.

## 4.3 H3

# H3 prüft Zusammenhang zwischen Muk und Alter. Geschlecht dient als Moderator. Bootstrapping nach Hayes wird dafür verwendet.

## Rekodierung der Geschlechter zum quasi-numerischen Skalenniveau

daten3 <- daten2
daten3$Geschlecht[daten2$Geschlecht == "männlich"] <- "1" # Beobachtungen den jeweiligen Geschlechtern zuweisen
daten3$Geschlecht[daten2$Geschlecht == "weiblich"] <- "2" # Beobachtungen den jeweiligen Geschlechtern zuweisen


daten3$Geschlecht <- as.numeric(daten3$Geschlecht)  # Umawndlung des Skalenniveus von Geschlecht zu quasi-numerisch

# Pakete für Moderationsanalyse nach Hayes laden 
library(processR)
library(processx)

## Linearität prüfen

# Linearität Muk ~ Alter(siehe auch H2)

plotModel(lm(Muk ~ Alter, data = daten3)) + ggtitle("Zusammenhang zwischen Muk und OCB") 

### Linearität zwischen Muk ~ Alter ist vorhanden.

# Linearität Muk ~ Geschlecht

plotModel(lm(Muk ~ Geschlecht, data = daten3)) + ggtitle("Zusammenhang zwischen Muk und Geschlecht")

### Linearität vorhanden zwischen Muk und Geschlecht

# Modell erstellen

library(interactions) #Paket für Interaktionen laden

mod_MukAlterGeschlecht <- lm(Muk ~ Alter +   Alter*Geschlecht, data = daten3) #Interaktionsmodell erstellen (Moderation)
summary(mod_MukAlterGeschlecht) #Ergenisaugabe des Modells der vorherigen Zeile

# mod_MukAlterGeschlecht nicht signifikant

interact_plot(model = mod_MukAlterGeschlecht, pred = Alter, modx = Geschlecht) 
# Interaktionsplot von mod_MukAlterGeschlecht: Die beiden Graphen von männlich bzw. weiblich kreuzen sich

## Normalverteilung der Residuen von mod_MukAlterGeschlecht

hist(residuals(mod_MukAlterGeschlecht))

# Verteilung der Residuen ist linkssteil und damit nicht-normalverteilt.

## Moderation nach Hayes mit Bootstrapping

process(y = "Muk", x = "Alter", w = "Geschlecht", model = 1, modelbt = 1, seed = 50000, data=daten3) #50000 Seeds gesetzt zur leichterten Replizierbarkeit

# p-Wert über .05 -> H3 kann nicht angenommen werden.

## Hetereskedaszität

library(lmtest) #benötigtes Paket laden

bptest(mod_MukAlterGeschlecht)

# Homoskedazität liegt vor.

## Autokorrelatiom

dwtest(mod_MukAlterGeschlecht)

# Keine Autokorrelation vorhanden.

#############Ende des Skripts#############







