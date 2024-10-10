##### Fallstudien 1 - Projekt 1 - Erste Notizen #####

setwd("C:/Users/jorri/Documents/TU Dortmund/WiSe 2024-25/Module/Fallstudien 1/Projekt 1")

census = read.csv("census_2022_2002.csv")

c2022 = census[census$Year == "2022", ]
c2002 = census[census$Year == "2002", ]

#TODO: Aufgaben
#   1. Beschreiben Sie die Häufigkeitsverteilungen der Merkmale. Betrachten Sie auch die
#      Unterschiede zwischen den Geschlechtern. (nur 2022)
#   2. Gibt es bivariate Zusammenhänge zwischen den Merkmalen? (nur 2022)
#   3. Wie unterscheiden sich die Merkmale zwischen den Subregionen? (nur 2022)
#   4. Wie haben sich die Merkmale in den letzten 20 Jahren, d.h. im Vergleich von 2002
#      zu 2022, entwickelt?


### Variablen einmal anschauen
unique(census$Year)
plot(census$Life_Expectancy_Overall)  
plot(census$Life_Expectancy_Male)
plot(census$Life_Expectancy_Female)
plot(census$Total_Fertility_Rate)
unique(census$Subregion)
unique(census$Region)

### Ideen zu 1. Häufigkeitsverteilungen einzelner Merkmale

# Year: trivial nur 2022 da nur dieses betrachtet wird
# Country: entweder sind Daten vorhanden oder nicht

# Life_Expectancy_Overall:

plot(c2022$Life_Expectancy_Overall)
boxplot(c2022$Life_Expectancy_Overall)
hist(c2022$Life_Expectancy_Overall, breaks = 20, freq = FALSE)

# Life_Expectancy_Male:

plot(c2022$Life_Expectancy_Male)
boxplot(c2022$Life_Expectancy_Male)
hist(c2022$Life_Expectancy_Male, breaks = 20, freq = FALSE)

# Life_Expectancy_Female:

plot(c2022$Life_Expectancy_Female)
boxplot(c2022$Life_Expectancy_Female)
hist(c2022$Life_Expectancy_Female, breaks = 20, freq = FALSE)

# Total_Fertility_Rate:

plot(c2022$Total_Fertility_Rate)
boxplot(c2022$Total_Fertility_Rate)
hist(c2022$Total_Fertility_Rate)

# Auf Regionenunterschiede erst in 3 Eingehen?
# Soll Bericht inhaltlich anhand dieser 4 Fragen strukturiert sein?



## Graphische Ideen:
# 1) Häufigkeitsverteilungen:
#
#   nominale Merkmale (Region, SUbregion) -> Histogramme  (Subregionen kenntlich
#                                             machen)
#                                         -> Mosaikplot?
#   lebenserwartungen -> boxplot (als Übersicht) für alle drei
#                     -> Histogramm -> genauere Übersicht (einzeln?)
#   geburtenrate -> boxplot 
#                -> Histogramm


# 2) bivariate Zusammenhänge
#
#   lebenserwartung -> boxplots auf Regionen aufteilen
#                        -> evtl nach geschlecht teilen
#   scatterplot: lebenserwartung vs geburtenrate (farblich regionen markieren)
#                                                (nach geschlecht je einen)
#   geburtenrate vs regionen -> boxplots auf regionen aufteilen
#   scatterplots für lebenserwartungs korrelationen

# 3) Subregionen Unterschiede
#
#   lebenserwartung -> Boxplots nach Regionen auf Subregionen aufteilen
#   scatterplot: lebenserwartung vs geburtenrate (farblich regionen markieren)
#                   (für jede Region eines) (nach Geschlecht je einen?)

# 4) Vgl von 2002 zu 2022
#
# alles von 1) bis 3) für 2002 machen, mit 2022 vergleichen -> wo signifikant
# wirkende unterschiede -> überlege sinnvolle grafische Darstellung
#
#
# lebenserwartung 2002 vs 2022 (jeweils noch für geschlechter)
plot(c2002$Life_Expectancy_Overall, c2022$Life_Expectancy_Overall)
# differenz als histogramm plotten
# relative änderung
#
# geburtenrate beeinflusst evtl steigerung der lebenserwartung
# -> geburtenrate vs relative änderung scatterplot
#
# geburtenrate 2002 vs 2022
#
# Regionen / Subregionenweise Schnitte/Kennzahlen vergleichen
plot(c2022$Life_Expectancy_Overall, c2022$Total_Fertility_Rate)
points(c2002$Life_Expectancy_Overall, c2002$Total_Fertility_Rate, col = "red")

# farbliche Aufteilung nach Regionen / Subregionen
# farbliche Aufteilung / symbolische Unterscheidung nach Jahren
# -> oder 2 Plots für Jahre