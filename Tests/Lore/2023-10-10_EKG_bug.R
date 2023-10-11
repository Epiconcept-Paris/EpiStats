library(EpiStats)

fluve <- readRDS("Tests/Lore/fluveclean_training_cc.rds")

table(fluve$case, useNA = "always")
table(fluve$fluvacc, useNA = "always")
table(fluve$onsetmonth, useNA = "always")
table(fluve$onsetmonth2, useNA = "always")


# CCInter bug -------------------------------------------------------------

ccinter(fluve, "case", "fluvacc", by = "onsetmonth") # KO
# Erreur dans if (ODD > 1) { : valeur manquante là où TRUE / FALSE est requis
ccinter(fluve, "case", "fluvacc", by = "onsetmonth2")


table(fluve$onsetmonth, useNA = "always")
table(fluve$fluvacc, fluve$case, useNA = "always")

table(fluve$fluvacc, fluve$case, fluve$onsetmonth, useNA = "always")
table(fluve$fluvacc, fluve$case, fluve$onsetmonth2, useNA = "always")

# CSInter bug -------------------------------------------------------------

csinter(fluve, "case", "fluvacc", by = "onsetmonth")
# BUT "One of your strata has zero cases in the cells. You cannot properly compute the MH-adjusted RR."
csinter(fluve, "case", "fluvacc", by = "onsetmonth2") #KO
# Erreur dans `[.default`(x, j, k, i) : indice hors limites
# De plus : Message d'avis :
# Dans rev(as.integer(levels(factor(x[, by])), na.rm = T)) :
#   NAs introduits lors de la conversion automatique


