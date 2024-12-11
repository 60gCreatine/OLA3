#### Opgave 3 – Julehandel i 2024 ####
##### Opgave 3.1 – Forudsigelse #####
# Lav en Machine Learning model, der med afsæt i DST’s forbrugertillidsindikator, kan forudsige om
# julehandlen i 2024 er større end i 2023.
# Hente data
library(dkstat)
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# For at lave et filer, kan vi variables ved FORV1$variables
FORV1_filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)

# Hente tabelens data med ovenstående filter
FORV1_Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")
unikke_indikator <- unique(FORV1_Data$INDIKATOR)
indikator_liste <- list()

# Lave nye lister hvor hver unikke indikatornavn
for (indikator in unikke_indikator) {
  indikator_liste[[indikator]] <- FORV1_Data[FORV1_Data$INDIKATOR == indikator,]}
FORV1_Total <- data.frame()
FORV1_Total <- as.data.frame(indikator_liste)
# Fjerne .
colnames(FORV1_Total) <- gsub("\\."," ",colnames(FORV1_Total))




#Vælger kun tid og værdier for FTI + 12 spørgsmål
FORV1_2000 <- FORV1_Total[304:nrow(FORV1_Total),c(2,3,6,9,12,15,18,21,24,27,30,33,36,39)]
# Fjerne value
colnames(FORV1_2000) <- gsub("\\ value","",colnames(FORV1_2000))
colnames(FORV1_2000)[1] <- "Tid"
##### Ændre fortegn #####
FORV1_2000$`Priser i dag  sammenlignet med for et år siden` <- -FORV1_2000$`Priser i dag  sammenlignet med for et år siden`
FORV1_2000$`Arbejdsløsheden om et år  sammenlignet med i dag` <- -FORV1_2000$`Arbejdsløsheden om et år  sammenlignet med i dag`
# Dividere vores tidskolonne med 3, aka Kvartalvis
FORV1_Q <- data.frame(
  Tid <- FORV1_2000$Tid[seq(1, nrow(FORV1_2000), by = 3)])

# Laver om til kvertal, seq = fra 1, til 1, fra hver 3.. Function tager der efter og beregne et gennemsnit af de 3 som input (i er nuværende række)
for (col in 2:ncol(FORV1_2000)) {
  FORV1_Q[[colnames(FORV1_2000)[col]]] <- round(sapply(seq(1, nrow(FORV1_2000), by = 3), function(i) {
    mean(FORV1_2000[i:(i+2), col], na.rm = TRUE)
  }),3)
}

NKHC021 <- dst_meta("NKHC021", lang = "da")
NKHC021_filter <- list(
  FORMAAAL="I alt",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret",
  Tid="*"
)
NKHC021_Data <- dst_get_data(table = "NKHC021", query = NKHC021_filter, lang = "da")

# Fjerne de første 37 rækker, da vi kun skal bruge fra 1999Q1, og og tække 1:3 grundet det ikke er relevant
Forbrugsdata <- NKHC021_Data[37:nrow(NKHC021_Data), 4:5]
colnames(Forbrugsdata)[1] = "Tid"
colnames(Forbrugsdata)[2] = "Dansk_forbrug"

Forbrugsdata$Årlig_vækst <- round(c(rep(NA, 4), diff(Forbrugsdata[,2], lag = 4) / Forbrugsdata[-(1:4),2] * 100),3)
# fjerne tomme rækker før 2000
Forbrugsdata <- na.omit(Forbrugsdata)

forbrug <- Forbrugsdata$Årlig_vækst
# For automatisk at lave vores FORV1 samme længde som i forbrug
forbrug_længde <- as.numeric(length(forbrug))
FTI <- FORV1_Q[1:forbrug_længde, ]
colnames(FTI)[1] = "Tid"
Spørgsmål <- FTI[,3:14]











