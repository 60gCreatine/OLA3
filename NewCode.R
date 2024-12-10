# Hente data
library(dkstat)
library(ggplot2)
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

#### Opgave 1.1 – Kombinationsalgoritme i R ####
# Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
# kvartal 2000 til og med 4. kvartal 2024.

# Først samler vi alle de forskellige FTI Spørgsmål i en vektor

# Alle mulige kombinmationer af x(det sidste tal) spørgsmål
#Kombination1 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],1)
#Kombination2 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],2)
#Kombination3 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],3)
#Kombination4 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],4)
#Kombination5 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],5)
#Kombination6 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],6)
#Kombination7 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],7)
#Kombination8 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],8)
#Kombination9 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],9)
#Kombination10 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],10)
#Kombination11 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],11)
#Kombination12 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],12)

# Lave alle mulige kombinationer af de 12 spørgsmål
  # Laves, fordi kombinationerne skal laves på navne, ikke selve værdierne
Spørgsmål_navne <- names(Spørgsmål)


Kombinationer <- list()
for (i in 1:length(Spørgsmål_navne)) {
  Kombi <- combn(Spørgsmål_navne, i, simplify = FALSE)
  Kombinationer <- c(Kombinationer, Kombi)
}

length(Kombinationer)
# Giver 4095 totalle antal af kombinationer



#### Opgave 1.2 – R2 og forbrugertillidsindikatorer ####
# Hvad er R2 for den indikatorer, der bedst forklarer variationen i den kvartalsvise årlige realvækst i
# husholdningernes forbrugsudgift? Diskutér om denne indikator er bedre end DI’s
# forbrugertillidsindikator. (Hint: se jeres besvarelse af opgave 2 i 2. OLA i flow 1 og opstil en tabel,
#                            der sammenligner de to indikatorer1)

lm_resultater <- list()
Spørgsmål <- FTI[,3:14]

for (i in 1:length(Kombinationer)) {
  Kombination <- Kombinationer[[i]]  
  Mean_FTI <- rowMeans(Spørgsmål[, Kombination, drop = FALSE], na.rm = TRUE) 
  
  # Byg data-frame til regression
  variables_df <- data.frame(FTI = Mean_FTI, forbrug = forbrug)
  
  # Kør lineær regression med fejl-håndtering
  lm_resultater[[i]] <- tryCatch({
    summary(lm(forbrug ~ FTI, data = variables_df))
  }, error = function(err) {
    cat(paste0("FEJL: ", err$message, "!!!!!\n"))
    NULL
  })
}

## tjekker med det virker med kombination 666
print(lm_resultater[[666]])
print(Kombinationer[[666]])
# "Danmarks økonomiske situation i dag  sammenlignet med for et år siden"                       
# "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr "                             
# "Regner med at kunne spare op i de kommende 12 måneder"                                       
# "Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener"

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.83462    0.42460  -4.321 3.76e-05 ***
#  FTI          0.27828    0.03156   8.818 4.76e-14 ***
# Multiple R-squared:  0.445,	Adjusted R-squared:  0.4392 
# F-statistic: 77.77 on 1 and 97 DF,  p-value: 4.756e-14



# Lav plottet
# Laveste punkt skal omsættes i forbindelse med tid
laveste <- data.frame(
  Tid = c(
    FTI$Tid[which.min(FTI$`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`)],
    FTI$Tid[which.min(FTI$`Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `)],
    FTI$Tid[which.min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`)],
    FTI$Tid[which.min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)]
  ),
  Value = c(
    min(FTI$`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`),
    min(FTI$`Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `),
    min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`),
    min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)
  )
)

ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation i dag  sammenlignet med for et år siden`, color = "Danmarks økonomiske situation i dag")) +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `, color = "Anskaffelse af større forbrugsgoder næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_point(data = laveste, aes(x = Tid, y = Value), color = "black") +
  geom_text(
    data = laveste,
    aes(x = Tid, y = Value, label = round(Value, 1)),
    size = 3,                # Større tekst
    color = "black",         # Sort tekst
    nudge_x = 400
  ) +
  labs(
    title = "2 variabler falder aldrig under +15",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"  # Titel for legend
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Kan nu tjekke for alle spm, og lave et nyt nul fra dem der er højt over 0 - se Baums artikel
ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation i dag  sammenlignet med for et år siden`), color = "blue") +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske  situation om et år  sammenlignet med i dag`), color = "red") +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation i dag  sammenlignet med for et år siden`), color = "green") +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation om et år  sammenlignet med i dag`), color = "purple") +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket`), color = "orange") +
  geom_line(data = FTI, aes(x = Tid, y = `Priser i dag  sammenlignet med for et år siden`), color = "brown") +
  geom_line(data = FTI, aes(x = Tid, y = `Priser om et år  sammenlignet med i dag`), color = "pink") +
  geom_line(data = FTI, aes(x = Tid, y = `Arbejdsløsheden om et år  sammenlignet med i dag`), color = "cyan") +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `), color = "yellow") +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`), color = "gray") +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`), color = "darkgreen") +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`), color = "darkred") +
  labs(
    title = "Tidsserieanalyse af økonomiske indikatorer",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Meget uoverskueligt, lad os finde dem som kun har min over 1
FTI_MIN <- NULL
for (col in colnames(FTI)[3:14]) {
  min_værdi <- min(FTI[[col]])  # Beregn minimum
  FTI_MIN <- rbind(FTI_MIN, data.frame(Spørgsmål = col, Minimum = min_værdi))
}

ggplot(resultat, aes(x = reorder(Spørgsmål, Minimum), y = Minimum, fill = Spørgsmål)) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label = round(Minimum, 1)), 
            position = position_stack(vjust = 0.5),  
            size = 3, color = "black") + 
  coord_flip() + 
  labs(
    title = "3 Spørgsmål falder aldrig under 15",
    x = "Spørgsmål",
    y = "Laveste historiske værdi"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  
    legend.position = "none"              
  )

# Forklare om de forskellige spørgsmål, hvorfor er de altid postive?
# Opret en data frame med de laveste punkter
laveste <- data.frame(
  Tid = c(
    FTI$Tid[which.min(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`)],
    FTI$Tid[which.min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`)],
    FTI$Tid[which.min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)]
  ),
  Value = c(
    min(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`),
    min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`),
    min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)
  ),
  Label = c(
    "Fornuftigt at spare op nuværende situation",
    "Regner med at spare op næste 12 mdr",
    "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener"
  )
)

# Plot med de laveste punkter vist
ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, color = "Fornuftigt at spare op nuværende situation")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vandret linje ved y = 0
  geom_point(data = laveste, aes(x = Tid, y = Value, color = Label), size = 2) +  # Tilføj laveste punkter
  geom_text(
    data = laveste,
    aes(x = Tid, y = Value, label = round(Value, 1)),
    hjust = 1.4,  # Flytter teksten lidt over punkterne
    size = 3,
    color = "black"
  ) +
  labs(
    title = "3 Spørgsmal falder aldrig under 0",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"  # Label for legend
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###### Lav nye 0 for 3 det aldrig rammer 0 ######
# Opret nye kolonner, hvor gennemsnittet er justeret til 0
FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` <- 
  FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` - 
  mean(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, na.rm = TRUE)

FTI$`Regner med at kunne spare op i de kommende 12 måneder` <- 
  FTI$`Regner med at kunne spare op i de kommende 12 måneder` - 
  mean(FTI$`Regner med at kunne spare op i de kommende 12 måneder`, na.rm = TRUE)

FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` <- 
  FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` - 
  mean(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, na.rm = TRUE)

ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, color = "Fornuftigt at spare op nuværende situation")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vandret linje ved 0
  labs(
    title = "De justerede kolonner falder nu under 0 (deres nye gennemsnit)",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Summary, men for nye nul #####
lm_resultater <- list()
Spørgsmål <- FTI[,3:14]

for (i in 1:length(Kombinationer)) {
  Kombination <- Kombinationer[[i]]  
  Mean_FTI <- rowMeans(Spørgsmål[, Kombination, drop = FALSE], na.rm = TRUE) 
  
  # Byg data-frame til regression
  variables_df <- data.frame(FTI = Mean_FTI, forbrug = forbrug)
  
  # Kør lineær regression med fejl-håndtering
  lm_resultater[[i]] <- tryCatch({
    summary(lm(forbrug ~ FTI, data = variables_df))
  }, error = function(err) {
    cat(paste0("FEJL: ", err$message, "!!!!!\n"))
    NULL
  })
}

lm_resultater[[666]]

#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.02743    0.22179   9.141 9.58e-15 ***
#  FTI          0.27828    0.03156   8.818 4.76e-14 ***
# Multiple R-squared:  0.445,	Adjusted R-squared:  0.4392 

##### Kun udtrække R^2 #####
top_df <- data.frame(kombinations_id = numeric(), R_squared = numeric())
for (i in 1:length(r_squared_liste)) {
    top_df <- rbind(top_df, data.frame(
      kombination_id = i,
      r_squared = r_squared_liste[[i]]
    ))}
top_df <- top_df[order(top_df$r_squared, decreasing = TRUE), ]
head(top_df,5)

###### Lille funktion til top, kan nu bare skrive top_(x), hvor x er hvor mange i toppen vi vil have ######
top_ <- function(top){
for (i in 1:nrow(top_df)){
  if (i <= top) {
    kombi_nr <- top_df$kombination_id[i]
    r2 <- top_df$r_squared[i]
    cat(paste0("Kombination nr: ",i," r2: ",round(r2,2)," har følgende spørgsmål: \n"))
    print(Kombinationer[[kombi_nr]])
  }}}
  
top_(5)

#### Opgave 1.3 – Spørgsmål i indikatoren ####
# Hvilke spørgsmål indgår i den indikator, 
# som er bedst til at forklare variationen i forbruget? 
# Giver kombinationen af spørgsmål analytisk mening? 
# I bedes overveje alternative indikatorer fra jeres oversigt.


