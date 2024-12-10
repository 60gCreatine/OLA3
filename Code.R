#Opgave 1 – Den bedste forbrugertillidsindikator



########################################################################################################
#### Opgave 1.1 – Kombinationsalgoritme i R ####
#Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
#kvartal 2000 til og med 4. kvartal 2024.
########################################################################################################
library(dkstat)
# Finde forbrugertillidsindikatoren
#dst_search("Forbrugerforventninger")


#FTI'er
# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")
#Filter
FORV1_filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)
#brug filter på hentet data
FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# Loop som gruppere de forskellige spørgsmål i seperate lister
FORV1Data <- as.data.frame(FORV1Data)
unikke_indikatorer <- unique(FORV1Data$INDIKATOR)
indikator_lister <- list()
for (indikator in unikke_indikatorer) {
  indikator_lister[[indikator]] <- FORV1Data[FORV1Data$INDIKATOR == indikator, ]
}

indikator_lister[["Forbrugertillidsindikatoren"]]

samlet_liste <- as.data.frame(indikator_lister)


dst_ftiMonths <- samlet_liste[304:nrow(samlet_liste),c(2,3,6,9,12,15,18,21,24,27,30,33,36,39)]

# Opret en ny dataframe med kvartalsdata ved at tage hver tredje række
DST_FTI <- data.frame(
  Tid = dst_ftiMonths$Forbrugertillidsindikatoren.TID[seq(1, nrow(dst_ftiMonths), by = 3)]  # Hent hver tredje værdi fra 'Tid'-kolonnen
)

# Beregn gennemsnittet af hver tredje række for alle kolonner fra 2 til sidste kolonne
for (col in 2:ncol(dst_ftiMonths)) {
  DST_FTI[[colnames(dst_ftiMonths)[col]]] <- sapply(seq(1, nrow(dst_ftiMonths), by = 3), function(i) {
    mean(dst_ftiMonths[i:(i+2), col], na.rm = TRUE)
  })
}

# DST_FTI[1:(nrow(DST_FTI)-2),] = altid 3. sidste linje
DST_FTI98 <- DST_FTI[1:(nrow(DST_FTI)


########## Det danske forbrug
#DanskForbrug <- dst_meta(table = "NAN3")
DanskForbrug <- dst_meta(table = "NKH1")
DanskForbrug <- list(
  TRANSAKT="P.31 Husholdningernes forbrugsudgifter",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret", 
  Tid="*"
)
ForbrugdataDST <- dst_get_data(table = "NKH1",query = DanskForbrug, lang="da")

Forbrugdata <- ForbrugdataDST[37:nrow(ForbrugdataDST), 4:5]

#realvækst

# Konverter forbrugsdata kolonnen til numerisk, hvis det ikke allerede er det
Forbrugdata[,2] <- as.numeric(Forbrugdata[,2])

# Beregn realvækst som procentændring fra periode til periode (ændringer i kædede værdier)
# Beregn år-til-år vækst (y/y)
Forbrugdata$Årlig_vækst <- c(rep(NA, 4), diff(Forbrugdata[,2], lag = 4) / Forbrugdata[-(1:4),2] * 100)

# Fjern NA-værdien i den første række for at undgå problemer i grafen
Forbrugdata <- na.omit(Forbrugdata)

DST_FTISPØRG <- DST_FTI[,2:ncol(DST_FTI)]


kombi1 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],1)
kombi2 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],2)
kombi3 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],3)
kombi4 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],4)
kombi5 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],5)
kombi6 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],6)
kombi7 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],7)
kombi8 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],8)
kombi9 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],9)
kombi10 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],10)
kombi11 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],11)
kombi12 <- combn(DST_FTISPØRG[1,2:ncol(DST_FTISPØRG)],12)



kombi6gns <- as.data.frame(kombi6)
kombi6gns <- colMeans(kombi6gns$V1)


















s########## 1.1
# Liste over kolonnenavne fra 2. kolonne og frem (de 12 spørgsmål)
kolonnenavne <- names(DST_FTI)[3:ncol(DST_FTI)]

# Opret en tom liste til at gemme alle kombinationer
alle_kombinationer <- list()

# Generer kombinationer af forskellige størrelser (fra 1 til 12)
for (kol in 1:length(kolonnenavne)) {
  kombi <- combn(kolonnenavne, kol, simplify = FALSE)
  alle_kombinationer <- c(alle_kombinationer, kombi)
}

# tjek hvor mange kombi der findes (4096) men kun vist 4095 fordi den første kombination er kombinationen af ingen.
length(alle_kombinationer)


########################################################################################################
#### Opgave 1.2 – R2 og forbrugertillidsindikatorer ####
#Hvad er R2 for den indikatorer, der bedst forklarer variationen i den kvartalsvise årlige realvækst i
#husholdningernes forbrugsudgift? Diskutér om denne indikator er bedre end DI’s
#forbrugertillidsindikator. (Hint: se jeres besvarelse af opgave 2 i 2. OLA i flow 1 og opstil en tabel,
#                            der sammenligner de to indikatorer1)
########################################################################################################

#                                                 slagplan
#                   1, lav kombi
#                   2, find FTI for hver kombi (gennemsnit)
#                   3, og loopscript lm for forbrug vs, alle nye FTI
#                   4, print top 5 r-square resultater
#
#########################################################################################################
# Antag at kolonnenavne indeholder dine 12 spørgsmål
#kolonnenavne <- names(DST_FTI98)[3:ncol(DST_FTI)]  # vi behøver den ikke fordi vi allerede har kaldt på den tidligere i koden

# Opret en tom liste til at gemme alle kombinationer
#alle_kombinationer <- list() 

# Tilføj den tomme kombination (hvis det er nødvendigt)
#alle_kombinationer[[1]] <- character(0)  # Tom kombination

# Generer kombinationer af forskellige størrelser (fra 1 til 12)
#for (k in 1:length(kolonnenavne)) {
#  kombi <- combn(kolonnenavne, k, simplify = FALSE)
#  alle_kombinationer <- c(alle_kombinationer, kombi)
#}



# Hvad vi skal bruge som afhængig variabel
forbrug <- Forbrugdata$Årlig_vækst  

# Opret en tom liste til at gemme resultater af regressionerne
regression_resultater <- list()

# Loop igennem alle kombinationer
for (i in 1:length(alle_kombinationer)) {  # Starter fra 2 for at undgå den tomme kombination
  kombi <- alle_kombinationer[[i]]
  
  # Beregn gennemsnit af de valgte kolonner for FTI
  FTI <- rowMeans(DST_FTI98[, kombi, drop = FALSE], na.rm = TRUE)
  
  # Tjek om længden af FTI matcher forbrug (efter evt. behandling af NA)
  if (length(FTI) == length(forbrug)) {
    # Opret data frame med FTI og forbrug
    model_data <- data.frame(FTI = FTI, forbrug = forbrug)
    
    # Udfør regression med fejlhåndtering
    regression_resultater[[i]] <- tryCatch({
      summary(lm(forbrug ~ FTI, data = model_data))
    }, error = function(e) NULL)  # Hvis der opstår en fejl, gemmes NULL
  }
}

# tjek om koden virker med kombi 666
if (!is.null(regression_resultater[[666]])) {
  print(regression_resultater[[666]])
} else {
  cat("Ingen gyldig regression for kombination 2\n")
}









# Opret en tom vektor til at gemme R-squared værdierne
r_squared_liste <- numeric(length(regression_resultater))

# Loop igennem regression_resultater for at udtrække R-squared værdierne
for (i in 1:length(regression_resultater)) {
  if (!is.null(regression_resultater[[i]])) {
    r_squared_liste[i] <- regression_resultater[[i]]$r.squared
  } else {
    r_squared_liste[i] <- NA  # Hvis regressionen er NULL, gem NA
  }
}

# Find indekserne af de top 10 bedste modeller baseret på R-squared
top_10_index <- order(r_squared_liste, decreasing = TRUE, na.last = NA)[1:10]

# Print de bedste kombinationer og deres R-squared værdier
for (i in top_10_index) {
  cat("Kombination:", paste(alle_kombinationer[[i]], collapse = " + "), "\n") # " + " tilføjer et + mellem vores kombinationer og "\n" laver linjeskift så vi ikke kommer til at udskrive på samme linje.
  cat("R-squared:", r_squared_liste[i], "\n\n")
}










# Funktion til at tjekke om en specifik kombination matcher en i kombinationslisten
søg_kombination <- function(specific_spm, alle_kombinationer) {
  # Loop igennem alle kombinationer
  result <- list()
  for (i in 1:length(alle_kombinationer)) {
    kombi <- alle_kombinationer[[i]]
    # Tjek om specific_spm matcher præcis med kombinationen (samme elementer og rækkefølge)
    if (length(specific_spm) == length(kombi) && all(specific_spm %in% kombi) && all(kombi %in% specific_spm)) {
      result[[length(result) + 1]] <- list(index = i, kombi = kombi)
    }
  }
  return(result)
}

# Eksempel: Filtrér efter den præcise kombination af spørgsmål
specific_spm <- c("Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value",
                  "Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value",
                  "Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value",
                  "Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value")

# Få en liste over kombinationer, der præcist matcher de ønskede spørgsmål
søgt_kombinationer <- søg_kombination(specific_spm, alle_kombinationer)

# Udskriv de kombinationer, der matcher præcist søgekriteriet, og deres resultater
for (item in søgt_kombinationer) {
  index <- item$index
  kombi <- item$kombi
  cat("Præcis kombination:", paste(kombi, collapse = " + "), "\n")
  
  # Hvis der er en gyldig regression for denne kombination
  if (!is.null(regression_resultater[[index]])) {
    cat("R-squared:", regression_resultater[[index]]$r.squared, "\n\n")
  } else {
    cat("Ingen gyldig regression for denne kombination.\n\n")
  }
}













#### Opgave 1.3 – Spørgsmål i indikatoren ####
#Hvilke spørgsmål indgår i den indikator, som er bedst til at forklarer variationen i forbruget? Giver
#kombinationen af spørgsmål analytisk mening? I bedes overveje alternative indikatorer fra jeres oversigt.

#VORES
#Kombination: 
#  Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value                        #MICRO
#Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value                              #MICRO
#Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value                         #MACRO *****
#Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value                              #MICRO
#Priser.i.dag..sammenlignet.med.for.et.år.siden.value                                                #MICRO
#Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value                               #MICRO
#Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder.value                                         #MICRP
#Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener.value  #MICRO
#R-squared: 0.4694522

#DI's
#kombination: 
#  Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value    #Mikor
#Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value     #MACRO *****
#Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value          #mikro
#Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value           #mikRo
#R-squared: 0.3656364                                                            #mikrO

#DST




#### Opgave 1.4 – Forudsigelser med afsæt i jeres indikatorer ####
#Forudsig udviklingen i husholdningernes forbrugsudgift i 3. og 4. kvartal 2024. 
#(Hint: brug jeres svar i opgave 2 fra OLA 2 sammen med jeres svar fra de forrige opgaver. 
#Vær opmærksom på, om tallene fra forbrugerundersøgelsen for oktober er udgivet og I har den første måned i 4. kvartal)

#1. Tag alle sidste kvartaler de sidste 5 år og lav gennemsnit på dem
#2. brug gennemsnit til at fylde sidste to måneder ud
#3. lav nyt kvartal









#### Opgave 1.5 – Sammenlign med en mikroøkonomisk indikator ####
#Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i
#forbrugertillidsundersøgelsen og sammenlign indikatoren med jeres tidligere svar i opgave 1.

mikro <- DST_FTI98[,c(1,3,4,7,11,12,13,14)]


mikro_kolonnenavne <- names(mikro)[2:ncol(mikro)]  

# Opret en tom liste til at gemme alle kombinationer
Mikro_kombinationer <- list() 

# Tilføj den tomme kombination (hvis det er nødvendigt)
Mikro_kombinationer[[1]] <- character(0)  # Tom kombination

# Generer kombinationer af forskellige størrelser (fra 1 til 12)
for (k in 1:length(mikro_kolonnenavne)) {
  mikro_kombi <- combn(mikro_kolonnenavne, k, simplify = FALSE)
  Mikro_kombinationer <- c(Mikro_kombinationer, mikro_kombi)
}



# Hvad vi skal bruge som afhængig variabel
forbrug <- Forbrugdata$Årlig_vækst  

# Opret en tom liste til at gemme resultater af regressionerne
mikro_regression_resultater <- list()

# Loop igennem alle kombinationer
for (i in 2:length(Mikro_kombinationer)) {  # Starter fra 2 for at undgå den tomme kombination
  mikro_kombi <- Mikro_kombinationer[[i]]
  
  # Beregn gennemsnit af de valgte kolonner for FTI
  mikroFTI <- rowMeans(mikro[, mikro_kombi, drop = FALSE], na.rm = TRUE)
  
  # Tjek om længden af FTI matcher forbrug (efter evt. behandling af NA)
  if (length(mikroFTI) == length(forbrug)) {
    # Opret data frame med FTI og forbrug
    mikro_model_data <- data.frame(mikroFTI = mikroFTI, forbrug = forbrug)
    
    # Udfør regression med fejlhåndtering
    mikro_regression_resultater[[i]] <- tryCatch({
      summary(lm(forbrug ~ mikroFTI, data = mikro_model_data))
    }, error = function(e) NULL)  # Hvis der opstår en fejl, gemmes NULL
  }
}

# tjek om koden virker med kombi 2
if (!is.null(mikro_regression_resultater[[2]])) {
  print(mikro_regression_resultater[[2]])
} else {
  cat("Ingen gyldig regression for kombination 2\n")
}









# Opret en tom vektor til at gemme R-squared værdierne
mikro_r_squared_liste <- numeric(length(mikro_regression_resultater))

# Loop igennem regression_resultater for at udtrække R-squared værdierne
for (i in 2:length(mikro_regression_resultater)) {
  if (!is.null(mikro_regression_resultater[[i]])) {
    mikro_r_squared_liste[i] <- mikro_regression_resultater[[i]]$r.squared
  } else {
    mikro_r_squared_liste[i] <- NA  # Hvis regressionen er NULL, gem NA
  }
}

# Find indekserne af de top 10 bedste modeller baseret på R-squared
mikro_top_10_index <- order(mikro_r_squared_liste, decreasing = TRUE, na.last = NA)[1:10]

# Print de bedste kombinationer og deres R-squared værdier
for (i in mikro_top_10_index) {
  cat("mikro Kombination:", paste(Mikro_kombinationer[[i]], collapse = " + "), "\n") # " + " tilføjer et + mellem vores kombinationer og "\n" laver linjeskift så vi ikke kommer til at udskrive på samme linje.
  cat("mikro R-squared:", mikro_r_squared_liste[i], "\n\n")
}

#####SJOV OG SPAS

#PCR 28 okt DST-FTI
DST_FTI98$forbrug <- Forbrugdata$Årlig_vækst
#install.packages("pls")
library(pls)
pcr.fit <- pcr(forbrug ~ 
                 Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value + 
                 Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value + 
                 Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value + 
                 Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag.value + 
                 Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value, 
               data = DST_FTI98, scale = TRUE, validation = "CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1 <- loadings.pcr.fit[1:4]^2
print(w.indicators.1)


#OPG2

#opg2.1
pcr.fit <- pcr(forbrug ~ 
                 Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value  +                   
                 Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value         +                   
                 Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value    +                   
                 Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag.value          +                    
                 Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value        +                    
                 Priser.i.dag..sammenlignet.med.for.et.år.siden.value                         +                     
                 Priser.om.et.år..sammenlignet.med.i.dag.value                                +                     
                 Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag.value                       +                     
                 Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value          +                   
                 Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation.value     +              
                 Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder.value         +                              
                 Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener.value,
               data = DST_FTI98, scale = TRUE, validation = "CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.2 <- loadings.pcr.fit[1:12]^2
print(w.indicators.2)


#opg 2.2 De vigtigste indikatorer



#ALLE INDIKATORER med deres værdi:
#Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value = 0.140058084
#Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value = 0.121029293
#Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value = 0.104278775
#Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag.value = 0.042763579
#Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value = 0.118382290
#Priser.i.dag..sammenlignet.med.for.et.år.siden.value = 0.112049116
#Priser.om.et.år..sammenlignet.med.i.dag.value = 0.002677694
#Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag.value = 0.067822292
#Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value = 0.142632888
#Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation.value = 0.001336788
#Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder.value = 0.065046515
#Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener.value = 0.081922686#

#rangeret efter største værdier
Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value = 0.142632888
Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value = 0.140058084
Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value = 0.121029293
Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value = 0.118382290
Priser.i.dag..sammenlignet.med.for.et.år.siden.value = 0.112049116
Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value = 0.104278775
Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener.value = 0.081922686
Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag.value = 0.067822292
Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder.value = 0.065046515
Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag.value = 0.042763579
Priser.om.et.år..sammenlignet.med.i.dag.value = 0.002677694
Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation.value = 0.001336788

#OPG 2.2
Hvilke 3 spørgsmål i forbrugerforventningsundersøgelsen har de højeste vægte? Diskutér om
spørgsmålene giver analytisk mening.

1. Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value - 0.142632888
2. Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value - 0.140058084
3. Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value - 0.121029293

A: Det er bemærkelsesværdigt at de alle 3 er micro-økonomiske faktorer, samtidig med at 2. og 3. er samme spørgsmål
bortset fra tidsperioden er i to forskellige retninger (forudseende og retrospektivt) forudseende er lidt noget gætværk
da de fleste folk hjemme i stuerne ikke har taget en økonomiuddannelse, i bedste tilfælde er det et kvalificeret gæt,
værste tilfælde er det bare et gæt.


#CORPLOT FOR AT BEVISE AT 2. og 3. har høj korelation 
# Ensure the corrplot package is loaded
library(corrplot)

# Select the relevant columns from your dataset
data_subset <- DST_FTI98[, c("Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value",
                             "Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value",
                             "Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value")]

colnames(data_subset) <- c("Forbrugsgoder", "Familiens.Sit.Dag", "Familiens.Sit.År")
cor_matrix <- cor(data_subset, use = "complete.obs")
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)

# Compute the correlation matrix
cor_matrix <- cor(DST_FTI98, use = "complete.obs")

# Create the correlation plot
corrplot(cor_matrix, method = "number", type = "lower", 
         tl.col = "black", tl.srt = 45, diag = FALSE, cl.pos = "n")





#OPG 2.3 Forudsig forbruget
Med afsæt i jeres vægte fra opgave 2.1 skal I beregne den årlige realvækst i husholdningernes
forbrugsudgift for 4. kvartal 2024. Hvad er årsagen til I ikke helt tror på jeres beregnede årlige
realvækst i husholdningernes forbrugsudgift for 4. kvartal 2024? Hvad skyldes det underlige
resultat?
  
  modeldf <- DST_FTI98[, c("Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value",
                           "Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value",
                           "Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag.value")]


modeldf$gennemsnit <- rowMeans(modeldf[, 1:3], na.rm = TRUE)

modeldf$realvækst <- Forbrugdata$Årlig_vækst


#forudsigelsesmodel på top-3  
#model
top3_model <- lm(modeldf$realvækst ~ modeldf$gennemsnit)
summary(top3_model) #p=8.04e-08 ***


new_data_top3 <- data.frame(gennemsnit = -2.7555556)
top3_model_ny <- lm(realvækst ~ gennemsnit, data = modeldf)
forudsigelse <- predict(top3_model_ny, newdata = new_data_top3)
print(paste("Forudsigelse af Realvækst for xxx baseret:", forudsigelse))
# [1] "Forudsigelse af Realvækst for Q3 2024 baseret på DST: 0.0420