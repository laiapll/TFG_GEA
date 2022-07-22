# Objectes (taules, llistes, data.frames...) sobre els que fem l'anàlisi

library(tidyverse)

dades <- openxlsx::read.xlsx("dadesESS.xlsx", rowNames=TRUE)

# Etiquetes dels codis:
etiquetes <- list()
etiquetes$pais <- levels(as.factor(dades$pais)) %>%
  countrycode::countrycode("iso2c", "country.name")
etiquetes$essround <- paste0("ESS", c(1:5))
etiquetes$genere <- c("M","F")
etiquetes$cohort <-c("1937-1948", "1949-1958", "1959-1968", "1969-1980")
etiquetes$classes <- c("Classe de servei de grau superior", "Classe de servei de grau inferior", "Propietaris de petites empreses", "Treballadors qualificats", "Treballadors no qualificats")
etiquetes$classes_n <- c("I", "II", "III", "IV", "V")

# Paleta de colors:
paleta <- c("1" = "#f8961e", "2" = "#f94144", "3" = "#43aa8b")

  
# Format de les variables:
dades$codi_pais <- as.factor(dades$pais)
dades$pais <- countrycode::countrycode(dades$codi_pais, "iso2c", "country.name")
dades$essround <- factor(dades$essround, levels=c(1:5), labels=etiquetes$essround)
dades$genere <- factor(dades$genere, levels=c(1:2), labels=etiquetes$genere)
dades$cohort <- factor(dades$cohort, levels=c(1:4), labels=etiquetes$cohort)
dades$origen <- factor(dades$origen, levels=c(1:5), labels=etiquetes$classes_n)
dades$desti <- factor(dades$desti, levels=c(1:5), labels=etiquetes$classes_n)

# Freqüències:
freqs <- dades %>%
  group_by(pais, genere, cohort, origen, desti) %>%
  summarise(Freq=n())

# Taules
paisos_t <- xtabs(Freq ~ origen + desti + pais, data=freqs)
paisos <- plyr::alply(paisos_t, 3, .dims = TRUE)

homes_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, genere==etiquetes$genere[1]))
homes <- plyr::alply(homes_t, 3, .dims = TRUE)

dones_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, genere==etiquetes$genere[2]))
dones <- plyr::alply(dones_t, 3, .dims = TRUE)

cohort1_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, cohort==etiquetes$cohort[1]))
cohort1 <- plyr::alply(cohort1_t, 3, .dims = TRUE)

cohort2_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, cohort==etiquetes$cohort[2]))
cohort2 <- plyr::alply(cohort2_t, 3, .dims = TRUE)

cohort3_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, cohort==etiquetes$cohort[3]))
cohort3 <- plyr::alply(cohort3_t, 3, .dims = TRUE)

cohort4_t <- xtabs(Freq ~ origen + desti + pais, data=filter(freqs, cohort==etiquetes$cohort[4]))
cohort4 <- plyr::alply(cohort4_t, 3, .dims = TRUE)

llistes <- list(paisos=paisos, dones=dones, homes=homes,
                cohort1=cohort1, cohort2=cohort2,
                cohort3=cohort3, cohort4=cohort4)

