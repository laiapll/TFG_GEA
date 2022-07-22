# Paquets
library(haven)
library(dplyr)
library(tidyverse)
library(labelled)
library(essurvey)


# Podem descarregar les dades del paquet "essurvey":
library("essurvey")
set_email(" ") #posar un email registrat
ess_data <- import_rounds(1:5, format = "spss")
names(ess_data) <- paste0("ESS", c(1:5))

# Dades complementàries de l'origen (Ganzeboom, 2014):
origens_isco88 <- read_sav("ESS12345_fm_isco.sav")

origens_isco88 <- origens_isco88 %>%
  mutate(fisco = ifelse(is.na(fisko)==FALSE, fisko,
                        ifelse(is.na(fisko_B), fisko_A, fisko_B)),
         misco = ifelse(is.na(misko)==FALSE, misko,
                        ifelse(is.na(misko_B), misko_A, misko_B))
         ) %>%
  select(!starts_with("fisko") & !starts_with("misko"))

ess_data_compl <- c(1:5) %>% lapply(function(x) {
  dad <- ess_data[[x]]
  orig <- origens_isco88 %>% filter(ESSROUND==x) %>% select(-ESSROUND)
  merge(dad, orig)
})


# Ara apliquem l'esquema de classes recomanat per l'ESS, de Daniel Oesch.
# També per als progenitors, i aplicant el criteri de dominància determinarem l'origen:
ess_data_compl <- ess_data_compl %>%
  lapply(function(x)
    mutate(x, desti = Oesch_class_isco88(iscoco, emplrel, emplno)[[1]],
           o_mare = Oesch_class_isco88_origen(misco, msempl, msupvis)[[1]],
           o_pare = Oesch_class_isco88_origen(fisco, fsempl, fsupvis)[[1]],
           origen = dominancia_origen(o_pare, o_mare)))

# Casos complets (de les variables que ens interessen):
  
casos_compl <- ess_data_compl %>%
  lapply(selecio_vars_final) %>%
  lapply(na.omit) %>%
  sapply(nrow)

casos_compl/sapply(ess_data_compl, nrow)*100 #Percentatge de casos complets

# Ja podem ajuntar les dades de les diverses onades:
variables <- c("idno","essround","pais","genere","naix","origen","desti")
dades <- data.frame()
for(i in 1:5){
  b <- ess_data_compl[[i]] %>% selecio_vars_final %>% haven::zap_labels()
  colnames(b) <- variables
  dades <- rbind(dades, b)
}

# Observacions que contenen NAs:
nrow(na.omit(dades)) / nrow(dades) *100
# dades <- na.omit(dades)

openxlsx::write.xlsx(dades, file = "dadesESS_1.xlsx", rowNames=TRUE)

