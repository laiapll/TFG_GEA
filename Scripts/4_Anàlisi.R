# Amb els objectes carregats al script 2.2 fem l'anàlisi:

library(FactoMineR)
library(logmult)
library(MASS)

#####################
# AC:

# Taula de mobilitat social a Espanya:
dades_ca <- dades %>% filter(pais=="Spain")
n <- dades_ca %>% with(table(origen,desti))

## AC simple
ca_simple <- CA(n, graph = F, ncp=nrow(n)-1)
plot(ca_simple)
summary(ca_simple)

## Gènere (concatenar taules)

dades_g <- dades_ca %>%
  dplyr::select(origen, desti, genere)

t_homes <- table(dades_g)[,,1] %>% as.data.frame() %>%
  mutate(desti = paste0("M_",desti))

t_dones <- table(dades_g)[,,2] %>% as.data.frame() %>%
  mutate(desti = paste0("F_",desti))

t_conc <- xtabs(Freq ~ origen + desti,
                data=rbind(t_homes, t_dones))

ca_genere <- CA(t_conc, graph = FALSE, ncp = 5)
plot(ca_genere)
summary(ca_genere)

## MCA, Mètode Burt, amb la cohort:
t_ca_cohort <- dades_ca %>% 
  dplyr::select(origen, desti, cohort) %>%
  mutate(origen = paste0("o_",origen),
         desti = paste0("d_",desti))

MCA(t_ca_cohort, method="Burt", graph = FALSE)
plot(ca_genere)
summary(ca_genere)


####################
# Mobilitat absoluta

## Mobilitat ascendent, descendent i herència

# Funció per calcular la mobilitat ascendent, descendent i herència
mabs1 <- function(llista){
  prop <- lapply(llista, function(x) prop.table(x)*100)
  taula <- rbind(
    prop %>% lapply(function(x) sum(x[lower.tri(x)])) %>%
      as.data.frame(row.names = "Ascendent"),
    prop %>% lapply(function(x) sum(diag(x))) %>%
      as.data.frame(row.names = "Herència"),
    prop %>% lapply(function(x) sum(x[upper.tri(x)])) %>%
      as.data.frame(row.names = "Descendent")
  ) %>% t() %>% as.data.frame() 
  return(taula)
}

herencia <- lapply(llistes, mabs1)

## Mobilitat estructural

# Funció per calcular la mobilitat estructural

mabs2 <- function(llista){
  taula <- llista %>% lapply(function (d){
    data.frame("Padres" = d %>% apply(1, sum) %>% prop.table()*100,
               "Hijos"  = d %>% apply(2, sum) %>% prop.table()*100) %>%
      mutate("Diferencia" = Padres - Hijos,
             "Diferencia absoluta" = abs(Diferencia))
  })
  estruct <- taula %>% sapply(function(x) colSums(x)[[4]]/2) %>%
    as.data.frame()
  colnames(estruct) <- "Mobilitat estructural (%)"
  #return(list(t=taula, mob.estruct=estruct))
  return(estruct)
}

mestructural <- lapply(llistes, mabs2)

#####################
# Mobilitat relativa:

## Mobilitat relativa a través de les cohorts:

mrel <- function(x, dad=dades){
  #Seleccionem les dades del país:
  dades_c <- dad %>% filter(pais==x)
  n_obs <- nrow(dades_c)
  
  # Format de freqüències i taula (3d)
  freqs_p <- dades_c %>% group_by(origen, desti, cohort) %>%
    summarise(Freq=n())
  taula_p <- xtabs(Freq ~ origen + desti + cohort, data=freqs_p)
  taula_p[taula_p==0] <- 0.0001 # No poden haver zeros
  
  #Models:
  fconst <- loglm(Freq ~ origen:desti + origen:cohort + desti:cohort,
                  data=taula_p, param=TRUE, fit=TRUE)
  unid <- unidiff(taula_p, verbose=FALSE)
  
  # L^2 de Schwartz:
  L2_s <- function(desv, gl){
    n_min <- apply(taula_p, 3, sum) %>% min
    l2 <- (desv-gl)/n_obs * n_min + gl
    return(l2)
  }
  fconst$dev_Schwartz <- L2_s(fconst$deviance, fconst$df)
  unid$dev_Schwartz <- L2_s(unid$deviance, unid$df.residual)
  
  # Taula comparativa
  comparacio <- matrix(NA, 3, 6) %>% data.frame()
  colnames(comparacio) <- c("Model", "L^2 (S)", "Graus de llibertat",
                            "Significació", "BIC", "D")
  comparacio[,1] <- c("1. Fluïdesa constant", "2. Diferències uniformes",
                      "Diferència 1-2")
  
  comparacio[1,2] <- fconst$dev_Schwartz
  comparacio[1,3] <- fconst$df
  comparacio[1,4] <- 1 - pchisq(fconst$dev_Schwartz, fconst$df)
  comparacio[1,5] <- fconst$deviance - fconst$df*log(n_obs) #BIC
  comparacio[1,6] <- sum(abs((fconst$fitted - taula_p))/(2*n_obs)) *100 #D
  
  comparacio[2,2] <- unid$dev_Schwartz
  comparacio[2,3] <- unid$df.residual
  comparacio[2,4] <- 1 - pchisq(unid$dev_Schwartz, unid$df.residual)
  comparacio[2,5] <- unid$deviance - unid$df.residual*log(n_obs) #BIC
  comparacio[2,6] <- summary(unid)$dissim *100 #D
  
  comparacio[3,2:3] <- apply(comparacio[1:2,2:3], 2, function(x) -diff(x))
  comparacio[3,4] <- 1 - pchisq(comparacio[3,2], abs(comparacio[3,3]))
  
  comparacio <- cbind("País"=x, comparacio)
  
  return(list(t=comparacio, layer=summary(unid)$layer))
}

mrel_pais <- lapply(etiquetes$pais, mrel)
mrel_dones <- lapply(etiquetes$pais,
                     function(x) mrel(x, filter(dades, genere==etiquetes$genere[2])))
mrel_homes <- lapply(etiquetes$pais,
                     function(x) mrel(x, filter(dades, genere==etiquetes$genere[1])))

names(mrel_pais)<-names(mrel_dones)<-names(mrel_homes)<-etiquetes$pais

# Significació
mrel_pais %>% sapply(function(x) x$layer[2:4,5] <0.05)
mrel_dones %>% sapply(function(x) x$layer[2:4,5] <0.05)
mrel_homes %>% sapply(function(x) x$layer[2:4,5] <0.05)
