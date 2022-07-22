# Amb els objectes carregats al script 2.2 fem l'anàlisi:

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(xtable)

#Gràfics AC:

ca_simple_plot <- rbind(ca_simple$row$coord[,1:2], ca_simple$col$coord[,1:2]) %>% as.data.frame() %>%
  cbind(Variable=rep(c("Origen", "Destí"), each=5),
        Label=rep(etiquetes$classes_n,2)) %>%
  ggplot(aes(x = `Dim 1`, y = `Dim 2`,
             col = Variable, shape = Variable,
             label = Label)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  labs(x = paste0("Dim 1 (", signif(ca_simple$eig[1,2], 3), "%)"),
       y = paste0("Dim 2 (", signif(ca_simple$eig[2,2], 3), "%)"),
       col = "", shape = "") +
  theme_light(base_size = 11) +
  coord_fixed(ratio = 1) +
  theme(legend.position="bottom") +
  scale_color_manual(values=c(paleta[[1]],paleta[[2]])) +
  #scale_shape_manual(values=c(16, 16, 17)) +
  geom_point(size=3) +
  geom_text_repel(size=3)

fl <- cbind(start=ca_simple$row$coord[,1:2], end=ca_simple$col$coord[,1:2])
for (i in c(1:5)){
  ca_simple_plot <- ca_simple_plot +
    geom_segment(aes_string(x=fl[i,1], y=fl[i,2],
                            xend=fl[i,3],yend=fl[i,4]),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color="gray", size = 0.1)
}


make.ca.plot.df <- function (ca.plot.obj, row.lab = "Rows", col.lab = "Columns") {
  r <- ca.plot.obj$row
  c <- ca.plot.obj$col
  df <- data.frame(Label = c(rownames(r$coord), rownames(c$coord)),
                   Dim1 = c(r$coord[,1], c$coord[,1]),
                   Dim2 = c(r$coord[,2], c$coord[,2]),
                   Size = c(r$cos2[,2], c$cos2[,2]),
                   Variable = c(rep(row.lab, nrow(r$coord)), rep(col.lab, nrow(c$coord))))
  rownames(df) <- 1:nrow(df)
  df
}

ca.plot.df <- make.ca.plot.df(ca_genere, "Origen", "Destí"); ca.plot.df$Label <- rep(etiquetes$classes_n, 3); ca.plot.df$Variable <- rep(c("Origen","Destí dones","Destí homes"), each=5)

p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable, #size = Size,
                            label = Label))

p1 <- p +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  labs(x = paste0("Dim 1 (", signif(ca_genere$eig[1,2], 3), "%)"),
       y = paste0("Dim 2 (", signif(ca_genere$eig[2,2], 3), "%)"),
       col = "", shape = "") +
  theme_light(base_size = 11) +
  coord_fixed(ratio = 1) +
  theme(legend.position="bottom") +
  scale_color_manual(values=c(paleta[[1]],paleta[[2]],paleta[[3]])) +
  scale_shape_manual(values=c(16, 16, 17))


p2 <- p1 + 
  geom_point(size=3) +
  geom_text_repel(size=3)

fl <- cbind(start=ca.plot.df[1:5, 2:3], end_f=ca.plot.df[6:10, 2:3],
            end_m=ca.plot.df[11:15, 2:3])
p3 <- p2
for (i in c(1:5)){
  p3 <- p3 +
    geom_segment(aes_string(x=fl[i,1], y=fl[i,2],
                            xend=fl[i,3],yend=fl[i,4]),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color="gray", size = 0.1)
  
  p3 <- p3 + 
    geom_segment(aes_string(x=fl[i,1], y=fl[i,2],
                            xend=fl[i,5], yend=fl[i,6]),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color="darkgray", size = 0.1)
}

ca_gen_plot <- p3


p <- ca_cohort$var$coord %>% as.data.frame() %>%
  mutate("Label"=c(rep(etiquetes$classes_n, 2), etiquetes$cohort)) %>%
  cbind("Variable"=rep(c("Origen", "Destí", "Cohort"),each=5)[-15]) %>%
  ggplot(aes(x = `Dim 1`, y = `Dim 2`,
             col = Variable, shape = Variable, #size = Size,
             label = Label))

p1 <- p +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  labs(x = paste0("Dim 1 (", signif(ca_cohort$eig[1,2], 3), "%)"),
       y = paste0("Dim 2 (", signif(ca_cohort$eig[2,2], 3), "%)"),
       col = "", shape = "") +
  theme_light(base_size = 11) +
  coord_fixed(ratio = 1) +
  theme(legend.position="bottom") +
  #scale_shape_manual(values=c(16, 16, 17)) +
  scale_color_manual(values=c(paleta[[1]],paleta[[2]],paleta[[3]]))

p2 <- p1 + 
  geom_point(size=3) +
  geom_text_repel(size=3)

ca_coh_plot <- p2

# Taules inèrcies:
ac_caption <- list(t1="Inèrcies principals", 
                   t2="Contribució relativa de l'eix a la inèrcia del perfil")

t1_simple <- list()
t1_simple$t <- ca_simple$eig[,1:2]
colnames(t1_simple$t) <- c("Inèrcia principal", "% inèrcia")
t1_simple$tot <- colSums(t1_simple$t) %>% as.data.frame() %>% t
rownames(t1_simple$tot) <- "Total"

xtableList(t1_simple, digits=c(NA, 4, 2),
           caption = paste0("AC origen-destí: ", ac_caption$t1),
           lab="tab:ac_simp1") %>%
  print(caption.placement = "top")

t2_simple <- rbind(ca_simple$row$cos2, ca_simple$col$cos2) %>% as.data.frame() %>%
  mutate(Classe=c(etiquetes$classes_n, etiquetes$classes_n),
         Variable=c(rep(c("Origen","Destí"), each=5)))
t2_simple <- t2_simple[,c(6,5,1:4)]

xtable(t2_simple,
       caption = paste0("AC origen-destí: ", ac_caption$t2),
       lab="tab:ac_simp2") %>%
  print(caption.placement = "top",
        include.rownames = FALSE)

t1_square <- tab4(ca_quad)$table

xtable(t1_square, digits=c(0,4,2,2,2),
       caption = paste0("AC origen-destí: ", ac_caption$t1),
       lab="tab:ac_sq1") %>%
  print(NA.string="-", caption.placement = "top")

ine <- round(tab4(ca_quad)$tot_inertias, 4) %>% paste0()
per <- round(tab4(ca_quad)$tot_inertias/tab4(ca_quad)$tot_inertias[1]*100, 2) %>% paste0()
names(ine) <- names(per) <- c("Total", "Simètrica", "antisimètrica")
t1_square_tot <- rbind("Inèrcia" = ine, "%" = per)
xtable(t1_square_tot, caption="AC origen-destí: Inèrcies totals") %>%
  print(caption.placement = "top")

t2_square <- data.frame(
  symm_12 = ca_quad$row$cos2[,1:2] %>% apply(1, sum),
  skew_67 = ca_quad$row$cos2[,6:7] %>% apply(1, sum)) %>%
  mutate(total = symm_12 + skew_67)

t2_square <- t2_square[1:nrow(n),]
colnames(t2_square) <- c("Simètrica (eixos 1 i 2)", "Antisimètrica (eixos 6 i 7)", "Total (eixos 1, 2, 6 i 7)")
rownames(t2_square) <- etiquetes$classes_n

xtable(t2_square, digits=3,
       caption = paste0("AC origen-destí: ", ac_caption$t2),
       lab="tab:ac_sq2") %>%
  print(caption.placement = "top")

# Gènere
t1_genere <- list()
t1_genere$t <- ca_genere$eig[,1:2]
colnames(t1_genere$t) <- c("Inèrcia principal", "% inèrcia")
t1_genere$tot <- colSums(t1_genere$t) %>% as.data.frame() %>% t
rownames(t1_genere$tot) <- "Total"

xtableList(t1_genere, digits=c(NA, 4, 2), 
           caption = paste0("AC origen-destí-gènere: ", ac_caption$t1),
           lab="tab:ac_gen1") %>%
  print(caption.placement = "top")

t2_genere <- rbind(ca_genere$row$cos2, ca_genere$col$cos2) %>% as.data.frame() %>%
  mutate(Classe=c(etiquetes$classes_n, rep(etiquetes$classes_n, each=2)),
         Gènere=c(rep(NA, 5), rep(c("F","M"), 5)))
t2_genere <- t2_genere[,c(6,5,1:4)]

xtable(t2_genere,
       caption = paste0("AC origen-destí-gènere: ", ac_caption$t2),
       lab="tab:ac_gen2") %>%
  print(caption.placement = "top",
        include.rownames = FALSE)

# Cohort
t1_cohort <- list()
t1_cohort$t <- ca_cohort$eig[,1:2]
colnames(t1_cohort$t) <- c("Inèrcia principal", "% inèrcia")
t1_cohort$tot <- colSums(t1_cohort$t) %>% as.data.frame() %>% t
rownames(t1_cohort$tot) <- "Total"

xtableList(t1_cohort, digits=c(NA, 4, 2),
           caption = paste0("AC origen-destí-cohort: ", ac_caption$t1),
           lab="tab:ac_coh1") %>%
  print(caption.placement = "top")

t2_cohort <- ca_cohort$var$cos2 %>% as.data.frame() %>%
  mutate(Categoria=c(rep(etiquetes$classes_n, 2), etiquetes$cohort),
         Variable=c(rep(c("Origen", "Destí", "Cohort"),each=5)[-15]))
t2_cohort <- t2_cohort[,c(7,6,1:4)]

xtable(t2_cohort,
       caption = paste0("AC origen-destí-cohort: ", ac_caption$t2),
       lab="tab:ac_coh2") %>%
  print(caption.placement = "top",
        include.rownames = FALSE)

# Gràfics mobilitat:

#Gràfic: Taxes de mobilitat ascendent, descendent i herència per país (%)

h1 <- herencia$paisos %>%
  rownames_to_column(var="pais") %>%
  arrange(Ascendent) #ordre paisos
h1$pais[h1$pais=="United.Kingdom"] <- "United Kingdom"

g2_mob <- rbind(
  data.frame(pais=c(1:21), mob=3, propo=h1$Ascendent),
  data.frame(pais=c(1:21), mob=2, propo=h1$Descendent),
  data.frame(pais=c(1:21), mob=1, propo=h1$Herència)) %>%
  mutate(mob=factor(mob),
         pais=factor(pais, levels=c(1:21), labels=h1$pais)) %>%
  ggplot(aes(x = pais, y = propo, fill = mob, label=paste0(round(propo,2), "%"))) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  xlab("") + ylab("") + ggtitle("") +
  scale_y_continuous(breaks=NULL) +
  geom_text(position = position_fill(vjust = 0.5), size = 3, color = "#ffffff") +
  scale_fill_manual(values= paleta,
                    #name="Mobilitat",
                    name="",
                    breaks=c(3,2,1),
                    labels=c("Ascendent","Descendent", "Herència")) +
  theme(legend.position = "top", legend.key.size = unit(1, "lines"),
        legend.text=element_text(size = 8, face="bold"),
        legend.title=element_text(size = 8.5, face="bold"))

# Gràfic: Relació entre les taxes de mobilitat ascendent i descendent per país.

g3_mob <- ggplot(h1, aes(x = Ascendent, y = Descendent, label = pais)) +
  geom_point() +
  #xlim(27.5,42.5) + ylim(27.5,42.5) +
  coord_fixed(ratio = 1) +
  xlab("Mobilitat ascendent (%)") +
  ylab("Mobilitat descendent (%)") +
  #theme_minimal() +
  geom_abline(colour = paleta[2], linetype="dotted") +
  geom_abline(intercept=mean(100-h1$Herència),
              slope= -1, colour = paleta[1], linetype="dotted") +
  ggrepel::geom_text_repel()


# Gràfic: Taxes de mobilitat estructural per país (%)

est1 <- mestructural$paisos
colnames(est1) <- c("m_estr")
est1 <- est1 %>%
  rownames_to_column(var="pais") %>%
  arrange(m_estr) #ordre paisos
est1$pais[est1$pais=="United.Kingdom"] <- "United Kingdom"


g4_mob <- data.frame(pais=c(1:21), mob=2, propo=est1$m_estr) %>%
  mutate(mob=factor(mob),
         pais=factor(pais, levels=c(1:21), labels=est1$pais)) %>%
  ggplot(aes(x = pais, y = propo)) +
  geom_bar(stat = "identity", fill = paleta[[3]]) +
  xlab("") +
  ylab("") +
  theme_minimal() + 
  coord_flip() +
  scale_y_continuous(breaks=NULL) +
  theme(panel.grid.minor = element_blank()) +
  geom_text(aes(label=paste0(round(propo,2), "%")), hjust=1.1, size=3)


#Gràfic: Relació entre l'herència social i la mobilitat estructural per país

g5_mob <- merge(
  arrange(h1, pais) %>% dplyr::select(pais, her=`Herència`),
  arrange(est1, pais)) %>%
  ggplot(aes(x = her, y = m_estr, label = pais)) +
  geom_point() +
  xlab("Herència (%)") + ylab("Mobilitat estructural (%)") +
  #theme_classic() +
  geom_smooth(method = "lm", se = F,
              colour = paleta[[3]], linetype="dotted") +
  ggrepel::geom_text_repel()


# Gràfic: Diferències en la mobilitat absoluta en homes i dones per país
h2 <-list()

h2$asc <- data.frame(herencia$dones$Ascendent, herencia$homes$Ascendent)
h2$desc <- data.frame(herencia$dones$Descendent, herencia$homes$Descendent)
h2$her <- data.frame(herencia$dones$`Herència`, herencia$homes$`Herència`)
h2$est <- data.frame(mestructural$dones, mestructural$homes)

h2 <- h2 %>% lapply(function(x) {
  colnames(x) <- c("Dones", "Homes")
  rownames(x) <- rownames(mestructural$dones)
  x <- rownames_to_column(x, var="País")
  x$`País`[x$`País`=="United.Kingdom"] <- "United Kingdom"
  return(x)
})

g6_mob <- h2 %>% lapply(function(d){
  ggplot(d, aes(x = Homes, y = Dones, label = `País`)) +
    geom_point() + #size=1
    xlab("") + ylab("") +
    #theme_minimal() +
    ggrepel::geom_text_repel() + #size=2.5
    geom_abline(colour = "red", linetype="dotted")
})

# Gràfic: Mobilitat ascendent, descendent i herència al llarg del temps per país

h3 <- data.frame()
for (i in 1:4){
  c <- cbind(herencia[[3+i]], "Cohort"=etiquetes$cohort[i])
  c <- rownames_to_column(c, "País")
  c$`País`[c$`País`=="United.Kingdom"] <- "United Kingdom"
  h3 <- rbind(h3, c)
}

g7_mob <- h3 %>%
  gather("Mobilitat", "Valor", -c(País, Cohort)) %>%
  ggplot(aes(x=as.factor(Cohort), y=Valor)) +
  geom_line(aes(group=Mobilitat, color=Mobilitat)) +
  facet_wrap(~`País`, scales="fixed", ncol=3) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "top", legend.key.size = unit(1, "lines"),
        legend.text=element_text(size = 8, face="bold"),
        legend.title=element_text(size = 8.5, face="bold")) +
  scale_color_manual(values= c(paleta[[3]],paleta[[2]], paleta[[1]]))


# Gràfic: Mobilitat estructural al llarg del temps per país

est3 <- data.frame()
for (i in 1:4){
  c <- cbind(mestructural[[3+i]], etiquetes$cohort[i])
  colnames(c) <- c("Estr", "Cohort")
  c <- rownames_to_column(c, "País")
  c$`País`[c$`País`=="United.Kingdom"] <- "United Kingdom"
  est3 <- rbind(est3, c)
}

g8_mob <- est3 %>% 
  ggplot(aes(x=as.factor(Cohort), group=`País`)) +
  geom_line(aes(y=Estr), color=paleta[3]) +
  facet_wrap(~`País`, scales="fixed", ncol=3) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Taules de mobilitat

xtable_models_annex <- function(llist, cap, labl){
  llist %>% lapply(function(x){
    t <- x$t
    t[2:3,1]<-NA
    colnames(t)[3:7] <- c("$L^2$ (S)","G. llib.","Sign.","BIC","D")
    return(t)}) %>%
    xtableList(digits = c(NA,NA, NA, 2, 0, 3, 2, 2),
               caption = cap,
               lab = labl) %>%
    print(caption.placement = "top",
          type = "latex",
          sanitize.text.function=function(x){x},
          include.rownames = FALSE,
          #size="\\fontsize{10pt}{10pt}\\selectfont",
          size="small",
          tabular.environment = 'longtable', floating = FALSE)
}

xtable_models_annex(mrel_pais, "Models de mobilitat relativa per país", "tab:annex_t1")
xtable_models_annex(mrel_dones, "Models de mobilitat relativa de les dones per país", "tab:annex_t2")
xtable_models_annex(mrel_homes, "Models de mobilitat relativa dels homes per país", "tab:annex_t3")

#  TAULA MODELS ESPANYA.
x <- mrel_pais$Spain
xtable_models <- function(x, cap, lab){
  xt <- x$t[,-c(1:2)] #%>% round(4)
  colnames(xt) <- c("$L^2$ (S)","G. llib.","Sign.","BIC","D")
  rownames(xt) <- c("1. Model de fluïdesa constant",
                    "2. Model de diferències uniformes",
                    "Diferència 1-2")
  
  xtable(xt, digits = c(NA, 2, 0, 3, 2, 2), caption = cap, label=lab) %>%
    print(type = "latex",
          sanitize.text.function=function(x){x},
          caption.placement="top")
}

xtable_models(mrel_pais$Spain, "Models de mobilitat relativa per a Espanya", "tab:mod_esp")
xtable_models(mrel_dones$Spain, "Models de mobilitat relativa per a les dones a Espanya", "tab:mod_esp_d")
xtable_models(mrel_homes$Spain, "Models de mobilitat relativa per als homes a Espanya", "tab:mod_esp_h")
