# Anàlisi descriptiva:

library(ggplot2)
library(gridExtra)

descr_1 <- rbind(
  group_by(dades, cat="Mida mostral per gènere", etiq = genere) %>% summarise(freq=n()),
  group_by(dades, cat="Mida mostral per cohort", etiq = cohort) %>% summarise(freq=n())
) %>%
  ggplot(aes(x=etiq, y=freq)) +
  geom_bar(stat = "identity", fill = paleta[[1]]) +
  xlab("") +
  ylab("") +
  ylim(c(0, 52000)) +
  theme_minimal() + 
  geom_text(aes(label=freq), vjust=-0.25, size=2.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_wrap(~ cat, scales = "free_x", ncol=2)

descr_p <- group_by(dades, cat="Mida mostral per país", etiq = pais) %>% summarise(freq=n()) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(x = reorder(etiq, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = paleta[[1]]) +
  xlab("") +
  ylab("") +
  ylim(c(0, 8000)) +
  theme_minimal() + 
  geom_text(aes(label=freq), vjust=-0.25, size=2.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_wrap(~ cat, scales = "free_x", ncol=1)

e_social <- rbind(
  #dades %>% with(prop.table(table(origen, dnn="occ"))*100) %>% data.frame(Tipus=1),
  #dades %>% with(prop.table(table(desti, dnn="occ"))*100) %>% data.frame(Tipus=2)) %>%
  dades %>% with(table(origen, dnn="occ")) %>% data.frame(Tipus=1),
  dades %>% with(table(desti, dnn="occ")) %>% data.frame(Tipus=2)) %>%
  mutate(occ=factor(occ),
         Tipus=factor(Tipus, levels=c(1,2), labels=c("Origen", "Destí")),
         cat="Mida mostral i estructura social") %>%
  ggplot(aes(x = occ, y = Freq, fill = Tipus, label=Freq)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) + 
  scale_color_manual(values=c(paleta[[2]], paleta[[3]])) +
  scale_fill_manual(values=c(paleta[[2]], paleta[[3]])) +
  theme_minimal() +
  geom_text(position = position_dodge(0.8), vjust=-1, size = 2.5, color = "black", check_overlap = TRUE) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  xlab("") +
  ylab("") + ylim(c(0, 40000)) + 
  facet_wrap(~ cat, scales = "free_x", ncol=1)


grid.arrange(descr_1, descr_p, e_social, nrow = 3)

