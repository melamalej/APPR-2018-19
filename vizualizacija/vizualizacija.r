# 3. faza: Vizualizacija podatkov
require(dplyr)
require(tidyr)
require(readr)
require(reshape2)

library(rgdal)
library(maptools)
library(ggpubr)
library(ggplot2)
library(digest)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
#to sem dodala rešila napako TRUE is not TRUE

#Regije
podatki_po_letih1 <- Regije %>% group_by(Leto) %>% 
  summarise(Skupno_stevilo_podjetij = sum(Stevilo_podjetij)) 
podatki_po_letih2 <- Regije %>% group_by(Leto) %>% 
  summarise(Skupno_stevilo_oseb = sum(Stevilo_oseb)) 
podatki_po_letih3 <- Regije %>% group_by(Leto) %>%
  summarise(Skupni_prihodek = sum(Skupni_prihodek))

podatki_po_letih <- cbind(podatki_po_letih1, podatki_po_letih2[2], podatki_po_letih3[2])

graf1 <- ggplot() + 
  geom_line(data=podatki_po_letih, aes(x=Leto, y=Skupno_stevilo_podjetij)) +
  ggtitle("Število podjetij v Sloveniji") + labs(x="Leto", y="Število podjetij")
 
graf2 <- ggplot() + 
  geom_line(data=podatki_po_letih, aes(x=Leto, y=Skupno_stevilo_oseb/1e3)) +
  ggtitle("Število oseb, ki delajo v Sloveniji (deljeno s 1000)") + labs(x="Leto", y="Število oseb, ki delajo")

#Graf spremembe prihodka in bdp

sprememba <- rep(0,12)
for (i in 7:18){
  sprememba[i-6] <- round(100*(podatki_po_letih3[i,2]-podatki_po_letih3[i-1,2])/podatki_po_letih3[i-1,2], 4)
}
Sprememba_prihodka <- unlist(sprememba)

sprememba_prihodka <- data.frame(c(2005:2016), Sprememba_prihodka)
colnames(sprememba_prihodka) <- c("Leto", "Sprememba_prihodka")

bdp2 <- bdp[1:12,]
Sprememba <- cbind(bdp2,Sprememba_prihodka)
Sprememba <- melt(Sprememba, "Leto")

graf3 <- ggplot(Sprememba, aes(Leto, y=value, col=variable)) +
  geom_line() + 
  ggtitle("Primerjava spremembe skupnega prihodka s spremembo bdp") + 
  labs(x="Leto", y="Sprememba v %")

#Panoge - število podjetij po panogi
st_pod <- Panoge2008[1:3]
sum <- Panoge2008 %>% group_by(Panoga) %>% 
  summarise(`Stevilo_podjetij` = sum(`Stevilo_podjetij`)) 
povprecje <- sum(sum$Stevilo_podjetij)/21
prva_polovica <- filter(sum, Stevilo_podjetij >= povprecje-30000)
druga_polovica <- filter(sum, Stevilo_podjetij < povprecje-30000)

Prva_polovica <- subset(st_pod, Panoga %in% prva_polovica$Panoga)
Druga_polovica <- subset(st_pod, Panoga %in% druga_polovica$Panoga)

g1 <- ggplot(Prva_polovica, 
            aes(Leto, Stevilo_podjetij, group=interaction(Panoga), color=Panoga)) + 
  geom_line() + 
  labs(x="Leto", y="Število podjetij") +
  theme(legend.position = 'right', legend.text = element_text(size=8)) 

g2 <- ggplot(Druga_polovica, 
             aes(Leto, Stevilo_podjetij, group=interaction(Panoga), color=Panoga)) + 
  geom_line() + 
  labs(x="Leto", y="Število podjetij") +
  theme(legend.position = 'right', legend.text = element_text(size=8)) 
                                                              

#Panoge

#1)
prihodki_po_panogi <- Panoge %>% group_by(Panoga) %>% 
  summarise(`Prihodki_od_prodaje_v_tisoc_EUR` = sum(`Prihodki_od_prodaje_v_tisoc_EUR`))
m <- max(prihodki_po_panogi$`Prihodki_od_prodaje_v_tisoc_EUR`)  
naj_prihodek <- filter(prihodki_po_panogi, `Prihodki_od_prodaje_v_tisoc_EUR`==m )
prva <- filter(Panoge, Panoga == "Trgovina,vzdrževanje in popravila motornih vozil")

presezek_po_panogi <- Panoge %>% group_by(Panoga) %>% 
  summarise("Bruto_poslovni_presezek_v_tisoc_EUR" = sum(`Bruto_poslovni_presezek_v_tisoc_EUR`))
m2 <- max(presezek_po_panogi$`Bruto_poslovni_presezek_v_tisoc_EUR`)  
naj_presezek <- filter(presezek_po_panogi, `Bruto_poslovni_presezek_v_tisoc_EUR`==m2 )
druga <- filter(Panoge, Panoga == "Predelovalne dejavnosti")

zaposleni_po_panogi <- Panoge %>% group_by(Panoga) %>% 
  summarise("Stevilo_zaposlenih" = sum(`Stevilo_zaposlenih`))
m3 <- max(zaposleni_po_panogi$`Stevilo_zaposlenih`)  
naj_zaposleni <- filter(zaposleni_po_panogi, `Stevilo_zaposlenih`==m3 )
#Spet Predelovalne dejavnosti

#2) 
n <- min(prihodki_po_panogi$`Prihodki_od_prodaje_v_tisoc_EUR`)
najslab_prihodek <- filter(prihodki_po_panogi, `Prihodki_od_prodaje_v_tisoc_EUR`==n )
tretja <- filter(Panoge, Panoga == "Rudarstvo")

n2 <- min(presezek_po_panogi$Bruto_poslovni_presezek_v_tisoc_EUR)  
najslab_presezek <- filter(presezek_po_panogi, `Bruto_poslovni_presezek_v_tisoc_EUR`==n2 )
#tudi rudarstvo
n3 <- min(presezek_po_panogi$Bruto_poslovni_presezek_v_tisoc_EUR[presezek_po_panogi$Bruto_poslovni_presezek_v_tisoc_EUR!=min(presezek_po_panogi$Bruto_poslovni_presezek_v_tisoc_EUR)] )
drugonajslab_presezek <- filter(presezek_po_panogi, `Bruto_poslovni_presezek_v_tisoc_EUR`==n3 )
cetrta <- filter(Panoge, Panoga == "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja")

n4 <-  min(prihodki_po_panogi$`Prihodki_od_prodaje_v_tisoc_EUR`[prihodki_po_panogi$`Prihodki_od_prodaje_v_tisoc_EUR`!=min(prihodki_po_panogi$`Prihodki_od_prodaje_v_tisoc_EUR`)] )
drugonajslab_prihodek <- filter(prihodki_po_panogi, `Prihodki_od_prodaje_v_tisoc_EUR`==n4)
peta <- filter(Panoge, Panoga == "Poslovanje z nepremičninami")

n5 <- min(zaposleni_po_panogi$`Stevilo_zaposlenih`)  
naj_zaposleni <- filter(zaposleni_po_panogi, `Stevilo_zaposlenih`==n5)
#Spet Rudarstvo

najboljsi <- rbind(prva, druga)
najslabsi <- rbind(tretja, cetrta, peta)

graf4 <- ggplot(najboljsi, 
                aes(x=Leto,y=Prihodki_od_prodaje_v_tisoc_EUR/1e3, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Prihodki od prodaje") + 
  labs(x="Leto", y="Prihodki od prodajev milijon EUR") 

graf5 <- ggplot(najboljsi, 
                aes(x=Leto,y=Bruto_poslovni_presezek_v_tisoc_EUR/1e3, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Bruto poslovni presezek") + 
  labs(x="Leto", y="Bruto poslovni presežek v milijon EUR") 

graf6 <- ggplot(najboljsi, 
                aes(x=Leto,y=Stevilo_zaposlenih, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Število zaposlenih") + 
  labs(x="Leto", y="Število zaposlenih") 

graf7 <- ggplot(najslabsi, 
                aes(x=Leto,y=Prihodki_od_prodaje_v_tisoc_EUR, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Prihodki od prodaje") + 
  labs(x="Leto", y="Prihodki od prodaje v milijon EUR") 

graf8 <- ggplot(najslabsi, 
                aes(x=Leto,y=Bruto_poslovni_presezek_v_tisoc_EUR, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Bruto poslovni presezek") + 
  labs(x="Leto", y="Bruto poslovni presežek v milijon EUR") 

graf9 <- ggplot(najslabsi, 
                aes(x=Leto,y=Stevilo_zaposlenih, group=interaction(Panoga), color=Panoga)) + 
  geom_line() +
  ggtitle("Število zaposlenih") + 
  labs(x="Leto", y="Število zaposlenih") 

figure <- ggarrange(graf7, graf8, graf9,
                    ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

figure2 <- ggarrange(graf4, graf5, graf6,
                    ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

# Uvozimo zemljevid.

zemljevid <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip",
                          "gadm36_SVN_1", encoding = "UTF-8") %>% fortify()

#Primerjava imen regij:
#zr <- zemljevid$NAME_1 %>% unique()
#rr <- Regije$Regija %>% unique()

Regije$Regija[Regije$Regija=="Posavska"] <- "Spodnjeposavska"
Regije$Regija[Regije$Regija=="Primorsko-notranjska"] <- "Notranjsko-kraška"
Regije$Regija <- factor(Regije$Regija)

# Zemljevid Slovenije pobarvan glede na število podjetij v regiji leta 2016

st_podjetij_2016 <- Regije %>% select(Regija, Leto, Stevilo_podjetij) %>% filter(Leto==2016)

zemljevid1 <- ggplot(left_join(zemljevid, st_podjetij_2016, by = c("NAME_1"='Regija'))) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Stevilo_podjetij)) +
  scale_fill_gradientn(colours = terrain.colors(10), trans = "log10")

# Zemljevid Slovenije pobarvan glede na prihodek v regiji leta 2016

prihodek_2016 <- Regije %>% select(Regija, Leto, Skupni_prihodek) %>% filter(Leto==2016)

zemljevid2 <- ggplot(left_join(zemljevid, prihodek_2016, by = c("NAME_1"='Regija'))) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Skupni_prihodek)) +
  scale_fill_gradientn(colours = terrain.colors(10), trans = "log10")


