# 3. faza: Vizualizacija podatkov
require(dplyr)
require(tidyr)
require(readr)

library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
#to sem dodala rešila napako TRUE is not TRUE

#povzetek

#skupno regije
podatki_po_letih1 <- Regije %>% group_by(Leto) %>% 
  summarise(Skupno_število_podjetij = sum(Število_podjetij)) 
podatki_po_letih2 <- Regije %>% group_by(Leto) %>% 
  summarise(Skupno_število_oseb = sum(Število_oseb)) 
podatki_po_letih3 <- Regije %>% group_by(Leto) %>%
  summarise(Skupni_prihodek = sum(Skupni_prihodek))

podatki_po_letih <- cbind(podatki_po_letih1, podatki_po_letih2[2], podatki_po_letih3[2])
#podatki_po_letih <- inner_join(podatki_po_letih1, podatki_po_letih2)
#podatki_po_letih <- inner_join(podatki_po_letih, podatki_po_letih3) 


graf <- ggplot(podatki_po_letih, aes(x=Leto, y=Skupno_število_podjetij/1e6))+ 
  geom_line(aes(size=Skupno_število_oseb)) + xlab("Leto") + ylab("Število") 
print(graf)
  

# Uvozimo zemljevid.

zemljevid <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip",
                          "gadm36_SVN_1") %>% fortify()

#Primerjava imen regij:
#zr <- zemljevid$NAME_1 %>% unique()
#rr <- Regije$Regija %>% unique()

Regije$Regija[Regije$Regija=="Posavska"] <- "Spodnjeposavska"
Regije$Regija[Regije$Regija=="Primorsko-notranjska"] <- "Notranjsko-kraška"
Regije$Regija <- factor(Regije$Regija)

# Zemljevid Slovenije pobarvan glede na število podjetij v regiji leta 2016

st_podjetij_2016 <- Regije %>% select(Regija, Leto, Število_podjetij) %>% filter(Leto==2016)

zemljevid1 <- ggplot(inner_join(zemljevid, st_podjetij_2016, by = c("NAME_1"='Regija'))) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Število_podjetij)) +
  scale_fill_gradientn(colours = terrain.colors(10), trans = "log10")
                
print(zemljevid1)

# Zemljevid Slovenije pobarvan glede na prihodek v regiji leta 2016

prihodek_2016 <- Regije %>% select(Regija, Leto, Skupni_prihodek) %>% filter(Leto==2016)

zemljevid2 <- ggplot(inner_join(zemljevid, prihodek_2016, by = c("NAME_1"='Regija'))) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Skupni_prihodek/1e6)) +
  scale_fill_gradientn(colours = terrain.colors(10), trans = "log10")
print(zemljevid2)

