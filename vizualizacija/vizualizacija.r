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
  geom_line(aes(size=Skupno_število_oseb,color="red")) + xlab("Leto") + ylab("Število") 
print(graf)
  

# Uvozimo zemljevid.
uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", "gadm36_SVN_0",
                pot.zemljevida="gadm36_SVN_0")
#zemljevid <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_SVN_gpkg.zip", "REGIJE",
                             encoding="Windows-1250")
ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE)
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
