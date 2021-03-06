# 2. faza: Uvoz podatkov

require(dplyr)
require(tidyr)
require(readr)
require(tibble)

library(reshape2)
library(rvest)
library(gsubfn)

#Opombe:
#Locale objekt poskuša zajeti vse privzete nastavitve, ki se lahko razlikujejo med državami.

#Regije (tabela1)
tabela1 <- read_csv2("podatki/st.podjetij_regije2008.csv", skip = 4, n_max = 12, col_names = c("Regija",2008:2016),
                     locale = locale(encoding = "Windows-1250"))
tabela1 <- tabela1 %>% melt(tabela1, id.vars = "Regija", measure.vars = names(tabela1)[-1],
                            value.name = "Stevilo_podjetij", variable.name = "Leto")
tabela1$Leto <- parse_number(tabela1$Leto)

tabela2 <- read_csv2("podatki/st.podjetij_regije2007.csv", skip = 4, n_max = 12, col_names = c("Regija", 1999:2007),
                     locale = locale(encoding = "Windows-1250"))
tabela2 <- tabela2 %>% melt(tabela2, id.vars = "Regija", measure.vars = names(tabela2)[-1],
                            value.name = "Stevilo_podjetij", variable.name = "Leto")
tabela2$Regija[tabela2$Regija == "Spodnjeposavska"] <- "Posavska"
tabela2$Regija[tabela2$Regija == "Notranjsko-kraška"] <- "Primorsko-notranjska"
tabela2$Leto <- parse_number(tabela2$Leto)

tabela3 <- read_csv2("podatki/st.oseb_regije2008.csv", skip = 4, n_max = 12, col_names = c("Regija", 2008:2016),
                     locale = locale(encoding = "Windows-1250"))
tabela3 <- tabela3 %>% melt(tabela3, id.vars = "Regija", measure.vars = names(tabela3)[-1],
                            value.name = "Stevilo_oseb", variable.name = "Leto")
tabela3$Leto <- parse_number(tabela3$Leto)

tabela4 <- read_csv2("podatki/st.oseb_regije2007.csv", skip = 4, n_max = 12, col_names = c("Regija", 1999:2007),
                     locale = locale(encoding = "Windows-1250"))
tabela4 <- tabela4 %>% melt(tabela4, id.vars = "Regija", measure.vars = names(tabela4)[-1],
                            value.name = "Stevilo_oseb", variable.name = "Leto")
tabela4$Regija[tabela4$Regija == "Spodnjeposavska"] <- "Posavska"
tabela4$Regija[tabela4$Regija == "Notranjsko-kraška"] <- "Primorsko-notranjska"
tabela4$Leto <- parse_number(tabela4$Leto)

tabela5 <- read_csv2("podatki/prihodek_regije2008.csv", skip = 4, n_max = 12, col_names = c("Regija", 2008:2016),
                     locale = locale(encoding = "Windows-1250"))
tabela5 <- tabela5 %>% melt(tabela5, id.vars = "Regija", measure.vars = names(tabela5)[-1],
                            value.name = "Skupni_prihodek", variable.name = "Leto")
tabela5$Leto <- parse_number(tabela5$Leto)

tabela6 <- read_csv2("podatki/prihodek_regije2007.csv", skip = 4, n_max = 12, col_names = c("Regija", 1999:2007),
                     locale = locale(encoding = "Windows-1250"))
tabela6 <- tabela6 %>% melt(tabela6, id.vars = "Regija", measure.vars = names(tabela6)[-1],
                            value.name = "Skupni_prihodek", variable.name = "Leto")
tabela6$Regija[tabela6$Regija == "Spodnjeposavska"] <- "Posavska"
tabela6$Regija[tabela6$Regija == "Notranjsko-kraška"] <- "Primorsko-notranjska"
tabela6$Leto <- parse_number(tabela6$Leto)

Regije2008 <- bind_cols(tabela1, tabela3[3], tabela5[3])
Regije2007 <- bind_cols(tabela2, tabela4[3], tabela6[3])

Regije <- rbind(Regije2007, Regije2008)

#Panoge
Panoge <- c(rep("Kmetijstvo in lov, gozdarstvo in ribištvo",9), 
            rep("Rudarstvo",9), rep("Predelovalne dejavnosti",9),
            rep("Oskrba z električno energijo, plinom in paro",9), 
            rep("Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja", 9),
            rep("Gradbeništvo",9), rep("Trgovina,vzdrževanje in popravila motornih vozil",9),
            rep("Promet in skladiščenje",9), rep("Gostinostvo",9), 
            rep("Informacijske in komunikacijske dejavnosti",9),
            rep("Finančne in zavarovalniške dejavnosti",9), 
            rep("Poslovanje z nepremičninami", 9),
            rep("Strokovne, znanstvene in tehnične dejavnosti", 9), 
            rep("Druge raznovrstne poslovne dejavnosti", 9),
            rep("Dejavnosti javne uprave in obrambe, dejavnost obvezne socialne varnosti",9),
            rep("Izobraževanje", 9), rep("Zdravstvo in socialno varstvo",9),
            rep("Kulturne, razvedrilne in rekreacijske dejavnosti",9),
            rep("Druge dejavnosti", 9), 
            rep("Dejavnost gospodinjestev z zaposlenim hišnim osebjem, proizvodnja za lastno rabo",9),
            rep("Dejavnost eksteritorialnih organizacij in teles",9))


#tabela2
p1 <- read_csv2("podatki/st.pod_panoge2008.csv", skip = 4, n_max = 2, col_names = Panoge)
panoge1 <- as.data.frame(t(p1), row.names = NULL)
panoge1 <- remove_rownames(panoge1)
panoge1 <- cbind(Panoge, panoge1)
colnames(panoge1) <- c("Panoga", "Leto", "Stevilo_podjetij")

p3 <- read_csv2("podatki/st.oseb_panoge2008.csv", skip = 4, n_max = 2, col_names = Panoge, na = c("z"))
panoge3 <- as.data.frame(t(p3), row.names = NULL)
panoge3 <- remove_rownames(panoge3)
panoge3 <- cbind(Panoge, panoge3)
colnames(panoge3) <- c("Panoga", "Leto", "Stevilo_oseb")

Panoge2008 <- bind_cols(panoge1, panoge3[3])
Panoge2008$Leto <- parse_number(Panoge2008$Leto)

#tabela3
p2 <- read_csv2("podatki/st.pod_panoge2007.csv", skip = 4, n_max = 9, col_names = c("Panoga", 1999:2007),
                locale = locale(encoding = "Windows-1250"))
panoge2 <- p2 %>% melt(p2, value.name = "Stevilo_podjetij", id.vars = "Panoga", measure.vars = names(p2)[-1],
                            variable.name = "Leto")
panoge2$Panoga[panoge2$Panoga == "C RUDARSTVO"] <- "Rudarstvo"
panoge2$Panoga[panoge2$Panoga == "D PREDELOVALNE DEJAVNOSTI"] <- "Predelovalne dejavnosti"
panoge2$Panoga[panoge2$Panoga == "E OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN VODO"] <- "Oskrba z električno energijo, plinom in vodo"
panoge2$Panoga[panoge2$Panoga == "F GRADBENIŠTVO"] <- "Gradbeništvo"
panoge2$Panoga[panoge2$Panoga == "G TRGOVINA, POPRAVILA MOTORNIH VOZIL IN IZDELKOV ŠIROKE PORABE"] <- "Trgovina, popravila motornih vozil in izdelkov široke porabe"
panoge2$Panoga[panoge2$Panoga == "H GOSTINSTVO"] <- "Gostinstvo"
panoge2$Panoga[panoge2$Panoga == "I PROMET, SKLADIŠČENJE IN ZVEZE"] <- "Promet, skladiščenje in zveze"
panoge2$Panoga[panoge2$Panoga == "J FINANČNO POSREDNIŠTVO"] <- "Finančno posredništvo"
panoge2$Panoga[panoge2$Panoga == "K POSLOVANJE Z NEPREMIČNINAMI, NAJEM IN POSLOVNE STORITVE"] <- "Poslovanje z nepremičninami, najem in poslovne storitve"

p4 <- read_csv2("podatki/st.oseb_panoge2007.csv", skip = 4, n_max = 9, col_names = c("Panoga", 1999:2007),
                locale = locale(encoding = "Windows-1250"))
panoge4 <- p4 %>% melt(p4, value.name = "Stevilo_oseb", id.vars = "Panoga", measure.vars = names(p4)[-1],
                       variable.name = "Leto")
panoge4$Panoga[panoge4$Panoga == "C RUDARSTVO"] <- "Rudarstvo"
panoge4$Panoga[panoge4$Panoga == "D PREDELOVALNE DEJAVNOSTI"] <- "Predelovalne dejavnosti"
panoge4$Panoga[panoge4$Panoga == "E OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN VODO"] <- "Oskrba z električno energijo, plinom in vodo"
panoge4$Panoga[panoge4$Panoga == "F GRADBENIŠTVO"] <- "Gradbeništvo"
panoge4$Panoga[panoge4$Panoga == "G TRGOVINA, POPRAVILA MOTORNIH VOZIL IN IZDELKOV ŠIROKE PORABE"] <- "Trgovina, popravila motornih vozil in izdelkov široke porabe"
panoge4$Panoga[panoge4$Panoga == "H GOSTINSTVO"] <- "Gostinstvo"
panoge4$Panoga[panoge4$Panoga == "I PROMET, SKLADIŠČENJE IN ZVEZE"] <- "Promet, skladiščenje in zveze"
panoge4$Panoga[panoge4$Panoga == "J FINANČNO POSREDNIŠTVO"] <- "Finančno posredništvo"
panoge4$Panoga[panoge4$Panoga == "K POSLOVANJE Z NEPREMIČNINAMI, NAJEM IN POSLOVNE STORITVE"] <- "Poslovanje z nepremičninami, najem in poslovne storitve"

Panoge2007 <- bind_cols(panoge2, panoge4[3]) 

#tabela4
panoge <- c("Rudarstvo","Predelovalne dejavnosti", 
            "Oskrba z električno energijo, plinom in paro",
            "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",
            "Gradbeništvo","Trgovina,vzdrževanje in popravila motornih vozil",
            "Promet in skladiščenje", "Gostinostvo",
            "Informacijske in komunikacijske dejavnosti",
            "Poslovanje z nepremičninami", 
            "Strokovne, znanstvene in tehnične dejavnosti", 
            "Druge raznovrstne poslovne dejavnosti")

Panoge2 <- c(rep(c("Rudarstvo","Predelovalne dejavnosti", 
                 "Oskrba z električno energijo, plinom in paro",
                 "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",
                 "Gradbeništvo","Trgovina,vzdrževanje in popravila motornih vozil",
                 "Promet in skladiščenje", "Gostinostvo",
                 "Informacijske in komunikacijske dejavnosti",
                 "Poslovanje z nepremičninami", 
                 "Strokovne, znanstvene in tehnične dejavnosti", 
                 "Druge raznovrstne poslovne dejavnosti"), 13))
Leta2 <- c(rep(2005:2017, each = 12))            
            
p5 <- read_csv2("podatki/letno.csv", skip = 2, n_max = 13, locale = locale(encoding = "Windows-1250"))
prihodki = p5[2:13,2:14]
presezek = p5[2:13,15:27]
zaposleni = p5[2:13,28:40]
prihodki <- data.frame(Prihodek=unlist(prihodki, use.names = FALSE))
presezek <- data.frame(Presezek=unlist(presezek, use.names = FALSE))
zaposleni <- data.frame(Zaposleni=unlist(zaposleni, use.names = FALSE))

Panoge <- data.frame(Panoge2,Leta2, prihodki, presezek, zaposleni)
Panoge$Leta2 <- parse_number(Panoge$Leta2)
colnames(Panoge) <- c("Panoga", "Leto", "Prihodki_od_prodaje_v_tisoc_EUR",
                       "Bruto_poslovni_presezek_v_tisoc_EUR", "Stevilo_zaposlenih")

Panoge.tidy <- melt(Panoge, id.vars =c("Panoga", "Leto"), measure.vars = c("Prihodki_od_prodaje_v_tisoc_EUR", "Bruto_poslovni_presezek_v_tisoc_EUR", "Stevilo_zaposlenih"),
                    variable.name = "Spremenljivka", value.name = "Stevilo")

Panoge.prih.pres. <- data.frame(Panoge2,Leta2, prihodki, presezek)
colnames(Panoge.prih.pres.) <- c("Panoga", "Leto", "Prihodki_od_prodaje_v_tisoc_EUR",
                      "Bruto_poslovni_presezek_v_tisoc_EUR")

Panoge.prih.pres.tidy <- melt(Panoge.prih.pres., id.vars =c("Panoga", "Leto"), measure.vars = c("Prihodki_od_prodaje_v_tisoc_EUR", "Bruto_poslovni_presezek_v_tisoc_EUR"),
                              variable.name = "Spremenljivka", value.name = "Stevilo")

Panoge.zap. <- data.frame(Panoge2,Leta2,zaposleni)
colnames(Panoge.zap.) <- c("Panoga", "Leto", "Stevilo_zaposlenih")
Panoge.zap.tidy <- melt(Panoge.zap., id.vars =c("Panoga", "Leto"), measure.vars = "Stevilo_zaposlenih",
                        variable.name = "Spremenljivka", value.name = "Stevilo")

#tabela5
link <- "https://en.wikipedia.org/wiki/Economy_of_Slovenia"
stran <- html_session(link) %>% read_html() 
bdp <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%.[[1]]%>% html_table()
summary(bdp) #character

bdp <- data.frame(Leto = 2005:2017, BDP = t(bdp[3, 5:17]), row.names = NULL)
bdp$X3 <- gsub("\\%", "", bdp$X3) #nadomesti vsa ujemanja niza z drugim nizom
bdp$X3 <- parse_number(bdp$X3)
names(bdp) <- c("Leto", "Sprememba_BDP")
bdp$`Sprememba_BDP`[5] <- -7.8
bdp$`Sprememba_BDP`[8] <- -2.7
bdp$`Sprememba_BDP`[9] <- -1.1
