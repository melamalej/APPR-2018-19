# 4. faza: Analiza podatkov
require(ggdendro)

#Napoved prihodkov od prodaje za panogo z največjimi prihodki za naslenjih 6 let
najboljsa <- melt(prva, Panoge.zap., id.vars =c("Panoga", "Leto"), 
                  measure.vars =  c("Prihodki_od_prodaje_v_tisoc_EUR", "Bruto_poslovni_presezek_v_tisoc_EUR", "Stevilo_zaposlenih"),
                  variable.name = "Spremenljivka", value.name = "Stevilo")
data1 <- subset(najboljsa, Spremenljivka == "Prihodki_od_prodaje_v_tisoc_EUR")

fit <- lm(data = data1, Stevilo ~ Leto)
l <- data.frame(Leto=seq(2005, 2023))
predict(fit, l)
napoved <- l %>% mutate(Stevilo=predict(fit, .))

lin1 <- ggplot(data1, aes(x=Leto,y=Stevilo/1e6)) +
  geom_line() +
  geom_point(data=napoved, aes(x=Leto, y=Stevilo/1e6), color="blue", size=3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Leto") + ylab("Prihodki od prodaje v milijardah EUR") 

#za shiny

Napoved <- function(data){
  fit <- lm(data = data, Stevilo ~ Leto)
  l <- data.frame(Leto=seq(2005, 2023))
  predict(fit, l)
  napoved <- l %>% mutate(Stevilo=predict(fit, .))
  
  return(napoved)
}

#razvrscanje
podatki <- Regije1 %>% filter(Leto==2016) %>% select(Regija, Stevilo_oseb)
regija <- podatki$Regija
row.names(podatki) <- podatki$Regija
podatki$Regija <- NULL 

#prilagoditev
lestvica <- scale(podatki)
#matrika različnosti (class: dist)
matrika <- dist(lestvica)
#hierarhično razvrščanje v 3 skupine
n <- 3
skupina <- hclust(matrika) 
#izris
ggdendrogram(skupina, rotate = FALSE, size = 2)
#dolocimo razvrstitev skupina
skupina <- hclust(matrika) %>% cutree(n)

skupine <- data.frame(regija, skupina)
skupine <- remove_rownames(skupine)

#katere regije so v skupini 2
skupine %>% filter(skupina==2) %>% select(regija)

zemljevid3 <- ggplot(left_join(zemljevid, skupine, by = c("NAME_1"='regija'))) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = skupina)) +
  labs(fill="Lestvica")  +
  ggtitle("Razvrščanje glede na število oseb, ki delajo") 
 
