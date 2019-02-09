# 4. faza: Analiza podatkov

#Napoved prihodkov od prodaje za panogo z največjimi prihodki za naslenjih 6 let
najboljsa <- melt(prva, Panoge.zap., id.vars =c("Panoga", "Leto"), 
                  measure.vars =  c("Prihodki_od_prodaje_v_tisoč_EUR", "Bruto_poslovni_presežek_v_tisoč_EUR", "Število_zaposlenih"),
                  variable.name = "Spremenljivka", value.name = "Število")
data1 <- subset(najboljsa, Spremenljivka == "Prihodki_od_prodaje_v_tisoč_EUR")

fit <- lm(data = data1, Število ~ Leto)
l <- data.frame(Leto=seq(2005, 2023))
predict(fit, l)
napoved <- l %>% mutate(Število=predict(fit, .))

lin1 <- ggplot(data1, aes(x=Leto,y=Število)) +
  geom_line() +
  geom_point(data=napoved, aes(x=Leto, y=Število), color="blue", size=3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Leto") + ylab("Prihodki od prodaje v tisoč EUR") 

#za shiny

Napoved <- function(data){
  fit <- lm(data = data, Število ~ Leto)
  l <- data.frame(Leto=seq(2005, 2023))
  predict(fit, l)
  napoved <- l %>% mutate(Število=predict(fit, .))
  
  return(napoved)
}




