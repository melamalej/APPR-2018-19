# 4. faza: Analiza podatkov

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




