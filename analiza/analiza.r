# 4. faza: Analiza podatkov

#Napoved prihodkov od prodaje za panogo z največjimi prihodki za naslenjih 6 let
najboljsa <- prva

fit <- lm(data = najboljsa, Prihodki_od_prodaje_v_tisoč_EUR ~ Leto)
l <- data.frame(Leto=seq(2005, 2023))
predict(fit, l)
napoved <- l %>% mutate(Prihodki_od_prodaje_v_tisoč_EUR=predict(fit, .))

lin1 <- ggplot(najboljsa, aes(x=Leto,y=Prihodki_od_prodaje_v_tisoč_EUR)) +
  geom_line() +
  geom_point(data=napoved, aes(x=Leto, y=Prihodki_od_prodaje_v_tisoč_EUR), color="blue", size=3) + 
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Leto") + ylab("Prihodki od prodaje v tisoč EUR") 




