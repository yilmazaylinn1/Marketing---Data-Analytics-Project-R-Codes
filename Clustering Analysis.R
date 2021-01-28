#Clustering
Kart_Bilet_Alim_MEF.f <- Kart_Bilet_Alim_MEF
Kart_Bilet_Alim_MEF.s <- scale(Kart_Bilet_Alim_MEF.f[-1]) #Normalizing the matrix

ggplot(Kart_Bilet_Alim_MEF, aes(Birim_Bilet_Fiyatý,Net_Satýþ_Geliri, color = Kart_Grubu))
+ geom_point()

set.seed(50)
Kart_Bilet_Alim_MEFCluster <- kmeans(Kart_Bilet_Alim_MEF[, 6:10], 6, nstart = 50)
Kart_Bilet_Alim_MEFCluster
table(Kart_Bilet_Alim_MEFCluster$cluster, Kart_Bilet_Alim_MEF$Kart_Grubu)


ggplot(data, aes(x = Cluster, fill = Kart_Grubu)) +
  theme_bw() +
  geom_bar() +
  labs(x = "# of Cluster",
       y = "# of Customer",
       title = "Kart Grubu")

ggplot(data, aes(x = Cluster, fill = Odeme_Tipi)) +
  theme_bw() +
  geom_bar() +
  labs(x = "# of Cluster",
       y = "# of Customer",
       title = "Ödeme Tipi")

ggplot(data, aes(x = Cluster, fill = Satis_Kanali)) +
  theme_bw() +
  geom_bar() +
  labs(x = "# of Cluster",
       y = "# of Customer",
       title = "Satýþ Kanalý")

ggplot(data, aes(x = Cluster, fill = Etkinlik_Adi)) +
  theme_bw() +
  geom_bar() +
  labs(x = "# of Cluster",
       y = "# of Customer",
       title = "Etkinlik Adý")

set.seed(20)
Cluster <- kmeans(Kart_Bilet_Alim_MEF.f, 3, nstart = 20)
Cluster
table(Cluster$cluster,Kart_Bilet_Alim_MEF$Ödeme_Tipi)

ggplot(Kart_Bilet_Alim_MEF, aes(Birim_Bilet_Fiyatý, Net_Satýþ_Geliri, color =
                                  kCluster$cluster)) + geom_point()



