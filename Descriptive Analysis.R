#Desscriptive Analysis
summary(Kart_Bilet_Alim_MEF$Kart_Grubu)
summary(Kart_Bilet_Alim_MEF$Sat��_Kanal�)
summary(Kart_Bilet_Alim_MEF$�deme_Tipi)
summary(Kart_Bilet_Alim_MEF$Sat��_D�nemi)
summary(Kart_Satis_MEFF$Ya�)
summary(Kart_Bilet_Alim_MEF$Net_Sat��_Geliri)
by(Kart_Satis_MEF$Ya�,Kart_Satis_MEF$Kart_Grubu, summary)
qplot(Ya�, data = Kart_Satis_MEF, fill = Kart_Grubu)
by(Kart_Satis_MEF$Ya�,Kart_Satis_MEF$Kanal, summary)
qplot(Ya�, data = Kart_Satis_MEF, fill = Kanal)
by(Kart_Satis_MEF$Kanal,Kart_Satis_MEF$Kart_Grubu, summary)
qplot(Kanal, data = Kart_Satis_MEF, fill = Kart_Grubu)
by(Kart_Bilet_Alim_MEF$ `Etkinlik Ad�`, Kart_Bilet_Alim_MEF $Sat��_Kanal�, summary)
qplot(`Etkinlik Ad�`, data = Kart_Bilet_Alim_MEF, fill = Sat��_Kanal�)






