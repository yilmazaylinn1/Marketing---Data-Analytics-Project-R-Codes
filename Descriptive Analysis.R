#Desscriptive Analysis
summary(Kart_Bilet_Alim_MEF$Kart_Grubu)
summary(Kart_Bilet_Alim_MEF$Satýþ_Kanalý)
summary(Kart_Bilet_Alim_MEF$Ödeme_Tipi)
summary(Kart_Bilet_Alim_MEF$Satýþ_Dönemi)
summary(Kart_Satis_MEFF$Yaþ)
summary(Kart_Bilet_Alim_MEF$Net_Satýþ_Geliri)
by(Kart_Satis_MEF$Yaþ,Kart_Satis_MEF$Kart_Grubu, summary)
qplot(Yaþ, data = Kart_Satis_MEF, fill = Kart_Grubu)
by(Kart_Satis_MEF$Yaþ,Kart_Satis_MEF$Kanal, summary)
qplot(Yaþ, data = Kart_Satis_MEF, fill = Kanal)
by(Kart_Satis_MEF$Kanal,Kart_Satis_MEF$Kart_Grubu, summary)
qplot(Kanal, data = Kart_Satis_MEF, fill = Kart_Grubu)
by(Kart_Bilet_Alim_MEF$ `Etkinlik Adý`, Kart_Bilet_Alim_MEF $Satýþ_Kanalý, summary)
qplot(`Etkinlik Adý`, data = Kart_Bilet_Alim_MEF, fill = Satýþ_Kanalý)






