#Market Basket Analysis
#In this section we called transaction csv data set
transactions_1 <- str_split_fixed(Kart_Bilet_MEF$Etkinlik_Adi, ",", n = Inf)
write.csv(transactions_1, file = "transactions.csv", row.names = F)
Kart_Bilet_MEF <- read.transactions("transactions.csv", format = "basket", sep = ",",
skip=0)

#In this section we analyzed the dimension of data.

dim(Kart_Bilet_MEF)
cat("Number of baskets:", length(Kart_Bilet_MEF))
cat("Number of unique items:", sum(size(Kart_Bilet_MEF)))

#In this section, we looked at the top 10 observations in the dataset.

ItemSetList <- Kart_Bilet_MEF@itemInfo
ItemSetList
inspect(Kart_Bilet_MEF[1:10])

#In this section, the event preference comment can be made by looking at the
#frequencies of the transactions.

itemFrequency(Kart_Bilet_MEF, type = "relative")

#Items Frequency plot 

itemFrequencyPlot(Kart_Bilet_MEF, topN=9, type="relative", main="Items Frequency",
                  cex.names=0.8)
head(sort(itemFrequency(Kart_Bilet_MEF), decreasing=FALSE), n=15)

#Number of Items in Particular Baskets

hist(size(Kart_Bilet_MEF), breaks = 0:10, xaxt="n", ylim=c(0,30),
main = "Number of items in particular baskets", xlab = "Items")
axis(1, at=seq(0,40,by=5), cex.axis=0.8)
cat("The biggest basket consists of", ncol(transactions_1), "products.")

rules <- apriori(Kart_Bilet_MEF, parameter = list(supp = 0.01, conf = 0.25))

options(digits=2)
inspect(rules[1:4])
rules<-sort(rules, by="confidence", decreasing=TRUE)
rules
subset.rules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.rules. <- rules[-subset.rules]
subset.matrix <- is.subset(rules, rules)
rules<-apriori(data=Kart_Bilet_MEF, parameter=list(supp=0.001,conf = 0.08),
               appearance = list(default="lhs",rhs="A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:4])
lhs
rules<-apriori(data=Kart_Bilet_MEF, parameter=list(supp=0.001,conf = 0.15,minlen=2),
appearance = list(default="rhs",lhs="A"),
control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:4])
lhs

library(arulesViz)
plot(rules,method="graph",enginer="interactive",shading=NA) 
               
              
               
