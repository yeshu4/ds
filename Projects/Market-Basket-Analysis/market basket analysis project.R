library(arules)
library(arulesViz)

market_basket<-read.transactions(
  
  file='market_basket.csv',
  sep=',',
  quote="",
  format = 'basket',
  rm.duplicates = TRUE,
  skip=1
)

summary(market_basket)

#Compute the number of items that were purchased : 
 18441 * 22347 * 0.0009914608
 
 
 market_basket %>% head(n = 5) %>% inspect
 
 
 itemFrequencyPlot(x     = market_basket,
                   topN  = 10,
                   # support =
                   type  = 'absolute',
                   horiz = TRUE,
                   col=brewer.pal(10,'Spectral')
                                      )
 
#------------------------
 rule1 <- market_basket %>%
   apriori(parameter = list(supp = 0.005, conf = 0.8)) %>%
   sort(by = 'confidence')
 
 
 summary(rule1)
 
 rule1 %>% head(n = 5) %>% inspect
 
 rule1 %>% tail(n = 5) %>% inspect
 
rule1 <- rule1 %>% sort(by = "lift") 

rule1 %>%  head(n = 5) %>% inspect


#plotting

plot(rule1,engine = "htmlwidget")
plot(rule1, method = "two-key",engine = "htmlwidget")
plot(rule1, method = "graph",engine = "htmlwidget")

#--------------------------------

rule2 <- market_basket %>%
  apriori(parameter = list(supp = 0.009, conf = 0.3)) %>%
  sort(by = 'confidence')


summary(rule2)

rule2 %>% head(n = 5) %>% inspect

rule2 %>% tail(n = 5) %>% inspect

#plotting
plot(rule2,engine = "htmlwidget")
plot(rule2, method = "two-key",engine = "htmlwidget")
plot(rule2, method = "graph",engine = "htmlwidget")

#--------------------------------

rule3 <- market_basket %>%
  apriori(parameter = list(supp = 0.02, conf = 0.5)) %>%
  sort(by = 'support')


summary(rule3)

rule3 %>% head(n = 5) %>% inspect

rule3 %>% tail(n = 5) %>% inspect

#plotting
plot(rule3,engine = "htmlwidget")
plot(rule3, method = "two-key",engine = "htmlwidget")
plot(rule3, method = "graph",engine = "htmlwidget")

