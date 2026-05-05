#An analysis of otter counts by year according to NBN Atlas
#Created by Ewan Guscott


library(ggplot2)

setwd("C:/Users/ewang/Documents/animal-occurence-analysis/Eurasian Otters")
otters <- read.csv("otter-occurences.csv")
summary(otters)
otters.reduced <- otters[,c("NBN.Atlas.record.ID","Scientific.name",  "Year", "Individual.count")]
head(otters.reduced)

#Assume that a count of NA just means one count
otters.reduced$Individual.count[is.na(otters.reduced$Individual.count)] <- 1
head(otters.reduced)
#otters.lm <- glm(damage_T_F ~ Year, family = binomial, data = Weevil_damage)


otters.reduced %>%
  group_by(Year) %>%
  summarise(Count = sum(Individual.count)) %>% 
  ggplot(aes(x = Year, y = Count)) + 
  geom_col()

#The graph shows that the occurences of eurasian otters

otters.reduced.m <- glm(Individual.count ~ Year, family = poisson, data = otters.reduced)
summary(otters.reduced.m)

#We can see from the coefficients that year does have an impact on the otter counts. 
#We can then use Chi-Square and degrees of freedom to get p value
# X^2 = 21997 - 21847 = 150
# p = 1 as there is difference of 1 between degrees of freedom 
#When put into this https://www.statology.org/chi-square-p-value-calculator/
#p-value is 0.000000 therefore this model is highly useful, as otter counts have increased significantly