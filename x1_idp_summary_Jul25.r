#use the refugees R package to summarize Internally displaced peoples % out of all populations protected by UNHCR


rm(list = ls())
library(refugees)
library(dplyr)

year = "2024"

dat = refugees::population
dat = dat[which(dat$year == year),]
dat$IDP = rowSums(dat[,11:12], na.rm = TRUE)
dat$Total = rowSums(dat[,8:15], na.rm = TRUE)
print(sum(dat$Total) / 1000000) #All populations in M:

sum_tab = 
  dat |>
  summarise(all_pop = sum(Total, na.rm = TRUE), .by = coo_name)
sum_tab2 = 
  dat |>
  summarise(idp_pop = sum(IDP, na.rm = TRUE), .by = coo_name)
all(sum_tab$coo_name == sum_tab2$coo_name)
sum_tab$idp_pop = sum_tab2$idp_pop
sum_tab$idp_percent = round((sum_tab$idp_pop / sum_tab$all_pop) * 100, 2)
sum_tab = sum_tab |> arrange(desc(all_pop))
rm(sum_tab2)

print(round((sum(sum_tab$all_pop[1:5]) / sum(dat$Total)) * 100, 2)) #All pop % of top 5 countries:
print(round((sum(sum_tab$idp_pop) / sum(sum_tab$all_pop)) * 100, 2)) #Overall IDP %:


sum_tab

