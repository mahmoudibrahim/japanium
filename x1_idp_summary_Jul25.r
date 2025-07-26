#use the refugees R package to summarize Internally displaced peoples % out of all populations protected by UNHCR (part 1)
#use the refugees R package to summarize refugees and asylum seekers top receiver countries according to UNHCR (part 2)


rm(list = ls())
library(refugees)
library(dplyr)

year = "2024"
nn = 10
###


dat = refugees::population
dat = dat[which(dat$year == year),]
dat$IDP = rowSums(dat[,11:12], na.rm = TRUE)
dat$REF = rowSums(dat[,8:10], na.rm = TRUE)
dat$Total = rowSums(dat[,8:15], na.rm = TRUE)
print(sum(dat$Total) / 1000000) #All populations in M:


sum_tab = 
  dat |>
  summarise(all_pop = sum(Total, na.rm = TRUE), .by = coo_name)
sum_tab2 = 
  dat |>
  summarise(idp_pop = sum(IDP, na.rm = TRUE), .by = coo_name)
sum_tab3 = 
  dat |>
  summarise(ref_pop = sum(REF, na.rm = TRUE), .by = coo_name)

all(sum_tab$coo_name == sum_tab2$coo_name)
sum_tab$idp_pop = sum_tab2$idp_pop
sum_tab$idp_percent = round((sum_tab$idp_pop / sum_tab$all_pop) * 100, 2)
sum_tab = sum_tab |> arrange(desc(all_pop))


all(sum_tab$coo_name == sum_tab3$coo_name)
sum_tab$ref_pop = sum_tab3$ref_pop
sum_tab$ref_percent = round((sum_tab$ref_pop / sum_tab$all_pop) * 100, 2)
sum_tab = sum_tab |> arrange(desc(all_pop))



print(round((sum(sum_tab$all_pop[1:nn]) / sum(dat$Total)) * 100, 2)) #All pop % of top 5 countries:
print(round((sum(sum_tab$idp_pop) / sum(sum_tab$all_pop)) * 100, 2)) #Overall IDP %:
sum_tab

####



REF_list = list()
for (i in 1:nn) {
  
  dat_temp = dat[dat$coo_name == sum_tab$coo_name[i],]
  dat_temp = dat_temp |> arrange(desc(REF))
  dat_temp$ref_percent = round((dat_temp$REF / sum(dat_temp$Total)) * 100, 2)
  REF_list[[i]] = dat_temp[which(dat_temp$ref_percent >= 1), c(2,5,18,20)]
  
}
REF_list


