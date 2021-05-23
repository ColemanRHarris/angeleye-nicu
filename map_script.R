zips = readxl::read_xlsx('~/Downloads/zips.xlsx')
zips = readr::read_csv("~/Downloads/zips.csv")

zip_counts = zips %>%
  count(Responses)

colnames(zip_counts) = c('region','value')
zip_counts$region[(zip_counts$region %in% usa::zipcodes$zip)]
zr = usa::zipcodes[usa::zipcodes$zip %in% zip_counts$region,]
zr$value = zip_counts[match(zr$zip,zip_counts$region),]$value

zr1 = zr %>%
  group_by(state) %>%
  summarise(sum(value))

zr2 = zr1

colnames(zr2) = c('region','value')
zr2$region = state.name[match(zr2$region,state.abb)]

zr2$value = as.numeric(zr2$value)
zr2 = zr2[!is.na(zr2$region),]
zr2$region = tolower(zr2$region)

zr2[zr2$region == "indiana",'value'] = zr2[zr2$region == "indiana",'value'] +1
zr2[zr2$region == "massachusetts",'value'] = zr2[zr2$region == "massachusetts",'value'] +1
zr2[zr2$region == "montana",'value'] = zr2[zr2$region == "montana",'value'] +1

state_choropleth(zr2,num_colors = 5)

### --- deprecated  vvvvvvv ------

##zips[unlist(lapply(zips$Responses,nchar)) != 5,]$Tags <- TRUE

norm_zips = zips[is.na(zips$Tags),]
out_zips = zips[!is.na(zips$Tags),]

require(tidyr)
require(dplyr)



library("choroplethrZip")
library(choroplethr)
data("zip.regions")

zip.regions$value = 0

for(i in 1:nrow(zip_counts)){
  row_ids = zip.regions$region == unname(unlist(zip_counts[i,'Responses']))
  row_bools = any(row_ids)
  if(row_bools){
    zip.regions[row_ids,]$value = zip_counts[i,]$n
  }
  if(!row_bools){
    print(unlist(unname(zip_counts[i,])))
  }
}

zr = zip.regions[!duplicated(zip.regions$region),]
zip_choropleth(zr,num_colors = 6)

zr1 = zr %>%
  group_by(state.name) %>%
  summarise(sum(value))
z
zr2 = read.csv("~/Downloads/zr1.csv")
colnames(zr2) = c('region','value') 
state_choropleth(zr2,
                 num_colors=5
                 )
