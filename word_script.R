loc = "~/Documents/Current/AngelEye/full_data/perceptions.xlsx"
dat = readxl::read_excel(loc)
dat = dat[-410,]


dat$Responses = stringr::str_replace_all(dat$Responses,"\r","")
dat$Responses = stringr::str_replace_all(dat$Responses,"\n","")

## transform tags into columns
l = list()
for(i in 1:nrow(dat)){
  #dat[i,]$Responses = stringr::str_replace()
  vals = stringr::str_split(string = dat[i,]$Tags, pattern = ", ")[[1]]
  l[[i]] = ifelse(is.na(vals),"None",vals)
}

all_tags = unique(unlist(l))
tag_df = data.frame(matrix(NA,nrow=nrow(dat),ncol=length(all_tags)))
colnames(tag_df) = all_tags

for(i in 1:nrow(dat)){
  vals = stringr::str_split(string = dat[i,]$Tags, pattern = ", ")[[1]]
  yes_ind = which(colnames(tag_df) %in% vals)
  row_vals = rep(0,length(all_tags))
  row_vals[yes_ind] = 1
  tag_df[i,] = row_vals
}

dat = cbind(dat,tag_df)

## remove any NAs
#dat = dat[complete.cases(dat),]

## load in full data
ae = readr::read_csv("~/Documents/Current/AngelEye/full_data/angeleye_survey_data_0205.csv")
ae = ae[,-1]
ae = ae[!is.na(ae$perceived_impact_NICU_webcam_use),]

## reorder dat
match_inds = match(stringr::str_replace_all(dat$Responses," ",""),stringr::str_replace_all(ae$perceived_impact_NICU_webcam_use," ",""))
weird_vals = which(abs(match(stringr::str_replace_all(dat$Responses," ",""),stringr::str_replace_all(ae$perceived_impact_NICU_webcam_use," ","")) - 1:483) > 2)
match_inds[weird_vals] = weird_vals
adj_dat = dat[match_inds,]

full_ae = cbind(ae,adj_dat[,4:27])
write.csv(full_ae, "~/Downloads/angeleye_survey_data_with_words_0323.csv",row.names = FALSE)
