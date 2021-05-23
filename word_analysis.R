aef = readr::read_csv("~/Documents/Current/AngelEye/full_data/angeleye_survey_data_with_words_0323.csv")

require(knitr) 
require(kableExtra)
require(dplyr)
require(tidyr)
require(ggplot2)

# aef$current_role = factor(aef$current_role)
# levels(aef$current_role) =  c("Frontline Bedside Nurse",
#                                     "Supervisor/Charge Nurse",
#                                     "Nurse Educator",
#                                     "Nursing Management (Assistant Nurse Manager, Nurse Manager)",
#                                     "Nursing Administration (Director, CNO, etc.)")
# 
# aef$age_range = factor(aef$age_range,
#                          levels = c("20-30",
#                                     "31-40",
#                                     "41-50",
#                                     "51-60",
#                                     ">60"))
# 
# aef$total_nursing_experience = factor(aef$total_nursing_experience)
# levels(aef$total_nursing_experience) = c("<1 yr",
#                                         "1-2 years",
#                                               "3-6 years",
#                                               "7-10 years",
#                                               "11-15 years",
#                                               "16-20 years",
#                                               "20+ years")
# 
# aef$nicu_nursing_experience = factor(aef$nicu_nursing_experience)
# levels(aef$nicu_nursing_experience) = c("<1 yr",
#                                        "1-2 years",
#                                        "3-6 years",
#                                        "7-10 years",
#                                        "11-15 years",
#                                        "16-20 years",
#                                        "20+ years")
# 
# aef$safety_security_webcams_NICU = factor(aef$safety_security_webcams_NICU,
#                                          levels = c("Not at all",
#                                                     "Not Much",
#                                                     "Neutral",
#                                                     "Somewhat",
#                                                     "Very Much",
#                                                     "I do not have experience working with webcams in NICU"))
# 
# aef$facility_clear_expectations = factor(aef$facility_clear_expectations,
#                                         levels = c("Not at all",
#                                                    "Not Much",
#                                                    "Neutral",
#                                                    "Somewhat",
#                                                    "Very Much",
#                                                    "I do not have experience working with webcams in NICU"))
# 
# aef$camera_other_binary = ifelse(!is.na(aef$camera_other),1,0)
# 
# aef[aef$camera_none == 1 &
#      (aef$camera_angeleye == 1 |
#         aef$camera_nicview == 1 |
#         aef$camera_other_binary == 1 |
#         aef$camera_tablet_smart == 1 |
#         aef$camera_internal == 1),]$camera_none = 0
# 
# aef$all_cameras = NA
# aef$all_cameras = ifelse(aef$camera_angeleye == 1 & aef$camera_nicview == 1, "AngelEye and NicView",NA)
# aef[is.na(aef$all_cameras),]$all_cameras = ifelse(aef[is.na(aef$all_cameras),]$camera_angeleye == 1, "AngelEye", NA)
# aef[is.na(aef$all_cameras),]$all_cameras = ifelse(aef[is.na(aef$all_cameras),]$camera_nicview == 1, "NicView", NA)
# aef[is.na(aef$all_cameras),]$all_cameras = ifelse(aef[is.na(aef$all_cameras),]$camera_internal == 1 |
#                                                   aef[is.na(aef$all_cameras),]$camera_tablet_smart == 1 |
#                                                   aef[is.na(aef$all_cameras),]$camera_other_binary == 1, "Other", NA)
# aef[is.na(aef$all_cameras),]$all_cameras = "I do not have experience working with webcams in NICU"
# aef$all_cameras = factor(aef$all_cameras,
#                         levels = c("AngelEye",
#                                    "NicView",
#                                    "AngelEye and NicView",
#                                    "Other",
#                                    "I do not have experience working with webcams in NICU"))
# 
# ## CONVERTING OUTCOMES
# conv = c("Neutral" = 3,
#          "Not at all" = 1,
#          "Not Much" = 2,
#          "Somewhat" = 4,
#          "Very Much" = 5,
#          "I do not have experience working with webcams in NICU"=NA)
# aef$safety_num = conv[aef$safety_security_webcams_NICU]
# aef$expect_num = conv[aef$facility_clear_expectations]
# 
# conv1 = c("Neutral"=3,
#           "Parents visit much less"=1,
#           "Parents visit slightly less"=2,
#           "Parents visit slightly more"=4,
#           "Parents visit much more"=5,
#           "I do not have experience working with webcams in NICU"=NA)
# aef$nicu_time_num = conv1[aef$time_in_NICU_webcam_use]
# 
# conv2 = c("Neutral"=3,
#           "Parents call much less"=5,
#           "Parents call slightly less"=4,
#           "Parents call slightly more"=2,
#           "Parents call much more"=1,
#           "I do not have experience working with webcams in NICU"=NA)
# aef$nicu_call_num = conv2[aef$calls_to_NICU_webcam_use]
# 
# conv3 = c("Somewhat Increased"=2,
#           "No Impact"=3,
#           "Increased Very Much"=1,
#           "Somewhat Decreased"=4,
#           "Decreased Very Much"=5,
#           "I do not have experience working with webcams in NICU"=NA)
# aef$stress_num = conv3[aef$stress_levels_webcam_use]
# 
# conv4 = c("Very likely"=5,
#           "Likely"=4,
#           "Neither likely nor unlikely"=3,
#           "Unlikely"=2,
#           "Very unlikely"=1)
# aef$rec_num = conv4[aef$recommend_NICU_webcams]

get_vals1 = function(dat=aef,
                    group,
                    column,
                    order=TRUE){
  ret = dat %>% 
    group_by(get(group)) %>% 
    summarise("perc"=mean(get(column),na.rm = T),
              #"sd"=sd(get(column),na.rm=T),
              "N"=(length(get(column))-sum(is.na(get(column)))),
              #"n_miss"=sum(is.na(get(column))),
              "N_yes"=sum(get(column)),
              .groups="drop")
  ret$feature = group
  ret = ret[,c("feature","get(group)","perc","N","N_yes")]
  colnames(ret)[2] = 'group'
  
  if(order){
    return(ret[order(ret$N,decreasing=T),])
  }
  return(ret)
}

get_table1 = function(dat=aef,
                     outcome){
  df = data.table::rbindlist(list(
    data.frame(get_vals1(dat,"age_range",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"current_role",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"hospital_region",outcome)),
    data.frame(get_vals1(dat,"total_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"nicu_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"nicu_level",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"experience_with_camera",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"facility_clear_expectations",outcome,order=FALSE)),
    data.frame(get_vals1(dat,"all_cameras",outcome,order=FALSE))
  ))
  
  df$"Tags (Total)" = paste0(df$N_yes, " (",
                          df$N, ")")
  df$"Percent" = paste0(round(df$perc,2)*100,"%")
  return(df)
}

# pre_plot_dat = get_table1(dat=aef,outcome="Negative Staff")
# pre_plot_dat$outcome = "Negative Staff"
# for(out in colnames(aef)[42:62]){
#   tab =get_table1(dat=aef,outcome=out)
#   tab$outcome = out
#   pre_plot_dat = rbind(pre_plot_dat,tab)
# }
# 
# pre_plot_dat = as.data.frame(pre_plot_dat)
# plot_dat = pre_plot_dat[,c("feature",'group','perc','N','outcome')]
# 
# plot_dat = plot_dat[plot_dat$feature == "age_range",]
# ggplot(plot_dat)+
#   geom_point(aes(x=group,y=perc,size=N,color=outcome)) +
#   facet_wrap(~outcome) +
#   theme(legend.position = "None")

### --- table of tags

table_dat = get_table1(dat=aef,outcome="Negative Staff")[,c(2,6,7)]
for(out in colnames(aef)[42:62]){
  table_dat = cbind(table_dat,get_table1(dat=aef,outcome=out)[,c(6,7)])
}


colnames(table_dat)[1] = "Demographics"
kable(table_dat) %>% 
  kable_styling(full_width = FALSE) %>%
  pack_rows("Age",1,5) %>%
  pack_rows("Current Role",6,10) %>%
  pack_rows("Hospital Region",11,15) %>%
  pack_rows("Total Nursing Experience",16,22) %>%
  pack_rows("NICU Nursing Experience",23,29) %>%
  pack_rows("NICU Level",30,33) %>%
  pack_rows("Experience with cameras",34,37) %>%
  pack_rows("Facility expectations",38,43) %>%
  pack_rows("Webcam system",44,48) %>%
  column_spec(1,bold=T,border_right = T) %>%
  column_spec(c(c(3:45)[which(3:45 %% 2 == 1)]),border_right = T) %>%
  add_header_above(c(" "=1,
                     "Negative Staff"=2,
                     "Positive Family"=2,
                     "Hardware"=2,
                     "Mixed Family"=2,
                     "Peace of Mind"=2,
                     "Excessive Calls"=2,
                     "Bonding"=2,
                     "Ease Anxiety"=2,
                     "Loved ones"=2,
                     "Infection Control"=2,
                     "Pumping/Milk"=2,
                     "COVID"=2,
                     "Inc Anxiety/Stress"=2,
                     "Inappropriate View"=2,
                     "HIPAA/Security"=2,
                     "Negative Family"=2,
                     "Access"=2,
                     "Dec Visitation"=2,
                     "Cam On/Off"=2,
                     "Guidelines/Expect"=2,
                     "Mixed Staff"=2,
                     "Positive Staff"=2))

## sentiment
library(SentimentAnalysis)
senti = analyzeSentiment(aef$perceived_impact_NICU_webcam_use)
aef$sentiment1 = senti$SentimentQDAP
aef$sentiment2 = senti$SentimentHE
aef$sentiment3 = senti$SentimentLM

get_vals2 = function(dat=aef,
                     group,
                     column,
                     order=TRUE){
  ret = dat %>% 
    group_by(get(group)) %>% 
    summarise("mean"=mean(get(column),na.rm = T),
              "sd"=sd(get(column),na.rm=T),
              "N"=(length(get(column))-sum(is.na(get(column)))),
              "n_miss"=sum(is.na(get(column))),
              .groups="drop")
  ret$feature = group
  ret = ret[,c("feature","get(group)","mean","sd","N","n_miss")]
  colnames(ret)[2] = 'group'
  
  if(order){
    return(ret[order(ret$N,decreasing=T),])
  }
  return(ret)
}

get_table2 = function(dat=aef,
                      outcome){
  df = data.table::rbindlist(list(
    data.frame(get_vals2(dat,"age_range",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"current_role",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"hospital_region",outcome)),
    data.frame(get_vals2(dat,"total_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"nicu_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"nicu_level",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"experience_with_camera",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"facility_clear_expectations",outcome,order=FALSE)),
    data.frame(get_vals2(dat,"all_cameras",outcome,order=FALSE))
  ))
  
  df$"Mean (SD)" = paste0(round(df$mean,2), " (",
                          round(df$sd,2), ")")
  df$"N (Missing)" = paste0(df$N, " (",
                            df$n_miss, ")")
  return(df)
}

tab2 = get_table2(outcome="sentiment1")[,c(2,7,8)]
tab2 = cbind(tab2,get_table2(outcome="sentiment2")[,c(7,8)])
tab2 = cbind(tab2,get_table2(outcome="sentiment3")[,c(7,8)])

kable(tab2) %>% 
  kable_styling(full_width = FALSE) %>%
  pack_rows("Age",1,5) %>%
  pack_rows("Current Role",6,10) %>%
  pack_rows("Hospital Region",11,15) %>%
  pack_rows("Total Nursing Experience",16,22) %>%
  pack_rows("NICU Nursing Experience",23,29) %>%
  pack_rows("NICU Level",30,33) %>%
  pack_rows("Experience with cameras",34,37) %>%
  pack_rows("Facility expectations",38,43) %>%
  pack_rows("Webcam system",44,48) %>%
  column_spec(1,bold=T,border_right = T) %>%
  column_spec(3,border_right = T) %>%
  add_header_above(c(" "=1,
                     "Sentiment QDAP"=2,
                     "Sentiment HE"=2,
                     "Sentiment LM"=2
                     ))

## bar chart of tags

vals = colSums(aef[,c(41:62)])
bdat = data.frame(vals)
bdat$tag = rownames(bdat)
ggplot(bdat)+
  geom_bar(aes(x=reorder(tag,-vals),y=vals,fill=tag),stat="identity")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "None") +
  scale_x_discrete("Tag") +
  scale_y_continuous("Number of Tags")

## word cloud

install.packages("wordcloud")
library(wordcloud)

all_words = stringr::str_replace_all(aef$perceived_impact_NICU_webcam_use,"\\.","")
all_words = stringr::str_replace_all(all_words,"\\,","")
all_words = stringr::str_replace_all(all_words,"\\(","")
all_words = stringr::str_replace_all(all_words,"\\)","")
all_words = stringr::str_replace_all(all_words,"\\:","")
all_words = stringr::str_replace_all(all_words,"\\;","")
all_words = stringr::str_replace_all(all_words,"\\?","")
all_words = stringr::str_replace_all(all_words,"\\-","")
all_words = stringr::str_replace_all(all_words,"\\--","")
all_words = stringr::str_replace_all(all_words,"\\…","")
all_words = stringr::str_replace_all(all_words,'\\"',"")
all_words = stringr::str_replace_all(all_words,'\\“',"")
all_words = stringr::str_replace_all(all_words,'\\”',"")

all_words = stringr::str_split(all_words," ")
full_list = unlist(all_words)
freqs = table(full_list)

words = data.frame(freqs)
words = words[words$Freq > 1,]

##require(stopwords)
all_stops = c(stopwords(source="snowball"),
              stopwords(source="stopwords-iso"),
              stopwords(source="smart"),
              stopwords(source="marimo"),
              stopwords(source="nltk")
              )
all_stops = unique(all_stops)

words$full_list = stringr::str_to_lower(words$full_list)
words = words[!(words$full_list %in% all_stops),]
words = words[order(words$Freq,decreasing = T),]
plot_words = head(words,100)

set.seed(117);wordcloud(words=plot_words$full_list,freq=plot_words$Freq,colors = (pals::ocean.curl(100)))

paletteer::paletteer_dynamic(palette = `pals::ocean.haline`,n=5)
