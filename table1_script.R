#ae = readr::read_csv("~/Documents/Current/AngelEye/full_data/angeleye_survey_data_0204.csv")
ae = readr::read_csv("~/Documents/Current/AngelEye/full_data/angeleye_survey_data_0205.csv")
ae = ae[,-1]


require(knitr) 
require(kableExtra)
require(dplyr)
require(tidyr)

# ae$current_role = factor(ae$current_role)
# levels(ae$current_role) =  c("Frontline Bedside Nurse",
#                                     "Supervisor/Charge Nurse",
#                                     "Nurse Educator",
#                                     "Nursing Management (Assistant Nurse Manager, Nurse Manager)",
#                                     "Nursing Administration (Director, CNO, etc.)")
# 
# ae$age_range = factor(ae$age_range,
#                          levels = c("20-30",
#                                     "31-40",
#                                     "41-50",
#                                     "51-60",
#                                     ">60"))
# 
# ae$total_nursing_experience = factor(ae$total_nursing_experience)
# levels(ae$total_nursing_experience) = c("<1 yr",
#                                         "1-2 years",
#                                               "3-6 years",
#                                               "7-10 years",
#                                               "11-15 years",
#                                               "16-20 years",
#                                               "20+ years")
# 
# ae$nicu_nursing_experience = factor(ae$nicu_nursing_experience)
# levels(ae$nicu_nursing_experience) = c("<1 yr",
#                                        "1-2 years",
#                                        "3-6 years",
#                                        "7-10 years",
#                                        "11-15 years",
#                                        "16-20 years",
#                                        "20+ years")
# 
# ae$safety_security_webcams_NICU = factor(ae$safety_security_webcams_NICU,
#                                          levels = c("Not at all",
#                                                     "Not Much",
#                                                     "Neutral",
#                                                     "Somewhat",
#                                                     "Very Much",
#                                                     "I do not have experience working with webcams in NICU"))
# 
# ae$facility_clear_expectations = factor(ae$facility_clear_expectations,
#                                         levels = c("Not at all",
#                                                    "Not Much",
#                                                    "Neutral",
#                                                    "Somewhat",
#                                                    "Very Much",
#                                                    "I do not have experience working with webcams in NICU"))
# 
# ae$camera_other_binary = ifelse(!is.na(ae$camera_other),1,0)
# 
# ae[ae$camera_none == 1 &
#      (ae$camera_angeleye == 1 |
#         ae$camera_nicview == 1 |
#         ae$camera_other_binary == 1 |
#         ae$camera_tablet_smart == 1 |
#         ae$camera_internal == 1),]$camera_none = 0
# 
# ae$all_cameras = NA
# ae$all_cameras = ifelse(ae$camera_angeleye == 1 & ae$camera_nicview == 1, "AngelEye and NicView",NA)
# ae[is.na(ae$all_cameras),]$all_cameras = ifelse(ae[is.na(ae$all_cameras),]$camera_angeleye == 1, "AngelEye", NA)
# ae[is.na(ae$all_cameras),]$all_cameras = ifelse(ae[is.na(ae$all_cameras),]$camera_nicview == 1, "NicView", NA)
# ae[is.na(ae$all_cameras),]$all_cameras = ifelse(ae[is.na(ae$all_cameras),]$camera_internal == 1 |
#                                                   ae[is.na(ae$all_cameras),]$camera_tablet_smart == 1 |
#                                                   ae[is.na(ae$all_cameras),]$camera_other_binary == 1, "Other", NA)
# ae[is.na(ae$all_cameras),]$all_cameras = "I do not have experience working with webcams in NICU"
# ae$all_cameras = factor(ae$all_cameras,
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
# ae$safety_num = conv[ae$safety_security_webcams_NICU]
# ae$expect_num = conv[ae$facility_clear_expectations]
# 
# conv1 = c("Neutral"=3,
#           "Parents visit much less"=1,
#           "Parents visit slightly less"=2,
#           "Parents visit slightly more"=4,
#           "Parents visit much more"=5,
#           "I do not have experience working with webcams in NICU"=NA)
# ae$nicu_time_num = conv1[ae$time_in_NICU_webcam_use]
# 
# conv2 = c("Neutral"=3,
#           "Parents call much less"=5,
#           "Parents call slightly less"=4,
#           "Parents call slightly more"=2,
#           "Parents call much more"=1,
#           "I do not have experience working with webcams in NICU"=NA)
# ae$nicu_call_num = conv2[ae$calls_to_NICU_webcam_use]
# 
# conv3 = c("Somewhat Increased"=2,
#           "No Impact"=3,
#           "Increased Very Much"=1,
#           "Somewhat Decreased"=4,
#           "Decreased Very Much"=5,
#           "I do not have experience working with webcams in NICU"=NA)
# ae$stress_num = conv3[ae$stress_levels_webcam_use]
# 
# conv4 = c("Very likely"=5,
#           "Likely"=4,
#           "Neither likely nor unlikely"=3,
#           "Unlikely"=2,
#           "Very unlikely"=1)
# ae$rec_num = conv4[ae$recommend_NICU_webcams]

get_vals = function(dat=ae,
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
  
  if(order){
    return(ret[order(ret$N,decreasing=T),])
  }
  return(ret)
}

get_table = function(dat=ae,
                     outcome){
  df = data.table::rbindlist(list(
    data.frame(get_vals(dat,"age_range",outcome,order=FALSE)),
    data.frame(get_vals(dat,"current_role",outcome,order=FALSE)),
    data.frame(get_vals(dat,"hospital_region",outcome)),
    data.frame(get_vals(dat,"total_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals(dat,"nicu_nursing_experience",outcome,order=FALSE)),
    data.frame(get_vals(dat,"nicu_level",outcome,order=FALSE)),
    data.frame(get_vals(dat,"experience_with_camera",outcome,order=FALSE)),
    data.frame(get_vals(dat,"facility_clear_expectations",outcome,order=FALSE)),
    data.frame(get_vals(dat,"all_cameras",outcome,order=FALSE))
  ))
  
  df$"Mean (SD)" = paste0(round(df$mean,2), " (",
                          round(df$sd,2), ")")
  df$"N (Missing)" = paste0(df$N, " (",
                            df$n_miss, ")")
  return(df)
}

table_dat = get_table(dat=ae,outcome="safety_num")[,c(2,7,8)]
table_dat = cbind(table_dat,get_table(dat=ae,outcome="nicu_time_num")[,c(7,8)])
table_dat = cbind(table_dat,get_table(dat=ae,outcome="nicu_call_num")[,c(7,8)])
#table_dat = cbind(table_dat,get_table(dat=ae,outcome="expect_num")[,c(7,8)])
table_dat = cbind(table_dat,get_table(dat=ae,outcome="stress_num")[,c(7,8)])
table_dat = cbind(table_dat,get_table(dat=ae,outcome="rec_num")[,c(7,8)])

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
  column_spec(c(3,5,7,9,11),border_right = T) %>%
  add_header_above(c(" "=1,
                     "Safety and security"=2,
                     "Time in NICU"=2,
                     "Calls to NICU"=2,
                     #"Clear expectations"=2,
                     "Stress levels"=2,
                     "Recommend NICU webcams"=2))
  

## --- TEXT
ae$explain_safety_security_webcams_NICU
ae$perceived_impact_NICU_webcam_use

####################
##
## ---------- DEPRECATED
##
####################

## add state for ZIPs
# ae[nchar(ae$hospital_zip) == 4,]$hospital_zip = paste0("0",ae[nchar(ae$hospital_zip) == 4,]$hospital_zip)
# zr = usa::zipcodes[!is.na(match(usa::zipcodes$zip,ae$hospital_zip)),]
# ae$hospital_state = NA
# ae$hospital_state = zr[match(ae$hospital_zip,zr$zip),]$state
# ae[is.na(ae$hospital_state),]$hospital_state = c("Other","MT","International","International","IN","International","International","Other","International",
#   "Other","International","International","International","International","International","International","Other")
# 
# ## add region for states
# ae$hospital_region = NA
# ae$hospital_region = usa::state.region[match(ae$hospital_state,usa::state.abb)]
# ae$hospital_region = as.character(ae$hospital_region)
# ae[is.na(ae$hospital_region),]$hospital_region = "Other"


## --- DEMOGRAPHICS

## current role
#table(ae$current_role)
#
### age
#table(ae$age_range)
#
#
#table(ae$hospital_region)
#
### nursing experience
#table(ae$total_nursing_experience)
#table(ae$nicu_nursing_experience)
#
### NICU level
#table(ae$nicu_level)
#ae[ae$nicu_level == "Other (please specify)",]$other_hosp_level

## camera type
#ae$camera_nicview = ifelse(is.na(ae$camera_nicview), 0,1)
#ae$camera_angeleye = ifelse(is.na(ae$camera_angeleye), 0,1)
#ae$camera_internal = ifelse(is.na(ae$camera_internal), 0,1)
#ae$camera_none = ifelse(is.na(ae$camera_none), 0,1)
#ae$camera_tablet_smart = ifelse(is.na(ae$camera_tablet_smart),0,1)

## camera experience
#table(ae$experience_with_camera)
#
#require(table1)

# table1(~ current_role + 
#          age_range + 
#          hospital_region + 
#          total_nursing_experience + 
#          nicu_nursing_experience + 
#          nicu_level +
#          experience_with_camera | safety_security_webcams_NICU, data=ae,
#        overall=F)
# 
# t1 = table1(~ safety_num | age_range,
#        data=ae,
#        transpose = T)



#get_table(dat=ae,outcome="nicu_time_num")[,c(2,7,8)]
## --- OUTCOMES
#table(ae$safety_security_webcams_NICU) ## safety
#table(ae$time_in_NICU_webcam_use) ## time in NICU
#table(ae$calls_to_NICU_webcam_use) ## calls to NICU
#table(ae$stress_levels_webcam_use) ## stress
#table(ae$facility_clear_expectations) ## expectations
#table(ae$recommend_NICU_webcams) ## recommend