##### Load libraries and fonts #####
library(RODBC)
library(tidyverse)
library(stringr)
library(extrafont)
library(googleway)
library(ggmap)
library(lubridate)
library(readxl)

# loads Windows fonts
loadfonts(device = "win")

##### Data Preprocessing - Clean, Reformat and Restructure #####
# import lottery dataset
lottery_query = "select sl.*, concat(info.stu_AddressStreet,', ',info.stu_City,', ',info.stu_State) as student_address, info.stu_DoB, info.stu_SchoolDistrict from custom.custom_Student_Lottery sl join custom.custom_Infosnap info on info.SubmissionID = sl.submission_id"

school_query = "select SchoolName_Abbreviation, concat(Address_School,', ',City_School,', ',State_School) as school_address from dw.DW_dimSchool where SchoolName_Abbreviation <> '-----'"

student_query = "select SystemStudentID, FirstName, LastName, SchoolYearEntryDate, SchoolYearExitDate, DateOfBirth, EnrollmentStatus, sch.SchoolName_Abbreviation from dw.DW_dimStudent st
	                join dw.DW_dimSchool sch on st.SchoolKEY_MostRecent = sch.SchoolKEY
                  
                  where sch.SchoolName_Abbreviation <> '-----'"

dbhandle = odbcConnect(dsn="schoolzilla",uid=Sys.getenv("SZ_USERNAME"),pwd=Sys.getenv("SZ_PWD"))
df = sqlQuery(dbhandle, lottery_query)
school_df = sqlQuery(dbhandle, school_query)
student_df = sqlQuery(dbhandle, student_query)
odbcCloseAll()

df = df %>% rename(school_2_enrollment_status = school_2_enrollmentstatus) 

# select the columns that are needed for the analysis and rename columns
temp_df = df %>%
  select(
    JONES_ID, student_name, student_address, stu_SchoolDistrict, stu_DoB, frl_status, grade, gender, submission_id, first_preference_school, overall_enrollment_status, parent_language, 
    inf_lottery_number, star_lottery_number, wh_lottery_number, amp_lottery_number, aca_lottery_number, OSIS_Number,
    applied_school_1, school_1_tier, school_1_enrollment_status, school_1_date_offered_seat, school_1_date_declined_seat, 
    applied_school_2, school_2_tier, school_2_enrollment_status, school_2_date_offered_seat, school_2_date_declined_seat, 
    applied_school_3, school_3_tier, school_3_enrollment_status, school_3_date_offered_seat, school_3_date_declined_seat, 
    applied_school_4, school_4_tier, school_4_enrollment_status, school_4_date_offered_seat, school_4_date_declined_seat, 
    applied_school_5, school_5_tier,school_5_enrollment_status, school_5_date_offered_seat, school_5_date_declined_seat
  ) %>% 
  arrange(student_name) %>%
  rename(
    osis_number = OSIS_Number,
    address = student_address,
    district = stu_SchoolDistrict,
    dob = stu_DoB,
    tier_school_1 = school_1_tier,
    enrollment_status_school_1 = school_1_enrollment_status, 
    date_offered_school_1 = school_1_date_offered_seat, 
    date_declined_school_1 = school_1_date_declined_seat, 
    tier_school_2 = school_2_tier, 
    enrollment_status_school_2 = school_2_enrollment_status, 
    date_offered_school_2 = school_2_date_offered_seat, 
    date_declined_school_2 = school_2_date_declined_seat, 
    tier_school_3 = school_3_tier, 
    enrollment_status_school_3 = school_3_enrollment_status, 
    date_offered_school_3 = school_3_date_offered_seat, 
    date_declined_school_3 = school_3_date_declined_seat, 
    tier_school_4 = school_4_tier, 
    enrollment_status_school_4 = school_4_enrollment_status, 
    date_offered_school_4 = school_4_date_offered_seat, 
    date_declined_school_4 = school_4_date_declined_seat, 
    tier_school_5 = school_5_tier,
    enrollment_status_school_5 = school_5_enrollment_status, 
    date_offered_school_5 = school_5_date_offered_seat, 
    date_declined_school_5 = school_5_date_declined_seat
  )

# reshape the dataset to get students listed multiple times and schools to be stacked on top of each other
tier_analysis = temp_df %>% 
  gather(key, value, -JONES_ID, -osis_number, -student_name, -dob, -address, -district, -frl_status, -grade, -gender, -submission_id, 
         -first_preference_school, -overall_enrollment_status, -parent_language, -inf_lottery_number,
         -star_lottery_number, -wh_lottery_number, -amp_lottery_number, -aca_lottery_number)

# retain the school number and modify the key for casting
tier_analysis$school_number = str_sub(tier_analysis$key, -1, -1)
tier_analysis$key = str_replace(tier_analysis$key, "_[0-9]", "")

# spread data wide and remove where tiers are not listed - means student didn't list
tier_analysis_complete = tier_analysis %>%
  spread(key, value) %>% 
  arrange(student_name) %>%
  filter(tier_school != "")

# modify some featuers
tier_analysis_complete$accepted = ifelse(tier_analysis_complete$enrollment_status_school %in% c("Accepted","accepted"), 1, 0)
tier_analysis_complete$declined = ifelse(tier_analysis_complete$enrollment_status_school %in% c("declined","Declined"), 1, 0)
tier_analysis_complete$unable_to_reach = ifelse(tier_analysis_complete$enrollment_status_school == "Unable to Reach", 1, 0)
tier_analysis_complete$undecided = ifelse(tier_analysis_complete$enrollment_status_school == "Undecided (48 hours to decide)", 1, 0)

# multiple tiers within some
tier_analysis_complete$tier_school = ifelse(tier_analysis_complete$tier_school == "Tier 2a" | tier_analysis_complete$tier_school == "Tier 2b", "Tier 2", tier_analysis_complete$tier_school)
tier_analysis_complete$tier_school = ifelse(tier_analysis_complete$tier_school == "Tier 4a" | tier_analysis_complete$tier_school == "Tier 4b", "Tier 4", tier_analysis_complete$tier_school)

# create a feature called accepted to kipp
tier_analysis_complete = left_join(tier_analysis_complete, 
  tier_analysis_complete %>% 
    group_by(submission_id) %>%
    summarize(accepted_to_kipp = max(accepted)),
  by = "submission_id")

# create a feature for the number of schools applied to
tier_analysis_complete = left_join(tier_analysis_complete, 
                                   tier_analysis_complete %>% 
                                     group_by(submission_id) %>%
                                     summarize(number_of_schools_applied_to = max(school_number)),
                                   by = "submission_id")

# create a tier feature and min_tier feature
tier_analysis_complete$tier = as.numeric(str_extract(tier_analysis_complete$tier_school, '[0-9]'))

# create a preferred feature
tier_analysis_complete$preferred[tier_analysis_complete$first_preference_school == tier_analysis_complete$applied_school] = 1

tier_analysis_complete = left_join(tier_analysis_complete, 
                                   tier_analysis_complete %>% 
                                     group_by(submission_id) %>%
                                     summarize(min_tier = min(tier)),
                                   by = "submission_id")

##### Number of Schools Applied to by Students #####
students_by_number_applied = tier_analysis_complete %>%
  group_by(number_of_schools_applied_to) %>%
  summarize(n = n_distinct(submission_id))

num_schools_applied = ggplot(students_by_number_applied, aes(x = number_of_schools_applied_to, y = n)) +
  geom_bar(stat = "identity", fill = "#255694") + 
  geom_text(aes(label = n), fontface = "bold", vjust = -1, size = 5) + 
  labs(x = "Number of Schools", y = "Number of Students", title = "Number of Schools a Student Applies to") + 
  scale_y_continuous(limits = c(0,1200)) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Numbers by School.jpeg", plot = num_schools_applied, path = "plots/", height = 8, width = 10)

##### Conduct analyses on acceptances by Tier #####
# overall by tiers
overall = tier_analysis_complete %>%
  group_by(tier_school) %>%
  summarize(accepted = sum(accepted), applied = n()) %>%
  mutate(percent_accepted = accepted / applied)

tier_level_graph = ggplot(overall, aes(x = tier_school, y = percent_accepted)) + 
  geom_bar(stat = "identity", fill = "#255694") + 
  geom_text(aes(label = paste0(round(percent_accepted * 100, 0), "%")), fontface = "bold", vjust = -1, size = 5) + 
  labs(x = "", y = "Percent Accepted", title = "Acceptances to KIPP by Tier") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Acceptances to KIPP by Tier.jpeg", plot = tier_level_graph, path = "plots/", height = 8, width = 10)

# number of students applied and by tier
print(paste0("Number of students that applied to KIPP schools: ", tier_analysis_complete %>% summarize(n = n_distinct(submission_id))))

tier_counts = tier_analysis_complete %>% 
  group_by(tier) %>% 
  summarize(count = n()) %>% 
  mutate(tier = paste0("Tier ", tier))

tier_counts$total = sum(tier_counts$count)
tier_counts$percent = tier_counts$count / tier_counts$total 

tier_plot = ggplot(tier_counts, aes(x = tier, y = percent)) + 
  geom_bar(stat = "identity", fill = "#255694") +
  geom_text(aes(label = paste0(round(percent * 100, 0), "%")), fontface = "bold", position = position_dodge(0.9), vjust = -1, size = 5) +
  labs(x = "", y = "Percent", title = "Percentage of Applications by Tier") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Tier Summary.jpeg", plot = tier_plot, path = "plots/", height = 8, width = 10)

##### Preferred Schools #####
preferred_only = subset(tier_analysis_complete, preferred == 1)

preferred_analysis = preferred_only %>%
  filter(date_offered_school != "") %>%
  group_by(tier_school) %>%
  summarize(accepted = sum(accepted), applied = n()) %>%
  mutate(percent_accepted = accepted / applied) 

preferred_by_tier = ggplot(preferred_analysis, aes(x = tier_school, y = percent_accepted)) +
  geom_bar(stat = "identity", fill = "#255694") + 
  geom_text(aes(label = paste0(round(percent_accepted * 100, 0), "%")), fontface = "bold", vjust = -1, size = 5) + 
  labs(x = "", y = "Percent Accepted", title = "Acceptance Rates by Tier of Students Offered Seat") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Preferred by Tier.jpeg", plot = preferred_by_tier, path = "plots/", height = 8, width = 10)

##### Not Preferred Schools #####
# take out students preferred schools examine rates of acceptance for students who didn't get into preferred schools 
not_pref = tier_analysis_complete %>%
  filter(is.na(preferred)) %>%
  mutate(tier = str_extract(tier_school, '[0-9]'))

not_pref = subset(not_pref, tier == min_tier)

not_pref = left_join(not_pref,
                     not_pref %>%
                       group_by(submission_id) %>%
                       summarize(accepted_to_kipp_np = max(accepted)),
                     by = "submission_id")

np2 = not_pref %>% 
  group_by(submission_id, min_tier) %>%
  summarize(apps_to_tier = n(), accepted = sum(accepted_to_kipp_np), declined = sum(declined)) %>%
  mutate(accepted_ind = if_else(accepted > 0, 1, 0))

np2_collapsed = np2 %>% 
  group_by(min_tier) %>% 
  summarize(accepted = sum(accepted_ind), declined = sum(declined), applied = n()) %>%
  mutate(percent_accepted = accepted / applied)

not_pref_graph = ggplot(np2_collapsed, aes(x = min_tier, y = percent_accepted)) + 
  geom_bar(stat = "identity", fill = "#255694") +
  geom_text(aes(label = paste0(round(percent_accepted * 100, 0), "%")), fontface = "bold", vjust = -1, size = 5) + 
  labs(x = "", y = "Percent Accepted", title = "Acceptance Rates by Tier for Students at\nNon-Preferred Schools") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Not Preferred Schools.jpeg", plot = not_pref_graph, path = "plots/", height = 8, width = 10)
  
##### What do students chose when they are not offered seat to preferred school? #####
# clean up existing data for students who were not offered spot
no_offer_at_pref = tier_analysis_complete %>%
  filter(date_offered_school == "" & preferred == 1) %>%
  mutate(tier = str_extract(tier_school, '[0-9]'))

# check duplicates ID issue
no_offer_at_pref$submission_id[duplicated(no_offer_at_pref$submission_id)]

# due to an ID being duplicated, clean up to make IDs unique
# student showed as applying twice to same school with same outcome
no_offer_at_pref2 = no_offer_at_pref[!duplicated(no_offer_at_pref$submission_id),]

perc_accept_at_un_prefd = no_offer_at_pref2 %>%
  group_by(tier_school) %>%
  summarize(accepted = sum(accepted), declined = sum(declined), applied = n()) %>%
  mutate(percent_accepted = accepted / applied,
         percent_declined = declined / applied) %>%
  gather(key, value, -tier_school) %>%
  spread(key, value)

perc_accept_at_un_pref_chart = ggplot(perc_accept_at_un_prefd, aes(x = tier_school, y = percent_accepted)) + 
  geom_bar(stat = "identity", fill = "#255694") +
  geom_text(aes(label = paste0(round(percent_accepted * 100, 0), "%","\n(", accepted, "/", applied  , ")")), fontface = "bold", vjust = -1, size = 4) + 
  labs(x = "", y = "Percent Accepted", title = "Acceptance Rates by Tier for Students\nNot Offered Seat at Preferred School") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Acceptance Rates When Not Offered Spot at Preferred School.jpeg", plot = perc_accept_at_un_pref_chart, path = "plots/", height = 8, width = 10)

##### Determine acceptance rates by distance away or time away for Tier 2 students in STAR and Infinity #####
# add a school abbreviation column to the complete dataframe to merge on addresses
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP STAR Elementary - Harlem"] = "STA H ES"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Washington Heights Elementary"] = "WH ES"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Infinity Elementary"] = "INF ES"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Academy Elementary"] = "ACA ES"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP AMP Elementary"] = "AMP ES"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Academy Middle"] = "ACA MS"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP STAR Middle"] = "STA H MS"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP AMP Middle"] = "AMP MS"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Infinity Middle"] = "INF MS"
tier_analysis_complete$schoolname_abbreviation[tier_analysis_complete$applied_school == "KIPP Washington Heights Middle"] = "WH MS"

school_df = school_df %>% rename(schoolname_abbreviation = SchoolName_Abbreviation)

tier_analysis_complete = left_join(tier_analysis_complete, school_df, by = "schoolname_abbreviation")

tier_analysis_complete = filter(tier_analysis_complete, !is.na(tier_analysis_complete$schoolname_abbreviation))
tier_analysis_complete$address = as.character(tier_analysis_complete$address)
tier_analysis_complete$school_address = as.character(tier_analysis_complete$school_address)

# create a data frame for each charter due to API call limits
aca_df = tier_analysis_complete[tier_analysis_complete$schoolname_abbreviation == "ACA ES" | tier_analysis_complete$schoolname_abbreviation == "ACA MS",]
amp_df = tier_analysis_complete[tier_analysis_complete$schoolname_abbreviation == "AMP ES" | tier_analysis_complete$schoolname_abbreviation == "AMP MS",]
wh_df = tier_analysis_complete[tier_analysis_complete$schoolname_abbreviation == "WH ES" | tier_analysis_complete$schoolname_abbreviation == "WH MS",]
star_df = tier_analysis_complete[tier_analysis_complete$schoolname_abbreviation == "STA H ES" | tier_analysis_complete$schoolname_abbreviation == "STA H MS",]
inf_df = tier_analysis_complete[tier_analysis_complete$schoolname_abbreviation == "INF ES" | tier_analysis_complete$schoolname_abbreviation == "INF MS",]

star_df_t2 = star_df %>% filter(tier == 2)
inf_df_t2 = inf_df %>% filter(tier == 2)

star_inf_t2 = bind_rows(star_df_t2, inf_df_t2)

# function for finding the distance between two addresses
for(i in 1:nrow(star_inf_t2)){
  tryCatch({
    reply = google_distance(origins = as.character(star_inf_t2$address[[i]]), destinations = as.character(star_inf_t2$school_address[[i]]), mode = "transit", key = Sys.getenv("GOOG_KEY"), simplify = TRUE)
    print(i)
    print(reply$status)
    print(reply$rows$elements[[1]]$duration$value[1])

    if((reply$status != "INVALID_REQUEST" | reply$status != "OVER_QUERY_LIMIT") & !is.null(reply$rows$elements[[1]]$distance$value[1])){
      star_inf_t2$duration[[i]] = reply$rows$elements[[1]]$duration$value[1]
      star_inf_t2$distance[[i]] = reply$rows$elements[[1]]$distance$value[1]
    } else {
      star_inf_t2$duration[[i]] = NA
      star_inf_t2$distance[[i]] = NA
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

star_inf_t2$duration_min = star_inf_t2$duration / 60

star_inf_duration_test = star_inf_t2 %>% 
  group_by(accepted, schoolname_abbreviation) %>% 
  summarize(count = n(), 
            mean_duration = mean(duration, na.rm = TRUE), 
            mean_distance = mean(distance, na.rm = TRUE), 
            sd_duration = sd(duration, na.rm = TRUE), 
            sd_distance = sd(distance, na.rm = TRUE))

mean_dist_plot = ggplot(star_inf_duration_test, aes(x = schoolname_abbreviation, y = mean_distance, group = accepted, fill = accepted)) +
  geom_bar(stat = "identity", position = "dodge")

mean_dur_plot = ggplot(star_inf_duration_test, aes(x = schoolname_abbreviation, y = mean_duration, group = accepted, fill = accepted)) +
  geom_bar(stat = "identity", position = "dodge")

sd_dist_plot = ggplot(star_inf_duration_test, aes(x = schoolname_abbreviation, y = sd_distance, group = accepted, fill = accepted)) +
  geom_bar(stat = "identity", position = "dodge")

sd_dur_plot = ggplot(star_inf_duration_test, aes(x = schoolname_abbreviation, y = sd_duration, group = accepted, fill = accepted)) +
  geom_bar(stat = "identity", position = "dodge")

# determine if time to travel to school affects acceptances
duration_check = star_inf_t2 %>% 
  fill(duration) %>% 
  group_by(schoolname_abbreviation, accepted) %>% 
  summarize(duration_min_avg = mean(duration_min, na.rm = TRUE))

duration_check_star = duration_check %>%
  filter(schoolname_abbreviation == "STA H ES") %>%
  mutate(accept_label = if_else(accepted == 1, "Accepted", "Did Not Accept"))

duration_plot = ggplot(duration_check_star, aes(x = schoolname_abbreviation, y = duration_min_avg, fill = accept_label, group = accept_label)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(duration_min_avg,0)), fontface = "bold", position = position_dodge(0.9), vjust = -1, size = 4) + 
  labs(x = "", y = "Minutes", title = "Average Minutes to STAR ES by\nAccepting a Seat at STAR ES") + 
  scale_y_continuous(limits = c(0,30)) +
  scale_fill_manual(name = "",values = c("#255694", "#E2842A")) + 
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Time Plot.jpeg", plot = duration_plot, path = "plots/", height = 8, width = 5)

star_es_t2 = star_inf_t2 %>% filter(schoolname_abbreviation == "STA H ES")
inf_es_t2 = star_inf_t2 %>% filter(schoolname_abbreviation == "INF ES")

test_log = glm(star_es_t2$accepted ~ star_es_t2$duration, family = binomial)
summary(test_log)

test_log2 = glm(inf_es_t2$accepted ~ inf_es_t2$duration, family = binomial)
summary(test_log2)

##### Get geocodes for addresses #####

for(j in 1:nrow(star_inf_t2)){
  tryCatch({
    reply = google_geocode(address = as.character(star_inf_t2$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
    print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))

    if(reply$status == "OK"){
      star_inf_t2$lat[[j]] = reply$results$geometry$location$lat[1]
      star_inf_t2$lng[[j]] = reply$results$geometry$location$lng[1]
    } else {
      star_inf_t2$lat[[j]] = NA
      star_inf_t2$lng[[j]] = NA
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

# for Star apps
# for(j in 1:nrow(star_df)){
#   tryCatch({
#     reply = google_geocode(address = as.character(star_df$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
#     print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))
#     
#     if(reply$status == "OK"){
#       star_df$lat[[j]] = reply$results$geometry$location$lat[1]
#       star_df$lng[[j]] = reply$results$geometry$location$lng[1]
#     } else {
#       star_df$lat[[j]] = NA
#       star_df$lng[[j]] = NA
#     }
#   }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
# }
# 
# # for Infinity apps
# for(j in 1:nrow(inf_df)){
#   tryCatch({
#     reply = google_geocode(address = as.character(inf_df$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
#     print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))
#     
#     if(reply$status == "OK"){
#       inf_df$lat[[j]] = reply$results$geometry$location$lat[1]
#       inf_df$lng[[j]] = reply$results$geometry$location$lng[1]
#     } else {
#       inf_df$lat[[j]] = NA
#       inf_df$lng[[j]] = NA
#     }
#   }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
# }
# 
# # to interact with in tableau
# location_df = bind_rows(star_df, inf_df)
# write_csv(location_df, "C:/Users/rmenon/Desktop/star_inf_locations.csv")

# for AMP apps
# for(j in 1:nrow(amp_df)){
#   tryCatch({
#     reply = google_geocode(address = as.character(amp_df$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
#     print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))
#     
#     if(reply$status == "OK"){
#       amp_df$lat[[j]] = reply$results$geometry$location$lat[1]
#       amp_df$lng[[j]] = reply$results$geometry$location$lng[1]
#     } else {
#       amp_df$lat[[j]] = NA
#       amp_df$lng[[j]] = NA
#     }
#   }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
# }
# 
# # for WH apps
# for(j in 1:nrow(wh_df)){
#   tryCatch({
#     reply = google_geocode(address = as.character(wh_df$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
#     print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))
#     
#     if(reply$status == "OK"){
#       wh_df$lat[[j]] = reply$results$geometry$location$lat[1]
#       wh_df$lng[[j]] = reply$results$geometry$location$lng[1]
#     } else {
#       wh_df$lat[[j]] = NA
#       wh_df$lng[[j]] = NA
#     }
#   }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
# }
# 
# # for Academy apps
# for(j in 1:nrow(aca_df)){
#   tryCatch({
#     reply = google_geocode(address = as.character(aca_df$address[[j]]), key = Sys.getenv("GOOG_GEO_KEY"), simplify = TRUE)
#     print(paste(j,paste0(reply$status," ", reply$results$geometry$location$lat, ":", reply$results$geometry$location$lng),sep = " "))
#     
#     if(reply$status == "OK"){
#       aca_df$lat[[j]] = reply$results$geometry$location$lat[1]
#       aca_df$lng[[j]] = reply$results$geometry$location$lng[1]
#     } else {
#       aca_df$lat[[j]] = NA
#       aca_df$lng[[j]] = NA
#     }
#   }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
# }

##### Attrition Data #####
## Question: What percentage of students who accept a spot at one of the schools actually attend?

# looking only at students who accepted - will need to change this
accepted_only = tier_analysis_complete %>% 
  filter(accepted == 1) %>%
  rename(SystemStudentID = osis_number)

accepted_only$student_name_dob = paste(accepted_only$student_name, accepted_only$dob, sep = " ")

accepted_only = left_join(accepted_only, student_df, by = "SystemStudentID")

colnames(accepted_only)[36] = "student_name_dob"

# create student_name_dob as a pseudo ID field
student_df$student_name = paste(student_df$FirstName, student_df$LastName, sep = " ")
student_df$student_name_dob = paste(student_df$student_name, student_df$DateOfBirth, sep = " ")

# do another left join hoping to get a better match
accepted_only = left_join(accepted_only, student_df, by ="student_name_dob")
accepted_only$found[!is.na(accepted_only$FirstName.x)] = 1
accepted_only$found2[!is.na(accepted_only$FirstName.y)] = 1
sum(accepted_only$found2, na.rm = TRUE)

# create a found_in_both column
accepted_only$found_in_both[accepted_only$found == 1 | accepted_only$found2 == 1] = 1

# missing about 23% of records now
print(paste("Percent of Records with an Enrollment:", as.character(round(sum(accepted_only$found_in_both, na.rm = TRUE) / nrow(accepted_only) * 100),0), "%", sep = " "))

# create flag for enrolled and not currently enrolled
accepted_only$enrolled[accepted_only$EnrollmentStatus.x == "Currently Enrolled" | accepted_only$EnrollmentStatus.y == "Currently Enrolled"] = 1
accepted_only$not_enrolled[accepted_only$EnrollmentStatus.x == "Not Currently Enrolled" | accepted_only$EnrollmentStatus.y == "Not Currently Enrolled"] = 1

# create a enrolled at beginning of year variable 
beginning_of_sy_interval = interval(ymd(20170801),ymd(20170822))

# change to date variable
accepted_only$SchoolYearEntryDate.x = as.Date(accepted_only$SchoolYearEntryDate.x)
accepted_only$SchoolYearEntryDate.y = as.Date(accepted_only$SchoolYearEntryDate.y)

accepted_only$entry_date = accepted_only$SchoolYearEntryDate.x
accepted_only$entry_date[is.na(accepted_only$entry_date)] = accepted_only$SchoolYearEntryDate.y

accepted_only$enrolled_at_boy[accepted_only$enrolled == 1 & accepted_only$entry_date %within% beginning_of_sy_interval] = 1
accepted_only$enrolled_at_boy[accepted_only$enrolled == 1 & accepted_only$SchoolYearEntryDate.y == "2017-08-21" & is.na(accepted_only$enrolled_at_boy)] = 1

# create a dataframe for analysis on just percent of groups enrolled
overall_attrition_at_site = accepted_only %>% 
  group_by(applied_school) %>% 
  summarize(total_accepted = sum(accepted, na.rm = TRUE), 
            enrolled_at_beginning = sum(enrolled_at_boy, na.rm = TRUE), 
            still_enrolled = sum(enrolled, na.rm = TRUE), 
            not_enrolled = sum(not_enrolled, na.rm = TRUE)) %>% 
  mutate(percent_enrolled_at_boy = enrolled_at_beginning / total_accepted, 
         enrolled_of_acc = still_enrolled / total_accepted, 
         not_enroll_pct = not_enrolled / total_accepted) %>%
  gather(key, value, -applied_school) 

overall_attrition_at_site_tier = accepted_only %>% 
  group_by(applied_school, tier) %>% 
  summarize(total_accepted = sum(accepted, na.rm = TRUE), 
            enrolled_at_beginning = sum(enrolled_at_boy, na.rm = TRUE), 
            still_enrolled = sum(enrolled, na.rm = TRUE), 
            not_enrolled = sum(not_enrolled, na.rm = TRUE)) %>% 
  mutate(percent_enrolled_at_boy = enrolled_at_beginning / total_accepted, 
         enrolled_of_acc = still_enrolled / total_accepted, 
         not_enroll_pct = not_enrolled / total_accepted) %>%
  gather(key, value, -c(applied_school,tier)) %>%
  filter(key %in% c( "total_accepted", "enrolled_at_beginning","still_enrolled")) %>%
  filter(applied_school %in% c("KIPP STAR Elementary - Harlem", "KIPP Infinity Elementary")) %>%
  mutate(status = if_else(key == "total_accepted","Accepted",
                          if_else(key == "enrolled_at_beginning", "Enrolled at BOY",""))) %>%
  filter(status != "") %>%
  filter(tier == 3 | tier == 2) %>%
  mutate(tier = paste0("Tier ", tier))

test_att_plot = ggplot(overall_attrition_at_site_tier, aes(x = status, y = value, group = applied_school, color = applied_school)) + 
  geom_line(size = 2) +
  geom_point(size = 3, shape = 15, stroke = 2) + 
  geom_text(aes(label = value), color = "black", fontface = "bold", vjust = -1, size = 4) + 
  labs(x = "", y = "Value", title = "Acceptance to Beginning of Year Enrollment") +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        legend.position = "bottom") +
  facet_wrap(~ tier)

ggsave("Slope Plot.jpeg", plot = test_att_plot, path = "plots/", height = 8, width = 10)

overall_attrition_at_site = overall_attrition_at_site %>%
  filter(key == "percent_enrolled_at_boy" | key == "enrolled_of_acc") %>% 
  mutate(timing = if_else(key == "percent_enrolled_at_boy", "Beginning\nof Year", "Currently\nEnrolled"))

overall_attrition_at_site$timing = as.factor(overall_attrition_at_site$timing)

# plot for attrition at each school site
# adjust the title for WH ES
overall_attrition_at_site$applied_school[overall_attrition_at_site$applied_school == "KIPP Washington Heights Elementary"] = "KIPP Wash. Heights Elementary"

attrition_plot = ggplot(overall_attrition_at_site, aes(x = timing, y = value)) +
  geom_point(size = 3, color = "#255694", shape = 19, stroke = 2) + 
  geom_line(aes(group = applied_school), color = "#255694", size = 1) + 
  geom_text(aes(label = paste0(round(value * 100, 0), "%")), fontface = "bold", vjust = -1, size = 4) + 
  facet_wrap(~ applied_school, nrow = 2) + 
  labs(x = "", y = "Percent", title = "Attrition from Enrolling at Beginning of Year\nto Still Enrolled") + 
  scale_y_continuous(limits = c(0,1.1), breaks = c(0,0.2,0.4,0.6,0.8,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Attrition by Site.jpeg", plot = attrition_plot, path = "plots/", height = 8, width = 11)

## Question: What percentage of students accepted for seats at the beginning of the school year by tier?
attrition_by_tier_site = accepted_only %>% 
  group_by(applied_school, min_tier) %>% 
  summarize(total_accepted = sum(accepted, na.rm = TRUE), 
            enrolled_at_beginning = sum(enrolled_at_boy, na.rm = TRUE), 
            still_enrolled = sum(enrolled, na.rm = TRUE), 
            not_enrolled = sum(not_enrolled, na.rm = TRUE)) %>% 
  mutate(percent_enrolled_at_boy = enrolled_at_beginning / total_accepted, 
         enrolled_of_acc = still_enrolled / total_accepted, 
         not_enroll_pct = not_enrolled / total_accepted)

# plot for enrolling at beginning of year - site-level
at_boy_site = ggplot(attrition_by_tier_site, aes(x = min_tier, y = percent_enrolled_at_boy)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ applied_school)

currently_enrolled_by_site = ggplot(attrition_by_tier_site, aes(x = min_tier, y = enrolled_of_acc)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ applied_school)

## Question; What does attrition look like by tier of application?
attrition_by_tier = accepted_only %>%
  group_by(tier) %>%
  summarize(total_accepted = sum(accepted, na.rm = TRUE), 
            enrolled_at_beginning = sum(enrolled_at_boy, na.rm = TRUE), 
            still_enrolled = sum(enrolled, na.rm = TRUE), 
            not_enrolled = sum(not_enrolled, na.rm = TRUE)) %>% 
  mutate(percent_enrolled_at_boy = enrolled_at_beginning / total_accepted, 
         enrolled_of_acc = still_enrolled / total_accepted, 
         not_enroll_pct = not_enrolled / total_accepted)


# plot for this at the beginning of year and still enrolled
enrolled_by_tier = ggplot(attrition_by_tier, aes(x = tier, y = enrolled_at_beginning)) +
  geom_bar(stat = "identity") 

still_enrolled_by_tier = ggplot(attrition_by_tier, aes(x = tier, y = still_enrolled)) + 
  geom_bar(stat = "identity")

##### STAR and Infinity overlap analysis #####
# filter for only STAR and Infinity and create indicator for whether student applied to one or the other
star_inf = tier_analysis_complete %>% 
  filter(schoolname_abbreviation == "STA H ES" | schoolname_abbreviation == "INF ES") %>%
  mutate(applied_to_star = if_else(schoolname_abbreviation %in% c("STA H ES", "STA H MS"), 1, 0),
         applied_to_inf = if_else(schoolname_abbreviation %in% c("INF ES", "INF MS"), 1, 0),
         count = 1)

star_inf_collapsed = star_inf %>% 
  group_by(schoolname_abbreviation, tier) %>%
  summarize(count = sum(count))

star_inf_overall_collapsed = star_inf %>%
  group_by(schoolname_abbreviation) %>% 
  summarize(total = sum(count))

star_inf_collapsed = left_join(star_inf_collapsed, star_inf_overall_collapsed, by = "schoolname_abbreviation") %>%
  filter(schoolname_abbreviation == "INF ES" | schoolname_abbreviation == "STA H ES") %>%
  mutate(percent = count / total,
         tier = paste0("Tier ",tier))

# distribution of applicants by tier at these schools
star_inf_app_totals = ggplot(star_inf_collapsed, aes(x = tier, y = percent)) + 
  geom_bar(stat = "identity", fill = "#255694") + 
  geom_text(aes(label = paste0(round(percent * 100,0),"%")), fontface = "bold", vjust = -1, size = 4) + 
  facet_wrap(~ schoolname_abbreviation, ncol = 2) + 
  labs(x = "", y = "Number of Applicants", title = "Distribution of STAR and Infinity Applications by Tier") + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Application Distribution by School.jpeg", plot = star_inf_app_totals, path = "plots/", height = 8, width = 10)

# collapse on a unique ID to bring back into the filtered dataset
star_inf_apps = star_inf %>%
  filter(schoolname_abbreviation == "STA H ES" | schoolname_abbreviation == "INF ES") %>%
  group_by(submission_id) %>% 
  summarize(applied_to_inf = sum(applied_to_inf, na.rm = FALSE), 
            applied_to_star = sum(applied_to_star, na.rm = FALSE)) %>% 
  mutate(applied_to_both = if_else(applied_to_inf >= 1 & applied_to_star >= 1, 1, 0)) %>%
  select(submission_id, applied_to_both)

number_of_apps_star_inf_by_student = full_join(star_inf, star_inf_apps, by = "submission_id") %>% 
  filter(schoolname_abbreviation == "STA H ES" | schoolname_abbreviation == "INF ES") %>%
  mutate(interested_schools = if_else(applied_to_both == 1, 2, 1))
  # group_by(submission_id) %>% 
  # summarize(star_app = sum(applied_to_star, na.rm = FALSE), 
  #           inf_app = sum(applied_to_inf, na.rm = FALSE)) %>% 
  # mutate(interested_schools = star_app + inf_app)

# someone applied to the same school twice
#number_of_apps_star_inf_by_student$interested_schools[number_of_apps_star_inf_by_student$interested_schools > 2] = 2
#number_of_apps_star_inf_by_student$star_app[number_of_apps_star_inf_by_student$star_app == 2] = 1

star_inf_app_freq = number_of_apps_star_inf_by_student %>%
  group_by(interested_schools) %>% 
  summarize(count = n_distinct(submission_id)) %>%
  mutate(number_of_schools = if_else(interested_schools == 1, "Applied to One School", "Applied to Infinity & STAR"))

apps_to = ggplot(star_inf_app_freq, aes(x = number_of_schools, y = count)) +
  geom_bar(stat = "identity", fill = "#255694", width = 0.5) + 
  geom_text(aes(label = count), fontface = "bold", vjust = -1, size = 4) + 
  labs(x = "", y = "Number of Applicants", title = "Overlap in Applications to STAR and Infinity ES") + 
  scale_y_continuous(limits = c(0,1500), breaks = c(0,300,600,900,1200,1500)) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Shared Applications.jpeg", plot = apps_to, path = "plots/", height = 8, width = 5)

# percent of applicants who applied to both schools or only one by tier to school
tiers_wide = star_inf %>%
  group_by(submission_id, schoolname_abbreviation, tier) %>%
  summarize(star_app = sum(applied_to_star, na.rm = FALSE), 
            inf_app = sum(applied_to_inf, na.rm = FALSE)) %>% 
  spread(schoolname_abbreviation, tier) %>% 
  select(submission_id, `INF ES`, `STA H ES`) %>% 
  group_by(submission_id) %>% 
  summarize(inf_es = sum(`INF ES`, na.rm = TRUE), 
            #inf_ms = sum(`INF MS`, na.rm = TRUE), 
            star_es = sum(`STA H ES`, na.rm = TRUE) 
            #star_ms = sum(`STA H MS`, na.rm = TRUE)
            )

# investigate tier 2 and preferences when it comes and outcomes for this group
star_inf_tier2 = number_of_apps_star_inf_by_student %>% 
  filter(tier == 2) %>% 
  filter(interested_schools == 2) %>% 
  group_by(submission_id) %>%
  summarize(applied_to_star = sum(applied_to_star, na.rm = FALSE),
            applied_to_inf = sum(applied_to_inf, na.rm = FALSE)) %>%
  mutate(applied_to_both = if_else(applied_to_star == 1 & applied_to_inf == 1,1,0))

star_inf_tier2 = inner_join(star_inf, star_inf_tier2, by = "submission_id") %>% 
  filter(tier == 2 & applied_to_both == 1)

pref_sch_star_inf_table = star_inf_tier2 %>% 
  group_by(schoolname_abbreviation) %>% 
  summarize(count = n(), 
            preferred = sum(preferred, na.rm = TRUE), 
            accepted = sum(if_else(date_offered_school != "",1,0))) %>% 
  mutate(percent_preferred = preferred / count, 
         percent_accepted = accepted / count) 

pref_sch_star_inf_t2_collapsed = star_inf_tier2 %>% 
  group_by(schoolname_abbreviation) %>% 
  summarize(count = n(), 
            preferred = sum(preferred, na.rm = TRUE), 
            accepted = sum(if_else(date_offered_school != "",1,0))) %>% 
  mutate(percent_preferred = preferred / count, 
         percent_accepted = accepted / count) %>% 
  gather(label, value, -schoolname_abbreviation) %>% 
  filter(label == "percent_preferred" | label == "percent_accepted") %>% 
  mutate(label = if_else(label == "percent_preferred", "Preferred", "Accepted"))

pref_sch_star_inf_plot = ggplot(pref_sch_star_inf_t2_collapsed, aes(x = schoolname_abbreviation, y = percent_accepted, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = paste0(round(percent_accepted*100,0),"%")), fontface = "bold", position = position_dodge(0.9), hjust = -0.75, size = 4) + 
  labs(x = "", y = "Percent of Applicants", title = "Percent of Tier 2 Applicants by Preference and Acceptance") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_fill_manual(name = "", values = c("#255694", "#E2842A")) + 
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) +
  coord_flip()

ggsave("Preference Plot.jpeg", plot = pref_sch_star_inf_plot, path = "plots/", height = 7, width = 7)

# get accurate count of applications to both star, infinity of just one or the other
inf_apps_by_star_tier_es = tiers_wide %>%
  group_by(star_es) %>%
  summarize(applied_to_star = sum(if_else(star_es >= 1, 1, 0))) %>%
  filter(star_es != 0) %>%
  mutate(app_count = sum(applied_to_star), 
         percent = applied_to_star / app_count) %>% 
  rename(tier = star_es,
         applied  = applied_to_star) %>%
  mutate(tier = paste0("Tier ", tier))

inf_apps_by_star_tier_es_plot = ggplot(inf_apps_by_star_tier_es, aes(x = tier, y = percent)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#255694") + 
  geom_text(aes(label = paste0(round(percent*100,0),"%")), fontface = "bold", position = position_dodge(0.9), vjust = -1, size = 4) + 
  labs(x = "Tier to STAR", y = "Percent of Applicants", title = "Percent of Applicants to STAR\nApplying to Infinity") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Inf Apps to Star.jpeg", plot = inf_apps_by_star_tier_es_plot, path = "plots/", height = 8, width = 5)

inf_apps_by_star_tier_es$tier = as.character(inf_apps_by_star_tier_es$tier)
inf_apps_by_star_tier_es$school = "STA H ES"

star_apps_by_inf_tier_es = tiers_wide %>%
  group_by(inf_es) %>%
  summarize(applied_to_star = sum(if_else(star_es >= 1, 1, 0))) %>%
  filter(inf_es != 0) %>%
  mutate(app_count = sum(applied_to_star), 
         percent = applied_to_star / app_count) %>% 
  rename(tier = inf_es,
        applied  = applied_to_star) %>%
  mutate(tier = paste0("Tier ", tier))

star_apps_by_inf_es_plot = ggplot(star_apps_by_inf_tier_es, aes(x = tier, y = percent)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#255694") + 
  geom_text(aes(label = paste0(round(percent*100,0),"%")), fontface = "bold", position = position_dodge(0.9), vjust = -1, size = 4) + 
  labs(x = "Tier to Infinity", y = "Percent of Applicants", title = "Percent of Applicants to Infinity\nApplying to STAR") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("STAR Apps to Inf.jpeg", plot = star_apps_by_inf_es_plot, path = "plots/", height = 8, width = 5)

star_apps_by_inf_tier_es$tier = as.character(star_apps_by_inf_tier_es$tier)
star_apps_by_inf_tier_es$school = "INF ES"

es_apps_by_other_tier = bind_rows(inf_apps_by_star_tier_es, star_apps_by_inf_tier_es) %>%
  select(tier, school, percent, applied)

es_by_other_tier_plot = ggplot(es_apps_by_other_tier, aes(x = tier, y = percent, group = school, fill = school)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = paste(round(percent*100,0),"%",sep="")), fontface = "bold", position = position_dodge(0.9), vjust = -1, size = 4) + 
  labs(x = "Application Tier of Other School", y = "Percent of Applicants Applying to Other School", title = "Overlap in Tier") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_fill_manual(name = "School", values = c("#255694","#F07F21")) +
  theme(text = element_text(family = "Calibri", face = "bold", size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("ES Tier of Prior School Year to Other School.jpeg", plot = es_by_other_tier_plot, path = "plots/", height = 8, width = 10)

# number of students who applied to both schools
star_inf = left_join(star_inf, star_inf_apps, by = "submission_id")

star_inf_app_counts = star_inf %>%
  group_by(applied_to_both) %>%
  summarize(both_count = n_distinct(submission_id)) %>%
  mutate(status = if_else(applied_to_both == 1, "applied_to_star_and_inf", "no_both")) %>%
  spread(status, both_count) %>%
  select(applied_to_star_and_inf, no_both) %>%
  summarize(applied_to_both = sum(applied_to_star_and_inf, na.rm = TRUE), no_both = sum(no_both, na.rm = TRUE)) %>%
  mutate(group = "Applied to Both",
         percent = round(applied_to_both / (applied_to_both + no_both) * 100, 0))

star_app_counts = star_inf %>%
  group_by(applied_to_star) %>%
  summarize(star_count = n_distinct(submission_id)) %>%
  mutate(status = if_else(applied_to_star == 1, "applied_to_star1", "no_star")) %>%
  spread(status, star_count) %>%
  select(applied_to_star1, no_star) %>%
  summarize(applied_to_star = sum(applied_to_star1, na.rm = TRUE), 
            no_star = sum(no_star, na.rm = TRUE)) %>%
  mutate(group = "Applied to STAR",
         percent = round(applied_to_star / (applied_to_star + no_star) * 100, 0))

inf_app_counts = star_inf %>%
  group_by(applied_to_inf) %>%
  summarize(inf_count = n_distinct(submission_id)) %>%
  mutate(status = if_else(applied_to_inf == 1, "applied_to_inf1", "no_inf")) %>%
  spread(status, inf_count) %>%
  select(applied_to_inf1, no_inf) %>%
  summarize(applied_to_inf = sum(applied_to_inf1, na.rm = TRUE), 
            no_inf = sum(no_inf, na.rm = TRUE)) %>%
  mutate(group = "Applied to Infinity",
         percent = round(applied_to_inf / (applied_to_inf + no_inf) * 100, 0))

# see what the acceptance for the overlap was


# percentage of students who chose one school over another when preferenced
# only want to look at STAR or Infinity preferenced schools
star_inf_pref = star_inf %>% filter(first_preference_school == "KIPP STAR Elementary - Harlem" | first_preference_school == "KIPP Infinity Elementary" | first_preference_school == "KIPP STAR Middle" | first_preference_school == "KIPP Infinity Middle")

star_inf_pref_counts = star_inf_pref %>% 
  group_by(applied_to_both) %>% 
  summarize(count = n_distinct(submission_id))

star_inf_pref_counts_tier = star_inf %>%
  group_by(tier, applied_to_both) %>%
  summarize(count = n_distinct(submission_id), 
            accept = sum(accepted, na.rm = FALSE)) %>%
  mutate(percent_accepted = round(accept/count * 100,0))

star_inf_pref_counts_tier_plot = ggplot(star_inf_pref_counts_tier, aes(x = tier, y = percent_accepted, fill = applied_to_both, group = applied_to_both)) +
  geom_bar(stat = "identity", position = "dodge")

star_inf_pref_acc = star_inf_pref %>% 
  group_by(applied_to_both, schoolname_abbreviation) %>% 
  summarize(count = n_distinct(submission_id), 
            accepted = sum(accepted, na.rm = FALSE)) %>%
  mutate(percent_accepted = round(accepted/count * 100, 0))

star_inf_pref_acc_plot = ggplot(star_inf_pref_acc, aes(x = schoolname_abbreviation, y = percent_accepted, group = applied_to_both, fill = applied_to_both)) +
  geom_bar(stat = "identity", position = "dodge")

##### Impact of Date of First being Contacted #####
# fix issues with dates
date_incidents = tier_analysis_complete %>% 
  filter(date_offered_school != "Distance" & date_offered_school != "") %>%
  group_by(date_offered_school) %>% 
  summarize(n = n(), 
            accepted = sum(accepted_to_kipp, na.rm = FALSE), 
            declined = sum(declined, na.rm = FALSE))

date_incidents$date_offered_school[date_incidents$date_offered_school == "2017-04-06"] = "4/6/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "2017-04-06"] = "4/6/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "2017-04-27"] = "4/27/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "4-6-17"] = "4/6/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "4-7-17"] = "4/7/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "4/17"] = "4/17/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "4/18/18"] = "4/18/2017"
date_incidents$date_offered_school[date_incidents$date_offered_school == "9/22/2107"] = "9/22/2017"

date_incidents$date_offered_school = mdy(date_incidents$date_offered_school)

date_incidents = date_incidents %>% 
  group_by(date_offered_school) %>%
  summarize(
    n = n(), 
    accepted = sum(accepted, na.rm = FALSE), 
    declined = sum(declined, na.rm = FALSE)
    ) %>%
  filter(!is.na(date_offered_school)) %>%
  mutate(percent_acceptance = accepted / (accepted + declined),
         percent_declined = declined / (accepted + declined),
         days_from_offer = date_offered_school - ymd(20170406))

date_incidents_banded = date_incidents %>%
  mutate(
    bands = if_else(days_from_offer <= 10, "10 or Less", 
                    if_else(days_from_offer <= 20, "11 - 20",
                            if_else(days_from_offer <= 30, "21 - 30",
                                    if_else(days_from_offer <= 40, "31 - 40",
                                            if_else(days_from_offer <= 50, "41 - 50", "51 or more")))))) %>%
  group_by(bands) %>%
  summarize(accept = sum(accepted, na.rm = FALSE),
            decline = sum(declined, na.rm = FALSE)) %>%
  mutate(percent_accept = accept / (accept + decline),
         percent_decline = decline / (accept + decline)) %>%
  select(bands, percent_accept, percent_decline) %>%
  gather(labels, percent, -bands) %>%
  mutate(labels = if_else(labels == "percent_accept", "Accept", "Decline")) %>%
  filter(labels == "Accept")

date_banded_plot = ggplot(date_incidents_banded, aes(x = bands, y = percent)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#255694") +
  geom_text(aes(label = round(percent * 100, 0)),  fontface = "bold", vjust = -1, size = 4, position = position_dodge(0.9)) +
  labs(x = "Days from April 6th, 2017", y = "Percent Acceptance", title = "Percent Accepted from First Potential Day of Offer") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(text = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

ggsave("Time by Band Plot.jpeg", plot = date_banded_plot, path = "plots/", height = 8, width = 10)

date_plot = ggplot(date_incidents, aes(x = date_offered_school, y = percent_acceptance)) + 
  scale_y_continuous(limits = c(0,1)) + 
  geom_smooth()