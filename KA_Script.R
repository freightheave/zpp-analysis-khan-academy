library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# # ---------------- Optional Stuff ----------------
#cbbPalette <- c("blue", "green", "yellow", "red")
cbbPalette <- c("#CC0000", "#006600", "#669999", "#00CCCC", 
                "#660099", "#CC0066", "#FF9999", "#FF9900", 
                "black", "black", "black", "black", "black")


# ---------------- Data Reading/Manipulation ----------------

me1 <- read.csv("D:/Repositories/zpp-analysis-khan-academy/M&E Insight Analyst Assignment - Data set - Copy.csv")
# replace with <path to the data.csv file> when uploading.

reg_stu <- data.frame(me1[,1:15])
#df with only student counts (no active hours)

act_hours <- data.frame(me1[,c(1,2,16:27)])
#df with active hours (no student count)

total_stu_per_dist <- reg_stu %>% group_by(District.Name) %>% summarise(Total.Registered = sum(Total.Number.of.Students.Registered))
#total registered by district.

max_students <-  max(total_stu_per_dist$Total.Registered)
# max students in a district.

long_reg_stu <- reg_stu %>%
  pivot_longer(cols = starts_with("ASPM.M"), 
               names_to = "Month", 
               values_to = "Active.Students")
# Convert reg_stu to long format for plotting ease.

long_reg_stu$Month <- factor(long_reg_stu$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
# Convert months into ordered factor.

summary(me1[,5:15])
#summary of active students/month across ZPS

summary(me1[,16:27])
#summary of active learning on KA across ZPS

months <- colnames(reg_stu)

# ---------------- Basic Calc. ----------------

mean_active_students_school <- rowMeans(reg_stu[4:15]) 
# across months PER SCHOOL. [maybe % of registered students is better?]

mean_active_students_school <- data.frame(mean_active_students_school) 
# convert to df

mean_active_students_school$School.Name <- reg_stu$School.Name 
# add school name to the new data frame

total_mean <- reg_stu %>% summarise(total_mean_students = mean(Total.Number.of.Students.Registered))
# total mean of registered students

student_mean_per_dist <- reg_stu %>% group_by(District.Name) %>% summarise(mean = mean(Total.Number.of.Students.Registered)) 
# mean of registered students per dist.

active_students_median <- reg_stu %>% summarize(across(4:15, \(x) median(x, na.rm = TRUE)))
# Median Active students per month ACROSS ALL SCHOOLS.

long_active_students_median <- active_students_median %>% 
  pivot_longer(cols = starts_with("ASPM.M"),
               names_to = "Month",
               values_to = "Active.Students")
# Converting to long df.

active_students_avg <- reg_stu %>% summarize(across(4:15, \(x) mean(x, na.rm = TRUE)))
# Mean Active students per month ACROSS ALL SCHOOLS.

long_active_students_avg <- active_students_avg %>% 
  pivot_longer(cols = starts_with("ASPM.M"),
               names_to = "Month",
               values_to = "Active.Students")
# Converting to long df.

long_active_students_avg$Month <- factor(long_active_students_avg$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
long_active_students_median$Month <- factor(long_active_students_median$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
# Ordering the months in avg and median calcs.

long_active_students_avg <- long_active_students_avg %>% mutate(Average.Active.Students = "Average Active Students")

# ---- Per Dist Calc. ----

district_level_data <- list()

# Loop through values 1 to 15
for (val in 1:15) {
  # Subset the data based on District.Name
  district_level_data[[paste0("dis", val)]] <- data.frame(subset(me1, District.Name == paste0("District ", val)))
}
#Create sub data frames of each district.

district_level_ASPM <- list()

# Loop through values 1 to 15
for (val in 1:15) {
  # Subset the data based on District.Name
  district_level_ASPM[[paste0("dis_ASPM", val)]] <- data.frame(subset(reg_stu, District.Name == paste0("District ", val)))
}

district_level_ALOKPAS <- list()

# Loop through values 1 to 15
for (val in 1:15) {
  # Subset the data based on District.Name
  district_level_ALOKPAS[[paste0("dis_ALOKPAS", val)]] <- data.frame(subset(act_hours, District.Name == paste0("District ", val)))
}





# ----------- Plot for Registered Students Per District -----------

reg_stu$District.Name <- factor(reg_stu$District.Name, levels = paste0("District ", 1:15), ordered = TRUE)
# Ordering the districts by creating ordered factor.

ggplot(data = reg_stu, mapping = aes(x= District.Name, y=Total.Number.of.Students.Registered)) + 
  geom_bar(stat='identity', aes(fill=Total.Number.of.Students.Registered)) + 
  scale_y_continuous(breaks = seq(0,max_students,by = 250)) +
  labs(x = "District Name", y = "Total Number of Students Registered", fill = "Students Registered") 
# + scale_fill_gradientn(colours = cbbPalette)
#Graph - students/school/dist.


# ----------- Plot for active students/school vs months. [Takes time to compute.] -----------

ggplot(data = long_reg_stu, aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Active Students", title = "Monthly Data for Schools") +
  theme(legend.position="none")
#Graph - Active Students/School/month


# ---------- Per school plot ---------

school_name_plot <- reg_stu[reg_stu$School.Name == "ZPP 132",]
#storing name of school for plotting, change 'school in RHS to vary the school.
#Now, need to convert school_name_plot into a long df.

specific_school_mean <- mean_active_students_school %>% filter(School.Name == "ZPP 132")
#specific mean picked from mean_active_students_school to overlay on to total graph.

long_school_name_plot <- school_name_plot %>% 
  pivot_longer(cols = starts_with("ASPM.M"),
               names_to = "Month",
               values_to = "Active.Students")
#Convert to long data format.

long_school_name_plot$Month <- factor(long_school_name_plot$Month, levels = c("ASPM.M1","ASPM.M2","ASPM.M3","ASPM.M4","ASPM.M5","ASPM.M6","ASPM.M7","ASPM.M8","ASPM.M9","ASPM.M10","ASPM.M11","ASPM.M12"), ordered = TRUE)
# Convert months into ordered factor.

ggplot() +
 geom_line(data=long_school_name_plot, mapping = aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
 geom_point(data=long_school_name_plot, mapping = aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
 geom_label(data = long_school_name_plot, aes(x = Month, y = Active.Students, label = Active.Students)) +
 geom_hline(data = specific_school_mean, aes(yintercept = mean_active_students_school, color = School.Name), linetype = "dashed") +
 geom_label(data = specific_school_mean, aes(
   x = levels(long_school_name_plot$Month)[length(levels(long_school_name_plot$Month))], 
   y = mean_active_students_school, 
   label = paste("Mean:", round(mean_active_students_school, 2)))) +
 labs(x = "Month", y = "Active Students", title = str_glue("Monthly Data for {long_school_name_plot$School.Name}")) +
 geom_line(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students), linetype = "dotdash") +
 geom_point(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students)) +
 geom_label(data = long_active_students_avg, aes(x = Month, y = Active.Students, label = paste(round(Active.Students, 2)))) +
 scale_fill_manual(values=c("#CC0000", "#006600", "#669999", "#00CCCC", 
                            "#660099", "#CC0066", "#FF9999", "#FF9900", 
                            "black", "black", "black", "black", "black"))
 scale_colour_manual(name = "Yes", values = c("{long_school_name_plot$School.Name}" = "#CC0000", "Mean" = "#669999"))
 
 
 
 # ------------ Per district Plots
 
 
 long_district_level_ASPM <- district_level_ASPM[[2]] %>%
   pivot_longer(cols = starts_with("ASPM.M"), 
                names_to = "Month", 
                values_to = "Active.Students")
 # Change the value inside [[]] to the corresponding district number (1 to 15)
 
 long_district_level_ASPM_2$Month <- factor(long_district_level_ASPM_2$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
 
 ggplot(data = long_district_level_ASPM_2, aes(x = Month, y = Active.Students, group = School.Name, colour = School.Name)) +
   geom_line() +
   geom_point() +
   labs(x = "Month", y = "Active Students", title = str_glue("Monthly Data for {district_level_ASPM[[2]]$District.Name}"))
   #geom_label(aes(label = Active.Students))
   #omitting for legibility

 ggplot(data = long_reg_stu, aes(x = District.Name, y =))









#{District 6 is doing something right by getting max signups.}

# TODO:
#Separate all the districts into their own dfs. DONE
#Active students per school vs time (average of dist is plotted as a st line.) DONE
#school with min active students in dist
#school with max active students in dist
#school with min/max learning/month in dist [group_by]



