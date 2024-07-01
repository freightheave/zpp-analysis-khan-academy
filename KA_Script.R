library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(glue)

# # ---------------- Optional Stuff ----------------
#cbbPalette <- c("blue", "green", "yellow", "red")
cbbPalette <- c("#CC0000", "#006600", "#669999", "#00CCCC", 
                "#660099", "#CC0066", "#FF9999", "#FF9900", 
                "black", "black", "black", "black", "black")


# ---------------- Data Reading/Manipulation ----------------

me1 <- read.csv("D:/Repositories/zpp-analysis-khan-academy/M&E Insight Analyst Assignment - Data set.csv")
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
# total mean of registered students (UNUSED)

student_mean_per_dist <- reg_stu %>% group_by(District.Name) %>% summarise(mean = mean(Total.Number.of.Students.Registered)) 
# mean of registered students per dist. (UNUSED)

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

school_name <- "ZPP 69"

school_name_plot <- reg_stu[reg_stu$School.Name == school_name,]
#storing name of school for plotting, change 'school in RHS to vary the school.
#Now, need to convert school_name_plot into a long df.

specific_school_mean <- mean_active_students_school %>% filter(School.Name == school_name)
#specific mean picked from mean_active_students_school to overlay on to total graph.

long_school_name_plot <- school_name_plot %>% 
  pivot_longer(cols = starts_with("ASPM.M"),
               names_to = "Month",
               values_to = "Active.Students")
#Convert to long data format.

long_school_name_plot$Month <- factor(long_school_name_plot$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
# Convert months into ordered factor.

ggplot() +
  geom_line(data = long_school_name_plot, aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
  geom_point(data = long_school_name_plot, aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
  geom_label(data = long_school_name_plot, aes(x = Month, y = Active.Students, label = Active.Students)) +
  labs(x = "Month", y = "Active Students", title = paste0('ZPP', 145)) +
  geom_line(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students), linetype = "dotdash") +
  geom_point(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students)) +
  geom_label(data = long_active_students_avg, aes(x = Month, y = Active.Students, label = paste(round(Active.Students, 2)))) +
  geom_hline(yintercept = specific_school_mean$mean_active_students_school, colour = "#FF9900", linetype = "dashed") +
  geom_label(aes(
    x = max(as.numeric(long_school_name_plot$Month)),
    y = specific_school_mean$mean_active_students_school,
    label = paste0("Mean:", round(specific_school_mean$mean_active_students_school))
  ), colour = "#FF9900")
# Plot for school with school mean across months and mean of school per month.
  
  
 # ------------ Per district Plots ------------
 
district_number <- 3
 
long_district_level_ASPM <- district_level_ASPM[[district_number]] %>%
   pivot_longer(cols = starts_with("ASPM.M"), 
                names_to = "Month", 
                values_to = "Active.Students")
# Change the value inside [[]] to the corresponding district number (1 to 15)

long_district_level_ASPM$Month <- factor(long_district_level_ASPM$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)

ggplot(data = long_district_level_ASPM, aes(x = Month, y = Active.Students, group = School.Name, colour = School.Name)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Active Students", title = paste0("Monthly Data for District ", district_number))
  #geom_label(aes(label = Active.Students))
  #omitting for legibility

## ---------------- NORMALISED COMPARISONS ----------------

reg_stu_with_percent <- reg_stu

for (val in 1:12) {
  col_name1 <- paste0("M", val)
  col_name2 <- sym(paste0("ASPM.M", val))
  
  reg_stu_with_percent <- reg_stu_with_percent %>%
    mutate(!!col_name1 := round((!!col_name2 / Total.Number.of.Students.Registered) * 100, 2))
}

# ----------- Per dist. percentages & calc. -----------

percent_district_level_ASPM <- list()

# Loop through values 1 to 15
for (val in 1:15) {
  # Subset the data based on District.Name
  percent_district_level_ASPM[[paste0("dis_ASPM", val)]] <- data.frame(subset(reg_stu_with_percent, District.Name == paste0("District ", val)))
}

median_percentages_reg_stu <- reg_stu_with_percent %>% summarise(across(16:27, \(x) median(x, na.rm = TRUE)))
#Calc. Median percentages of active users.

mean_percentages_reg_stu <- reg_stu_with_percent %>% summarise(across(16:27, \(x) mean(x, na.rm = TRUE)))
# Mean doesn't make as much sense for directly computing on %data, still can be handy to have it calculated.

long_medians <- median_percentages_reg_stu %>% 
  pivot_longer(cols = starts_with("M"),
               names_to = "Month",
               values_to = "Median.Percentages")


# ----------- Per School percentage calc. -----------

percent_school_name <- "ZPP 145"

percent_school_plot <- reg_stu_with_percent[reg_stu_with_percent$School.Name == percent_school_name,]
# storing name of school for plotting, change 'school in RHS to vary the school.
# Now, need to convert school_name_plot into a long df.

long_percent_school_plot <- percent_school_plot %>% 
  pivot_longer(cols = starts_with("M"),
               names_to = "Percent.Month",
               values_to = "Percent.Active.Students")
#Convert to long data format.

long_percent_school_plot1 <- percent_school_plot %>% 
  pivot_longer(cols = starts_with("ASPM.M"),
               names_to = "Month",
               values_to = "Active.Students")

percent_data_combined <- cbind(long_percent_school_plot[, c("District.Name", "School.Name", "Total.Number.of.Students.Registered", "Percent.Month", "Percent.Active.Students")], long_percent_school_plot1[, c("Active.Students", "Month")])

rm(long_percent_school_plot)
rm(long_percent_school_plot1)
# Freeing up temp variables.

percent_data_combined$Percent.Month <- factor(percent_data_combined$Percent.Month, levels = paste0("M",1:12), ordered = TRUE)
# Convert months into ordered factor.

ggplot() +
  geom_line(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, group = School.Name, color = School.Name)) +
  geom_point(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, group = School.Name, color = School.Name)) +
  geom_label(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, label = paste0(round(Percent.Active.Students, 1)))) +
  geom_hline(yintercept = 100, color = "blue", linetype = "dashed") +
  geom_label(aes(
    x = max(as.numeric(percent_data_combined$Percent.Month)) - 0.5,
    y = 100,
    label = "Max"
  )) +
  scale_y_continuous(breaks = seq(0, 150, by = 10)) +
  geom_line(data=long_medians, aes(x = Month, y = Median.Percentages, colour = "Median", group = 1)) +
  geom_point(data=long_medians, aes(x = Month, y = Median.Percentages, colour = "Median", group = 1)) +
  geom_label(data = long_medians, aes(x = Month, y = Median.Percentages, label = paste0(round(Median.Percentages,1)))) +
  labs(x = "Month", y = "Active Students as a % of Total Registrations", title = paste0("Percentage Data for ", percent_school_plot$School.Name))
   

## ------------ Hours worked metric ------------

#Need to plot ideal hours - 30 * registered students [Overlay]
#Calculated - 30 mins * Active Students
#Real - real life.

string = "ZPP 145"

df_hours <- act_hours[act_hours$School.Name == string,]

long_df_hours <- df_hours %>% 
  pivot_longer(
    cols = starts_with("ALOKPAS.M"),
    names_to = ("Month"),
    values_to = ("Avg.Learning.Time")
  )
#long data format for plotting learning hours.

long_df_hours$Month <- factor(long_df_hours$Month, levels = paste0("ALOKPAS.M", 1:12), ordered = TRUE)

ggplot() +
  geom_line(data = long_df_hours, aes(x = Month, y = Avg.Learning.Time, colour = School.Name, group = School.Name)) +
  geom_point(data = long_df_hours, aes(x = Month, y = Avg.Learning.Time, colour = School.Name, group = School.Name)) +
  geom_label(data = long_df_hours, aes(x = Month, y = Avg.Learning.Time, label = Avg.Learning.Time)) +
  geom_hline(yintercept = 30, linetype = "dashed") +
  geom_label(aes(
    x = length(long_df_hours$Month) - 2,
    y = 30,
    label = "Ideal Learning Time (mins)"
  )) +
  labs(x = "Month", y = "Avg. Learning Per Student (mins)", title = paste0("Learning Time data for ", string))


## ------ District Level ---------

district_number_LH <- 3

long_district_level_ALOKPAS <- district_level_ALOKPAS[[district_number_LH]] %>%
  pivot_longer(cols = starts_with("ALOKPAS.M"), 
               names_to = "Month", 
               values_to = "Avg.Learning")
# Change the value inside [[]] to the corresponding district number (1 to 15)

long_district_level_ALOKPAS$Month <- factor(long_district_level_ALOKPAS$Month, levels = paste0("ALOKPAS.M", 1:12), ordered = TRUE)

ggplot(data = long_district_level_ALOKPAS, aes(x = Month, y = Avg.Learning, group = School.Name, colour = School.Name)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Average Learning per Student  (mins) ", title = paste0("Monthly Data for District ", district_number_LH))
#geom_label(aes(label = Avg.Learning))


## Find list of schools who performed the best!
## Criteria is to have avg learning on KA more than the recommended amount of 30 mins.
## Answer is ZPP 42, 45, 70, 71, 91, 103, 124.

performers_hours <- list()

for (school_number in 1:155) {
  hours_val = 0
  for (cols in 3:14) {
    if(act_hours[school_number, cols] >= 30) {
      hours_val = hours_val + 1
    }
  }
  
  if (hours_val >= 9) {
    len <- length(performers)
    performers[[len+1]] <- (act_hours[school_number,2])
  }
}

## Find list of schools who consistently had more than the avg number of active users for that month.

active_performers <- list()

for (school_number in 1:155) {
  students_val = 0
  for (cols in 4:15) {
    if(reg_stu[school_number, cols] >= active_students_avg[1,(cols-3)]) {
      students_val = students_val + 1
    }
  }
  
  if (students_val >= 9) {
    len <- length(performers)
    active_performers[[len+1]] <- (reg_stu[school_number,2])
  }
}













