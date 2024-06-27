library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# # ---------------- Optional Stuff ----------------
#cbbPalette <- c("blue", "green", "yellow", "red")
#cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# ---------------- Data Reading/Manipulation ----------------

me1 <- read.csv("C:/Users/troll/Downloads/M&E Insight Analyst Assignment - Data set - Copy.csv")
# replace with <path to the data.csv file> when uploading.

reg_stu <- data.frame(me1[,1:15])
#df with only student counts (no active hours)

act_hours <- data.frame(me1[,c(1,2,17:28)])
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

summary(me1[,17:28])
#summary of active learning on KA across ZPS

months <- colnames(reg_stu)

# ---------------- Basic Calc. ----------------

mean_active_students_school <- rowMeans(reg_stu[4:15]) 
#across months per school. [maybe % of registered students is better?]

mean_active_students_school <- data.frame(mean_active_students_school) 
# convert to df

mean_active_students_school$School.Name <- reg_stu$School.Name 
# add school name to the new data frame

total_mean <- reg_stu %>% summarise(total_mean_students = mean(Total.Number.of.Students.Registered))
# total mean of students

student_mean_per_dist <- reg_stu %>% group_by(District.Name) %>% summarise(mean = mean(Total.Number.of.Students.Registered)) 
# mean of students per dist.



# ----------- Plot for Registered Students Per District -----------

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
  labs(x = "Month", y = "Active Students", title = "Monthly Data for Schools")
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

ggplot(data=long_school_name_plot, mapping = aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
  geom_line() +
  geom_point() +
  geom_label(data = long_school_name_plot, aes(x = Month, y = Active.Students, label = Active.Students)) +
  geom_hline(data = specific_school_mean, aes(yintercept = mean_active_students_school, color = School.Name), linetype = "dashed") +
  geom_label(data = specific_school_mean, aes(
    x = levels(long_school_name_plot$Month)[length(levels(long_school_name_plot$Month))], 
    y = mean_active_students_school, 
    label = paste("Mean:", round(mean_active_students_school, 2)))) +
  labs(x = "Month", y = "Active Students", title = str_glue("Monthly Data for {long_school_name_plot$School.Name}"))





#Separate all the districts into their own dfs.
#{District 6 is doing something right by getting max signups.}

# TODO:
#Active students per school vs time (average of dist is plotted as a st line.)
#school with min active students in dist
#school with max active students in dist
#school with min/max learning/month in dist [group_by]

#---------------- in dev ------------------

# df2 <- reg_stu %>% 
#   gather(Col, Val, -District.Name) 
# 
# 
# 
# ggplot(df2, aes(Col, Val, group = District.Name, col = District.Name)) +
#   geom_line() +
#   facet_grid(District.Name ~ .)
# 
# Group by COLUMNS and PLOT with facets. (Needs data source fix)
# df %>%
#   gather(Col, Val, -Col0) %>%
#   ggplot(aes(Col, Val, group = Col0, col = Col0)) +
#   geom_line() +
#   facet_grid(Col0 ~ .)
# 
# df2 <- reg_stu %>% 
#   gather(Col, Val, -District.Name) 
# 
# 
# 
# ggplot(df2, aes(Col, Val, group = District.Name, col = District.Name)) +
#   geom_line() +
#   facet_grid(District.Name ~ .)

