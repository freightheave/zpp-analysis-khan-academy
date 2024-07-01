#This part of the code is separate from the original R file as this code generates PNGs of the plots to a specified folder.


# ---------------- DISTRICT LEVEL PLOT GENERATOR [OPTIONAL CODE] ----------------

for (val in 1:15) {
  district_data <- eval(parse(text = paste0("district_level_ASPM[[", val, "]]")))
  
  long_district_level_ASPM <- district_data %>%
    pivot_longer(cols = starts_with("ASPM.M"), 
                 names_to = "Month", 
                 values_to = "Active.Students")
  # Change the value inside [[]] to the corresponding district number (1 to 15)
  
  long_district_level_ASPM$Month <- factor(long_district_level_ASPM$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
  
  district_plot <- ggplot(data = long_district_level_ASPM, aes(x = Month, y = Active.Students, group = School.Name, colour = School.Name)) +
    geom_line() +
    geom_point() +
    labs(x = "Month", y = "Active Students", title = str_glue(paste0("Monthly Data for {district_level_ASPM[[",val,"]]$District.Name}")))
  #geom_label(aes(label = Active.Students))
  #omitting for legibility
  
  ggsave(
    filename = paste0("District Level ASPM", val, ".png"),
    plot = district_plot,
    device = "png",
    path = "PATH/TO/YOUR/FOLDER",
    width = 1366,
    height = 768,
    units = c("px"),
    dpi = 100
  )
}

# ---------------- DISTRICT LEVEL PERCENTAGE PLOT GENERATOR [OPTIONAL CODE] ----------------

for (val in 1:15) {
  percent_district_data <- eval(parse(text = paste0("percent_district_level_ASPM[[", val, "]]")))
  
  long_district_level_ASPM <- percent_district_data[,c(1:2,16:27)] %>%
    pivot_longer(cols = starts_with("M"), 
                 names_to = "Month", 
                 values_to = "Percent.Active.Students")
  # Change the value inside [[]] to the corresponding district number (1 to 15)
  
  long_district_level_ASPM$Month <- factor(long_district_level_ASPM$Month, levels = paste0("M", 1:12), ordered = TRUE)
  
  district_plot <- ggplot(data = long_district_level_ASPM, aes(x = Month, y = Percent.Active.Students, group = School.Name, colour = School.Name)) +
    geom_line() +
    geom_point() +
    labs(x = "Month", y = "Percentage of Active Students", title = str_glue(paste0("Monthly Data for {district_level_ASPM[[",val,"]]$District.Name}"))) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    geom_label(aes(
      x = length(long_district_level_ASPM$Month),
      y = 100,
      label = "Max"))
  #geom_label(aes(label = Percent.Active.Students))
  # omitting for legibility
  
  ggsave(
    filename = paste0("District Level Percent ASPM", val, ".png"),
    plot = district_plot,
    device = "png",
    path = "PATH/TO/YOUR/FOLDER",
    width = 1366,
    height = 768,
    units = c("px"),
    dpi = 100
  )
}


# ---------------- SCHOOL LEVEL PLOT GENERATOR [OPTIONAL CODE] ----------------

for (val in 1:155) {
  # Filter data for the current school
  temp_df <- reg_stu %>% filter(School.Name == paste0("ZPP ", val))
  
  # Calculate the mean of active students for the specific school
  specific_school_mean <- mean(rowMeans(temp_df[4:15], na.rm = TRUE))
  
  # Convert data to long format
  long_school_name_plot <- temp_df %>% 
    pivot_longer(cols = starts_with("ASPM.M"),
                 names_to = "Month",
                 values_to = "Active.Students")
  
  
  long_school_name_plot$Month <- factor(long_school_name_plot$Month, levels = paste0("ASPM.M", 1:12), ordered = TRUE)
  
  
  school_plot <- ggplot() +
    geom_line(data = long_school_name_plot, aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
    geom_point(data = long_school_name_plot, aes(x = Month, y = Active.Students, group = School.Name, color = School.Name)) +
    geom_label(data = long_school_name_plot, aes(x = Month, y = Active.Students, label = Active.Students)) +
    geom_hline(yintercept = specific_school_mean$mean_active_students_school, color = "#FF9900", linetype = "dashed") +
    geom_label(
      aes(
        x = length(long_school_name_plot$Month) - 0.5, 
        y = specific_school_mean$mean_active_students_school,
        label = glue("Mean ZPP {val}: {round(specific_school_mean$mean_active_students_school, 2)}")
      ),
      color = "#FF9900"
    ) +
    labs(x = "Month", y = "Active Students", title = str_glue("Monthly Data for {paste0('ZPP', val)}")) +
    geom_line(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students), linetype = "dotdash") +
    geom_point(data = long_active_students_avg, aes(x = Month, y = Active.Students, group = Average.Active.Students, color = Average.Active.Students)) +
    geom_label(data = long_active_students_avg, aes(x = Month, y = Active.Students, label = paste(round(Active.Students, 2)))) +
    scale_colour_manual(name = "Legend", values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                                    "#660099", "#CC0066", "#FF9999", "#FF9900", 
                                                    "black", "black", "black", "black", "black"))
  
  # Save the plot
  ggsave(
    filename = paste0("School", val, ".png"),
    plot = school_plot,
    device = "png",
    path = "PATH/TO/YOUR/FOLDER",
    width = 1750,
    height = 768,
    units = "px",
    dpi = 100
  )
}


# ---------------- SCHOOL LEVEL PERCENTAGE PLOT GENERATOR [OPTIONAL CODE] ----------------

for (val in 1:155) {
  temp_df <- reg_stu_with_percent %>% filter(School.Name == paste0("ZPP ", val))
  
  #percent_school_plot <- reg_stu_with_percent[reg_stu_with_percent$School.Name == "ZPP 145",]
  #storing name of school for plotting, change 'school in RHS to vary the school.
  #Now, need to convert school_name_plot into a long df.
  
  long_percent_school_plot <- temp_df %>% 
    pivot_longer(cols = starts_with("M"),
                 names_to = "Percent.Month",
                 values_to = "Percent.Active.Students")
  #Convert to long data format.
  
  long_percent_school_plot1 <- temp_df %>% 
    pivot_longer(cols = starts_with("ASPM.M"),
                 names_to = "Month",
                 values_to = "Active.Students")
  
  percent_data_combined <- cbind(long_percent_school_plot[, c("District.Name", "School.Name", "Total.Number.of.Students.Registered", "Percent.Month", "Percent.Active.Students")], long_percent_school_plot1[, c("Active.Students", "Month")])
  
  percent_data_combined$Percent.Month <- factor(percent_data_combined$Percent.Month, levels = paste0("M",1:12), ordered = TRUE)
  # Convert months into ordered factor.
  
  school_plot <- ggplot() +
    geom_line(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, group = School.Name, color = School.Name)) +
    geom_point(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, group = School.Name, color = School.Name)) +
    geom_label(data = percent_data_combined, aes(x = Percent.Month, y = Percent.Active.Students, label = paste0(round(Percent.Active.Students, 1)))) +
    geom_hline(yintercept = 100, color = "blue", linetype = "dashed") +
    geom_label(aes(
      x = length(percent_data_combined$Percent.Month) - 0.5,
      y = 100,
      label = "Max"
    )) +
    scale_y_continuous(breaks = seq(0, 150, by = 10)) +
    geom_line(data=long_medians, aes(x = Month, y = Median.Percentages, colour = "Median", group = 1)) +
    geom_point(data=long_medians, aes(x = Month, y = Median.Percentages, colour = "Median", group = 1)) +
    geom_label(data = long_medians, aes(x = Month, y = Median.Percentages, label = paste0(round(Median.Percentages,1)))) +
    labs(x = "Month", y = "Active Students as a % of Total Registrations", title = paste0("Percentage Data for ", percent_school_plot$School.Name))
  
  ggsave(
    filename = paste0("School_Percent", val, ".png"),
    plot = school_plot,
    device = "png",
    path = "D:/Repositories/zpp-analysis-khan-academy/bin",
    width = 1750,
    height = 768,
    units = "px",
    dpi = 100
  )
}



## ------------ Hours worked metric ------------

#Need to plot ideal hours - 30 * registered students [Overlay]
#Calculated - 30 mins * Active Students
#Real - real life.


for (val in 1:155) {
  string = paste0("ZPP ", val)
  
  df_hours <- act_hours[act_hours$School.Name == string,]
  
  long_df_hours <- df_hours %>% 
    pivot_longer(
      cols = starts_with("ALOKPAS.M"),
      names_to = ("Month"),
      values_to = ("Avg.Learning.Time")
    )
  #long data format for plotting learning hours.
  
  long_df_hours$Month <- factor(long_df_hours$Month, levels = paste0("ALOKPAS.M", 1:12), ordered = TRUE)
  
  school_plot <- ggplot() +
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
  
  # Save the plot
  ggsave(
    filename = paste0("School", val, ".png"),
    plot = school_plot,
    device = "png",
    path = "D:/Repositories/zpp-analysis-khan-academy/bin",
    width = 1750,
    height = 768,
    units = "px",
    dpi = 100
  )
}





## ------ District Level ---------

for (val in 1:15) {
  district_number_LH <- val
  
  long_district_level_ALOKPAS <- district_level_ALOKPAS[[district_number_LH]] %>%
    pivot_longer(cols = starts_with("ALOKPAS.M"), 
                 names_to = "Month", 
                 values_to = "Avg.Learning")
  # Change the value inside [[]] to the corresponding district number (1 to 15)
  
  long_district_level_ALOKPAS$Month <- factor(long_district_level_ALOKPAS$Month, levels = paste0("ALOKPAS.M", 1:12), ordered = TRUE)
  
  plot_dist <- ggplot(data = long_district_level_ALOKPAS, aes(x = Month, y = Avg.Learning, group = School.Name, colour = School.Name)) +
    geom_line() +
    geom_point() +
    labs(x = "Month", y = "Average Learning per Student  (mins) ", title = paste0("Monthly Data for District ", district_number_LH)) +
    geom_hline(yintercept = 30, linetype = "dashed") +
    geom_label(aes(
      x = length(long_df_hours$Month) - 1,
      y = 30,
      label = "Ideal Learning Time (mins)"
    )) +
    labs(x = "Month", y = "Avg. Learning Per Student (mins)", title = paste0("Learning Time data for District ", district_number_LH))
  #geom_label(aes(label = Avg.Learning))
  
  # Save the plot
  ggsave(
    filename = paste0("District", val, ".png"),
    plot = plot_dist,
    device = "png",
    path = "D:/Repositories/zpp-analysis-khan-academy/bin",
    width = 1750,
    height = 768,
    units = "px",
    dpi = 100
  )
}



rm(temp_df)