install.packages("ggplot2")
install.packages("kableExtra")
install.packages("webshot")
install.packages("tidyr")
install.packages("reshape2")
install.packages("scales")
install.packages("dplyr")
install.packages("knitr")
install.packages("kableExtra")
install.packages("readxl")
webshot::install_phantomjs()

# Load the necessary packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(scales)
library(dplyr)
library(knitr)
library(kableExtra)
library(webshot)
library(readxl)


#Qualitative Analysis
# Creat vectors
Precision <- c(0.5, 0.25, 0.25, 0.67, 0.5, 0.5, 0.25, 0.25, 0.25, 1.0, 0.5, 0.5, 0.33, 0.0, 0.25, 
               1.0, 0.33, 0.66, 0.25, 0.25, 0.66, 1.0, 1.0, 0.5, 0.5, 0.5, 0.5, 0.0, 0.5, 1.0)
Recall <- c(1.00, 0.50, 0.50, 0.50, 0.67, 0.33, 0.50, 0.50, 0.50, 0.75, 0.50, 0.50, 1.00, 0.00, 1.00, 
            1.00, 0.50, 0.67, 0.50, 0.50, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)
F1 <- c(0.67, 0.33, 0.33, 0.57, 0.57, 0.40, 0.33, 0.33, 0.33, 0.86, 0.50, 0.50, 0.40, 0.00, 0.40, 
        1.00,0.40, 0.80, 0.33, 0.33, 0.80, 1.00, 1.00, 0.57, 0.67, 0.67, 0.67, 0.00, 0.67, 1.00)

# Create data frames
data <- data.frame(
  Precision = Precision,
  Recall = Recall,
  F1 = F1,
  Model = rep(c("GPT4O", "Gemini Advanced", "GPT4"), times= 10),
  Patient = rep(1:10, times = 3)
)

# Calculate the F1 score for each model
mean_scores <- data %>%
  group_by(Model) %>%
  summarise(mean_F1 = mean(F1))
# Calculate the average Recall for each model
mean_recall <- data %>%
  group_by(Model) %>%
  summarise(mean_Recall = mean(Recall))
# Calculate the average Precision for each model
mean_precision <- data %>%
  group_by(Model) %>%
  summarise(mean_Precision = mean(Precision))

# Create the F1 score plot 
F1_plot<-ggplot(data, aes(x = F1, y = Model, color = Model)) +
  geom_point(shape = 16, size = 3) +  # Use solid circles for individual scores
  scale_color_manual(values = c("GPT4O" = "#1f77b4", "Gemini Advanced" = "#ff7f0e", "GPT4" = "#2ca02c")) +  # Set colors manually
  geom_point(data = mean_scores, aes(x = mean_F1, y = Model), 
             shape = 17, color = "#d62728", size = 4) +  # Add mean values as yellow triangles
  theme_minimal() +  # Use a minimal theme
  labs(
    x = "F1 Score",
    y = "Model",
    color = "Model"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered title
    axis.text.x = element_text(size = 12),  # X-axis font size
    axis.text.y = element_text(size = 12),  # Y-axis font size
    legend.position = "top",   # Legend on top
    legend.title = element_text(size = 12),   # Legend title font size
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  ggtitle("F1 Scores Distribution Across Models")
print(F1_plot)

# Save F1 Score Plot as pdf
ggsave("F1_score_plot.pdf", plot = F1_plot,width = 8, height = 6)


# Create the Recall score plot 
recall_plot <- ggplot(data, aes(x = Recall, y = Model, color = Model)) +
  geom_point(shape = 16, size = 3) +  # Use solid circles for individual scores
  scale_color_manual(values = c("GPT4O" = "#1f77b4", "Gemini Advanced" = "#ff7f0e", "GPT4" = "#2ca02c")) +  # Set colors manually (consistent)
  geom_point(data = mean_recall, aes(x = mean_Recall, y = Model), 
             shape = 17, color = "#d62728", size = 4) +  # Add mean values as red triangles (consistent with F1 plot)
  theme_minimal() +  # Use a minimal theme
  labs(
    x = "Recall",
    y = "Model",
    color = "Model"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered title
    axis.text.x = element_text(size = 12),  # X-axis font size
    axis.text.y = element_text(size = 12),  # Y-axis font size
    legend.position = "top",  # Legend on top
    legend.title = element_text(size = 12),  # Legend title font size
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  ggtitle("Recall Scores Distribution Across Models")

print(recall_plot)

# Save Recall score plot as PDF
ggsave("Recall_Scores_Distribution.pdf", plot = recall_plot, width = 8, height = 6)



# Create the Precision score plot 
precision_plot <- ggplot(data, aes(x = Precision, y = Model, color = Model)) +
  geom_point(shape = 16, size = 3) +  # Use solid circles for individual scores
  scale_color_manual(values = c("GPT4O" = "#1f77b4", "Gemini Advanced" = "#ff7f0e", "GPT4" = "#2ca02c")) +  # Set colors manually (consistent)
  geom_point(data = mean_precision, aes(x = mean_Precision, y = Model), 
             shape = 17, color = "#d62728", size = 4) +  # Add mean values as red triangles (consistent with F1 plot)
  theme_minimal() +  # Use a minimal theme
  labs(
    x = "Precision",
    y = "Model",
    color = "Model"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered title
    axis.text.x = element_text(size = 12),  # X-axis font size
    axis.text.y = element_text(size = 12),  # Y-axis font size
    legend.position = "top",  # Legend on top
    legend.title = element_text(size = 12),  # Legend title font size
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  ggtitle("Precision Scores Distribution Across Models")
print(precision_plot)

# Save Precision score plot as PDF
ggsave("Precision_Scores_Distribution.pdf", plot = precision_plot, width = 8, height = 6)



#AI test Analysis
# Read the Excel file (assuming the sheet is named 'AI test')
file_path <- "C:/Users/15812/Desktop/AI test.xlsx"
data1 <- read_excel(file_path, sheet = "AI test1")
data2 <- read_excel(file_path, sheet = "AI test2")
data3 <- read_excel(file_path, sheet = "AI test3")
data4 <- read_excel(file_path, sheet = "AI test4")
data5 <- read_excel(file_path, sheet = "AI test5")
data6 <- read_excel(file_path, sheet = "AI test6")
data7 <- read_excel(file_path, sheet = "AI test7")
data8 <- read_excel(file_path, sheet = "AI test8")
data9 <- read_excel(file_path, sheet = "AI test9")
data10 <- read_excel(file_path, sheet = "AI test10")

# Specify the columns for which to calculate the mean
columns_to_summarize <- c("Human", "GPT4", "GPT4O", "Gemini Advanced")

# Calculate the mean for each table and add a table number
means_sheet1 <- data1 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test1")

means_sheet2 <- data2 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test2")

means_sheet3 <- data3 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test3")

means_sheet4 <- data4 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test4")

means_sheet5 <- data5 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test5")

means_sheet6 <- data6 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test6")

means_sheet7 <- data7 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test7")

means_sheet8 <- data8 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test8")

means_sheet9 <- data9 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test9")

means_sheet10 <- data10 %>%
  select(all_of(columns_to_summarize)) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Sheet = "AI test10")

# Combine the means of all tables
combined_means <- bind_rows(
  means_sheet1, means_sheet2, means_sheet3, means_sheet4, means_sheet5,
  means_sheet6, means_sheet7, means_sheet8, means_sheet9, means_sheet10
)

# Convert to 'long format' for use with ggplot
combined_means_long <- combined_means %>%
  pivot_longer(cols = -Sheet, names_to = "Model", values_to = "Mean")

# Plotting
ggplot(combined_means_long, aes(x = Mean, y = Sheet, color = Model)) +
  geom_point(size = 4) +  # Use scatter plot
  scale_x_continuous(breaks = c(0, 2.5, 5.0, 7.5, 10.0), limits = c(0, 10.0)) +  # Customize x-axis range
  scale_y_discrete(labels = c("AI test10","AI test9","AI test8","AI test7","AI test6","AI test5", "AI test4", "AI test3", "AI test2", "AI test1")) +  # Customize y-axis labels
  labs(
    x = "Mean Score",  # x-axis label
    y = "AI test (AI test1-AI test10)",  # y-axis label
    title = "Average Scores by Model Across Sheets",  # Title
    color = "Model"  # Legend title
  ) +
  theme_minimal() +  # Use minimal theme
  theme(
    axis.line.x = element_line(color = "black"),  # Add solid line on x-axis
    axis.line.y = element_line(color = "black"),  # Add solid line on y-axis
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "grey80"),  # Retain horizontal grid lines
    axis.title = element_text(size = 12),  # Axis titles font size
    axis.text = element_text(size = 10),   # Axis text font size
    legend.title = element_text(size = 12),  # Legend title font size
    legend.text = element_text(size = 10)   # Legend text font size
  ) +
  # Add blue dashed lines
  geom_vline(xintercept = 0, linetype = "dashed", color = "deepskyblue", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "deepskyblue", size = 1) +
  # Add annotations "Least likely" and "Most likely"
  annotate("text", x = 0, y = 3.5, label = "Least likely", color = "dodgerblue", size = 4, hjust = -0.1) +
  annotate("text", x = 10, y = 3.5, label = "Most likely", color = "dodgerblue", size = 4, hjust = 1.1) +
  # Add tick mark annotations
  annotate("segment", x = 0, xend = 0, y = -0.2, yend = 0, color = "black") +
  annotate("segment", x = 2.5, xend = 2.5, y = -0.2, yend = 0, color = "black") +
  annotate("segment", x = 5.0, xend = 5.0, y = -0.2, yend = 0, color = "black") +
  annotate("segment", x = 7.5, xend = 7.5, y = -0.2, yend = 0, color = "black") +
  annotate("segment", x = 10.0, xend = 10.0, y = -0.2, yend = 0, color = "black")



# Save plot as pdf
ggsave("plot.pdf", device = "pdf", width = 10, height = 8, units = "in")





#Clinical Trial Analysis
# Create a data frame.
data <- data.frame(
  model = c("GPT4O", "Gemini Advanced", "GPT4"),
  invalid = c(17, 23, 9),
  total = c(37, 34, 22)
)

# Calculate the effective number of experiments.
data$valid <- data$total - data$invalid

# Convert the data to long format for plotting.
data_long <- melt(data, id.vars = "model", measure.vars = c("invalid", "valid"))

# set colors
colors <- c("GPT4O" = "seagreen", "Gemini Advanced" = "steelblue", "GPT4" = "hotpink")

  # Draw a horizontal bar chart.
p <- ggplot(data_long, aes(x = model, y = value, fill = interaction(model, variable))) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Gemini Advanced.invalid" = "lightblue", "Gemini Advanced.valid" = "steelblue",
    "GPT4O.invalid" = "lightgreen", "GPT4O.valid" = "seagreen",
    "GPT4.invalid" = "lightpink", "GPT4.valid" = "hotpink"
  )) +
  labs(title = "Comparison of Valid and Invalid Clinical Trial Recommendations",
       x = "Models", y = "Number of Trials") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_blank()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # Set the legend to 2 rows, arranged by rows.

print(p)


# Save the chart in PDF format.
ggsave("comparison_of_trials.pdf", plot = p, device = "pdf", width = 8, height = 6)





#Repeatability Analysis

# Create sample data.
data <- data.frame(
  Patient = rep(1:10, each = 4),
  Model = rep(c("Human", "Gemini Advanced", "GPT4O", "GPT4"), times = 10),
  Overlap = c(5, 1, 4, 2, 6, 3, 5, 2, 6, 2, 2, 3, 5, 3, 4, 2, 
              5, 2, 4, 2, 5, 2, 3, 2, 4, 1, 1, 2, 5, 4, 3, 4,
              4, 2, 2, 2, 4, 1, 2, 2)
)

# Color mapping: navy blue represents overlap ≥ 3, steel blue represents overlap = 2, and light blue represents overlap = 1.
data$color <- ifelse(data$Overlap >= 3, "navyblue",
                     ifelse(data$Overlap == 2, "steelblue", "lightblue"))

# Create a new dataset, retaining only the points in navy blue (Overlap ≥ 3).
deep_blue_data <- data[data$Overlap >= 3, ]

# Plot the chart and modify the x-axis.
ggplot(data, aes(x = Patient, y = Model)) +
  # Plot all the points.
  geom_point(aes(color = color), size = 5) +
  # Only connect the navy blue points.
  geom_line(data = deep_blue_data, aes(group = Patient), linetype = "solid", color = "darkblue") +
  # Use custom x-axis labels.
  scale_x_continuous(breaks = 1:10, labels = paste0("P", 1:10)) +
  scale_color_identity() + 
  theme_minimal() +
  labs(title = "LLM vs Human Treatment Overlap",
       x = "Patient",
       y = "Model")

# Save the chart in PDF format.
ggsave("LLM_vs_Human_Treatment_Overlap.pdf", width = 8, height = 6)




# Create sample data: patient, overlapping options, and total options.
data <- data.frame(
  Patient = 1:10,
  Gemini_Advanced = c(0.4444, 0.4545, 0.2000, 0.4444, 0.3333, 0.3750, 0.0909, 0.3000, 0.1818, 0.2857),
  GPT4O = c(0.1250, 0.3750, 0.2500, 0.5000, 0.2222, 0.2222, 0.1111, 0.4444, 0.2000, 0.1250),
  GPT4 = c(0.2500, 0.2000, 0.3750, 0.2500, 0.2000, 0.2857, 0.2500, 0.4444, 0.1818, 0.2000)
)

# Use the gather function to convert wide-format data into long-format.
data_long <- gather(data, key = "Model", value = "Overlap", -Patient)

# Convert the overlap ratio to a percentage.
data_long$Overlap <- data_long$Overlap * 100

# Draw a bar chart in percentage form.
p <- ggplot(data_long, aes(x = factor(Patient), y = Overlap, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge to separate the bars in the bar chart.
  scale_fill_manual(values = c("Gemini_Advanced" = "lightblue",   # Light blue
                               "GPT4O" = "steelblue",        # Moderate blue.
                               "GPT4" = "navyblue")) +      # Navyblue
  scale_y_continuous(labels = percent_format(scale = 1)) +   # Format the Y-axis labels as percentages.
  labs(title = "Overlap Rate for 3 Models Across Patients",
       x = "Patient",
       y = "Overlap Rate (%)",   # Display the Y-axis labels as percentages.
       fill = "Model") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),   # Center the title.
    axis.text.x = element_text(size = 12),               # X-axis label font size.
    axis.text.y = element_text(size = 12),               # Y-axis label font size.
    legend.title = element_text(size = 12),              # Legend title font size.
    legend.text = element_text(size = 10)                # Legend content font size.
  )

# print plot
print(p)

# Save the chart in PDF format.
ggsave("Overlap_Rate_for_3_Models_Across_Patients.pdf", plot = p, width = 8, height = 6)



# Create a data frame: model name and the total number of treatment options generated by each.
data <- data.frame(
  Model = c("Human", "Gemini Advanced", "GPT4O", "GPT4"),
  Total_Treatment_Options = c(49, 84, 100, 90)  
)

# Draw a horizontal bar chart.
p <- ggplot(data, aes(x = Model, y = Total_Treatment_Options)) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  coord_flip() +  
  labs(title = "Total Treatment Options Generated by Models",
       x = "Model",
       y = "No. of Treatment Options") +
  theme_minimal()

# print plot
print(p)

# Save the chart in PDF format.
ggsave("Total_Treatment_Options_Generated_by_Models.pdf", plot = p, width = 8, height = 6)

