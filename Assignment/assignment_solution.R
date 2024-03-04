#setwd("/Users/sherrybhalla/projects/computational_biology_course/assignment")
metabrick <- read_csv('metabric_sample_file.csv')
metabrick=as.data.frame(metabrick)
metabrick$Chemotherapy
class(metabrick)

library("tidyverse")
metabrick$Chemotherapy


#Count the number of patients in each tumor stage category
table(metabrick$Tumour_stage)
class(metabrick$Tumour_stage)
metabrick %>% count(Tumour_stage)
#Given the dataset, how would you filter out patients who did not receive any chemotherapy or radiotherapy?  (3 Marks)
subset=metabrick %>% filter(Chemotherapy == "NO" & Radiotherapy == "NO")
dim(subset)
# 2nd way
table(metabrick$Chemotherapy,metabrick$Radiotherapy)
colnames(metabrick)
table(metabrick$ER_status)
# Create a new binary variable for TNBC
metabrick <- metabrick %>%
  mutate(TNBC = ifelse(ER_status == "Negative" & PR_status == "Negative" & HER2_status == "Negative", "Yes", "No"))

table(metabrick$TNBC)
colnames(metabrick)
scatter_plot <- ggplot(metabrick, aes(x = Age_at_diagnosis, y = Survival_time)) +
  geom_point() +
  labs(x = "Age at Diagnosis", y = "Survival Time")
scatter_plot


# Create a boxplot
boxplot <- ggplot(metabrick, aes(x = Cancer_type, y = Tumour_size)) +
  geom_boxplot() +
  labs(x = "Cancer Type", y = "Tumour Size")

# Print the boxplot
print(boxplot)

# improve the plot
boxplot + theme(axis.text.x = element_text(angle = 90))

boxplot + theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                     size = 12, angle = 45,hjust=1),
          axis.text.y = element_text(face = "bold", color = "blue", 
                                     size = 12, angle = 45))

# Vertical rotation of x axis text
scatter_plot <- ggplot(metabrick, aes(x = Tumour_size, y = Lymph_nodes_examined_positive)) +
  geom_point() +
  labs(x = "Tumour Size", y = "Lymph Node Positivity") +
  facet_wrap(~ Cancer_type)

# Print the scatter plot
print(scatter_plot)
