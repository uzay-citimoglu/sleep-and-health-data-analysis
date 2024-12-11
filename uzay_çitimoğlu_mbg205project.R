remove()

sleep$scorrelation_testsleep$sleep_duration [sleep$sleep_duration > 15 ] <- NA
data <- na.omit(data)

unique(sleep$sleep_duration)

data <- sleep
data <- merge(data, sd, by = "ID", all.x = TRUE)
data <- merge(data, hp, by = "ID", all.x = TRUE)
data <- merge(data, bmi, by = "ID", all.x = TRUE)
data <- merge(data, cortisol, by = "ID", all.x = TRUE)
data <- merge(data, cognitive, by = "ID", all.x = TRUE)
data <- merge(data, pathology, by = "ID", all.x = TRUE)
data <- merge(data, immunostain, by = "ID", all.x = TRUE)
data <- merge(data, bdnf, by = "ID",all.x=TRUE)
#------------------------------------------------------------------------------
cortest_data <- subset(sleep, select = -sleep_difficulty)

cortest_data <- na.omit(cortest_data)

cor.test(cortest_data$sleep_duration, cortest_data$night_wakes)

# Create a customized scatter plot
ggplot(cortest_data, aes(x = sleep_duration, y = night_wakes)) +
  geom_point(size = 3, color = "black", shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Scatter Plot of Sleep Duration vs. Night Wakes",
       x = "Sleep Duration (hours)",
       y = "Number of Night Wakes") +
  theme_minimal()
#------------------------------------------------------------------------------
sex <- subset(sd, select = -age)
sex <- subset(sex, select = -work)
sex <- subset(sex, select = -marriage)
sex <- subset(sex, select = -health_rate)
sex <- subset(sex, select = -smoke)
sex <- subset(sex, select = -sleep_medicine)

duration <- subset(sleep, select = -sleep_difficulty)
duration <- subset(duration, select = -night_wakes)

t_testdata <- sex
t_testdata <- merge(t_testdata, duration, by="ID", all.x = TRUE)



t_testdata <- na.omit(t_testdata)

t.test(sleep_duration ~ sex, data = t_testdata)

ggplot(data, aes(x = sex, y = sleep_duration, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sleep Duration by Sex",
       x = "Sex",
       y = "Sleep Duration (hours)") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "mediumorchid1")) +
  theme(legend.position = "none")
#------------------------------------------------------------------------------
hp1 <- na.omit(hp)

linear_testdata <- merge(hp, duration, by="ID", all.x = TRUE)
linear_testdata <- na.omit(linear_testdata)

model <- lm(sleep_duration ~ mental_health_score + physical_health_score + hypertension + stroke + parkinson + diabetes + cancer + breath_shortness, data = linear_testdata)
model
ggplot(linear_testdata, aes(x = mental_health_score, y = sleep_duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "green") +
  labs(title = "Scatter Plot of Sleep Duration vs Mental Health Score",
       x = "Mental Health Score",
       y = "Sleep Duration (hours)") +
  theme_minimal()

ggplot(linear_testdata, aes(x = physical_health_score, y = sleep_duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Scatter Plot of Sleep Duration vs Physical Health Score",
       x = "Physical Health Score",
       y = "Sleep Duration (hours)") +
  theme_minimal()
#------------------------------------------------------------------------------
cortest_data2 <- merge(bmi, duration, by="ID", all.x = TRUE)
cortest_data2 <- na.omit(cortest_data2)

cor.test(cortest_data2$sleep_duration, cortest_data2$bmi)

ggplot(cortest_data2, aes(x = sleep_duration, y = bmi)) +
  geom_point(size = 3, color = "darkmagenta", shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "aquamarine") +
  labs(title = "Scatter Plot of Sleep Duration vs. BMI",
       x = "Sleep Duration (hours)",
       y = "BMIs") +
  theme_minimal()
#------------------------------------------------------------------------------
cortisol1 <- na.omit(cortisol)

cormatrix1 <- merge(cortisol1, duration, by='ID', all.x=TRUE)

cormatrix <- na.omit(cormatrix)

cormatrix$ID[cormatrix$ID == 'P1343'] <- NA
cor(cormatrix[c("sleep_duration", "cortisol_9am", 
                "cortisol_10am", "cortisol_11am", "cortisol_14pm", 
                "cortisol_18pm", "cortisol_22pm")])

ggplot(cormatrix, aes(x = sleep_duration, y = cortisol_22pm)) +
  geom_point(size = 3, color = "black", shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Scatter Plot of Sleep Duration vs. Cortisol Levels 22 PM",
       x = "Sleep Duration (hours)",
       y = "Cortisol Levels 22 PM") +
  theme_minimal()
#------------------------------------------------------------------------------
cognitive <- na.omit(cognitive)
pairedttest <- t.test(cognitive$cognitive_error_1, cognitive$cognitive_error_2, paired = TRUE)
pairedttest

install.packages("reshape2")
library(reshape2)

data_long <- melt(cognitive, variable.name = "Cognitive_Error_Type", value.name = "Score")


boxplot(Score ~ Cognitive_Error_Type, data = data_long,
        main = "Boxplot of Cognitive Error 1 and 2",
        xlab = "Cognitive Error Type",
        ylab = "Score",
        col = c("lightblue", "lightgreen"))
#------------------------------------------------------------------------------
anova<-merge(pathology,duration,by="ID", all.x=TRUE)
anova<-na.omit(anova)

anova$Alzheimer_score <- gsub("0-I", "I", anova$Alzheimer_score)
anova$Alzheimer_score <- gsub("I-II", "II", anova$Alzheimer_score)
anova$Alzheimer_score <- gsub("II-III", "III", anova$Alzheimer_score)
anova$Alzheimer_score <- gsub("III-IV", "IV", anova$Alzheimer_score)
anova$Alzheimer_score <- gsub("IV-V", "V", anova$Alzheimer_score)
anova$Alzheimer_score <- gsub("V-VI", "VI", anova$Alzheimer_score)

anova_result <- aov(sleep_duration ~ Alzheimer_score, data = anova)
anova_result

post_hoc <- TukeyHSD(anova_result, "Alzheimer_score")
post_hoc
plot(post_hoc)

unique(anova$Alzheimer_score)
anova$Alzheimer_score [anova$Alzheimer_score == 'IIII' ] <- 'III'

ggplot(anova, aes(x = Alzheimer_score, y = sleep_duration, fill = Alzheimer_score)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Sleep Duration by Alzheimer Score",
    x = "Alzheimer Score",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal() +  theme(legend.position = "none")
#------------------------------------------------------------------------------
cor_testdata3<-merge(duration,immunostain,by="ID", all.x=TRUE)
cor_testdata3<-na.omit(cor_testdata3)

correlation_test <- cor.test(cor_testdata3$sleep_duration, cor_testdata3$Hippocampus_immunostain)
correlation_test

boxplot(cor_testdata3$sleep_duration, cor_testdata3$Hippocampus_immunostain,
        main = "Boxplot for Sleep duration and Immunostaining",
        xlab = "Hippocampus immunostaining",
        ylab = "Sleep Duration(Hours)",
        col = c("lightblue", "pink"))

süper3lü<-merge(anova,immunostain,by="ID", all.x=TRUE)
süper3lü<-merge(süper3lü,cognitive,by="ID", all.x=TRUE)
süper3lü<-na.omit(süper3lü)

süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == '0' ] <- 0
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'I' ] <- 1
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'II' ] <- 2
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'III' ] <- 3
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'IV' ] <- 4
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'V' ] <- 5
süper3lü$Alzheimer_score [süper3lü$Alzheimer_score == 'VI' ] <- 6

süper3lü$Alzheimer_score <- as.numeric(süper3lü$Alzheimer_score)

cor(süper3lü[c("Alzheimer_score", "sleep_duration", 
               "Hippocampus_immunostain", "cognitive_error_1", 
               "cognitive_error_2")])
#------------------------------------------------------------------------------
chi_test <- subset(sd, select = -age)
chi_test <- subset(chi_test, select = -work)
chi_test <- subset(chi_test, select = -smoke)
chi_test <- subset(chi_test, select = -sleep_medicine)
chi_test <- subset(chi_test, select = -sex)
chi_test <- na.omit(chi_test)
chi_testdata <-merge(duration,chi_test,by="ID", all.x=TRUE)
contingency_table <- table(chi_test$marriage, chi_test$health_rate)

print(contingency_table)

chi_square_result <- chisq.test(contingency_table)

chi_square_result

ggplot(chi_test, aes(x = marriage, fill = health_rate)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Bar Plot of Health Rating by Marital Status",
    x = "Marital Status",
    y = "Count",
    fill = "Health Rating"
  ) +
  theme_minimal()
#------------------------------------------------------------------------------
bdnf1 <- na.omit(bdnf1)
bdnf1 <- merge(immunostain,bdnf,by="ID", all.x=TRUE)
cor.test(bdnf1$Hippocampus_immunostain, bdnf1$BDNF_gene_expression)
cor.test(bdnf1$Hippocampus_immunostain, bdnf1$BDNF_protein_concentration)


ggplot(bdnf1, aes(x = Hippocampus_immunostain, y = BDNF_gene_expression )) +
  geom_point(size = 3, color = "black", shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "red4") +
  labs(title = "Scatter Plot of Hippocampus Immunostain vs. BDNF Gene Expression",
       x = "Hippocampus Immunostain",
       y = "BDNF Gene Expression") +
  theme_minimal()

ggplot(bdnf1, aes(x = Hippocampus_immunostain, y = BDNF_protein_concentration)) +
  geom_point(size = 3, color = "black", shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "green4") +
  labs(title = "Scatter Plot of Hippocampus Immunostain vs. BDNF Protein Concentration",
       x = "Hippocampus Immunostain",
       y = "BDNF Protein Concentration") +
  theme_minimal()
