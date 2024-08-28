library(ggplot2)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(ggpubr)
library(RVAideMemoire)
library(MatchIt)

#Read CSV
BMD<-read.csv("C:/Users/Sebastian/Desktop/Research/GFO Projecten/BMD perforation/Analyse GFO + UKK/BMD.csv",header = TRUE, sep = ";", dec = ",")

# Assuming your data frame is named 'BMD' and it includes:
# 'treatment' as the binary indicator (1 for GI perforation, 0 for without),
# 'age' as the patient's age,
# and other covariates...

# Step 1: Estimate Propensity Scores
# Logistic regression model: treatment ~ age + other covariates
ps_model <- glm(Group ~ Age+Gender, family = binomial(), data = BMD)

# Step 2: Match Data using optimal matching method
match_data <- matchit(Group ~ Age+Gender, data = BMD, method = "optimal", distance = ps_model$fitted.values)

# Step 3: Check Balance (optional but recommended)
summary(match_data)

# Step 4: Create a new, matched dataset
matched_data <- match.data(match_data)

# Step 5: Analyze the Matched Data
# Now you can proceed with your analysis on the 'matched_data' dataset

#Boxplot
ggplot(matched_data, aes(x=Perforation, y=BoneMinDensity, fill=Perforation)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(legend.position="right", plot.title = element_text(size=14)) +
  ggtitle("  Bone mineral density comparison") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  xlab("GI perforation") + ylab("BMD (mg/ml)") + 
  theme(axis.title.x = element_text(hjust=0.5),
    axis.title.y = element_text(hjust=0.5))+
  theme(axis.text = element_text(face="bold"))+
  theme(axis.title = element_text(face="bold"))

#Normality test and Wilcoxon test for statistical significance of BMD
shapiro.test(matched_data$BoneMinDensity[1:37])
shapiro.test(matched_data$BoneMinDensity[38:74])

p<-wilcox.test(matched_data$BoneMinDensity[1:37], matched_data$BoneMinDensity[38:74], alternative = "two.sided")
p$p.value

# Test for age/ gender differences between groups (first load .xlsx file)
levene<-leveneTest(BMD$Age[1:37], BMD$Age[38:74], alternative = "two.sided")
pAlt <- wilcox.test(matched_data$Age[1:37], matched_data$Age[38:74], alternative = "two.sided")
pAlt$p.value

# Test for gender differences between both groups (first load .xlsx file)
dat <- data.frame(
  "Male" = c(25, 23),
  "Female" = c(12, 14),
  row.names = c("Perforation", "No Perforation"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("Male", "Female")

dat

pGen<-chisq.test(dat)
pGen$p.value

# Test for BMD differences between gender per group (first load .xlsx file)
matched_data_perforation <- matched_data[38:74,]
matched_data_control <- matched_data[1:37,]

p<-wilcox.test(matched_data_perforation$BoneMinDensity[which(matched_data_perforation$Gender=="2")], matched_data_perforation$BoneMinDensity[which(matched_data_perforation$Gender=="1")], alternative = "two.sided")
p$p.value

p<-wilcox.test(matched_data_control$BoneMinDensity[which(matched_data_control$Gender=="2")], matched_data_control$BoneMinDensity[which(matched_data_perforation$Gender=="1")], alternative = "two.sided")
p$p.value

# Calculate Pearson correlation coefficients and significance between Age and BMD in both the perforation and the control groups
cor(matched_data_perforation$BoneMinDensity, matched_data_perforation$Age, method = c("pearson"))
cor.test(matched_data_perforation$BoneMinDensity, matched_data_perforation$Age, method=c("pearson"))

cor(matched_data_control$BoneMinDensity, matched_data_control$Age, method = c("pearson"))
cor.test(matched_data_control$BoneMinDensity, matched_data_control$Age, method=c("pearson"))