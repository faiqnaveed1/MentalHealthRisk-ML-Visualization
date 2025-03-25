# MentalHealthRisk-ML-Visualization
Used R for statistical analysis of personal traits related to mental health illness development. Applied Chi-square test, created mosaic plots, ANOVA, strip charts, and box plots. Performed Tukey HSD test, and generated scree plots and biplots for data visualization and insights.


Data Set used:

pumf


Immigration Status vs. Anxiety:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCFIMM to a factor
pumf$SDCFIMM <- factor(pumf$SDCFIMM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCFIMM, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Immigration Status vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Gender Status vs. Anxiety:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GENDER to a factor
pumf$GENDER <- factor(pumf$GENDER, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GENDER, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis =0.4, main = "Gender vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


Age vs anxiety:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert DHHGAGEto a factor
pumf$DHHGAGE <- factor(pumf$DHHGAGE, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$DHHGAGE, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Age vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Sexuality vs anxiety 

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SORLGBTS a factor
pumf$SORLGBTS <- factor(pumf$SORLGBTS, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SORLGBTS, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Sexuality vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Location vs anxiety:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GEODVPSZa factor
pumf$GEODVPSZ <- factor(pumf$GEODVPSZ, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GEODVPSZ, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Location vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


(Visible Minority Group vs. Anxiety Disorder)

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCGVVMfactor
pumf$SDCGVVM <- factor(pumf$SDCGVVM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCGVVM, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Visible minority vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


Immigration Status vs. Depression:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCFIMM to a factor
pumf$SDCFIMM <- factor(pumf$SDCFIMM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCFIMM, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Immigration Status vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")



Gender Status vs. Depression:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GENDER to a factor
pumf$GENDER <- factor(pumf$GENDER, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GENDER, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Gender vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


Age vs Depression:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert DHHGAGEto a factor
pumf$DHHGAGE <- factor(pumf$DHHGAGE, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$DHHGAGE, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Age vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Sexuality vs Depression

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SORLGBTS a factor
pumf$SORLGBTS <- factor(pumf$SORLGBTS, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SORLGBTS, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Sexuality vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")








Location vs Depression:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GEODVPSZa factor
pumf$GEODVPSZ <- factor(pumf$GEODVPSZ, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GEODVPSZ, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Location vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")





Visible Minority Group vs. Depression

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCGVVMfactor
pumf$SDCGVVM <- factor(pumf$SDCGVVM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCGVVM, pumf$CCC280BA)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light green", "light blue"),
           cex.axis = 0.4, main = "Visible minority vs Depression",
           sub = "Presence of Anxiety", ylab = "Relative Frequency"

Immigration Status vs. Schizophrenia:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCFIMM to a factor
pumf$SDCFIMM <- factor(pumf$SDCFIMM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCFIMM, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Immigration Status vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Gender Status vs. Schizophrenia:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GENDER to a factor
pumf$GENDER <- factor(pumf$GENDER, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GENDER, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Gender vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


Age vs Schizophrenia:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert DHHGAGEto a factor
pumf$DHHGAGE <- factor(pumf$DHHGAGE, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$DHHGAGE, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Age vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Sexuality vs Schizophrenia

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SORLGBTS a factor
pumf$SORLGBTS <- factor(pumf$SORLGBTS, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SORLGBTS, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Sexuality vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")

Location vs Schizophrenia:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert GEODVPSZa factor
pumf$GEODVPSZ <- factor(pumf$GEODVPSZ, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$GEODVPSZ, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Location vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")



Visible Minority Group vs. Schizophrenia

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert SDCGVVMfactor
pumf$SDCGVVM <- factor(pumf$SDCGVVM, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$SDCGVVM, pumf$CCCDSCZO)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("light blue", "goldenrod1"),
           cex.axis = 0.4, main = "Visible minority vs Schizophrenia",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")




DATASET:

AD2 and pumf

Age vs anxiety:


CREATING THE MOSAIC PLOT AND CHI SQUARE TEST:

#input of dataset
pumf<- read. csv (“pumf.csv”)

# Convert DHHGAGEto a factor
pumf$DHHGAGE <- factor(pumf$DHHGAGE, 
                        levels = c(1, 2), 
                        labels = c("Immigrant", "Non-Immigrant"))

contingency_table_imm <- table(pumf$DHHGAGE, pumf$CCC_290A)

#cleaning data by removing rows with 0’s
contingency_table_imm <- contingency_table_imm[rowSums(contingency_table_imm) > 0, ]
contingency_table_imm <- contingency_table_imm[, colSums(contingency_table_imm) > 0]

#create the contingency table
print(contingency_table_imm)

#chi-square test
Xsq_imm <- chisq.test(contingency_table_imm, correct = FALSE)

#chi-square test results
Xsq_imm

#mosiac plot production
par(pty = "s") 
mosaicplot(t(contingency_table_imm), col = c("firebrick", "goldenrod1"),
           cex.axis = 0.4, main = "Age vs Anxiety",
           sub = "Presence of Anxiety", ylab = "Relative Frequency")


FOR THE CREATION OF ANOVA SET AND STRIP CHART FOR AGE VS ANXIETY

#input of dataset
AD2 <- read.csv("AD2.csv")

# creating the summary and head
summary(AD2)
head(AD2)

# Filter dataset for anxiety by age group
AD2 <- subset(AD2, Group == "By Age" & grepl("Anxiety", Indicator, ignore.case = TRUE))

# Order age groups as factors
AD2$Subgroup <- factor(anxiety_data$Subgroup, 
                                levels = c("18 - 29 years", "30 - 39 years", 
                                           "40 - 49 years", "50 - 59 years", 
                                           "60 - 69 years", "70 - 79 years", 
                                           "80 years and over"))



# Strip chart

ggplot(AD2, aes(x = Subgroup, y = Value)) +
  geom_point(size = 3, shape = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               colour = "black", width = 0.1,
               position = position_nudge(x = 0.15)) +
  stat_summary(fun = mean, geom = "point",
               size = 3,
               position = position_nudge(x = 0.15)) +
  labs(x = "Age Group", y = "Anxiety Level (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA
anxietyAnova <- lm(Value ~ Subgroup, data = AD2)
anova(anxietyAnova)


Wealth vs Anxiety:

CREATION OF BOX PLOT AND PRODUCTION OF ANOVA AND TUKEY HSD:

#input the data set
money <- read.csv("money.csv")

#summarize the set
summary(money)

#produce the unique values in dataset
unique(money)

#produce variable names in dataset
colnames(money)
        

# Convert wealth into categorical groups (quartiles)
money$wealth<- cut(money$wealth1, 
                                breaks = quantile(money$wealth1, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                labels = c("Low", "Medium-Low", "Medium-High", "High"),
                                include.lowest = TRUE)


# ANOVA 
wealthAnova <- lm(anx ~ wealth, data = money)
anova_result <- anova(wealthAnova)
print(anova_result)

# Tukey's HSD
TukeyHSD(aov(anx ~ wealth, data = money))


# Load required library
library(ggplot2)

# Boxplot
ggplot(money, aes(x = wealth, y = anx)) +
  geom_boxplot() +
  labs(x = "Wealth Group", y = "Anxiety Level (%)")


CREATION OF THE SCREE PLOT AND BIPLOT USING AD2 DATA:

#input the data set
AD2<- read.csv("AD2.csv")

#Conduction of PCA
AD2.pca <- princomp(scaledAD2)

summary(AD2.pca)

AD2.pca$sdev # standard deviations

AD2.pca$sdev^2 # eigenvalues/variances

AD2.pca$sdev^2 / sum(AD2.pca$sdev^2) # proportion of variance

#Production of Scree plot
get_eig(AD2.pca) # eigenvalues/variances

fviz_eig(AD2.pca, geom = "bar", bar_width = 0.4) +
  
  ggtitle("")

#Production of Biplot
fviz_pca_biplot(AD2.pca, label = "var") +
  ggtitle("PCA Biplot") +
  coord_fixed()







