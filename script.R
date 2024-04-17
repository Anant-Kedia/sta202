#Importing relevant libraries
library(readxl)
library(dplyr)
library(corrplot)
library(writexl)
library(psych)

#Importing data from xlsx sheets
data_state <- read_excel("ASER 2023 Education Data.xlsx", sheet = "State Information")
data_14 <- read_excel("ASER 2023 Education Data.xlsx", sheet = "14-16")
data_17 <- read_excel("ASER 2023 Education Data.xlsx", sheet = "17-18")

#Get data for different genders
all_14 = filter(data_14, Gender=='All')
all_17 = filter(data_17, Gender=='All')
male_14 = filter(data_14, Gender=='Male')
male_17 = filter(data_17, Gender=='Male')
female_14 = filter(data_14, Gender=='Female')
female_17 = filter(data_17, Gender=='Female')


#Add state data into the datasets
state_all_14 = merge(all_14, data_state, by = 'State')
state_all_17 = merge(all_17, data_state, by = 'State')
state_male_14 = merge(male_14, data_state, by = 'State')
state_male_17 = merge(male_17, data_state, by = 'State')
state_female_14 = merge(female_14, data_state, by = 'State')
state_female_17 = merge(female_17, data_state, by = 'State')

#Rename headers
names(state_all_14) <- gsub(x = names(state_all_14), pattern = '-',
                            replacement = '_')
names(state_all_17) <- gsub(x = names(state_all_17), pattern = '-',
                            replacement = '_')
names(state_male_14) <- gsub(x = names(state_all_14), pattern = '-',
                             replacement = '_')
names(state_male_17) <- gsub(x = names(state_all_17), pattern = '-',
                             replacement = '_')
names(state_female_14) <- gsub(x = names(state_all_14), pattern = '-',
                               replacement = '_')
names(state_female_17) <- gsub(x = names(state_all_17), pattern = '-',
                               replacement = '_')

#Get data of South Indian states and North Indian states

s_states <- c('Andhra Pradesh', 'Karnataka', 'Kerala', 'Tamil Nadu', 'Telangana')
n_states <- c('Haryana', 'Himachal Pradesh', 'Jammu and Kashmir', 'Punjab', 'Rajasthan')
south_all_14 = dplyr::filter(state_all_14, State %in% s_states)
south_all_17 = dplyr::filter(state_all_17, State %in% s_states)
north_all_14 = dplyr::filter(state_all_14, State %in% n_states)
north_all_17 = dplyr::filter(state_all_17, State %in% n_states)

#Perform 2 sample independent t-tests

x = c(floor(sum(south_all_14$Sample_Youths * south_all_14$Reading)),
      floor(sum(north_all_14$Sample_Youths * north_all_14$Reading)))

n = c(sum(south_all_14$Sample_Youths), sum(north_all_14$Sample_Youths))

prop.test(x, n, correct = FALSE)

#Generate scatter plot matrix

pairs.panels(state_female_14[c(-1,-2,-14,-16,-17,-18)], density = FALSE, ellipses = FALSE,
             lm = TRUE, rug = FALSE)

#Get only relevant data for regression

rel_all_14 = subset(state_all_14, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))
rel_all_17 = subset(state_all_17, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))
rel_male_14 = subset(state_male_14, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))
rel_male_17 = subset(state_male_17, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))
rel_female_14 = subset(state_female_14, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))
rel_female_17 = subset(state_female_17, select = -c(State, Gender, District, Sample_Villages, Sample_Youths, Sample_Households))

#Generate correlation matrix and remove rows with nulls

corrplot(cor(rel_female_17, use = "complete.obs"), method = 'number', type = 'upper')

#Multiple Linear Regression Models
model_all_14 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                  Unenrolled + Smartphone_Posession +
                  Working + IX_X_Schools, data = rel_all_14)

model_all_17 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                    Unenrolled + Smartphone_Posession +
                    Working + XI_XII_Schools, data = rel_all_17)

model_male_14 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                     Unenrolled + Smartphone_Posession +
                     Working + IX_X_Schools, data = rel_male_14)

model_male_17 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                     Unenrolled + Smartphone_Posession +
                     Working + XI_XII_Schools, data = rel_male_17)

model_female_14 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                       Unenrolled + Smartphone_Posession +
                       Working + IX_X_Schools, data = rel_female_14)

model_female_17 = lm(formula = Reading ~ Enrol_X + Enrol_XI_XII + 
                       Unenrolled + Smartphone_Posession +
                       Working + XI_XII_Schools, data = rel_female_17)

#Model summary and diagnostic plots
plot(model_male_14)
summary(model_male_14)

#Additional Models after considering sample sizes

sample_data_female_14 = subset(state_female_14, select = c(3:13)) * t(subset(state_female_14, select = c(15)))
model_f14 = lm(formula = Reading ~ ., data = sample_data_female_14)

sample_data_female_17 = subset(state_female_17, select = c(3:13)) * t(subset(state_female_17, select = c(15)))
model_f17 = lm(formula = Reading ~ ., data = sample_data_female_17)

sample_data_male_14 = subset(state_male_14, select = c(3:13)) * t(subset(state_male_14, select = c(15)))
model_m14 = lm(formula = Reading ~ ., data = sample_data_male_14)

sample_data_male_17 = subset(state_male_17, select = c(3:13)) * t(subset(state_male_17, select = c(15)))
model_m17 = lm(formula = Reading ~ ., data = sample_data_male_17)

sample_data_all_14 = subset(state_all_14, select = c(3:13)) * t(subset(state_all_14, select = c(15)))
model_a14 = lm(formula = Arithmetic ~ ., data = sample_data_all_14)

sample_data_all_17 = subset(state_all_17, select = c(3:13)) * t(subset(state_all_17, select = c(15)))
model_a17 = lm(formula = Reading ~ ., data = sample_data_all_17)

#Additional Model summary and diagnostic plots
plot(model_a14)

summary(model_a14)

