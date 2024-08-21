library(openxlsx)

ori_UKBB <- read.xlsx("/Users/pyc/Desktop/research project/UKBB_cancers copy.xlsx")
ori_UKBB_copy <- ori_UKBB


###################################################################################
library(dplyr)

## 1
BC_filter <- ori_UKBB %>%
  filter(
    type_of_cancer_icd10_f40006_0_0 %in% paste0("C50", 0:9) | is.na(type_of_cancer_icd10_f40006_0_0),
    if_else(is.na(type_of_cancer_icd10_f40006_0_0), type_of_cancer_icd10_f40006_1_0 %in% paste0("C50", 0:9) | is.na(type_of_cancer_icd10_f40006_1_0), TRUE),
    if_else(is.na(type_of_cancer_icd10_f40006_0_0) & is.na(type_of_cancer_icd10_f40006_1_0), type_of_cancer_icd10_f40006_2_0 %in% paste0("C50", 0:9) | is.na(type_of_cancer_icd10_f40006_2_0), TRUE),
    if_else(is.na(type_of_cancer_icd10_f40006_0_0) & is.na(type_of_cancer_icd10_f40006_1_0) & is.na(type_of_cancer_icd10_f40006_2_0), type_of_cancer_icd10_f40006_3_0 %in% paste0("C50", 0:9) | is.na(type_of_cancer_icd10_f40006_3_0), TRUE),
    if_else(is.na(type_of_cancer_icd10_f40006_0_0) & is.na(type_of_cancer_icd10_f40006_1_0) & is.na(type_of_cancer_icd10_f40006_2_0) & is.na(type_of_cancer_icd10_f40006_3_0), type_of_cancer_icd10_f40006_4_0 %in% paste0("C50", 0:9) | is.na(type_of_cancer_icd10_f40006_4_0), TRUE)
  )
# 473169 obs left



# Filter the participants with BC before the baseline time
baseline_filter <- BC_filter %>%
  filter(CancerPreAssessment == "FALSE" | is.na(CancerPreAssessment))
# 460789 obs left



# age data
baseline_filter <- baseline_filter %>%
  mutate(age_at_BC_diagnosed = case_when(
    type_of_cancer_icd10_f40006_0_0 %in% paste0("C50", 0:9) ~ age_at_cancer_diagnosis_f40008_0_0,
    type_of_cancer_icd10_f40006_1_0 %in% paste0("C50", 0:9) ~ age_at_cancer_diagnosis_f40008_1_0,
    type_of_cancer_icd10_f40006_2_0 %in% paste0("C50", 0:9) ~ age_at_cancer_diagnosis_f40008_2_0,
    type_of_cancer_icd10_f40006_3_0 %in% paste0("C50", 0:9) ~ age_at_cancer_diagnosis_f40008_3_0,
    type_of_cancer_icd10_f40006_4_0 %in% paste0("C50", 0:9) ~ age_at_cancer_diagnosis_f40008_4_0,
    is.na(type_of_cancer_icd10_f40006_0_0) ~ NA,
    is.na(type_of_cancer_icd10_f40006_1_0) ~ NA,
    is.na(type_of_cancer_icd10_f40006_2_0) ~ NA,
    is.na(type_of_cancer_icd10_f40006_3_0) ~ NA,
    is.na(type_of_cancer_icd10_f40006_4_0) ~ NA,
  ))

baseline_filter$age_at_BC_diagnosed <- 
  ifelse(is.na(baseline_filter$type_of_cancer_icd10_f40006_0_0) & is.na(baseline_filter$type_of_cancer_icd10_f40006_1_0) & is.na(baseline_filter$type_of_cancer_icd10_f40006_2_0)
         & is.na(baseline_filter$type_of_cancer_icd10_f40006_3_0) & is.na(baseline_filter$type_of_cancer_icd10_f40006_4_0), baseline_filter$age_when_attended_assessment_centre_f21003_0_0,
         baseline_filter$age_at_BC_diagnosed)

# If ethnic background = -1, -3, transfer them to NA
baseline_filter$ethnic_background_f21000_0_0.x <- 
  ifelse(baseline_filter$ethnic_background_f21000_0_0.x == -3 | 
         baseline_filter$ethnic_background_f21000_0_0.x == -1, NA, baseline_filter$ethnic_background_f21000_0_0.x)



## Age at the BC diagnosis
BC_age_data <- baseline_filter %>%
  select(starts_with("type_of_cancer_icd10"), age_at_BC_diagnosed)


## Exclude the samples with all data missing
UKBB_filtered_1 <- baseline_filter[apply(baseline_filter[, !(names(baseline_filter) %in% c("X1", "eid"))], 1, function(x) !all(is.na(x))), ]
# 231655 obs left 


# Transfer energy data into numeric
UKBB_filtered_1 <- UKBB_filtered_1 %>%
  mutate(across(c(energy_f100002_0_0, energy_f100002_1_0, energy_f100002_2_0, energy_f100002_3_0, energy_f100002_4_0), as.numeric))

## Separate assessments into different data frame, 0_0, 1_0, 2_0
Assessment.0 <- UKBB_filtered_1 %>%
  select(ends_with("0_0"))

Assessment.1 <- UKBB_filtered_1 %>%
  select(ends_with("1_0"))

Assessment.2 <- UKBB_filtered_1 %>%
  select(ends_with("2_0"))



# Exclude dietary data when individuals with energy < 2029 or > 14644 (extreme energy)
# Assessment_0_0
rows_to_na.0 <- !is.na(Assessment.0$energy_f100002_0_0) & 
  (Assessment.0$energy_f100002_0_0 < 2092 | Assessment.0$energy_f100002_0_0 > 14644)

Assessment.0 <- Assessment.0 %>%
  mutate(across(everything(), ~ ifelse(rows_to_na.0, NA, .)))

# Assessment_1_0
rows_to_na.1 <- !is.na(Assessment.1$energy_f100002_1_0) & 
  (Assessment.1$energy_f100002_1_0 < 2092 | Assessment.1$energy_f100002_1_0 > 14644)

Assessment.1 <- Assessment.1 %>%
  mutate(across(everything(), ~ ifelse(rows_to_na.1, NA, .)))

# Assessment_2_0
rows_to_na.2 <- !is.na(Assessment.2$energy_f100002_2_0) & 
  (Assessment.2$energy_f100002_2_0 < 2092 | Assessment.2$energy_f100002_2_0 > 14644)

Assessment.2 <- Assessment.2 %>%
  mutate(across(everything(), ~ ifelse(rows_to_na.2, NA, .)))



## Cooked vegetable frequency
# If the frequency is -1 or -3, transfer them to NA
cooked.veg_0_0 <- Assessment.0$cooked_vegetable_intake_f1289_0_0
cooked.veg_0_0 <- ifelse(cooked.veg_0_0 == -1 | cooked.veg_0_0 == -3, NA, cooked.veg_0_0)
cooked.veg_0_0 <- ifelse(cooked.veg_0_0 == -10, 0.5, cooked.veg_0_0) #If -10, transfer to 0.5
head(cooked.veg_0_0, 100) #check

cooked.veg_1_0 <- Assessment.1$cooked_vegetable_intake_f1289_1_0
cooked.veg_1_0 <- ifelse(cooked.veg_1_0 == -1 | cooked.veg_1_0 == -3, NA, cooked.veg_1_0)
cooked.veg_1_0 <- ifelse(cooked.veg_1_0 == -10, 0.5, cooked.veg_1_0)
head(cooked.veg_1_0, 100)

cooked.veg_2_0 <- Assessment.2$cooked_vegetable_intake_f1289_2_0
cooked.veg_2_0 <- ifelse(cooked.veg_2_0 == -1 | cooked.veg_2_0 == -3, NA, cooked.veg_2_0)
cooked.veg_2_0 <- ifelse(cooked.veg_2_0 == -10, 0.5, cooked.veg_2_0)
head(cooked.veg_2_0, 100)

## Raw vegetable frequency
raw.veg_0_0 <- Assessment.0$salad_raw_vegetable_intake_f1299_0_0
raw.veg_0_0 <- ifelse(raw.veg_0_0 == -1 | raw.veg_0_0 == -3, NA, raw.veg_0_0)
raw.veg_0_0 <- ifelse(raw.veg_0_0 == -10, 0.5, raw.veg_0_0)
head(raw.veg_0_0, 200)

raw.veg_1_0 <- Assessment.1$salad_raw_vegetable_intake_f1299_1_0
raw.veg_1_0 <- ifelse(raw.veg_1_0 == -1 | raw.veg_1_0 == -3, NA, raw.veg_1_0)
raw.veg_1_0 <- ifelse(raw.veg_1_0 == -10, 0.5, raw.veg_1_0)
head(raw.veg_1_0, 200)

raw.veg_2_0 <- Assessment.2$salad_raw_vegetable_intake_f1299_2_0
raw.veg_2_0 <- ifelse(raw.veg_2_0 == -1 | raw.veg_2_0 == -3, NA, raw.veg_2_0)
raw.veg_2_0 <- ifelse(raw.veg_2_0 == -10, 0.5, raw.veg_2_0)
head(raw.veg_2_0, 200)

## Fresh fruit frequency
fresh.fruit_0_0 <- Assessment.0$fresh_fruit_intake_f1309_0_0
fresh.fruit_0_0 <- ifelse(fresh.fruit_0_0 == -1 | fresh.fruit_0_0 == -3, NA, fresh.fruit_0_0)
fresh.fruit_0_0 <- ifelse(fresh.fruit_0_0 == -10, 0.5, fresh.fruit_0_0)
head(fresh.fruit_0_0, 200)

fresh.fruit_1_0 <- Assessment.1$fresh_fruit_intake_f1309_1_0
fresh.fruit_1_0 <- ifelse(fresh.fruit_1_0 == -1 | fresh.fruit_1_0 == -3, NA, fresh.fruit_1_0)
fresh.fruit_1_0 <- ifelse(fresh.fruit_1_0 == -10, 0.5, fresh.fruit_1_0)
head(fresh.fruit_1_0, 200)

fresh.fruit_2_0 <- Assessment.2$fresh_fruit_intake_f1309_2_0
fresh.fruit_2_0 <- ifelse(fresh.fruit_2_0 == -1 | fresh.fruit_2_0 == -3, NA, fresh.fruit_2_0)
fresh.fruit_2_0 <- ifelse(fresh.fruit_2_0 == -10, 0.5, fresh.fruit_2_0)
head(fresh.fruit_2_0, 200)

## Dried fruit frequency
dried.fruit_0_0 <- Assessment.0$dried_fruit_intake_f1319_0_0
dried.fruit_0_0 <- ifelse(dried.fruit_0_0 == -1 | dried.fruit_0_0 == -3, NA, dried.fruit_0_0)
dried.fruit_0_0 <- ifelse(dried.fruit_0_0 == -10, 0.5, dried.fruit_0_0)
head(dried.fruit_0_0, 100)

dried.fruit_1_0 <- Assessment.1$dried_fruit_intake_f1319_1_0
dried.fruit_1_0 <- ifelse(dried.fruit_1_0 == -1 | dried.fruit_1_0 == -3, NA, dried.fruit_1_0)
dried.fruit_1_0 <- ifelse(dried.fruit_1_0 == -10, 0.5, dried.fruit_1_0)
head(dried.fruit_1_0, 100)

dried.fruit_2_0 <- Assessment.2$dried_fruit_intake_f1319_2_0
dried.fruit_2_0 <- ifelse(dried.fruit_2_0 == -1 | dried.fruit_2_0 == -3, NA, dried.fruit_2_0)
dried.fruit_2_0 <- ifelse(dried.fruit_2_0 == -10, 0.5, dried.fruit_2_0)
head(dried.fruit_2_0, 100)


## Processed meat frequency
processed.meat_0_0 <- Assessment.0$processed_meat_intake_f1349_0_0
processed.meat_0_0 <- ifelse(processed.meat_0_0 == -1 | processed.meat_0_0 == -3, NA, processed.meat_0_0)
head(processed.meat_0_0, 200)

processed.meat_1_0 <- Assessment.1$processed_meat_intake_f1349_1_0
processed.meat_1_0 <- ifelse(processed.meat_1_0 == -1 | processed.meat_1_0 == -3, NA, processed.meat_1_0)
head(processed.meat_1_0, 200)

processed.meat_2_0 <- Assessment.2$processed_meat_intake_f1349_2_0
processed.meat_2_0 <- ifelse(processed.meat_2_0 == -1 | processed.meat_2_0 == -3, NA, processed.meat_2_0)
head(processed.meat_2_0, 200)


## Poultry intake frequency
poultry_0_0 <- Assessment.0$poultry_intake_f1359_0_0
poultry_0_0 <- ifelse(poultry_0_0 == -1 | poultry_0_0 == -3, NA, poultry_0_0)
head(poultry_0_0, 200)

poultry_1_0 <- Assessment.1$poultry_intake_f1359_1_0
poultry_1_0 <- ifelse(poultry_1_0 == -1 | poultry_1_0 == -3, NA, poultry_1_0)
head(poultry_1_0, 200)

poultry_2_0 <- Assessment.2$poultry_intake_f1359_2_0
poultry_2_0 <- ifelse(poultry_2_0 == -1 | poultry_2_0 == -3, NA, poultry_2_0)
head(poultry_2_0, 200)


## Beef intake frequency
beef_0_0 <- Assessment.0$beef_intake_f1369_0_0
beef_0_0 <- ifelse(beef_0_0 == -1 | beef_0_0 == -3, NA, beef_0_0)
head(beef_0_0, 200)

beef_1_0 <- Assessment.1$beef_intake_f1369_1_0
beef_1_0 <- ifelse(beef_1_0 == -1 | beef_1_0 == -3, NA, beef_1_0)
head(beef_1_0, 200)

beef_2_0 <- Assessment.2$beef_intake_f1369_2_0
beef_2_0 <- ifelse(beef_2_0 == -1 | beef_2_0 == -3, NA, beef_2_0)
head(beef_2_0, 200)


## Lamb mutton intake frequency
lamb_0_0 <- Assessment.0$lambmutton_intake_f1379_0_0
lamb_0_0 <- ifelse(lamb_0_0 == -1 | lamb_0_0 == -3, NA, lamb_0_0)
head(lamb_0_0, 200)

lamb_1_0 <- Assessment.1$lambmutton_intake_f1379_1_0
lamb_1_0 <- ifelse(lamb_1_0 == -1 | lamb_1_0 == -3, NA, lamb_1_0)
head(lamb_1_0, 200)

lamb_2_0 <- Assessment.2$lambmutton_intake_f1379_2_0
lamb_2_0 <- ifelse(lamb_2_0 == -1 | lamb_2_0 == -3, NA, lamb_2_0)
head(lamb_2_0, 200)


## Pork intake frequency
pork_0_0 <- Assessment.0$pork_intake_f1389_0_0
pork_0_0 <- ifelse(pork_0_0 == -1 | pork_0_0 == -3, NA, pork_0_0)
head(pork_0_0, 200)

pork_1_0 <- Assessment.1$pork_intake_f1389_1_0
pork_1_0 <- ifelse(pork_1_0 == -1 | pork_1_0 == -3, NA, pork_1_0)
head(pork_1_0, 200)

pork_2_0 <- Assessment.2$pork_intake_f1389_2_0
pork_2_0 <- ifelse(pork_2_0 == -1 | pork_2_0 == -3, NA, pork_2_0)
head(pork_2_0, 200)

## Bread intake frequency
bread_0_0 <- Assessment.0$bread_intake_f1438_0_0
bread_0_0 <- ifelse(bread_0_0 == -1 | bread_0_0 == -3, NA, bread_0_0)
bread_0_0 <- ifelse(bread_0_0 == -10, 0.5, bread_0_0)
head(bread_0_0, 200)

bread_1_0 <- Assessment.1$bread_intake_f1438_1_0
bread_1_0 <- ifelse(bread_1_0 == -1 | bread_1_0 == -3, NA, bread_1_0)
bread_1_0 <- ifelse(bread_1_0 == -10, 0.5, bread_1_0)
head(bread_1_0, 200)

bread_2_0 <- Assessment.2$bread_intake_f1438_2_0
bread_2_0 <- ifelse(bread_2_0 == -1 | bread_2_0 == -3, NA, bread_2_0)
bread_2_0 <- ifelse(bread_2_0 == -10, 0.5, bread_2_0)
head(bread_2_0, 200)


## Cereal intake frequency
cereal_0_0 <- Assessment.0$cereal_intake_f1458_0_0
cereal_0_0 <- ifelse(cereal_0_0 == -1 | cereal_0_0 == -3, NA, cereal_0_0)
cereal_0_0 <- ifelse(cereal_0_0 == -10, 0.5, cereal_0_0)
head(cereal_0_0, 200)

cereal_1_0 <- Assessment.1$cereal_intake_f1458_1_0
cereal_1_0 <- ifelse(cereal_1_0 == -1 | cereal_1_0 == -3, NA, cereal_1_0)
cereal_1_0 <- ifelse(cereal_1_0 == -10, 0.5, cereal_1_0)
head(cereal_1_0, 200)

cereal_2_0 <- Assessment.2$cereal_intake_f1458_2_0
cereal_2_0 <- ifelse(cereal_2_0 == -1 | cereal_2_0 == -3, NA, cereal_2_0)
cereal_2_0 <- ifelse(cereal_2_0 == -10, 0.5, cereal_2_0)
head(cereal_2_0, 200)

## Alcohol intake score (take the first column of alcohol data)
alcohol_0_0 <- Assessment.0$alcohol_intake_frequency_f1558_0_0


alcohol <- data.frame(alcohol_0_0)

# Alcohol WCRF score
alcohol <- alcohol %>% mutate(score.alcohol_0_0 = recode(alcohol_0_0,
                                                         "Never"= 1,
                                                         "Special occasions only" = 1,
                                                         "One to three times a month" = 0.5,
                                                         "Daily or almost daily" = 0,
                                                         "Three or four times a week" = 0,
                                                         "Once or twice a week" = 0,
                                                         .default = NA_real_))


as.numeric(alcohol$score.alcohol_0_0)


###### Oily and non-oily fish
oily_fish <- Assessment.0$oily_fish_intake_f1329_0_0
oily_fish <- ifelse(oily_fish == -1 | oily_fish == -3, NA, oily_fish)
head(oily_fish, 200)

non_oily_fish <- Assessment.0$nonoily_fish_intake_f1339_0_0
non_oily_fish <- ifelse(non_oily_fish == -1 | non_oily_fish == -3, NA, non_oily_fish)
head(non_oily_fish, 200)



############################# Assessment_0_0 #######################################

## vegetables and fruits intake 
Cooked.vegetable.intake_0_0 <- cooked.veg_0_0*35
Salad.raw.vegetable.intake_0_0 <- raw.veg_0_0*35
Fresh.fruit.intake_0_0 <- fresh.fruit_0_0*118
Dried.fruit.intake_0_0 <- dried.fruit_0_0*10

## fibre content 
fibre.Cooked.vegetable_0_0 <- cooked.veg_0_0*1
fibre.raw.vegetable_0_0 <- raw.veg_0_0*1
fibre.Fresh.fruit_0_0 <- fresh.fruit_0_0*2
fibre.Dried.fruit_0_0 <- dried.fruit_0_0*0.5


# Bread fibre intake
df.bread_0_0 <- data.frame(bread_0_0, Assessment.0$bread_type_f1448_0_0)

# According to different bread types to calculate the fibre intake (per portion) for bread intake
fibre.breads.0 <- df.bread_0_0 %>%
  mutate(bread_0_0 = case_when(
    Assessment.0.bread_type_f1448_0_0 == 1 ~ bread_0_0 * 0.68,
    Assessment.0.bread_type_f1448_0_0 == 2 ~ bread_0_0 * 1.26,
    Assessment.0.bread_type_f1448_0_0 == 3 ~ bread_0_0 * 1.80,
    Assessment.0.bread_type_f1448_0_0 == 4 ~ bread_0_0 * 1.25,
    is.na(Assessment.0.bread_type_f1448_0_0) ~ bread_0_0 * 1.25,
    TRUE ~ bread_0_0
  ))
# extract bread intake data
fibre.Bread_0_0 <- fibre.breads.0$bread_0_0

# Cereal fibre intake
df.cereal_0_0 <- data.frame(cereal_0_0, Assessment.0$cereal_type_f1468_0_0)

# According to different cereal types to calculate the fibre intake (per portion) for cereal intake
fibre.cereals.0 <- df.cereal_0_0 %>%
  mutate(cereal_0_0 = case_when(
    Assessment.0.cereal_type_f1468_0_0 == 1 ~ cereal_0_0 * 7.16,
    Assessment.0.cereal_type_f1468_0_0 == 2 ~ cereal_0_0 * 2.92,
    Assessment.0.cereal_type_f1468_0_0 == 3 ~ cereal_0_0 * 1.92,
    Assessment.0.cereal_type_f1468_0_0 == 4 ~ cereal_0_0 * 4.18,
    Assessment.0.cereal_type_f1468_0_0 == 5 ~ cereal_0_0 * 0.54,
    is.na(Assessment.0.cereal_type_f1468_0_0) ~ cereal_0_0 * 3.34,
    TRUE ~ cereal_0_0
  ))

fibre.Cereal_0_0 <- fibre.cereals.0$cereal_0_0



## red and processed meat
Processed.meat.intake_0_0 <- processed.meat_0_0*52.5
Beef.intake_0_0 <- beef_0_0*120
Lamb.mutton.intake_0_0 <- lamb_0_0*120
Pork.intake_0_0 <- pork_0_0*120
Poultry.intake_0_0 <- poultry_0_0*130

## ultra-processed foods (aUPFs)
energy.bread_0_0 <- bread_0_0*103 # sliced bread
energy.cereal_0_0 <-cereal_0_0*150 # take average of energy (per portion) of cereal types

## The total grams, kcals, or units of food they intake per day or per week(for meat & alcohol).
intake_total.0 <- data.frame(Cooked.vegetable.intake_0_0, Salad.raw.vegetable.intake_0_0, Fresh.fruit.intake_0_0, Dried.fruit.intake_0_0, 
                             fibre.Cooked.vegetable_0_0, fibre.raw.vegetable_0_0, fibre.Fresh.fruit_0_0, fibre.Dried.fruit_0_0, fibre.Bread_0_0, fibre.Cereal_0_0,
                             Processed.meat.intake_0_0, Beef.intake_0_0, Lamb.mutton.intake_0_0, Pork.intake_0_0, Poultry.intake_0_0,
                             energy.bread_0_0, energy.cereal_0_0)



oily.fish.intake <- oily_fish*100
non.oily.fish.intake <- non_oily_fish*100


############################# Assessment_1_0 #######################################

## vegetables and fruits intake 
Cooked.vegetable.intake_1_0 <- cooked.veg_1_0*35
Salad.raw.vegetable.intake_1_0 <- raw.veg_1_0*35
Fresh.fruit.intake_1_0 <- fresh.fruit_1_0*118
Dried.fruit.intake_1_0 <- dried.fruit_1_0*10

## fibre content 
fibre.Cooked.vegetable_1_0 <- cooked.veg_1_0*1
fibre.raw.vegetable_1_0 <- raw.veg_1_0*1
fibre.Fresh.fruit_1_0 <- fresh.fruit_1_0*2
fibre.Dried.fruit_1_0 <- dried.fruit_1_0*0.5


# Bread fibre intake
df.bread_1_0 <- data.frame(bread_1_0, Assessment.1$bread_type_f1448_1_0)

# According to different bread types to calculate the fibre intake (per portion) for bread intake
fibre.breads.1 <- df.bread_1_0 %>%
  mutate(bread_1_0 = case_when(
    Assessment.1.bread_type_f1448_1_0 == 1 ~ bread_1_0 * 0.68,
    Assessment.1.bread_type_f1448_1_0 == 2 ~ bread_1_0 * 1.26,
    Assessment.1.bread_type_f1448_1_0 == 3 ~ bread_1_0 * 1.80,
    Assessment.1.bread_type_f1448_1_0 == 4 ~ bread_1_0 * 1.25,
    is.na(Assessment.1.bread_type_f1448_1_0) ~ bread_1_0 * 1.25,
    TRUE ~ bread_1_0
  ))
# extract bread intake data
fibre.Bread_1_0 <- fibre.breads.1$bread_1_0

# Cereal fibre intake
df.cereal_1_0 <- data.frame(cereal_1_0, Assessment.1$cereal_type_f1468_1_0)

# According to different cereal types to calculate the fibre intake (per portion) for cereal intake
fibre.cereals.1 <- df.cereal_1_0 %>%
  mutate(cereal_1_0 = case_when(
    Assessment.1.cereal_type_f1468_1_0 == 1 ~ cereal_1_0 * 7.16,
    Assessment.1.cereal_type_f1468_1_0 == 2 ~ cereal_1_0 * 2.92,
    Assessment.1.cereal_type_f1468_1_0 == 3 ~ cereal_1_0 * 1.92,
    Assessment.1.cereal_type_f1468_1_0 == 4 ~ cereal_1_0 * 4.18,
    Assessment.1.cereal_type_f1468_1_0 == 5 ~ cereal_1_0 * 0.54,
    is.na(Assessment.1.cereal_type_f1468_1_0) ~ cereal_1_0 * 3.34,
    TRUE ~ cereal_1_0
  ))

fibre.Cereal_1_0 <- fibre.cereals.1$cereal_1_0



## red and processed meat
Processed.meat.intake_1_0 <- processed.meat_1_0*52.5
Beef.intake_1_0 <- beef_1_0*120
Lamb.mutton.intake_1_0 <- lamb_1_0*120
Pork.intake_1_0 <- pork_1_0*120
Poultry.intake_1_0 <- poultry_1_0*130

## ultra-processed foods (aUPFs)
energy.bread_1_0 <- bread_1_0*103 # sliced bread
energy.cereal_1_0 <-cereal_1_0*150 # take average of energy (per portion) of cereal types

## The total grams, kcals, or units of food they intake per day or per week(for meat & alcohol).
intake_total.1 <- data.frame(Cooked.vegetable.intake_1_0, Salad.raw.vegetable.intake_1_0, Fresh.fruit.intake_1_0, Dried.fruit.intake_1_0, 
                             fibre.Cooked.vegetable_1_0, fibre.raw.vegetable_1_0, fibre.Fresh.fruit_1_0, fibre.Dried.fruit_1_0, fibre.Bread_1_0, fibre.Cereal_1_0,
                             Processed.meat.intake_1_0, Beef.intake_1_0, Lamb.mutton.intake_1_0, Pork.intake_1_0, Poultry.intake_1_0,
                             energy.bread_1_0, energy.cereal_1_0)

############################# Assessment_2_0 #######################################

## vegetables and fruits intake 
Cooked.vegetable.intake_2_0 <- cooked.veg_2_0*35
Salad.raw.vegetable.intake_2_0 <- raw.veg_2_0*35
Fresh.fruit.intake_2_0 <- fresh.fruit_2_0*118
Dried.fruit.intake_2_0 <- dried.fruit_2_0*10

## fibre content 
fibre.Cooked.vegetable_2_0 <- cooked.veg_2_0*1
fibre.raw.vegetable_2_0 <- raw.veg_2_0*1
fibre.Fresh.fruit_2_0 <- fresh.fruit_2_0*2
fibre.Dried.fruit_2_0 <- dried.fruit_2_0*0.5


# Bread fibre intake
df.bread_2_0 <- data.frame(bread_2_0, Assessment.2$bread_type_f1448_2_0)

# According to different bread types to calculate the fibre intake (per portion) for bread intake
fibre.breads.2 <- df.bread_2_0 %>%
  mutate(bread_2_0 = case_when(
    Assessment.2.bread_type_f1448_2_0 == 1 ~ bread_2_0 * 0.68,
    Assessment.2.bread_type_f1448_2_0 == 2 ~ bread_2_0 * 1.26,
    Assessment.2.bread_type_f1448_2_0 == 3 ~ bread_2_0 * 1.80,
    Assessment.2.bread_type_f1448_2_0 == 4 ~ bread_2_0 * 1.25,
    is.na(Assessment.2.bread_type_f1448_2_0) ~ bread_2_0 * 1.25,
    TRUE ~ bread_2_0
  ))
# extract bread intake data
fibre.Bread_2_0 <- fibre.breads.2$bread_2_0

# Cereal fibre intake
df.cereal_2_0 <- data.frame(cereal_2_0, Assessment.2$cereal_type_f1468_2_0)

# According to different cereal types to calculate the fibre intake (per portion) for cereal intake
fibre.cereals.2 <- df.cereal_2_0 %>%
  mutate(cereal_2_0 = case_when(
    Assessment.2.cereal_type_f1468_2_0 == 1 ~ cereal_2_0 * 7.16,
    Assessment.2.cereal_type_f1468_2_0 == 2 ~ cereal_2_0 * 2.92,
    Assessment.2.cereal_type_f1468_2_0 == 3 ~ cereal_2_0 * 1.92,
    Assessment.2.cereal_type_f1468_2_0 == 4 ~ cereal_2_0 * 4.18,
    Assessment.2.cereal_type_f1468_2_0 == 5 ~ cereal_2_0 * 0.54,
    is.na(Assessment.2.cereal_type_f1468_2_0) ~ cereal_2_0 * 3.34,
    TRUE ~ cereal_2_0
  ))

fibre.Cereal_2_0 <- fibre.cereals.2$cereal_2_0



## red and processed meat
Processed.meat.intake_2_0 <- processed.meat_2_0*52.5
Beef.intake_2_0 <- beef_2_0*120
Lamb.mutton.intake_2_0 <- lamb_2_0*120
Pork.intake_2_0 <- pork_2_0*120
Poultry.intake_2_0 <- poultry_2_0*130

## ultra-processed foods (aUPFs)
energy.bread_2_0 <- bread_2_0*103 # sliced bread
energy.cereal_2_0 <-cereal_2_0*150 # take average of energy (per portion) of cereal types

## The total grams, kcals, or units of food they intake per day or per week(for meat & alcohol).
intake_total.2 <- data.frame(Cooked.vegetable.intake_2_0, Salad.raw.vegetable.intake_2_0, Fresh.fruit.intake_2_0, Dried.fruit.intake_2_0, 
                             fibre.Cooked.vegetable_2_0, fibre.raw.vegetable_2_0, fibre.Fresh.fruit_2_0, fibre.Dried.fruit_2_0, fibre.Bread_2_0, fibre.Cereal_2_0,
                             Processed.meat.intake_2_0, Beef.intake_2_0, Lamb.mutton.intake_2_0, Pork.intake_2_0, Poultry.intake_2_0,
                             energy.bread_2_0, energy.cereal_2_0)

###################################################################################

combined_intake <- cbind(intake_total.0, intake_total.1, intake_total.2)

## Take average from three assessments for each food intake
combined_intake <- combined_intake %>%
  mutate(mean.cooked.veg = rowMeans(select(., Cooked.vegetable.intake_0_0, Cooked.vegetable.intake_1_0, Cooked.vegetable.intake_2_0), na.rm = T),
         mean.raw.veg = rowMeans(select(., Salad.raw.vegetable.intake_0_0, Salad.raw.vegetable.intake_1_0, Salad.raw.vegetable.intake_2_0), na.rm = T),
         mean.fresh.fruit = rowMeans(select(., Fresh.fruit.intake_0_0, Fresh.fruit.intake_1_0, Fresh.fruit.intake_2_0), na.rm = T),
         mean.dried.fruit = rowMeans(select(., Dried.fruit.intake_0_0, Dried.fruit.intake_1_0, Dried.fruit.intake_2_0), na.rm = T),
         mean.fibre.cooked.veg = rowMeans(select(., fibre.Cooked.vegetable_0_0, fibre.Cooked.vegetable_1_0, fibre.Cooked.vegetable_2_0), na.rm = T),
         mean.fibre.raw.veg = rowMeans(select(., fibre.raw.vegetable_0_0, fibre.raw.vegetable_1_0, fibre.raw.vegetable_2_0), na.rm = T),
         mean.fibre.fresh.fruit = rowMeans(select(., fibre.Fresh.fruit_0_0, fibre.Fresh.fruit_1_0, fibre.Fresh.fruit_2_0), na.rm = T),
         mean.fibre.dried.fruit = rowMeans(select(., fibre.Dried.fruit_0_0, fibre.Dried.fruit_1_0, fibre.Dried.fruit_2_0), na.rm = T),
         mean.fibre.bread = rowMeans(select(., fibre.Bread_0_0, fibre.Bread_1_0, fibre.Bread_2_0), na.rm = T),
         mean.fibre.cereal = rowMeans(select(., fibre.Cereal_0_0, fibre.Cereal_1_0, fibre.Cereal_2_0), na.rm = T),
         mean.processed.meat = rowMeans(select(., Processed.meat.intake_0_0, Processed.meat.intake_1_0, Processed.meat.intake_2_0), na.rm = T),
         mean.beef = rowMeans(select(., Beef.intake_0_0, Beef.intake_1_0, Beef.intake_2_0), na.rm = T),
         mean.lamb = rowMeans(select(., Lamb.mutton.intake_0_0, Lamb.mutton.intake_1_0, Lamb.mutton.intake_2_0), na.rm = T),
         mean.pork = rowMeans(select(., Pork.intake_0_0, Pork.intake_1_0, Pork.intake_2_0), na.rm = T),
         mean.poultry = rowMeans(select(., Poultry.intake_0_0, Poultry.intake_1_0, Poultry.intake_2_0), na.rm = T),
         mean.energy.bread = rowMeans(select(., energy.bread_0_0, energy.bread_1_0, energy.bread_2_0), na.rm = T),
         mean.energy.cereal = rowMeans(select(., energy.cereal_0_0, energy.cereal_1_0, energy.cereal_2_0), na.rm = T),
         oily.fish = oily.fish.intake, non.oily.fish = non.oily.fish.intake)

# Extract all the mean values
mean_intake <- combined_intake %>%
  select(starts_with("mean"))

# Transfer NaN to NA
mean_intake <- mean_intake %>% mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# Add alcohol data into mean_intake data frame
mean_intake <- mean_intake %>% 
  mutate(score.alcohol = alcohol$score.alcohol_0_0)


####################### BMI and physical activity data #############################

### Body mass index (BMI)
BMI_physical_acti <- UKBB_filtered_1 %>%
  select(body_mass_index_bmi_f21001_0_0.x, body_mass_index_bmi_f21001_1_0.x, body_mass_index_bmi_f21001_2_0.x, summed_met_minutes_per_week_for_all_activity_f22040_0_0)

BMI_physical_acti <- BMI_physical_acti %>%
  mutate(mean.BMI = rowMeans(select(., body_mass_index_bmi_f21001_0_0.x, body_mass_index_bmi_f21001_1_0.x, body_mass_index_bmi_f21001_2_0.x), na.rm = TRUE))

# Transfer NaN to NA
BMI_physical_acti <- BMI_physical_acti %>%
  mutate(mean.BMI = ifelse(is.nan(mean.BMI), NA, mean.BMI))


##################### Add the columns needed to do dietary NA filtering ########################

# Add the BC data into the total data frame to do further filtering
total_data <- mean_intake %>%
  mutate(BC_0_0 = UKBB_filtered_1$type_of_cancer_icd10_f40006_0_0, BC_1_0 = UKBB_filtered_1$type_of_cancer_icd10_f40006_1_0, BC_2_0 = UKBB_filtered_1$type_of_cancer_icd10_f40006_2_0,
         BC_3_0 = UKBB_filtered_1$type_of_cancer_icd10_f40006_3_0, BC_4_0 = UKBB_filtered_1$type_of_cancer_icd10_f40006_4_0)


# Add BMI and physical activity data to the total data frame
total_data <- total_data %>%
  mutate(mean.BMI = BMI_physical_acti$mean.BMI, met.physical.acti = BMI_physical_acti$summed_met_minutes_per_week_for_all_activity_f22040_0_0)

# Add age data 
total_data <- total_data %>%
  mutate(age.data = UKBB_filtered_1$age_when_attended_assessment_centre_f21003_0_0)

# Add ethnic background
total_data <- total_data %>%
  mutate(ethnic.background = UKBB_filtered_1$ethnic_background_f21000_0_0.x)

total_data <- total_data %>%
  mutate(oily.fish = combined_intake$oily.fish, non.oily.fish = combined_intake$non.oily.fish)

total_data <- total_data %>%
  mutate(smoking_status = UKBB_filtered_1$smoking_status_f20116_0_0)

total_data <- total_data %>%
  mutate(rs174547 = UKBB_filtered_1$rs174547_faffy5595737)
  
  
  
################ Exclude the individuals with all NA in dietary intake column and cancer type column ######

# Create an index to identify all rows where all dietary data are NA
dietary_na_rows_index <- which(apply(mean_intake, 1, function(x) all(is.na(x)))) # 1313 individuals lack of all the dietary data


# mean_intake_BC has n rows
n <- nrow(mean_intake)

# Create a logical vector of length n, initially set to FALSE
rows_to_keep <- rep(FALSE, n)

# Set non-index positions to TRUE
rows_to_keep[-dietary_na_rows_index] <- TRUE

# Use the logical vector to filter the data
filtered_data <- total_data %>%          ### 230342 obs left
  filter(rows_to_keep)


##################################################################################


########################## Imputation ############################################

### Imputation with average
Imputation_intake <- filtered_data


Imputation_intake$mean.cooked.veg <- ifelse(is.na(Imputation_intake$mean.cooked.veg), 
                                            median.default(Imputation_intake$mean.cooked.veg, na.rm = T), Imputation_intake$mean.cooked.veg)
Imputation_intake$mean.raw.veg <-  ifelse(is.na(Imputation_intake$mean.raw.veg), 
                                          median.default(Imputation_intake$mean.raw.veg, na.rm = T), Imputation_intake$mean.raw.veg)
Imputation_intake$mean.fresh.fruit <-  ifelse(is.na(Imputation_intake$mean.fresh.fruit), 
                                              median.default(Imputation_intake$mean.fresh.fruit, na.rm = T), Imputation_intake$mean.fresh.fruit)
Imputation_intake$mean.dried.fruit <-  ifelse(is.na(Imputation_intake$mean.dried.fruit), 
                                              median.default(Imputation_intake$mean.dried.fruit, na.rm = T), Imputation_intake$mean.dried.fruit)

Imputation_intake$mean.fibre.cooked.veg <-  ifelse(is.na(Imputation_intake$mean.fibre.cooked.veg), 
                                                   median.default(Imputation_intake$mean.fibre.cooked.veg, na.rm = T), Imputation_intake$mean.fibre.cooked.veg)
Imputation_intake$mean.fibre.raw.veg <- ifelse(is.na(Imputation_intake$mean.fibre.raw.veg), 
                                               median.default(Imputation_intake$mean.fibre.raw.veg, na.rm = T), Imputation_intake$mean.fibre.raw.veg)
Imputation_intake$mean.fibre.fresh.fruit <-  ifelse(is.na(Imputation_intake$mean.fibre.fresh.fruit), 
                                                    median.default(Imputation_intake$mean.fibre.fresh.fruit, na.rm = T), Imputation_intake$mean.fibre.fresh.fruit)
Imputation_intake$mean.fibre.dried.fruit <-  ifelse(is.na(Imputation_intake$mean.fibre.dried.fruit), 
                                                    median.default(Imputation_intake$mean.fibre.dried.fruit, na.rm = T), Imputation_intake$mean.fibre.dried.fruit)
Imputation_intake$mean.fibre.bread <-  ifelse(is.na(Imputation_intake$mean.fibre.bread), 
                                              median.default(Imputation_intake$mean.fibre.bread, na.rm = T), Imputation_intake$mean.fibre.bread)
Imputation_intake$mean.fibre.cereal <- ifelse(is.na(Imputation_intake$mean.fibre.cereal), 
                                              median.default(Imputation_intake$mean.fibre.cereal, na.rm = T), Imputation_intake$mean.fibre.cereal)

Imputation_intake$mean.processed.meat <-  ifelse(is.na(Imputation_intake$mean.processed.meat), 
                                                 median.default(Imputation_intake$mean.processed.meat, na.rm = T), Imputation_intake$mean.processed.meat)
Imputation_intake$mean.beef <-  ifelse(is.na(Imputation_intake$mean.beef), 
                                       median.default(Imputation_intake$mean.beef, na.rm = T), Imputation_intake$mean.beef)
Imputation_intake$mean.lamb <-  ifelse(is.na(Imputation_intake$mean.lamb), 
                                       median.default(Imputation_intake$mean.lamb, na.rm = T), Imputation_intake$mean.lamb)
Imputation_intake$mean.pork <-  ifelse(is.na(Imputation_intake$mean.pork), 
                                       median.default(Imputation_intake$mean.pork, na.rm = T), Imputation_intake$mean.pork)
Imputation_intake$mean.poultry <-  ifelse(is.na(Imputation_intake$mean.poultry), 
                                          median.default(Imputation_intake$mean.poultry, na.rm = T), Imputation_intake$mean.poultry)

Imputation_intake$mean.energy.bread <-  ifelse(is.na(Imputation_intake$mean.energy.bread), 
                                               median.default(Imputation_intake$mean.energy.bread, na.rm = T), Imputation_intake$mean.energy.bread)
Imputation_intake$mean.energy.cereal <-  ifelse(is.na(Imputation_intake$mean.energy.cereal), 
                                                median.default(Imputation_intake$mean.energy.cereal, na.rm = T), Imputation_intake$mean.energy.cereal)


Imputation_intake$score.alcohol <-  ifelse(is.na(Imputation_intake$score.alcohol), 
                                           median.default(Imputation_intake$score.alcohol, na.rm = T), Imputation_intake$score.alcohol)

Imputation_intake$mean.BMI <- ifelse(is.na(Imputation_intake$mean.BMI), 
                                     median.default(Imputation_intake$mean.BMI, na.rm = T), Imputation_intake$mean.BMI)
Imputation_intake$met.physical.acti <- ifelse(is.na(Imputation_intake$met.physical.acti), 
                                              median.default(Imputation_intake$met.physical.acti, na.rm = T), Imputation_intake$met.physical.acti)

Imputation_intake$oily.fish <- ifelse(is.na(Imputation_intake$oily.fish), 
                                              median.default(Imputation_intake$oily.fish, na.rm = T), Imputation_intake$oily.fish)

Imputation_intake$non.oily.fish <- ifelse(is.na(Imputation_intake$non.oily.fish), 
                                      median.default(Imputation_intake$non.oily.fish, na.rm = T), Imputation_intake$non.oily.fish)



###################### Sum each food category ######################################

# sum of vegetables and fruits   
Sum <- Imputation_intake %>%
  mutate(vegetables_fruits = mean.cooked.veg + mean.raw.veg + mean.fresh.fruit + mean.dried.fruit)

# sum of fibre intake
Sum <- Sum %>%
  mutate(Total_fibre = mean.fibre.cooked.veg + mean.fibre.raw.veg + mean.fibre.fresh.fruit + mean.fibre.dried.fruit + mean.fibre.bread + mean.fibre.cereal)

## sum of ultra processed food kcals
Sum <- Sum %>%
  mutate(Ultra_processed_food = mean.energy.bread + mean.energy.cereal)


# Calculate the 1/3 and 2/3 quantiles
q1 <- quantile(Sum$Ultra_processed_food, 1/3)
q2 <- quantile(Sum$Ultra_processed_food, 2/3)

# Assign each data point to the corresponding tertile
aUPFs_tertiles <- ifelse(Sum$Ultra_processed_food <= q1, "Tertile 1",
                         ifelse(Sum$Ultra_processed_food <= q2, "Tertile 2", "Tertile 3"))
Sum <- Sum %>%
  mutate(aUPFs_tertiles = aUPFs_tertiles)

## sum of processed meat
Sum <- Sum %>%
  mutate(Total_processed_meat = mean.processed.meat)

## sum of red meats
Sum <- Sum %>%
  mutate(Total_red_meat = mean.beef + mean.lamb + mean.pork + mean.poultry)

## BMI
Sum <- Sum %>%
  mutate(BMI = Imputation_intake$mean.BMI)

## Physical activity MET
Sum <- Sum %>%
  mutate(Met_physical_acti = Imputation_intake$met.physical.acti)



## Extract a data frame to summary different categories of foods (lack of some UPFs and sugar-sweetened drink)
Final_data <- Sum %>%
  select(vegetables_fruits, Total_fibre, aUPFs_tertiles, Total_processed_meat, Total_red_meat, score.alcohol, BMI, Met_physical_acti, oily.fish, non.oily.fish)


# Round the sum intake data to one decimal place
Final_data <- Final_data %>%
  mutate(across(where(is.numeric), ~round(., 1)))


#################### Calculate the WCRF scores ######################################

# Scores of vegetables and fruits
Final_data <- Final_data %>% 
  mutate(score.veg.fruit = case_when(
    vegetables_fruits < 200 ~ "0",      
    vegetables_fruits >= 200 & vegetables_fruits < 400 ~ "0.25",  
    vegetables_fruits >= 400 ~ "0.5"         
  ))


# Scores of fibre 
Final_data <- Final_data %>% 
  mutate(score.fibre = case_when(
    Total_fibre < 15 ~ "0",       
    Total_fibre >= 15 & Total_fibre < 30 ~ "0.25", 
    Total_fibre >= 30 ~ "0.5"        
  ))



# Scores of aUPFs
Final_data <- Final_data %>% 
  mutate(score.aUPFs = case_when(
    aUPFs_tertiles == "Tertile 1" ~ "1",       
    aUPFs_tertiles == "Tertile 2" ~ "0.5", 
    aUPFs_tertiles == "Tertile 3" ~ "0"        
  ))


# Scores of red and processed meat
Final_data <- Final_data %>% 
  mutate(score.red.processed.meat = case_when(
    Total_red_meat <= 500 & Total_processed_meat < 21 ~ "1",       
    Total_red_meat <= 500 & Total_processed_meat >= 21 & Total_processed_meat <100 ~ "0.5", 
    Total_red_meat > 500 | Total_processed_meat >= 100 ~ "0"        
  ))




# Scores of BMI
Final_data <- Final_data %>% 
  mutate(score.BMI = case_when(
    BMI >= 18.5 & BMI <= 24.9 ~ "0.5",       
    BMI >= 25 & BMI <= 29.9 ~ "0.25", 
    BMI < 18.5 | BMI >= 30 ~ "0"        
  ))

# Scores of Total moderate-vigorous physical activity (MET min/wk)
Final_data <- Final_data %>% 
  mutate(score.physical.acti = case_when(
    Met_physical_acti >= 600 ~ "1",       
    Met_physical_acti >= 300 & BMI < 600 ~ "0.5", 
    Met_physical_acti < 300 ~ "0"        
  ))

### WCRF score data frame
WCRF_score <- Final_data %>%
  select(score.veg.fruit, score.fibre, score.aUPFs, score.red.processed.meat, score.alcohol, score.BMI, score.physical.acti)



# Transfer character to numeric
WCRF_score <- WCRF_score %>%
  mutate(across(where(is.character), as.numeric))


WCRF_score <- WCRF_score %>%
  mutate(WCRF.score = score.veg.fruit + score.fibre + score.aUPFs + score.red.processed.meat
         + score.alcohol + score.BMI + score.physical.acti)


###################################################################################



BC_data <- filtered_data %>%
  select(BC_0_0, BC_1_0, BC_2_0, BC_3_0, BC_4_0)

BC_data <- BC_data %>%
  mutate(BC = if_else(if_all(everything(), is.na), 0, 1))

table(BC_data$BC)
# 4406 obs are BC patients

# Extract WCRF scores and BC data
logistic_data <- WCRF_score %>%
  select(WCRF.score) %>%
  mutate(BC = BC_data$BC, age = Imputation_intake$age.data, ethnic = Imputation_intake$ethnic.background, smoking_status = Imputation_intake$smoking_status,
         rs174547 = Imputation_intake$rs174547, oily_fish = Final_data$oily.fish, non_oily_fish = Final_data$non.oily.fish)
# 230342 obs

logistic_data$BC <- as.factor(logistic_data$BC) # Transfer numbers 0 and 1 of BC variables to factors



logistic_data$ethnic <- ifelse(logistic_data$ethnic == 1 | logistic_data$ethnic == 1001 |logistic_data$ethnic == 1002 | logistic_data$ethnic == 1003, "White", logistic_data$ethnic)
logistic_data$ethnic <- ifelse(logistic_data$ethnic == 2 |logistic_data$ethnic == 2001 | logistic_data$ethnic == 2002 | logistic_data$ethnic == 2003 | 
                                 logistic_data$ethnic == 2004 | logistic_data$ethnic == 6, "Mixed and other", logistic_data$ethnic)
logistic_data$ethnic <- ifelse(logistic_data$ethnic == 3 | logistic_data$ethnic == 3001 | logistic_data$ethnic == 3002 | logistic_data$ethnic == 3003 |
                                 logistic_data$ethnic == 3004, "Asian or Asian British", logistic_data$ethnic)
logistic_data$ethnic <- ifelse(logistic_data$ethnic == 4 | logistic_data$ethnic == 4001 | logistic_data$ethnic == 4002 | logistic_data$ethnic == 4003, 
                               "Black and Black British", logistic_data$ethnic)
logistic_data$ethnic <- ifelse(logistic_data$ethnic == 5, "Chinese", logistic_data$ethnic)


logistic_data$ethnic <- as.factor(logistic_data$ethnic)


###############################################################################################

## Filter the participants with missing covariates 
covariates_NA_filter <- logistic_data %>%
  filter(!is.na(ethnic), !is.na(smoking_status))  ## 693 (ethnicity) + 1 (smoking) obs were removed

covariates_NA_filter <- covariates_NA_filter %>%
  filter(!smoking_status == "Prefer not to answer") ## 806 obs were removed (smoking)
  

covariates_NA_filter$smoking_status <- as.factor(covariates_NA_filter$smoking_status)  
  
covariates_NA_filter <- covariates_NA_filter %>%
  filter(!rs174547 == "0 0" & !is.na(rs174547)) ## 7514 obs were removed

covariates_NA_filter$rs174547 <- as.factor(covariates_NA_filter$rs174547)  
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(rs174547_ = if_else(rs174547 == "T T", 2,
                            if_else(rs174547 == "C T", 1, 0)))

covariates_NA_filter$rs174547_ <- as.numeric(covariates_NA_filter$rs174547_)  

covariates_NA_filter <- covariates_NA_filter %>%
  mutate(smoking_code = if_else(smoking_status == "Never", 0,
                 if_else(smoking_status == "Previous", 1, 2)))
  
covariates_NA_filter$smoking_code <- as.factor(covariates_NA_filter$smoking_code)

#### modified WCRF score (add fish score) 
# Calculate the 1/3 and 2/3 quantiles
F1 <- quantile(covariates_NA_filter$oily_fish, 1/3)
F2 <- quantile(covariates_NA_filter$oily_fish, 2/3)
#F2 <- 300

# Assign each data point to the corresponding tertile
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(oily_fish_tertile = if_else(oily_fish <= F1, "Low",
                                     if_else(oily_fish <= F2, "Middle", "High")))
## 0-100 = low, 200-300 = middle, 400-500 = high

covariates_NA_filter$oily_fish_tertile <- as.factor(covariates_NA_filter$oily_fish_tertile)
covariates_NA_filter$oily_fish_tertile <- relevel(covariates_NA_filter$oily_fish_tertile, ref = "Low")

## WCRF score + extra oily fish score
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(WCRF_fish =if_else(oily_fish_tertile == "High", WCRF.score+1,
                            if_else(oily_fish_tertile == "Middle", WCRF.score+0.5, WCRF.score+0)) )

##### Rerun the code for splitting WCRF score/WCRF_fish to high and low groups
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(WCRF.score.group = if_else(WCRF.score <= 2.75, 0, 1))
  
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(WCRF.fish.group = if_else(WCRF_fish <= 3.25, 0, 1))
  
covariates_NA_filter$WCRF.score.group <- as.factor(covariates_NA_filter$WCRF.score.group)
covariates_NA_filter$WCRF.fish.group <- as.factor(covariates_NA_filter$WCRF.fish.group)


########### model 1 ######################

# Set the reference of ethnicity as "white"
covariates_NA_filter$ethnic <- relevel(covariates_NA_filter$ethnic, ref = "White")

model_1 <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code, data = covariates_NA_filter , family = "binomial")

summary(model_1)


coefficients_1 <- summary(model_1)$coefficients
odds_ratio_1 <- exp(coefficients_1)
print(odds_ratio_1) # 0.88

# 95% confidence intervals of coefficient of WCRF score
estimate_1 <- coefficients_1[2, 1]
standard_error_1 <- coefficients_1[2, 2]

estimate_1 - 1.96 * standard_error_1 
estimate_1 + 1.96 * standard_error_1 

# 95% confidence intervals of odds ratio
CI_min_1 <- exp(estimate_1 - 1.96 * standard_error_1) # z2.5% = 1.96
CI_max_1 <- exp(estimate_1 + 1.96 * standard_error_1)
print(CI_min_1) # 0.831
print(CI_max_1) # 0.940



############### Separate the data with menopausal status (pre or post) #############
covariates_NA_filter <- covariates_NA_filter %>%
  mutate(menopausal_status = if_else(age <= 50, "pre", "post" ))

Pre_menopausal <- covariates_NA_filter %>%
  filter(menopausal_status == "pre")

Post_menopausal <- covariates_NA_filter %>%
  filter(menopausal_status == "post")


##### Pre-menopausal model 1
Pre_menopausal$ethnic <- relevel(Pre_menopausal$ethnic, ref = "White")

model_1_pre <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code, data = Pre_menopausal , family = "binomial")

summary(model_1_pre)
coefficients_1_pre <- summary(model_1_pre)$coefficients
odds_ratio_1_pre <- exp(coefficients_1_pre)
print(odds_ratio_1_pre) # 1.01

# 95% confidence intervals of coefficient of WCRF score
estimate_1_pre <- coefficients_1_pre[2, 1]
standard_error_1_pre <- coefficients_1_pre[2, 2]

estimate_1_pre - 1.96 * standard_error_1_pre 
estimate_1_pre + 1.96 * standard_error_1_pre 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_1_pre - 1.96 * standard_error_1_pre) # 0.846
exp(estimate_1_pre + 1.96 * standard_error_1_pre) # 1.195 ## do not know the association




###### Post-menopausal model 1
Post_menopausal$ethnic <- relevel(Post_menopausal$ethnic, ref = "White")

model_1_post <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code, data = Post_menopausal , family = "binomial")


summary(model_1_post)
coefficients_1_post <- summary(model_1_post)$coefficients
odds_ratio_1_post <- exp(coefficients_1_post)
print(odds_ratio_1_post) # 0.863

# 95% confidence intervals of coefficient of WCRF score
estimate_1_post <- coefficients_1_post[2, 1]
standard_error_1_post <- coefficients_1_post[2, 2]

estimate_1_post - 1.96 * standard_error_1_post 
estimate_1_post + 1.96 * standard_error_1_post 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_1_post - 1.96 * standard_error_1_post) # 0.822
exp(estimate_1_post + 1.96 * standard_error_1_post) # 0.946





########## Model 2 (modified WCRF score (with oily fish scores) #############################
covariates_NA_filter$ethnic <- relevel(covariates_NA_filter$ethnic, ref = "White")

model_2 <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code, data = covariates_NA_filter , family = "binomial")

summary(model_2)
coefficients_2 <- summary(model_2)$coefficients
odds_ratio_2 <- exp(coefficients_2)
print(odds_ratio_2) # 0.882


# 95% confidence intervals of coefficient of WCRF score
estimate_2 <- coefficients_2[2, 1]
standard_error_2 <- coefficients_2[2, 2]

estimate_2 - 1.96 * standard_error_2 
estimate_2 + 1.96 * standard_error_2 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_2 - 1.96 * standard_error_2) # 0.829
exp(estimate_2 + 1.96 * standard_error_2) # 0.940


##### Pre-menopausal model 2
Pre_menopausal$ethnic <- relevel(Pre_menopausal$ethnic, ref = "White")
model_2_pre <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code, data = Pre_menopausal , family = "binomial")


summary(model_2_pre)
coefficients_2_pre <- summary(model_2_pre)$coefficients
odds_ratio_2_pre <- exp(coefficients_2_pre)
print(odds_ratio_2_pre) # 0.85

# 95% confidence intervals of coefficient of WCRF score
estimate_2_pre <- coefficients_2_pre[2, 1]
standard_error_2_pre <- coefficients_2_pre[2, 2]

estimate_2_pre - 1.96 * standard_error_2_pre 
estimate_2_pre + 1.96 * standard_error_2_pre 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_2_pre - 1.96 * standard_error_2_pre) # 0.747
exp(estimate_2_pre + 1.96 * standard_error_2_pre) # 0.977 ## do not know the association



###### Post-menopausal model 2

model_2_post <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code, data = Post_menopausal , family = "binomial")


summary(model_2_post)
coefficients_2_post <- summary(model_2_post)$coefficients
odds_ratio_2_post <- exp(coefficients_2_post)
print(odds_ratio_2_post) # 0.89

# 95% confidence intervals of coefficient of WCRF score
estimate_2_post <- coefficients_2_post[2, 1]
standard_error_2_post <- coefficients_2_post[2, 2]

estimate_2_post - 1.96 * standard_error_2_post # -0.185
estimate_2_post + 1.96 * standard_error_2_post # -0.100

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_2_post - 1.96 * standard_error_2_post) # 0.831
exp(estimate_2_post + 1.96 * standard_error_2_post) # 0.958


######### Model 3 (Model 1 + rs174547 + rs174547*WCRF score) ###########################


model_3 <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score.group, data = covariates_NA_filter , family = "binomial")
summary(model_3)

coefficients_3<- summary(model_3)$coefficients
odds_ratio_3 <- exp(coefficients_3)
print(odds_ratio_3) # 0.926

# 95% confidence intervals of coefficient of WCRF score
estimate_3 <- coefficients_3[2, 1]
standard_error_3 <- coefficients_3[2, 2]

estimate_3 - 1.96 * standard_error_3 # -0.140
estimate_3 + 1.96 * standard_error_3 # -0.013

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_3 - 1.96 * standard_error_3) # 0.870
exp(estimate_3 + 1.96 * standard_error_3) # 0.987

# Pre-menopausal
model_3_pre <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score.group, data = Pre_menopausal , family = "binomial")


summary(model_3_pre)
coefficients_3_pre <- summary(model_3_pre)$coefficients
odds_ratio_3_pre <- exp(coefficients_3_pre)
print(odds_ratio_3_pre) # 0.942

# 95% confidence intervals of coefficient of WCRF score
estimate_3_pre <- coefficients_3_pre[2, 1]
standard_error_3_pre <- coefficients_3_pre[2, 2]

estimate_3_pre - 1.96 * standard_error_3_pre 
estimate_3_pre + 1.96 * standard_error_3_pre 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_3_pre - 1.96 * standard_error_3_pre) # 0.711
exp(estimate_3_pre + 1.96 * standard_error_3_pre) # 1.250

###### Post-menopausal model 3

model_3_post <- glm(BC ~ WCRF.score.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score.group, data = Post_menopausal , family = "binomial")


summary(model_3_post)
coefficients_3_post <- summary(model_3_post)$coefficients
odds_ratio_3_post <- exp(coefficients_3_post)
print(odds_ratio_3_post) # 0.918

# 95% confidence intervals of coefficient of WCRF score
estimate_3_post <- coefficients_3_post[2, 1]
standard_error_3_post <- coefficients_3_post[2, 2]

estimate_3_post - 1.96 * standard_error_3_post # -0.154
estimate_3_post + 1.96 * standard_error_3_post # -0.018

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_3_post - 1.96 * standard_error_3_post) # 0.857
exp(estimate_3_post + 1.96 * standard_error_3_post) # 0.982  



######### model 4 (Model 2 + rs174547 + rs174547*WCRF score)

model_4 <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.fish.group, data = covariates_NA_filter , family = "binomial")
summary(model_4)

coefficients_4 <- summary(model_4)$coefficients
odds_ratio_4 <- exp(coefficients_4)
print(odds_ratio_4) # 0.910

# 95% confidence intervals of coefficient of WCRF score
estimate_4 <- coefficients_4[2, 1]
standard_error_4 <- coefficients_4[2, 2]

estimate_4 - 1.96 * standard_error_4 # -0.153
estimate_4 + 1.96 * standard_error_4 # -0.035

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_4 - 1.96 * standard_error_4) # 0.858
exp(estimate_4 + 1.96 * standard_error_4) # 0.958


###### Pre-menopausal model 4

model_4_pre <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.fish.group, data = Pre_menopausal , family = "binomial")


summary(model_4_pre)
coefficients_4_pre <- summary(model_4_pre)$coefficients
odds_ratio_4_pre <- exp(coefficients_4_pre)
print(odds_ratio_4_pre) # 1.00

# 95% confidence intervals of coefficient of WCRF score
estimate_4_pre <- coefficients_4_pre[2, 1]
standard_error_4_pre <- coefficients_4_pre[2, 2]

estimate_4_pre - 1.96 * standard_error_4_pre # -0.147
estimate_4_pre + 1.96 * standard_error_4_pre # 0.149

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_4_pre - 1.96 * standard_error_4_pre) # 0.863
exp(estimate_4_pre + 1.96 * standard_error_4_pre) # 1.161

###### Post-menopausal model 4
model_4_post <- glm(BC ~ WCRF.fish.group + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.fish.group, data = Post_menopausal , family = "binomial")


summary(model_4_post)
coefficients_4_post <- summary(model_4_post)$coefficients
odds_ratio_4_post <- exp(coefficients_4_post)
print(odds_ratio_4_post) # 0.893

# 95% confidence intervals of coefficient of WCRF score
estimate_4_post <- coefficients_4_post[2, 1]
standard_error_4_post <- coefficients_4_post[2, 2]

estimate_4_post - 1.96 * standard_error_4_post 
estimate_4_post + 1.96 * standard_error_4_post 

# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(estimate_4_post - 1.96 * standard_error_4_post) # 0.841
exp(estimate_4_post + 1.96 * standard_error_4_post) # 0.947

#########################



############### WCRF score as continuous variable ############### 
########### model 1
# Set the reference of ethnicity as "white"
covariates_NA_filter$ethnic <- relevel(covariates_NA_filter$ethnic, ref = "White")

con_model_1 <- glm(BC ~ WCRF.score + age + ethnic + smoking_code, data = covariates_NA_filter , family = "binomial")

summary(con_model_1) # 1.27e-05


con_coefficients_1 <- summary(con_model_1)$coefficients
con_odds_ratio_1 <- exp(con_coefficients_1)
print(con_odds_ratio_1) # 0.91

# 95% confidence intervals of coefficient of WCRF score
con_estimate_1 <- con_coefficients_1[2, 1]
con_standard_error_1 <- con_coefficients_1[2, 2]


# 95% confidence intervals of odds ratio
exp(con_estimate_1 - 1.96 * con_standard_error_1) # 0.87 # z2.5% = 1.96
exp(con_estimate_1 + 1.96 * con_standard_error_1) # 0.95



##### Pre-menopausal model 1
Pre_menopausal$ethnic <- relevel(Pre_menopausal$ethnic, ref = "White")

con_model_1_pre <- glm(BC ~ WCRF.score + age + ethnic + smoking_code, data = Pre_menopausal , family = "binomial")

summary(con_model_1_pre)  # 0.411276   
con_coefficients_1_pre <- summary(con_model_1_pre)$coefficients
con_odds_ratio_1_pre <- exp(con_coefficients_1_pre)
print(con_odds_ratio_1_pre) # 0.97

# 95% confidence intervals of coefficient of WCRF score
con_estimate_1_pre <- con_coefficients_1_pre[2, 1]
con_standard_error_1_pre <- con_coefficients_1_pre[2, 2]



# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_1_pre - 1.96 * con_standard_error_1_pre) # 0.89
exp(con_estimate_1_pre + 1.96 * con_standard_error_1_pre) # 1.05 ## do not know the association




###### Post-menopausal model 1
Post_menopausal$ethnic <- relevel(Post_menopausal$ethnic, ref = "White")

con_model_1_post <- glm(BC ~ WCRF.score + age + ethnic + smoking_code, data = Post_menopausal , family = "binomial")


summary(con_model_1_post) # 5.83e-06 
con_coefficients_1_post <- summary(con_model_1_post)$coefficients
con_odds_ratio_1_post <- exp(con_coefficients_1_post)
print(con_odds_ratio_1_post) # 0.90

# 95% confidence intervals of coefficient of WCRF score
con_estimate_1_post <- con_coefficients_1_post[2, 1]
con_standard_error_1_post <- con_coefficients_1_post[2, 2]



# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_1_post - 1.96 * con_standard_error_1_post) # 0.85
exp(con_estimate_1_post + 1.96 * con_standard_error_1_post) # 0.94





########## Model 2 (modified WCRF score (with oily fish scores) #############################
covariates_NA_filter$ethnic <- relevel(covariates_NA_filter$ethnic, ref = "White")

con_model_2 <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code, data = covariates_NA_filter , family = "binomial")

summary(con_model_2) # 2.80e-05
con_coefficients_2 <- summary(con_model_2)$coefficients
con_odds_ratio_2 <- exp(con_coefficients_2)
print(con_odds_ratio_2) # 0.93


# 95% confidence intervals of coefficient of WCRF score
con_estimate_2 <- con_coefficients_2[2, 1]
con_standard_error_2 <- con_coefficients_2[2, 2]



# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_2 - 1.96 * con_standard_error_2) # 0.89
exp(con_estimate_2 + 1.96 * con_standard_error_2) # 0.96


##### Pre-menopausal model 2
Pre_menopausal$ethnic <- relevel(Pre_menopausal$ethnic, ref = "White")
con_model_2_pre <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code, data = Pre_menopausal , family = "binomial")


summary(con_model_2_pre) # 0.379279   
con_coefficients_2_pre <- summary(con_model_2_pre)$coefficients
con_odds_ratio_2_pre <- exp(con_coefficients_2_pre)
print(con_odds_ratio_2_pre) # 0.97

# 95% confidence intervals of coefficient of WCRF score
con_estimate_2_pre <- con_coefficients_2_pre[2, 1]
con_standard_error_2_pre <- con_coefficients_2_pre[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_2_pre - 1.96 * con_standard_error_2_pre) # 0.90
exp(con_estimate_2_pre + 1.96 * con_standard_error_2_pre) # 1.04 ## do not know the association



###### Post-menopausal model 2

con_model_2_post <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code, data = Post_menopausal , family = "binomial")


summary(con_model_2_post) # 2.03e-05
con_coefficients_2_post <- summary(con_model_2_post)$coefficients
con_odds_ratio_2_post <- exp(con_coefficients_2_post)
print(con_odds_ratio_2_post) # 0.91

# 95% confidence intervals of coefficient of WCRF score
con_estimate_2_post <- con_coefficients_2_post[2, 1]
con_standard_error_2_post <- con_coefficients_2_post[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_2_post - 1.96 * con_standard_error_2_post) # 0.88
exp(con_estimate_2_post + 1.96 * con_standard_error_2_post) # 0.95


######### Model 3 (Model 1 + rs174547 + rs174547*WCRF score) ###########################


con_model_3 <- glm(BC ~ WCRF.score + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score, data = covariates_NA_filter , family = "binomial")
summary(con_model_3) # 0.00916

con_coefficients_3<- summary(con_model_3)$coefficients
con_odds_ratio_3 <- exp(con_coefficients_3)
print(con_odds_ratio_3) # 0.89

# 95% confidence intervals of coefficient of WCRF score
con_estimate_3 <- con_coefficients_3[2, 1]
con_standard_error_3 <- con_coefficients_3[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_3 - 1.96 * con_standard_error_3) # 0.81
exp(con_estimate_3 + 1.96 * con_standard_error_3) # 0.97

# Pre-menopausal
con_model_3_pre <- glm(BC ~ WCRF.score + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score, data = Pre_menopausal , family = "binomial")


summary(con_model_3_pre) # 0.971621 
con_coefficients_3_pre <- summary(con_model_3_pre)$coefficients
con_odds_ratio_3_pre <- exp(con_coefficients_3_pre)
print(con_odds_ratio_3_pre) # 1.00

# 95% confidence intervals of coefficient of WCRF score
con_estimate_3_pre <- con_coefficients_3_pre[2, 1]
con_standard_error_3_pre <- con_coefficients_3_pre[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_3_pre - 1.96 * con_standard_error_3_pre) # 0.83
exp(con_estimate_3_pre + 1.96 * con_standard_error_3_pre) # 1.21

###### Post-menopausal model 3

con_model_3_post <- glm(BC ~ WCRF.score + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF.score, data = Post_menopausal , family = "binomial")


summary(con_model_3_post) #  0.00287
con_coefficients_3_post <- summary(con_model_3_post)$coefficients
con_odds_ratio_3_post <- exp(con_coefficients_3_post)
print(con_odds_ratio_3_post) # 0.85

# 95% confidence intervals of coefficient of WCRF score
con_estimate_3_post <- con_coefficients_3_post[2, 1]
con_standard_error_3_post <- con_coefficients_3_post[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_3_post - 1.96 * con_standard_error_3_post) # 0.77
exp(con_estimate_3_post + 1.96 * con_standard_error_3_post) # 0.95  



######### model 4 (Model 2 + rs174547 + rs174547*WCRF score)

con_model_4 <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF_fish, data = covariates_NA_filter , family = "binomial")
summary(con_model_4) # 0.00821

con_coefficients_4 <- summary(con_model_4)$coefficients
con_odds_ratio_4 <- exp(con_coefficients_4)
print(con_odds_ratio_4) # 0.90

# 95% confidence intervals of coefficient of WCRF score
con_estimate_4 <- con_coefficients_4[2, 1]
con_standard_error_4 <- con_coefficients_4[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_4 - 1.96 * con_standard_error_4) # 0.83
exp(con_estimate_4 + 1.96 * con_standard_error_4) # 0.97


###### Pre-menopausal model 4

con_model_4_pre <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF_fish, data = Pre_menopausal , family = "binomial")


summary(con_model_4_pre) # 0.931702  
con_coefficients_4_pre <- summary(con_model_4_pre)$coefficients
con_odds_ratio_4_pre <- exp(con_coefficients_4_pre)
print(con_odds_ratio_4_pre) # 1.01

# 95% confidence intervals of coefficient of WCRF score
con_estimate_4_pre <- con_coefficients_4_pre[2, 1]
con_standard_error_4_pre <- con_coefficients_4_pre[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_4_pre - 1.96 * con_standard_error_4_pre) # 0.86
exp(con_estimate_4_pre + 1.96 * con_standard_error_4_pre) # 1.18

###### Post-menopausal model 4
con_model_4_post <- glm(BC ~ WCRF_fish + age + ethnic + smoking_code + rs174547_ + rs174547_*WCRF_fish, data = Post_menopausal , family = "binomial")


summary(con_model_4_post) # 0.00243
con_coefficients_4_post <- summary(con_model_4_post)$coefficients
con_odds_ratio_4_post <- exp(con_coefficients_4_post)
print(con_odds_ratio_4_post) # 0.87

# 95% confidence intervals of coefficient of WCRF score
con_estimate_4_post <- con_coefficients_4_post[2, 1]
con_standard_error_4_post <- con_coefficients_4_post[2, 2]


# 95% confidence intervals of odds ratio  (z2.5% = 1.96)
exp(con_estimate_4_post - 1.96 * con_standard_error_4_post) # 0.79
exp(con_estimate_4_post + 1.96 * con_standard_error_4_post) # 0.95

#########################







