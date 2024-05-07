# Set random seed
set.seed(23)

# Load Training set
X = read.csv("D:\\Master Courses\\22 Winter Quarter\\STAT 642 - Data Mining for Business Analytics\\Final Project\\Dataset\\train.csv")
X = subset(X, select = -c(id))

# Load Test set
X_Ftest = read.csv("D:\\Master Courses\\22 Winter Quarter\\STAT 642 - Data Mining for Business Analytics\\Final Project\\Dataset\\test.csv")
X_Ftest2 = subset(X_Ftest, select = -c(id))

# Checking the NA value in each columns.
na_count <-sapply(X, function(X) sum(is.na(X)))
na_count <- data.frame(na_count)
na_count
# Ref:https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column

# count the frequency of each value
## Count the number of unknown in Client Information
table(X$ploan)
# no: 23795, unknown: 663, yes: 4373
table(X$job)
table(X$civil)
table(X$edu)
table(X$credit)
table(X$hloan)
table(X$ctype)
# cellular: 18283, telephone: 10548 
table(X$month)
table(X$day)
table(X$ccontact)
table(X$lcdays)
table(X$pcontact)
table(X$ccontact)

# One-hot-encoder on Categorical Variables
require(dbplyr)
require(dplyr)
require(ggplot2)
require(ggthemes)

# Data Visualization in training set
## Outcome
ggplot(X, aes(x = outcome)) +
  geom_bar() +
  labs(x = "outcome", y = "frequency", title = "Num of Opening Account") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## education
ggplot(X, aes(x = edu)) +
  geom_bar(aes(fill = edu)) +
  labs(x = "education", y = "frequency", title = "education level") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## job
ggplot(X, aes(x = job)) +
  geom_bar(aes(fill = job)) +
  labs(x = "job", y = "frequency", title = "Job of client") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## civil
ggplot(X, aes(x = civil)) +
  geom_bar(aes(fill = civil)) +
  labs(x = "civil", y = "frequency", title = "civil status") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## credit
ggplot(X, aes(x = credit)) +
  geom_bar(aes(fill = credit)) +
  labs(x = "credit", y = "frequency", title = "credit") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## hloan
ggplot(X, aes(x = hloan)) +
  geom_bar(aes(fill = hloan)) +
  labs(x = "hloan", y = "frequency", title = "hloan") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## ploan
ggplot(X, aes(x = ploan)) +
  geom_bar(aes(fill = ploan)) +
  labs(x = "ploan", y = "frequency", title = "ploan") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## ctype
ggplot(X, aes(x = ctype)) +
  geom_bar(aes(fill = ctype)) +
  labs(x = "ctype", y = "frequency", title = "ctype") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## age
barplot(table(X$age))

# Missing Value imputation by kNN
## Replace unknown with NA
X$job[X$job == "unknown"] <- NA
X$civil[X$civil == "unknown"] <- NA
X$edu[X$edu == "unknown"] <- NA
X$credit[X$credit == "unknown"] <- NA
X$hloan[X$hloan == "unknown"] <- NA
X$ploan[X$ploan == "unknown"] <- NA

library(VIM)

df_imp <- kNN(X, variable = c("job", "civil", "edu",
                              "credit", "hloan", "ploan"), k = 20)
table(df_imp$credit)

table(X$credit)

X <- df_imp
X = subset(X, select = -c(job_imp, civil_imp, edu_imp,
                          credit_imp, hloan_imp, ploan_imp))


## job column
X <- X %>%
  mutate(job_admin = ifelse(job == "admin.", 1, 0),
         job_artisan = ifelse(job == "artisan", 1, 0),
         job_entrepreneur = ifelse(job == "entrepreneur", 1, 0),
         job_housemaid = ifelse(job == "housemaid", 1, 0),
         job_management = ifelse(job == "management", 1, 0),
         job_retired = ifelse(job == "retired", 1, 0),
         job_selfemployed = ifelse(job == "selfemployed", 1, 0),
         job_services = ifelse(job == "services", 1, 0),
         job_student = ifelse(job == "student", 1, 0),
         job_technician = ifelse(job == "technician", 1, 0))

X = subset(X, select = -c(job))

## Civil column
X <- X %>%
  mutate(civil_divorced = ifelse(civil == "divorced", 1, 0),
         civil_married = ifelse(civil == "married", 1, 0))

X = subset(X, select = -c(civil))

## edu column
X <- X %>%
  mutate(edu_12K = ifelse(edu == "12K", 1, 0),
         edu_4K = ifelse(edu == "4K", 1, 0),
         edu_9K = ifelse(edu == "9K", 1, 0),
         edu_6K = ifelse(edu == "6K", 1, 0),
         edu_appr = ifelse(edu == "apprenticeship", 1, 0),
         edu_illi = ifelse(edu == "illiterate", 1, 0))

X = subset(X, select = -c(edu))

## credit column
X <- X %>%
  mutate(credit_no = ifelse(credit == "no", 1, 0))

X = subset(X, select = -c(credit))

## hloan column
X <- X %>%
  mutate(hloan_no = ifelse(hloan == "no", 1, 0))

X = subset(X, select = -c(hloan))

## ploan column
X <- X %>%
  mutate(ploan_no = ifelse(ploan == "no", 1, 0))

X = subset(X, select = -c(ploan))

## ctype column
X <- X %>%
  mutate(ctype_cel = ifelse(ctype == "cellular", 1, 0))

X = subset(X, select = -c(ctype))

## month column
X <- X %>%
  mutate(mon_apr = ifelse(month == "apr", 1, 0),
         mon_aug = ifelse(month == "aug", 1, 0),
         mon_dec = ifelse(month == "dec", 1, 0),
         mon_jul = ifelse(month == "jul", 1, 0),
         mon_jun = ifelse(month == "jun", 1, 0),
         mon_mar = ifelse(month == "mar", 1, 0),
         mon_may = ifelse(month == "may", 1, 0),
         mon_nov = ifelse(month == "nov", 1, 0),
         mon_oct = ifelse(month == "oct", 1, 0))

X = subset(X, select = -c(month))

## day column
X <- X %>%
  mutate(day_mon = ifelse(day == "mon", 1, 0),
         day_tue = ifelse(day == "tue", 1, 0),
         day_wed = ifelse(day == "wed", 1, 0),
         day_thu = ifelse(day == "thu", 1, 0))

X = subset(X, select = -c(day))

## presult column
X <- X %>%
  mutate(pre_non = ifelse(presult == "nonexistent", 1, 0),
         pre_fail = ifelse(presult == "failure", 1, 0))

X = subset(X, select = -c(presult))

## Numeric Variables
## age column
X <- X %>%
  mutate(scaled_age = (age - min(age)) /
           (max(age) - min(age)))

X = subset(X, select = -c(age))

## ccontact column
X <- X %>%
  mutate(scaled_cc = (ccontact - min(ccontact)) /
           (max(ccontact) - min(ccontact)))

X = subset(X, select = -c(ccontact))

## lcdays column
X$lcdays[X$lcdays == 999] <- -1

X <- X %>%
  mutate(scaled_lcdays = (lcdays - min(lcdays)) /
           (max(lcdays) - min(lcdays)))

X = subset(X, select = -c(lcdays))

## pcontact column
X <- X %>%
  mutate(scaled_pcontact = (pcontact - min(pcontact)) /
           (max(pcontact) - min(pcontact)))

X = subset(X, select = -c(pcontact))

## employment column
X <- X %>%
  mutate(scaled_employment = (employment - min(employment)) /
           (max(employment) - min(employment)))

X = subset(X, select = -c(employment))

## cprice column
X <- X %>%
  mutate(scaled_cprice = (cprice - min(cprice)) /
           (max(cprice) - min(cprice)))

X = subset(X, select = -c(cprice))

## cconf column
X <- X %>%
  mutate(scaled_cconf = (cconf - min(cconf)) /
           (max(cconf) - min(cconf)))

X = subset(X, select = -c(cconf))

## euri3 column
X <- X %>%
  mutate(scaled_euri3 = (euri3 - min(euri3)) /
           (max(euri3) - min(euri3)))

X = subset(X, select = -c(euri3))

## employees column
X <- X %>%
  mutate(scaled_employees = (employees - min(employees)) /
           (max(employees) - min(employees)))

X = subset(X, select = -c(employees))

## Preprocessing on training set is done, next is for test set.
## Replace unknown with NA
X_Ftest2$job[X_Ftest2$job == "unknown"] <- NA
X_Ftest2$civil[X_Ftest2$civil == "unknown"] <- NA
X_Ftest2$edu[X_Ftest2$edu == "unknown"] <- NA
X_Ftest2$credit[X_Ftest2$credit == "unknown"] <- NA
X_Ftest2$hloan[X_Ftest2$hloan == "unknown"] <- NA
X_Ftest2$ploan[X_Ftest2$ploan == "unknown"] <- NA


df_imp2 <- kNN(X_Ftest2, variable = c("job", "civil", "edu",
                                      "credit", "hloan", "ploan"), k = 20)
table(df_imp2$job)

table(X_Ftest2$job)

X_Ftest2 <- df_imp2
X_Ftest2 = subset(X_Ftest2, select = -c(job_imp, civil_imp, edu_imp,
                                        credit_imp, hloan_imp, ploan_imp))

## job column
X_Ftest2 <- X_Ftest2 %>%
  mutate(job_admin = ifelse(job == "admin.", 1, 0),
         job_artisan = ifelse(job == "artisan", 1, 0),
         job_entrepreneur = ifelse(job == "entrepreneur", 1, 0),
         job_housemaid = ifelse(job == "housemaid", 1, 0),
         job_management = ifelse(job == "management", 1, 0),
         job_retired = ifelse(job == "retired", 1, 0),
         job_selfemployed = ifelse(job == "selfemployed", 1, 0),
         job_services = ifelse(job == "services", 1, 0),
         job_student = ifelse(job == "student", 1, 0),
         job_technician = ifelse(job == "technician", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(job))

## Civil column
X_Ftest2 <- X_Ftest2 %>%
  mutate(civil_divorced = ifelse(civil == "divorced", 1, 0),
         civil_married = ifelse(civil == "married", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(civil))

## edu column
X_Ftest2 <- X_Ftest2 %>%
  mutate(edu_12K = ifelse(edu == "12K", 1, 0),
         edu_4K = ifelse(edu == "4K", 1, 0),
         edu_9K = ifelse(edu == "9K", 1, 0),
         edu_6K = ifelse(edu == "6K", 1, 0),
         edu_appr = ifelse(edu == "apprenticeship", 1, 0),
         edu_illi = ifelse(edu == "illiterate", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(edu))

## credit column
X_Ftest2 <- X_Ftest2 %>%
  mutate(credit_no = ifelse(credit == "no", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(credit))

## hloan column
X_Ftest2 <- X_Ftest2 %>%
  mutate(hloan_no = ifelse(hloan == "no", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(hloan))

## ploan column
X_Ftest2 <- X_Ftest2 %>%
  mutate(ploan_no = ifelse(ploan == "no", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(ploan))

## ctype column
X_Ftest2 <- X_Ftest2 %>%
  mutate(ctype_cel = ifelse(ctype == "cellular", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(ctype))

## month column
X_Ftest2 <- X_Ftest2 %>%
  mutate(mon_apr = ifelse(month == "apr", 1, 0),
         mon_aug = ifelse(month == "aug", 1, 0),
         mon_dec = ifelse(month == "dec", 1, 0),
         mon_jul = ifelse(month == "jul", 1, 0),
         mon_jun = ifelse(month == "jun", 1, 0),
         mon_mar = ifelse(month == "mar", 1, 0),
         mon_may = ifelse(month == "may", 1, 0),
         mon_nov = ifelse(month == "nov", 1, 0),
         mon_oct = ifelse(month == "oct", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(month))

## day column
X_Ftest2 <- X_Ftest2 %>%
  mutate(day_mon = ifelse(day == "mon", 1, 0),
         day_tue = ifelse(day == "tue", 1, 0),
         day_wed = ifelse(day == "wed", 1, 0),
         day_thu = ifelse(day == "thu", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(day))

## presult column
X_Ftest2 <- X_Ftest2 %>%
  mutate(pre_non = ifelse(presult == "nonexistent", 1, 0),
         pre_fail = ifelse(presult == "failure", 1, 0))

X_Ftest2 = subset(X_Ftest2, select = -c(presult))

## Numeric Variables
## age column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_age = (age - min(age)) /
           (max(age) - min(age)))

X_Ftest2 = subset(X_Ftest2, select = -c(age))

## ccontact column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_cc = (ccontact - min(ccontact)) /
           (max(ccontact) - min(ccontact)))

X_Ftest2 = subset(X_Ftest2, select = -c(ccontact))

## lcdays column
X_Ftest2$lcdays[X_Ftest2$lcdays == 999] <- -1

X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_lcdays = (lcdays - min(lcdays)) /
           (max(lcdays) - min(lcdays)))

X_Ftest2 = subset(X_Ftest2, select = -c(lcdays))

## pcontact column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_pcontact = (pcontact - min(pcontact)) /
           (max(pcontact) - min(pcontact)))

X_Ftest2 = subset(X_Ftest2, select = -c(pcontact))

## employment column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_employment = (employment - min(employment)) /
           (max(employment) - min(employment)))

X_Ftest2 = subset(X_Ftest2, select = -c(employment))

## cprice column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_cprice = (cprice - min(cprice)) /
           (max(cprice) - min(cprice)))

X_Ftest2 = subset(X_Ftest2, select = -c(cprice))

## cconf column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_cconf = (cconf - min(cconf)) /
           (max(cconf) - min(cconf)))

X_Ftest2 = subset(X_Ftest2, select = -c(cconf))

## euri3 column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_euri3 = (euri3 - min(euri3)) /
           (max(euri3) - min(euri3)))

X_Ftest2 = subset(X_Ftest2, select = -c(euri3))

## employees column
X_Ftest2 <- X_Ftest2 %>%
  mutate(scaled_employees = (employees - min(employees)) /
           (max(employees) - min(employees)))

X_Ftest2 = subset(X_Ftest2, select = -c(employees))


## Data Preprocessing on CHARACTER variable is done.

summary(X)
X = X[,c(2:47, 1)]
summary(X)



# Split training set again
smp_size <- floor(0.7 * nrow(X)) # auc:0.79894
# smp_size <- floor(0.6 * nrow(X)) auc:0.79709
# smp_size <- floor(0.8 * nrow(X)) auc:0.79873
# smp_size <- floor(0.75 * nrow(X)) auc:0.79385


set.seed(23)
train_ind <- sample(seq_len(nrow(X)), size = smp_size)
?createDataPartition
?downSample


# Specify the target variable
# target_variable <- X$outcome

# Downsample the majority class
# downsample <- downSample(x = X, y = X$outcome, list = TRUE, yname = "outcome")

# train_ind <- createDataPartition(downsample$outcome, p = 0.7, list = FALSE, times = 1)


Xtrain <- X[train_ind, ]
Xtest <- X[-train_ind, ]

table(Xtrain$outcome)

table(Xtrain$outcome)

y1 <- ifelse(Xtrain[,47] == 1, 1, 0)

require(pROC)

# ------MODEL SELECTION------

xgb_params = list(
  objective = "binary:logistic",
  eta = 0.08,
  gamma = 0,
  max.depth = 2,
  min_child_weight = 1.9,
  eval_metric = "auc"
)

require(xgboost)

?xgboost

length(as.vector(Xtrain$outcome))

xgb <- xgboost(data = as.matrix(Xtrain[,1:46]), 
               label = as.vector(Xtrain$outcome),
               params = xgb_params, nthread = 12, 
               nrounds = 620, verbose = TRUE,
               early_stopping_rounds = 100, 
               maximize = TRUE)

xgb.prob <- predict(xgb, as.matrix(Xtest[,1:46]), 
                    type = "prob")
xgb.roc <- roc(Xtest$outcome, xgb.prob, 
               plot = FALSE, quiet = TRUE)
auc(xgb.roc) # auc: 18:22 0.7968  18:23 0.7909  18:37 0.7972  18:38 0.7961 18:46 0.7971
# It is very confused to get auc = 1, let me try on real test set.

colnames(X_Ftest2)

Xtrain[,47]

xgb.prob2 <- predict(xgb, as.matrix(X_Ftest2[,1:46]), type = "prob")

pred2 <- predict(xgb, as.matrix(X_Ftest2[,1:46]), type = "prob")

# class.pred2 <- 1*(pred2 > 0.2)

Id <- X_Ftest$id
outcome <- pred2

output <- data.frame(Id, outcome)

# write.csv(output, file = "predictions0324_8.csv", row.names = FALSE)


# Try to use whole training dataset
xgb_whole <- xgboost(data = as.matrix(X[,1:46]), label = as.vector(X$outcome),
               params = xgb_params, nthread = 12, nrounds = 620, verbose = TRUE,
               early_stopping_rounds = 100)

xgb.prob_whole <- predict(xgb_whole, as.matrix(X_Ftest2[,1:46]), type = "prob")

class.pred_whole <- 1*(xgb.prob_whole > 0.2)

Id <- X_Ftest$id
outcome <- xgb.prob_whole

output <- data.frame(Id, outcome)

write.csv(output, file = "predictions0324_9.csv", row.names = FALSE)




