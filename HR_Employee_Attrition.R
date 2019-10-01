#The main aim of this project is to find the employee attrition rate or baiscally the turnover rate of employees inside
#an organization

#Importing the necessary libraries
install.packages('skimr')
library(rattle)
library(tree)
library(caTools)
library(reshape2)
library(forcats)
library(ggrepel)
library(plotrix)
library(RColorBrewer)
library(ggpubr)
library(magrittr)
library(cowplot)
library(repr)
library(treemapify)
library(treemap)
library(scales)
library(psych)
library(ggthemes)
library(ggplot2)
library(lgbdl)
library(corrgram)
library(rpart.plot)
library(ggcorrplot)
library(h2o)
library(xgboost)
library(rpart)
library(e1071)
library(randomForest)
library(caret)
library(viridis)
library(plotly)
library(GGally)
library(skimr)
library(tidyverse)
library(pROC)

library(devtools)
options(devtools.install.args = "--no-multiarch")
install_git("https://github.com/Microsoft/LightGBM", subdir = "R-package")

HR <- read.csv('HR-Employee-Attrition.csv')

head(HR)

#Implementing a deep summary with skim and Kable
HR %>% glimpse()

#Inference:
# There are 1470 observations (rows), 35 features (variables)
## We can observe that there are only two datatypes in our dataset which are integer and Factor

#Checking for any missing values in the dataset
sapply(HR, function(x)sum(is.na(x)))
#We can see that there are no missing values in our dataset


#Checking the distribution of our labels
options(repr.plot.width = 8, repr.plot.height = 4)
percentageattrition <- HR %>% group_by(Attrition) %>% summarise(Count = n()) %>%
mutate(pct = round(prop.table(Count),2) * 100) %>% ggplot(aes(x= Attrition,y=pct)) + geom_bar(stat = 'identity',fill = "orange", color="grey40")+geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.2f%%", pct)),
            hjust=0.5, vjust=-3, size=4, 
            colour="black", fontface="bold") + theme_bw() + labs(x="Employee Attrition", y="Percentage") + 
  labs(title="Employee Attrition (%)") + theme(plot.title=element_text(hjust=0.5))

plot_grid(percentageattrition, align="h", ncol=1)
#There is an imbalance in the dataset with 84% of the employees did not quit the organization while 16% of the employees quuit the organization

# Anaysis of the Genderr
ageavg <- HR %>% select(Gender,ï..Age) %>% group_by(Gender) %>% summarize(avg=mean(ï..Age))
ageavg

#Analysis of Age Distribution by Gender
HRtext <- data.frame(label = c("Mean = 37.33 \n Years Old", "Mean = 36.65 \n Years Old"),Gender   = c("Female", "Male"))
HRtext

distgender <- HR %>% select(Gender, ï..Age) %>% filter(Gender == 'Male' | Gender== "Female") %>% 
  filter(!is.na(ï..Age)) %>% group_by(Gender) %>% 
  ggplot(aes(x=ï..Age)) + geom_density(aes(fill=Gender), alpha=0.8, show.legend=FALSE) + facet_wrap(~Gender) + theme_minimal() + 
  geom_vline(aes(xintercept=mean(ï..Age)),
             color="red", linetype="dashed", size=1) + labs(title="Age Distribution") + 
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("orange", "green")) + 
  geom_text(
    data    = HRtext,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1
  )

distoverall <- HR %>% select(Gender, ï..Age) %>% filter(!is.na(ï..Age)) %>% 
  ggplot(data=HR, mapping=aes(x=ï..Age)) + geom_density(color="pink", fill="darkblue") + 
  geom_vline(aes(xintercept=mean(ï..Age)),
             color="red", linetype="dashed", size=1) +  theme_minimal() + labs(x="Overall Age") + 
  annotate("text", label = "Mean = 36.92 Years Old", x = 50, y = 0.03, color = "black")

plot_grid(distgender, distoverall , nrow = 2)

# Distribution Of Job Satistifaction
attritionbox <- HR %>% select(Attrition,JobSatisfaction,Gender) %>% ggplot(aes(x= Attrition,y=JobSatisfaction,fill = Attrition)) + geom_boxplot(color = 'black')+theme_minimal() + facet_wrap(~Gender)+
  scale_fill_manual(values=c("orange", "red"))
attritionbox

#Income Distribution by Gender
v <-ggplot(HR,aes(x=Gender,y= MonthlyIncome,fill= Gender,color = Gender))+geom_boxplot()+scale_fill_manual(values=c("green", "pink")) + scale_color_manual(values=c("green", "pink")) +
  coord_flip() + labs(title="Are there any Gender Disparities in Income?")
v

#Analysis of Average Income and Presence by Department
options(repr.plot.width=10, repr.plot.height=8) 

incomegender <- HR %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income)) + geom_bar(stat="identity", fill="#2E9AFE", width=0.5) + 
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-2, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=360) + labs(title="Average Salary by Gender", x="Gender",y="Salary") +  theme_minimal() + theme(plot.title=element_text(size=14, hjust=0.5))
incomegender
# # How many people work in each department by gender
departmentgender <- HR %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("blue", "orange")) + 
  labs(title="Number of Employees \n
by Department",x="Department", y="Number of employees")

plot_grid(incomegender, departmentgender,ncol=2, nrow=1)

#Summary
#The average age of females is 37.33 and for males is 36.65 and both distributions are similar.
# individuals who didn't leave the organization, job satisfaction levels are practically the same.But for people who left the organization , females had a lower satisfaction level as opposed to males.
#The average salaries for both the genders are the same where females have a salary of 6686.5$ while males have a salary of 6380.51$
#higher number of males in the three departments however, females are more predominant in the Research and Development department.

#Analysis of Income Towards Attrition
# Let's Analyze if income was a major element when it came to leaving the company.
# Let's start by taking the average monthly income of people who left the company and people who stayed in the company
# Group by department

incomeavg<-HR %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
  summarize(incavg=mean(MonthlyIncome)) %>%
  ggplot(aes(x=reorder(Department, incavg), y=incavg, fill=Attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Attrition) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("yellow", "orange")) + 
  labs(y="Average Income", x="Department", title="Average Income by Department \n and Attrition Status") + 
  geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(incavg,2))),
            hjust=-0.5, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=90)
incomeavg


#Analysis of Satisfaction by Income
options(repr.plot.width=8, repr.plot.height=5) 

# Turn the column to factor: One because it should not be considered an integer
# Two: Will help us sort in an orderly manner.
HR$JobSatisfaction <- as.factor(HR$JobSatisfaction)

inchigh <- HR %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
  summarize(med1=median(MonthlyIncome)) %>%
  ggplot(aes(x=fct_reorder(JobSatisfaction, -med1), y=med1, color=Attrition)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=JobSatisfaction, 
                   xend=JobSatisfaction, 
                   y=0, 
                   yend=med1)) + facet_wrap(~Attrition) + 
  labs(title="Do Employees leave due to Income?", 
       subtitle="by Attrition Status",
       y="Median Income",
       x="Level of Job Satisfaction") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6), plot.title=element_text(hjust=0.5), strip.background = element_blank(),
        strip.text = element_blank()) + 
  coord_flip() + theme_minimal() + scale_color_manual(values=c('purple', "green")) + 
  geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med1,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)
inchigh

#Summary
#We see considerate amount of differences in every department by each attrition level
# We can infer that less the job satisfaction the more wide is the gap according to every attrition status by the levels of income


#Analysis of the working environment
# We will now explore how many employees are associated with each JobRole
options(repr.plot.width=10, repr.plot.height=10) 
ggplot(HR,aes(x=JobRole,fill=JobRole))+geom_bar()+coord_flip()

# We will now explore how many employees are associated with each EducationField
# Create a TreeMap with the number of Employees by JobRole
library(tree)
educarol <- HR %>% select(EducationField) %>% group_by(EducationField) %>% summarize(amount=n()) %>%
  ggplot(aes(area=amount, fill=EducationField, label=EducationField)) +  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.position = "none") +
  labs(
    title = "Major Job Roles Inside the Organization",
    caption = "The area of each tile represents the number of
    employees by type of job role.",
    fill = "JobRole"
  )

educarol

#Summary
#We infer that Sales and Research Scientist are the job positions having the maximum number of employees
#We infer that Medical and Life Sciences fields have the maximimum number of Employees


#Implementing the correlation matrix
options(repr.plot.width=10, repr.plot.height=10) 

nums <- select_if(HR, is.numeric)
nums

corr <- round(cor(nums),1)

ggcorrplot(corr, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("green", "orange", "red"), 
           title="Employee Attritions correlation matrix", 
           ggtheme=theme_minimal())

#Summary
#Correlation Matrix determines the degree of association between any two variables in a dataset
#We infer that as the total working years increases the monthly income of an employee increases
# We infer that as the percent salary increases the performance rating increases
# We infer as the years with current manager increases the number of years increases since last promotion.
#We infer as the age increases the monthly income increases

HR_df <- HR

HR_df1 <- HR
set.seed(123)

#Shuffling my data before splitting
HR_df < HR_df[sample(nrow(HR_df)),]

#Encoding the ordinal variables
HR_df1$BusinessTravel = factor(HR_df1$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))
# Transforming the datatype from integer to Factors from the ordinal Variables

HR_df1$Education<-as.factor(HR_df1$Education)
HR_df1$EnvironmentSatisfaction<-as.factor(HR_df1$EnvironmentSatisfaction)
HR_df1$JobInvolvement<-as.factor(HR_df1$JobInvolvement)
HR_df1$JobLevel<-as.factor(HR_df1$JobLevel)
HR_df1$JobSatisfaction<-as.factor(HR_df1$JobSatisfaction)
HR_df1$PerformanceRating<-as.factor(HR_df1$PerformanceRating)
HR_df1$RelationshipSatisfaction<-as.factor(HR_df1$RelationshipSatisfaction)
HR_df1$StockOptionLevel<-as.factor(HR_df1$StockOptionLevel)
HR_df1$TrainingTimesLastYear<-as.factor(HR_df1$TrainingTimesLastYear)
HR_df1$WorkLifeBalance<-as.factor(HR_df1$WorkLifeBalance)

# Delete unecessary columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount")

HR_df1[cols] <- NULL

#Splitting our Dataset
# Splitting our data
Indextrain <- createDataPartition(HR_df1$Attrition, p=0.8, 
                                  list=FALSE, times=1)

train <- HR_df1[Indextrain,]
test <- HR_df1[-Indextrain,]

# Checking that both the training and testing sets have the same label proportions.
trainpop <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

testpop <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

trainpop
testpop

# We can see that there is an imbalance in our dataset. We will use the technique of cross validation
#and oversampling to solve it
dtree <- rpart(Attrition ~., data = train)
preds <- predict(dtree, test, type = "class")

rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc

prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)


#A rather poor AUC with a rather poor sensitivity. It seems that building a single tree will not bring us anywhere.

#However, while not being useful in general, such a model can nevertheless help us see some patterns. Let us plot the tree and see if we can find any.
# Pruning & plotting the tree

dtreepr <- prune(dtree, cp = 0.01666667)
predspr <- predict(dtreepr, test, type = "class")

rocvpr <- roc(as.numeric(test$Attrition), as.numeric(predspr))
rocvpr$auc

rpart.plot(dtreepr, 
           type = 4, 
           extra = 104, 
           tweak = 0.9, 
           fallen.leaves = F)

#We have pruned the tree a bit, just so that it is not too crowded and too incomprehensible (our AUC did not suffer much as you can see).

#You can see that the most important variables seem to be overtime and monthly income - something we have already discerned through our graphic EDA. Remember, the sensitivity of this model is quite low, which is why we would in principle advise against any general interventions on this basis.

#However, we can see that a major percentage of those who left can be relatively reliably identified using the criteria of combined overtime and monthly income. If we consider them jointly, this points to another factor: effort-reward imbalance. This is why it is not entirely unuseful after all to plot a decision tree - it makes you see some patterns that you might have forgotten during your EDA

# Random Forest and Simple GBM

# Random forest

fit.forest <- randomForest(Attrition ~., data = train)
rfpreds <- predict(fit.forest, test, type = "class")

rocrf <- roc(as.numeric(test$Attrition), as.numeric(rfpreds))
rocrf$auc


# Setting the basic train control used in all GBM models

ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Simple GBM

gbmfit <- train(Attrition ~., 
                data = train, 
                method = "gbm", 
                verbose = FALSE, 
                metric = "ROC", 
                trControl = ctrl)

gbmpreds <- predict(gbmfit, test)

rocgbm <- roc(as.numeric(test$Attrition), as.numeric(gbmpreds))
rocgbm$auc


#Modeling (GBM with weighting, SMOTE and up & down-sampling)
#Mightbe we should check the class imbalanceNote that usually this would be considered if the ratio between classes is 1:10 or higher; in our case it's 1:5, but still it may be justified since we have seen with the decision tree that our main problem is predicting those who actually leave (sensitivity).

#Tryiing different techniques which are weighting (punishing the errors in the minority class), down-sampling (randomly removing cases from the majority class), up-sampling (randomly replicating instances in the minority class) and SMOTE (downsampling and synthesizing new minority cases).

ctrl$seeds <- gbmfit$control$seeds

# Weighting 

model_weights <- ifelse(train$Attrition == "No",
                        (1/table(train$Attrition)[1]) * 0.5,
                        (1/table(train$Attrition)[2]) * 0.5)

weightedfit <- train(Attrition ~ .,
                     data = train,
                     method = "gbm",
                     verbose = FALSE,
                     weights = model_weights,
                     metric = "ROC",
                     trControl = ctrl)

weightedpreds <- predict(weightedfit, test)
rocweight <- roc(as.numeric(test$Attrition), as.numeric(weightedpreds))
rocweight$auc

# UP-sampling

ctrl$sampling <- "up"

upfit <- train(Attrition ~., 
               data = train, 
               method = "gbm", 
               verbose = FALSE, 
               metric = "ROC", 
               trControl = ctrl)

uppreds <- predict(upfit, test)
rocup <- roc(as.numeric(test$Attrition), as.numeric(uppreds))
rocup$auc


#Modeling (Random Forest with weighting, SMOTE and up & down-sampling)
#Mightbe we should check the class imbalanceNote that usually this would be considered if the ratio between classes is 1:10 or higher; in our case it's 1:5, but still it may be justified since we have seen with the decision tree that our main problem is predicting those who actually leave (sensitivity).

#Tryiing different techniques which are weighting (punishing the errors in the minority class), down-sampling (randomly removing cases from the majority class), up-sampling (randomly replicating instances in the minority class) and SMOTE (downsampling and synthesizing new minority cases).
# Setting the basic train control used in all GBM models

ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Simple Randomn Forest

rffit <- train(Attrition ~., 
                data = train, 
                method = "rf", 
                verbose = FALSE, 
                metric = "ROC", 
                trControl = ctrl)

rfpreds <- predict(rffit, test)

rocrf <- roc(as.numeric(test$Attrition), as.numeric(rfpreds))
rocrf$auc
ctrl$seeds <- rffit$control$seeds

# Weighting 

model_weights <- ifelse(train$Attrition == "No",
                        (1/table(train$Attrition)[1]) * 0.5,
                        (1/table(train$Attrition)[2]) * 0.5)

weightedfit <- train(Attrition ~ .,
                     data = train,
                     method = "rf",
                     verbose = FALSE,
                     weights = model_weights,
                     metric = "ROC",
                     trControl = ctrl)

weightedpreds <- predict(weightedfit, test)
rocweight <- roc(as.numeric(test$Attrition), as.numeric(weightedpreds))
rocweight$auc

# UP-sampling

ctrl$sampling <- "up"

upfit <- train(Attrition ~., 
               data = train, 
               method = "rf", 
               verbose = FALSE, 
               metric = "ROC", 
               trControl = ctrl)

uppreds <- predict(upfit, test)
rocup <- roc(as.numeric(test$Attrition), as.numeric(uppreds))
rocup$auc

#Undestanding the model and making sense of it
#How can we help ourselves with these findings? Complex algorithms aren't easy to interpret, but there are several ways in which they can be useful:
#We can examine the variable importance list, and see which factors in general are helpful in determining the outcome (e.g. attrition); this can be also useful in determining where should we carry out our (HR) audit first


ggplot(varImp(weightedfit)) + 
  geom_bar(stat = 'identity', fill = 'orange', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()

#The top 5 factors which influence the attrition are
#Monthly Income
#Total Working Years
#Age
#Daily Rate
#Monthly Rate