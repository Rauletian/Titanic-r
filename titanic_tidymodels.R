library(tidyverse)
library(na.tools)
library(mice)
library(randomForest)
library(stringr)
library(caret)
library(tidymodels)
library(ranger)

train <- read_csv("R/datasets/train.csv")
test<-read_csv("R/datasets/test.csv")
bind_rows(test,train)->full

full%>%
  summarise_all(funs(sum(is.na(.))))->na_resume
table(na_resume)


full%>%
  separate(Name,into=c("Surname","a"),sep=",")->full

full$Title <- gsub('(.*, )|(\\..*)', '', full$a)

full%>%
  select(-4)->full

table(full$Title,full$Sex)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'


full%>%
  mutate(familySize=full$SibSp+full$Parch+1, 
         familySurSiz=paste0(Surname,familySize))->full


full$Age<-replace_na(full$Age,mean(full$Age))

set.seed(123)
imput<-mice(full[,2:11], m=5)
print(imput)
imput$imp$Age

complete(imput)->values_complete

full%>%
  group_by(Pclass)%>%
  summarise(avgFare=mean(Fare))->meanFareClass

full$Age<-values_complete$Age

full$Fare<-values_complete$Fare

full%>%
  group_by(Pclass)%>%
  summarise(avgFare=mean(Fare))->meanFareClass




#full%>%
#mutate(PPayed=if Pclass==3&Fare>13.29292:
#PPayed="Above Average")




full%>%
  mutate(lstage=case_when(Age<13~"Child",
                          Age>=13&Age<18~"Teenager",
                          Age>=18&Age<65~"Adult",
                          Age>=65~"Elder"))->full


full$Cabin%>%
  na.replace("N")->full$Cabin

full%>%
  separate(Cabin, into=c("deck"), sep=1, fill="left")->full

###-------Fare----------
full%>%
  group_by(deck)%>%
  summarise(FareM=mean(Fare))->FareDeck

FareDeck%>%
  summarise(quants = quantile(FareM, 
                              probs = c(0.33, 0.66,0.99)))->FareQuants

FareDeck%>%
  mutate(Quant=case_when(FareM<29.60320~"Low",
                         FareM>=29.60320&FareM<53.44338~"Medium",
                         FareM>=53.44338&FareM<121.22656~" Medium High",
                         FareM>=121.22656~" High"
  ))->FareDeck
full%>%
  left_join(FareDeck, by="deck")->full

full%>%
  group_by(Pclass)%>%
  summarise(meanFare=mean(Fare))->summaryFare

full%>%
  mutate(AvgFareP=case_when(Pclass==1~87.50899,
                            Pclass==2~21.17920,
                            Pclass==3~13.29292))->full


full%>%
  mutate(Ppayed=case_when(Fare>AvgFareP~"Above average",
                          Fare<AvgFareP~"Below average",
                          Fare==AvgFareP~"Average"))->full

full%>%
  select(-AvgFareP)->full

full%>%
  mutate(FamilyMem=SibSp+Parch,
         TravAlone=case_when(FamilyMem==0~TRUE,
                             FamilyMem!=0~FALSE))%>%
  select(-FamilyMem)->full


full%>%
  ggplot(aes(x=TravAlone,fill=Ppayed))+
  geom_bar()

full%>%
  ggplot(aes(x=Quant,fill=Survived))+
  geom_bar()


full%>%
  ggplot(aes(x=lstage,fill=TravAlone))+
  geom_bar()

full$Quant%>%
  factor(levels = c("Low","Medium","Medium High","High"))->a

full%>%
  mutate(QuantFactor=a)->full



full%>%
  group_by(Quant)%>%
  summarise(count=length(Quant))->countQuants


full%>%
  filter(Survived==1)%>%
  group_by(Quant)%>%
  summarise(positive=sum(Survived))->probSurvQuant

probSurvQuant%>%
  left_join(countQuants, by="Quant")%>%
  mutate(prob=positive/count)->probSurvQuant


probSurvQuant%>%
  mutate(TickEff=case_when(Quant==" High"~"Best",
                           Quant==" Medium High"~"Third Best",
                           Quant=="Low"~"Fourth Best",
                           Quant=="Medium"~"Second Best"))->probSurvQuant

full%>%
  left_join(probSurvQuant, by="Quant")->full

full%>%
  select(-positive,-count,-prob)->full

#full%>%
#mutate(EconStatus=case_when(Fare>=87.50899~"High",
#Fare>=21.17920~"Medium",
#Fare>=13.29408~"Medium Low",
#Fare>=0~"Low"
#)
#)->full

full%>%
  ggplot(aes(x=Embarked, fill=EconStatus))+
  geom_bar()
full%>%
  filter(is.na(full$EconStatus)==TRUE)->na_econStatus

full$Embarked%>%
  na.replace("S")->a
a->full$Embarked




full%>%
  mutate(PriceP=paste(QuantFactor,Ppayed,sep="+"))->full

full%>%
  select(-Ppayed,-QuantFactor)->full
full%>%
  select(-Quant)->full


full%>%
  summarise_all(funs(sum(is.na(.))))->na_resume
View(na_resume)



#####------------tidymodels----------
full$Survived%>%
  factor(levels = c("0","1"))->a

full$Survived<-a



testComplete<-full[1:418,]

trainComplete<-full[419:1309,]

a<-initial_split(trainComplete,prop=0.8,breaks = 2)
train<-training(a)
test<-testing(a)





rec_titanic<-recipe(train, Survived~PassengerId,Pclass,
                    Sex,Age,Fare,Deck,
                    Survived,familySize,
                    lstage,TravAlone,TickEff,PriceP)%>%
  update_role(PassengerId, new_role = "ID")%>%
  step_dummy(all_nominal(),-all_outcomes())


prep_rec<-prep(rec_titanic)
juiced<-juice(prep_rec)


tune_spec<-rand_forest(mtry=tune(),
                       trees=100,
                       min_n=tune())%>%
  set_mode("classification")%>%
  set_engine("ranger")

tune_wf<-workflow()%>%
  add_recipe(rec_titanic)%>%
  add_model(tune_spec)
########--------tune hyperparameters-------

set.seed(123)
trees_fold<-vfold_cv(train, 2)

set.seed(234)
 
tune_res<-tune_grid(tune_wf,
                    resamples=trees_fold,
                    5)





collect_metrics(tune_res)%>%
  filter(.metric=="accuracy",mean>0.715)%>%
  mutate(mtry=factor(mtry))%>%
  ggplot(aes(x=min_n, y=mean,color=mtry))+
  geom_point()+
  geom_line()
  
best_tune<-select_best(tune_res, metric="accuracy")
final_specs<-finalize_model(tune_spec,best_tune)

library(vip)
final_specs%>%
  set_engine("ranger", importance="permutation")%>%
  fit(Survived~.,
      data = juice(prep_rec))

final_wf<-workflow()%>%
  add_recipe(rec_titanic)%>%
  add_model(final_specs)

final_pred<-final_wf%>%
  last_fit(a)
final_pred%>%
  collect_metrics()

final_complete<-fit(final_wf,trainComplete)
predict(final_complete,testComplete)->pred

solution <- data.frame(PassengerID = testComplete$PassengerId,
                       Survived = pred)
colnames(solution)<-c("PassengerId","Survived")

write.csv(solution, file = 'rfTFinal_mod_Solution.csv', row.names = F)
