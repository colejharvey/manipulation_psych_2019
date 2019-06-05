###Reading and cleaning survey-experiment data (election manipulation and attitudes / emotions)
###Cole Harvey
###April 25, 2019
rm(list=ls())
library(tidyverse)


dataset <- read.csv("Dataset_Manipulation and social psychology_ PS subject pool 2019_April 25, 2019.csv")

###Q1

traits1a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits1a"]]), "\\|"))
names(traits1a)[1] <- "traits1a.party"
names(traits1a)[2] <- "traits1a.decisive"
names(traits1a)[3] <- "traits1a.type"
names(traits1a)[4] <- "traits1a.allegation"

dataset <- cbind(dataset, traits1a)

traits1b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits1b"]]), "\\|"))
names(traits1b)[1] <- "traits1b.party"
names(traits1b)[2] <- "traits1b.decisive"
names(traits1b)[3] <- "traits1b.type"
names(traits1b)[4] <- "traits1b.allegation"

dataset <- cbind(dataset, traits1b)

###Q2


traits2a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits2a"]]), "\\|"))
names(traits2a)[1] <- "traits2a.party"
names(traits2a)[2] <- "traits2a.decisive"
names(traits2a)[3] <- "traits2a.type"
names(traits2a)[4] <- "traits2a.allegation"

dataset <- cbind(dataset, traits2a)

traits2b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits2b"]]), "\\|"))
names(traits2b)[1] <- "traits2b.party"
names(traits2b)[2] <- "traits2b.decisive"
names(traits2b)[3] <- "traits2b.type"
names(traits2b)[4] <- "traits2b.allegation"

dataset <- cbind(dataset, traits2b)


###Q3


traits3a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits3a"]]), "\\|"))
names(traits3a)[1] <- "traits3a.party"
names(traits3a)[2] <- "traits3a.decisive"
names(traits3a)[3] <- "traits3a.type"
names(traits3a)[4] <- "traits3a.allegation"

dataset <- cbind(dataset, traits3a)

traits3b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits3b"]]), "\\|"))
names(traits3b)[1] <- "traits3b.party"
names(traits3b)[2] <- "traits3b.decisive"
names(traits3b)[3] <- "traits3b.type"
names(traits3b)[4] <- "traits3b.allegation"

dataset <- cbind(dataset, traits3b)


###Q4


traits4a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits4a"]]), "\\|"))
names(traits4a)[1] <- "traits4a.party"
names(traits4a)[2] <- "traits4a.decisive"
names(traits4a)[3] <- "traits4a.type"
names(traits4a)[4] <- "traits4a.allegation"

dataset <- cbind(dataset, traits4a)

traits4b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits4b"]]), "\\|"))
names(traits4b)[1] <- "traits4b.party"
names(traits4b)[2] <- "traits4b.decisive"
names(traits4b)[3] <- "traits4b.type"
names(traits4b)[4] <- "traits4b.allegation"

dataset <- cbind(dataset, traits4b)


###Q5


traits5a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits5a"]]), "\\|"))
names(traits5a)[1] <- "traits5a.party"
names(traits5a)[2] <- "traits5a.decisive"
names(traits5a)[3] <- "traits5a.type"
names(traits5a)[4] <- "traits5a.allegation"

dataset <- cbind(dataset, traits5a)

traits5b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits5b"]]), "\\|"))
names(traits5b)[1] <- "traits5b.party"
names(traits5b)[2] <- "traits5b.decisive"
names(traits5b)[3] <- "traits5b.type"
names(traits5b)[4] <- "traits5b.allegation"

dataset <- cbind(dataset, traits5b)



###Q6


traits6a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits6a"]]), "\\|"))
names(traits6a)[1] <- "traits6a.party"
names(traits6a)[2] <- "traits6a.decisive"
names(traits6a)[3] <- "traits6a.type"
names(traits6a)[4] <- "traits6a.allegation"

dataset <- cbind(dataset, traits6a)

traits6b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits6b"]]), "\\|"))
names(traits6b)[1] <- "traits6b.party"
names(traits6b)[2] <- "traits6b.decisive"
names(traits6b)[3] <- "traits6b.type"
names(traits6b)[4] <- "traits6b.allegation"

dataset <- cbind(dataset, traits6b)



###Q7


traits7a <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits7a"]]), "\\|"))
names(traits7a)[1] <- "traits7a.party"
names(traits7a)[2] <- "traits7a.decisive"
names(traits7a)[3] <- "traits7a.type"
names(traits7a)[4] <- "traits7a.allegation"

dataset <- cbind(dataset, traits7a)

traits7b <- do.call(rbind.data.frame, strsplit(as.character(dataset[["traits7b"]]), "\\|"))
names(traits7b)[1] <- "traits7b.party"
names(traits7b)[2] <- "traits7b.decisive"
names(traits7b)[3] <- "traits7b.type"
names(traits7b)[4] <- "traits7b.allegation"

dataset <- cbind(dataset, traits7b)


###Naming and cleaning DVs

  ##Binary choice variables

dataset$q1.electionA <- ifelse(dataset$Q59 == "Election A", 1, 0)
dataset$q1.electionB <- ifelse(dataset$Q59 == "Election B", 1, 0)

dataset$q2.electionA <- ifelse(dataset$Q61 == "Election A", 1, 0)
dataset$q2.electionB <- ifelse(dataset$Q61 == "Election B", 1, 0)

dataset$q3.electionA <- ifelse(dataset$Q62 == "Election A", 1, 0)
dataset$q3.electionB <- ifelse(dataset$Q62 == "Election B", 1, 0)

dataset$q4.electionA <- ifelse(dataset$Q64 == "Election A", 1, 0)
dataset$q4.electionB <- ifelse(dataset$Q64 == "Election B", 1, 0)

dataset$q5.electionA <- ifelse(dataset$Q65 == "Election A", 1, 0)
dataset$q5.electionB <- ifelse(dataset$Q65 == "Election B", 1, 0)

dataset$q6.electionA <- ifelse(dataset$Q72 == "Election A", 1, 0)
dataset$q6.electionB <- ifelse(dataset$Q72 == "Election B", 1, 0)

dataset$q7.electionA <- ifelse(dataset$Q75 == "Election A", 1, 0)
dataset$q7.electionB <- ifelse(dataset$Q75 == "Election B", 1, 0)


  #Renaming variables
  #Make a tibble

dataset <- as_tibble(dataset)

  #Ratings
dataset2 <- rename(dataset, q2.rating = Q67, q3.rating = Q71, q4.rating = Q70, q5.rating  = Q68, 
                   q6.rating = Q73, q7.rating = Q76)
  #Controls

dataset2 <- rename(dataset2, gender = Q2, age = Q3, hispanic = Q4, race = Q5, income = Q6, ideology = Q7, work.status = Q15,
                  party.id  = Q8, party.lean = Q11, strength.dem = Q9, strength.rep = Q10, interest.politics = Q12,
                  activities.protest = Q38_1, activities.contact.official = Q38_2, activities.volunteer = Q38_3,
                  activities.donate = Q38_4, activities.comment.online = Q38_5, activities.held.office = Q38_6, voted.2018 = Q14, influence.on.politics = Q18)

#####
 
  
  
test.model.a <- glm(q1.electionA~traits1a.party + traits1a.decisive + traits1a.type + traits1a.allegation, 
                  family=binomial(link="logit"), data=dataset[-1,])
summary(test.model.a)

test.model.b <- glm(q1.electionB~traits1b.party + traits1b.decisive + traits1b.type + traits1b.allegation, 
                    family=binomial(link="logit"), data=dataset[-1,])
summary(test.model.b)
