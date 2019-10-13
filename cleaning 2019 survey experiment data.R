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
 
  #Choice

dataset2 <- rename(dataset2, q1.choice = Q59, q2.choice = Q61, q3.choice = Q62, q4.choice = Q64, q5.choice = Q65,
                   q6.choice = Q72, q7.choice = Q75)

  #Controls

dataset2 <- rename(dataset2, gender = Q2, age = Q3, hispanic = Q4, race = Q5, income = Q6, ideology = Q7, work.status = Q15,
                  party.id  = Q8, party.lean = Q11, strength.dem = Q9, strength.rep = Q10, interest.politics = Q12,
                  activities.protest = Q38_1, activities.contact.official = Q38_2, activities.volunteer = Q38_3,
                  activities.donate = Q38_4, activities.comment.online = Q38_5, activities.held.office = Q38_6, voted.2018 = Q14, influence.on.politics = Q18)

  #Emotions

dataset2 <- rename(dataset2, emotion.anger = Q16_1, emotion.disgust = Q16_2, emotion.outrage = Q16_3, emotion.fear = Q16_4,
                   emotion.nervous = Q16_5, emotion.hopeful = Q16_6, emotion.proud = Q16_7, emotion.happy = Q16_8)

 #Morality

dataset2 <- rename(dataset2, vignette.impression = Q22, core.conviction = Q23_1, right.wrong = Q23_2)

 #Group efficacy

dataset2 <- rename(dataset2, dem.change.together = Q25_1, dem.improve.fairness = Q25_2, dem.stand.up = Q25_3, dem.influence.govt = Q25_4)
dataset2 <- rename(dataset2, rep.change.together = Q51_1, rep.improve.fairness = Q51_2, rep.stand.up = Q51_3, rep.influence.govt = Q51_4)

 #Group identity

dataset2 <- rename(dataset2, dem.important = Q26, dem.describe = Q29, dem.we = Q27, dem.self = Q28)
dataset2 <- rename(dataset2, rep.important = Q30, rep.describe = Q31, rep.we = Q32, rep.self = Q33)


 #Manipulation checks

dataset2 <- rename(dataset2, vignette.recall = Q35, dem.demonstration = Q45_1, dem.voice = Q45_2,
                   dem.with.copartisans = Q45_3, dem.petition = Q45_4, dem.donate.party = Q45_5, dem.donate.ngo = Q45_6,
                   dem.volunteer.ngo = Q45_7, 
                   rep.demonstration = Q46_1, rep.voice = Q46_2,
                   rep.with.copartisans = Q46_3, rep.petition = Q46_4, rep.donate.party = Q46_5, rep.donate.ngo = Q46_6,
                   rep.volunteer.ngo = Q46_7)


write.csv(dataset2, "~/Research projects/manipulation_psych_2019/dataset_updated.csv")

#####
dataset <- read.csv("dataset_updated.csv") 
dataset <- dataset %>% mutate(q1.choice.binary = if_else(q1.choice == "Election B", 1, 0))
  
  
test.model.a <- glm(q1.electionA~traits1a.party + traits1a.decisive + traits1a.type + traits1a.allegation 
                     #+ traits1b.party + traits1b.decisive + traits1b.type + traits1b.allegation
                  ,family=binomial(link="logit"), data=dataset[-5,])
summary(test.model.a)


test.model.b <- glm(q1.electionB~traits1b.party + traits1b.decisive + traits1b.type + traits1b.allegation
                    ,family=binomial(link="logit"), data=dataset[-5,])
summary(test.model.b)


####Disregard below except as examples

dataset <- read.csv("dataset_updated.csv") 

d.q1.party <- gather(dataset[-5,], "traits1a.party", "traits1b.party", key = "profile.party", value="party")
d.q1.party <- d.q1.party %>% select("ResponseId", "party")

d.q1.dec <- gather(dataset[-5,], 'traits1a.decisive', 'traits1b.decisive', key="profile.dec", value="decisiveness")
d.q1.dec <- d.q1.dec %>% select("ResponseId", "decisiveness")

d.q1.type <- gather(dataset[-5,], 'traits1a.type', 'traits1b.type', key="profile.type", value="type")
d.q1.type <- d.q1.type %>% select("ResponseId", "type")

d.q1.allegation <- gather(dataset[-5,], 'traits1a.allegation', 'traits1b.allegation', key="profile.allegation", value="allegation")
d.q1.allegation <- d.q1.allegation %>% select("ResponseId", "allegation")

q1.data <- left_join(d.q1.party, d.q1.dec, by="ResponseId")
q1.data <- left_join(q1.data, d.q1.type, by="ResponseId")
q1.data <- left_join(q1.data, d.q1.allegation, by="ResponseId")
q1.data <- arrange(q1.data, ResponseId)

###
dataset <- read.csv("dataset_updated.csv") 

d.q1.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q1.electionA", "q1.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q1.response <- arrange(d.q1.response, ResponseId)

###Getting the traits
d.q1.response <- inner_join(d.q1.response, dataset[-5,], by = "ResponseId")
d.q1.response <- d.q1.response %>% filter(ResponseId != "Response ID")

d.q1.response <- d.q1.response %>% mutate(party = ifelse(election.option=="q1.electionA", as.character(traits1a.party), as.character(traits1b.party)))
d.q1.response <- d.q1.response %>% mutate(decisive = ifelse(election.option=="q1.electionA", as.character(traits1a.decisive), as.character(traits1b.decisive)))
d.q1.response <- d.q1.response %>% mutate(type = ifelse(election.option=="q1.electionA", as.character(traits1a.type), as.character(traits1b.type)))
d.q1.response <- d.q1.response %>% mutate(allegation = ifelse(election.option=="q1.electionA", as.character(traits1a.allegation), as.character(traits1b.allegation)))

###Q2

d.q2.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q2.electionA", "q2.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q2.response <- arrange(d.q2.response, ResponseId)

d.q2.response <- inner_join(d.q2.response, dataset[-5,], by = "ResponseId")
d.q2.response <- d.q2.response %>% filter(ResponseId != "Response ID")

d.q2.response <- d.q2.response %>% mutate(party = ifelse(election.option=="q2.electionA", as.character(traits2a.party), as.character(traits2b.party)))
d.q2.response <- d.q2.response %>% mutate(decisive = ifelse(election.option=="q2.electionA", as.character(traits2a.decisive), as.character(traits2b.decisive)))
d.q2.response <- d.q2.response %>% mutate(type = ifelse(election.option=="q2.electionA", as.character(traits2a.type), as.character(traits2b.type)))
d.q2.response <- d.q2.response %>% mutate(allegation = ifelse(election.option=="q2.electionA", as.character(traits2a.allegation), as.character(traits2b.allegation)))


data.full <- rbind(d.q1.response, d.q2.response)


###Q3

d.q3.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q3.electionA", "q3.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q3.response <- arrange(d.q3.response, ResponseId)

d.q3.response <- inner_join(d.q3.response, dataset[-5,], by = "ResponseId")
d.q3.response <- d.q3.response %>% filter(ResponseId != "Response ID")

d.q3.response <- d.q3.response %>% mutate(party = ifelse(election.option=="q3.electionA", as.character(traits3a.party), as.character(traits3b.party)))
d.q3.response <- d.q3.response %>% mutate(decisive = ifelse(election.option=="q3.electionA", as.character(traits3a.decisive), as.character(traits3b.decisive)))
d.q3.response <- d.q3.response %>% mutate(type = ifelse(election.option=="q3.electionA", as.character(traits3a.type), as.character(traits3b.type)))
d.q3.response <- d.q3.response %>% mutate(allegation = ifelse(election.option=="q3.electionA", as.character(traits3a.allegation), as.character(traits3b.allegation)))


###Q4

d.q4.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q4.electionA", "q4.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q4.response <- arrange(d.q4.response, ResponseId)

d.q4.response <- inner_join(d.q4.response, dataset[-5,], by = "ResponseId")
d.q4.response <- d.q4.response %>% filter(ResponseId != "Response ID")

d.q4.response <- d.q4.response %>% mutate(party = ifelse(election.option=="q4.electionA", as.character(traits4a.party), as.character(traits4b.party)))
d.q4.response <- d.q4.response %>% mutate(decisive = ifelse(election.option=="q4.electionA", as.character(traits4a.decisive), as.character(traits4b.decisive)))
d.q4.response <- d.q4.response %>% mutate(type = ifelse(election.option=="q4.electionA", as.character(traits4a.type), as.character(traits4b.type)))
d.q4.response <- d.q4.response %>% mutate(allegation = ifelse(election.option=="q4.electionA", as.character(traits4a.allegation), as.character(traits4b.allegation)))


###Q5
d.q5.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q5.electionA", "q5.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q5.response <- arrange(d.q5.response, ResponseId)

d.q5.response <- inner_join(d.q5.response, dataset[-5,], by = "ResponseId")
d.q5.response <- d.q5.response %>% filter(ResponseId != "Response ID")

d.q5.response <- d.q5.response %>% mutate(party = ifelse(election.option=="q5.electionA", as.character(traits5a.party), as.character(traits5b.party)))
d.q5.response <- d.q5.response %>% mutate(decisive = ifelse(election.option=="q5.electionA", as.character(traits5a.decisive), as.character(traits5b.decisive)))
d.q5.response <- d.q5.response %>% mutate(type = ifelse(election.option=="q5.electionA", as.character(traits5a.type), as.character(traits5b.type)))
d.q5.response <- d.q5.response %>% mutate(allegation = ifelse(election.option=="q5.electionA", as.character(traits5a.allegation), as.character(traits5b.allegation)))


###Q6
d.q6.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q6.electionA", "q6.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q6.response <- arrange(d.q6.response, ResponseId)

d.q6.response <- inner_join(d.q6.response, dataset[-5,], by = "ResponseId")
d.q6.response <- d.q6.response %>% filter(ResponseId != "Response ID")

d.q6.response <- d.q6.response %>% mutate(party = ifelse(election.option=="q6.electionA", as.character(traits6a.party), as.character(traits6b.party)))
d.q6.response <- d.q6.response %>% mutate(decisive = ifelse(election.option=="q6.electionA", as.character(traits6a.decisive), as.character(traits6b.decisive)))
d.q6.response <- d.q6.response %>% mutate(type = ifelse(election.option=="q6.electionA", as.character(traits6a.type), as.character(traits6b.type)))
d.q6.response <- d.q6.response %>% mutate(allegation = ifelse(election.option=="q6.electionA", as.character(traits6a.allegation), as.character(traits6b.allegation)))


###Q7
d.q7.response <- melt(dataset[-5,], id.vars="ResponseId", 
                      measure.vars = c("q7.electionA", "q7.electionB"),
                      variable.name = "election.option", value.name = "response")
d.q7.response <- arrange(d.q7.response, ResponseId)

d.q7.response <- inner_join(d.q7.response, dataset[-5,], by = "ResponseId")
d.q7.response <- d.q7.response %>% filter(ResponseId != "Response ID")

d.q7.response <- d.q7.response %>% mutate(party = ifelse(election.option=="q7.electionA", as.character(traits7a.party), as.character(traits7b.party)))
d.q7.response <- d.q7.response %>% mutate(decisive = ifelse(election.option=="q7.electionA", as.character(traits7a.decisive), as.character(traits7b.decisive)))
d.q7.response <- d.q7.response %>% mutate(type = ifelse(election.option=="q7.electionA", as.character(traits7a.type), as.character(traits7b.type)))
d.q7.response <- d.q7.response %>% mutate(allegation = ifelse(election.option=="q7.electionA", as.character(traits7a.allegation), as.character(traits7b.allegation)))



data.full <- rbind(d.q1.response, d.q2.response, d.q3.response, d.q4.response, d.q5.response, 
                   d.q6.response, d.q7.response)

write.csv(data.full, "choice response data.csv")

test.model <- glm(response ~ as.factor(party) + as.factor(decisive) + as.factor(type) + 
                    as.factor(allegation ) 
                    ,family=binomial(link="logit"), data=data.full)
summary(test.model)



###Protest response questions (5-7)

data.protest.q5 <- melt(dataset, id.vars="ResponseId", 
                     measure.vars = c("q5.rating", "q6.rating", "q7.rating"),
                     variable.name = "protest.question", value.name = "response.protest")
data.protest.q5 <- arrange(data.protest, ResponseId)


data.elections.q5_7 <- data.full %>% filter(response == 1) %>% 
  filter(election.option == "q5.electionA" | election.option == "q5.electionB"| 
           election.option == "q6.electionA" | election.option == "q6.electionB" |
           election.option == "q7.electionA" | election.option == "q7.electionB")

data.elections.q5_7 <- data.elections.q5_7 %>% mutate(protest.response = ifelse(election.option == "q5.electionA" |
                             election.option == "q5.electionB",
                             q5.rating, ifelse(election.option == "q6.electionA" | election.option == "q6.electionB",
                             q6.rating, q7.rating)))



library(MASS)
test.protest <- polr(as.factor(protest.response) ~ as.factor(party) + as.factor(decisive) + as.factor(type) + 
                       as.factor(allegation), data = data.elections.q5_7)
summary(test.protest)
ctable <- coef(summary(test.protest))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
#Can make plots using this code https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/


###Test data for vignette type
dataset.vignette <- dataset[-5,] %>% filter(ResponseId != "Response ID") 
dataset.vignette <- dataset.vignette %>% mutate(dem.we.num = as.numeric(dem.we))
dataset.vignette <- dataset.vignette %>% mutate(dem.imp.num = as.numeric(dem.important))
dataset.vignette <- dataset.vignette %>% mutate(dem.desc.num = as.numeric(dem.describe))
dataset.vignette <- dataset.vignette %>% mutate(dem.self.num = as.numeric(dem.self))
dataset.vignette <- dataset.vignette %>% mutate(dem.id.index = dem.we.num + dem.imp.num + dem.desc.num + dem.self.num)


dataset.vignette <- dataset.vignette %>% mutate(rep.we.num = as.numeric(rep.we))
dataset.vignette <- dataset.vignette %>% mutate(rep.imp.num = as.numeric(rep.important))
dataset.vignette <- dataset.vignette %>% mutate(rep.desc.num = as.numeric(rep.describe))
dataset.vignette <- dataset.vignette %>% mutate(rep.self.num = as.numeric(rep.self))
dataset.vignette <- dataset.vignette %>% mutate(rep.id.index = rep.we.num + rep.imp.num + rep.desc.num +rep.self.num)

dataset.vignette <- dataset.vignette %>% mutate(party.id.index = dem.id.index + rep.id.index)

dataset.vignette <- dataset.vignette %>% mutate(party.selfid = ifelse(party.id == "Independent",
                                                                      party.lean, party.id))

write.csv(dataset.vignette, "dataset_vignette.csv")


model.test.id <- lm(dem.id.index ~ vignette.type + party.selfid, data=dataset.vignette)
summary(model.test.id)

###Next steps are to construct the indicies for the other DVs
