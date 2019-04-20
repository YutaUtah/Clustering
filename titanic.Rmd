---
title: "titanic"
author: "Yuta Hayashi"
date: "4/18/2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r,message=FALSE,warning=FALSE}
library(ggplot2) # visualization
library(ggthemes) # visualization
library(ggridges) # visualization
library(ggforce) # visualization
library(ggExtra) # visualization
library(GGally) # visualisation
library(scales) # visualization
library(grid) # visualisation
library(gridExtra) # visualisation
library(corrplot) # visualisation
library(VIM) # missing values
library(readr) #read data

# wrangle
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(readr) # data input
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(modelr) # factor manipulation

# model
library(randomForest) # classification
library(xgboost) # classification
library(ROCR) # model validation
```

```{r,message=FALSE,warning=FALSE}
test <- read_csv("titanic/test.csv")
train <- read_csv("titanic/train.csv")
```

We are using readr’s read_csv function to read in the data sets, instead of the default read.csv. 

```{r}
#convert Sex, Pclass, Embarked, and Survived to factors

train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

test <- test %>% mutate(
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

#combine train and test dataset binding by rows
combine  <- bind_rows(train, test) 
```

```{r}
summary(combine)
```

1.Pclass is the Ticket-class: first (1), second (2), and third (3) class tickets were used. We turned it into a factor.

2.Name is the name of the passenger. The names also contain titles and some persons might share the same surname; indicating family relations. We know that some titles can indicate a certain age group. For instance Master is a boy while Mr is a man. This feature is a character string of variable length but similar format.

3.Sex is an indicator whether the passenger was female or male. This is another factor we created from a categorical text string.

4.Age is the integer age of the passenger. There are NaN values in this column.

5.SibSp is another ordinal integer feature describing the number of siblings or spouses travelling with each passenger.

6.Parch is another ordinal integer features that gives the number of parents or children travelling with each passenger.

7.Ticket is a character string of variable length that gives the ticket number.

8.Fare is a float feature showing how much each passenger paid for their rather memorable journey.

9.Cabin gives the cabin number of each passenger. This is another string feature.

10.Embarked shows the port of embarkation as a categorical factor.


```{r}
train %>%
  count(Survived)
```

The next code block is simply extracting the numbers for survivals and non survivals for the example afterwards:

```{r}
surv <- train %>% count(Survived) %>% filter(Survived == 1) %>% .$n
nosurv <- train %>% count(Survived) %>% filter(Survived == 0) %>% .$n
surv/(surv+nosurv)*100
nosurv/(surv+nosurv)*100
```

What we see are the different combinations of missing values for the individual features. For instance, there are 529 NA’s in Cabin alone, 158 in Cabin and Age simultaneously, 1 in Fare, and so on. It is crucial to recognize the number of missing values.

```{r}
aggr(combine, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)
```

```{r}
p_age = ggplot(train) + 
  geom_freqpoly(mapping = aes(x=Age, color=Survived), binwidth=1) +
  guides(fill=TRUE) + 
  theme_few()

p_class = ggplot(train, mapping=aes(x=Sex, fill=Survived)) +
  geom_bar(stat = 'count', position='fill') + 
  labs(x= 'Sex') +
  scale_fill_discrete(name="Survived") +
  theme_few()

p_sex = ggplot(train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Sex') +
  scale_fill_discrete(name="Surv") +
  theme_few()

p_class = ggplot(train, mapping = aes(x = Pclass, fill = Survived, colour = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'class') +
  theme(legend.position = "none")

p_emb = ggplot(train, aes(Embarked, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Embarked') +
  theme(legend.position = "none")

p_sib = ggplot(train, aes(SibSp, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'SibSp') +
  theme(legend.position = "none")

p_par = ggplot(train, aes(Parch, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Parch') +
  theme(legend.position = "none")

p_fare = ggplot(train) +
  geom_freqpoly(mapping = aes(Fare, color = Survived), binwidth = 0.05) +
  scale_x_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,1,2,3,4,4,5,6,7),3,3,byrow=TRUE)
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(p_age, p_sex, p_fare, p_class, p_emb, p_sib, p_par, layout=layout)  
```

```{r}
train %>% group_by(Survived) %>% summarise(median_age = median(Age, na.rm=TRUE))
train %>% group_by(Survived, Sex) %>% count(Sex)
train %>% mutate(single = SibSp==0) %>% count(single) %>% group_by(single) %>% mutate(freq = n/nrow(train))
```

```{r}
train %>% select(-PassengerId, -Name, -Cabin, -Ticket) %>% mutate(Sex = fct_recode(Sex, "0" = "male", "1" = "female")) %>%

  mutate(Sex = as.integer(Sex),

         Pclass = as.integer(Pclass),

         Survived = as.integer(Survived),

         Embarked = as.integer(Embarked)) %>%

  cor(use="complete.obs") %>%

  corrplot(type="lower", diag=FALSE)
```

