#2/2/26
#Starting to attempt to code ESS Survey data about cars and climate change
#
#Set working directory
setwd('C:\\Users\\shabo\\Documents\\School\\IA work\\ESS IA 2026\\R code for graphing')
#bring in the data
dat <- read.csv(file.choose())
#
#using chatgpt strat to attempt to fix errors in names between factors and whats in the csv file
names(dat)
#
#changing variables to factors for graphs
dat$Age <- as.factor(dat$Age)
dat$Education <- as.factor(dat$Education)
#errors below here, above ran fine since its one word, more than one word and it is complicated
#attempt to find out if the error is in the column name
"Drive.Car" %in% names(dat)  # Should return TRUE if it exists
#Figured it out, I do need dots instead of spaces even though it is within the backticks so it shouldn't be a problem
dat$Drive.Car <- as.factor(dat$Drive.Car)
dat$Car.type <- as.factor(dat$Car.type)
dat$'Average.mpg.of.car' <- as.factor(dat$'Average.mpg.of.car')
dat$Electric.hybrid.car <- as.factor(dat$Electric.hybrid.car)
dat$Specific.gas.type <- as.factor(dat$Specific.gas.type)
dat$Do.you.know.what.climate.change.is. <- as.factor(dat$Do.you.know.what.climate.change.is.)
dat$Causes.of.climate.change <- as.factor(dat$Causes.of.climate.change)
dat$Do.you.know.what.an.ecological.footprint.is. <- as.factor(dat$Do.you.know.what.an.ecological.footprint.is.)
dat$Describe.ecological.footprint <- as.factor(dat$Describe.ecological.footprint)
#error in the one below, idk why, names seem exactly the same
dat$Can.cars.negatively.impact.the.environment <- as.factor(dat$Can.cars.negatively.impact.the.environment)
#fixed the error, not sure what it was, just copy and pasted the name from google sheets and changed spaces to dots to fix
#these fine
dat$What.are.the.impacts.cars.can.have <- as.factor(dat$What.are.the.impacts.cars.can.have)
dat$Time.spent.driving <- as.factor(dat$Time.spent.driving)
dat$Public.transport.use <- as.factor(dat$Public.transport.use)

#ordering all the variables so graphs are cleaner
dat$Education <- factor(dat$Education, 
                        levels = c("H.S.", "UG", "Grad.", "PhD"))

dat$Age <- factor(dat$Age, 
                        levels = c("< 20", "20-30", "30-40", "40-50", "50-60", "> 60"))

dat$Average.mpg.of.car <- factor(dat$Average.mpg.of.car,
                          levels = c("< 15", "15-30", "30-45", "45-60", "> 60", "Electric"))

dat$Time.spent.driving <-factor(dat$Time.spent.driving,
                          levels = c("< 1", "1-4", "5-10", "10-20", "> 20"))

dat$Public.transport.use <-factor(dat$Public.transport.use,
                          levels = c("Never", "Rarely", "Sometimes", "Usually", "Always"))

#may need to re-order the other dataframes for the graphs were knowledge is a variable
knowledge.vs.car.mpg$Average.mpg.of.car <- factor(knowledge.vs.car.mpg$Average.mpg.of.car,
                                 levels = c("< 15", "15-30", "30-45", "45-60", "> 60", "Electric"))

knowledge.vs.age$Age <- factor(knowledge.vs.age$Age, 
                  levels = c("< 20", "20-30", "30-40", "40-50", "50-60", "> 60"))

#shortening titles for some factors
dat <- dat %>%
  mutate(Age = recode(Age,
                      "Under 20" = "< 20",
                      "60+" = "> 60"))

dat <- dat %>%
  mutate(Time.spent.driving = recode(Time.spent.driving,
                      "<1 Hour" = "< 1",
                      "1-4 hours" = "1-4",
                      "5-10 hours" = "5-10",
                      "10-20 hours" = "10-20",
                      "20+ hours" = "> 20"))

dat <- dat %>%
  mutate(Average.mpg.of.car = recode(Average.mpg.of.car,
                                     "< 15 miles per gallon" = "< 15",
                                     "> 60 miles per gallon" = "> 60",
                                     "15-30 miles per gallon" = "15-30",
                                     "30-45 miles per gallon" = "30-45",
                                     "45-60 miles per gallon" = "45-60"))

#making total dataframes for percentage charts

totals.for.driving.time <- dat %>%
  dplyr::count(Time.spent.driving)


#attempts to make bar graphs - now adding stacked bar charts w/ percents and total #'s for options
#Much better visually and for analyzing the data

g <- ggplot(dat, aes(Education))
g + geom_bar(aes(fill = Average.mpg.of.car)) +
labs(y = "Count", x = "Education", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Education vs mpg for count.png")

#OR

ggplot(dat, aes(x = Education, fill = Average.mpg.of.car)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.education,
            aes(x = Education, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Education", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Education vs mpg for percentage.png")



#facet graph, faceting by car type

ggplot(dat, aes(x=Education, fill = Average.mpg.of.car))+
  geom_bar()+
  facet_wrap(~Car.type) +
  labs(y = "Count", x = "Education", fill = "Car Type")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Education vs mpg facet by car type for count.png")

#other facet graph faceting by mpg now
ggplot(dat, aes(x=Education, fill = Car.type))+
  geom_bar()+
  facet_wrap(~Average.mpg.of.car) +
  labs(y = "Count", x = "Education", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Education vs car type facet by mpg for count.png")


#back to normal stacked bar charts, making them for count and percentage now

g <- ggplot(dat, aes(Age))
g + geom_bar(aes(fill = Average.mpg.of.car)) +
  labs(y = "Count", x = "Age", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Age vs mpg for count.png")

#OR

ggplot(dat, aes(x = Age, fill = Average.mpg.of.car)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.age,
            aes(x = Age, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Age", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Age vs mpg for percentage.png")



#Analyzes if education makes a difference in knowledge of ecological footprint
#making a percentage chart, so need a totals dataframe for ecological footprint

totals.for.ecological.footprint.knowledge <- dat %>%
  dplyr::count(Do.you.know.what.an.ecological.footprint.is.)

ggplot(dat, aes(x = Education, fill = Do.you.know.what.an.ecological.footprint.is.)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.education,
            aes(x = Education, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Education", fill = "Do you know what an ecological footprint is?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\education vs ecological footprint knowledge for percentage.png")



#Are cars that have better mpg used more
g <- ggplot(dat, aes(Time.spent.driving))
g + geom_bar(aes(fill = Average.mpg.of.car)) +
  labs(y = "Count", x = "Time Spent Driving(hrs)", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Time spent driving vs average mpg for count.png")



#what type of car owners use public transport
g <- ggplot(dat, aes(Public.transport.use))
g + geom_bar(aes(fill = Average.mpg.of.car))

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport vs average mpg for count.png")

#OR

#counting totals for public transport to make percentage graph
totals.for.public.transport <- dat %>%
  dplyr::count(Public.transport.use)

ggplot(dat, aes(x = Public.transport.use, fill = Average.mpg.of.car)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.public.transport,
            aes(x = Public.transport.use, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Public Transport Use", fill = "Average MPG of car")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport vs average mpg for percentage.png")



#Are more eco-consious people using public transport more?
g <- ggplot(dat, aes(Public.transport.use))
g + geom_bar(aes(fill = Electric.hybrid.car)) +
  labs(y = "Count", x = "Public Transport Use", fill = "Electric or hybrid car?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport use vs electric or hybrid cars for count.png")

#OR

ggplot(dat, aes(x = Public.transport.use, fill = Electric.hybrid.car)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.public.transport,
            aes(x = Public.transport.use, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Public transport use", fill = "Electric or hybrid car?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport use vs electric or hybrid cars for percentage.png")



#do people who drive more or less using public transport more
g <- ggplot(dat, aes(Public.transport.use))
g + geom_bar(aes(fill = Time.spent.driving)) +
  labs(y = "Count", x = "Public Transport Use", fill = "Time spent driving(hrs)")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport use vs time spent driving for count.png")

#OR

ggplot(dat, aes(x = Public.transport.use, fill = Time.spent.driving)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.public.transport,
            aes(x = Public.transport.use, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Public transport use", fill = "Time spent driving(hrs)")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Public transport use vs time spent driving for percentage.png")


#creating graphs using knowledge as a variable

#Steps in the hashtags(#)

#Add row ID for each respondent
dat$id <- 1:nrow(dat)
#Make new data frame of just ID, car type, and causes of CC
dat2<- dat[, c("id", "Car.type", "Causes.of.climate.change")]
#use tidyverse and dplyr parse my data into new dataframe

knowledge.vs.car.type <- dat2 %>%
separate_rows(Causes.of.climate.change, sep = ",") %>%
mutate(Causes.of.climate.change = trimws(Causes.of.climate.change))


#new plots from dataframe of knowledge vs car type
g <- ggplot(knowledge.vs.car.type, aes(Car.type))
g + geom_bar(aes(fill = Causes.of.climate.change))+
  labs(y = "Count", x = "Car Type", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Car type vs causes of climate change for count.png")

#creating a dataframe for the totals of each of the car.type responses
totals.for.car.type <- dat %>%
  dplyr::count(Car.type)

#now we want to make a summary of the percentages of the causes for each of the car types
ggplot(knowledge.vs.car.type, aes(x = Car.type, fill = Causes.of.climate.change)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.car.type,
            aes(x = Car.type, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Car Type", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\percentage of CC causes per car type.png")



#now doing the whole process as above, but with MPG instead of car types

#Make new data frame of just ID, car type, and causes of CC
dat2<- dat[, c("id", "Average.mpg.of.car", "Causes.of.climate.change")]
#use tidyverse and dplyr parse my data into new dataframe

knowledge.vs.car.mpg <- dat2 %>%
  separate_rows(Causes.of.climate.change, sep = ",") %>%
  mutate(Causes.of.climate.change = trimws(Causes.of.climate.change))


#new plots from dataframe of knowledge vs car mpg
g <- ggplot(knowledge.vs.car.mpg, aes(Average.mpg.of.car))
g + geom_bar(aes(fill = Causes.of.climate.change)) +
  labs(y = "Count", x = "Average MPG of car", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Average mpg of car vs causes of climate change for count.png")


#creating a dataframe for the totals of each of the car.type responses
totals.for.mpg <- dat %>%
  dplyr::count(Average.mpg.of.car)

#shortening titles of knowledge.vs.car.mpg to make a cleaner graph
knowledge.vs.car.mpg <- knowledge.vs.car.mpg %>%
  mutate(Average.mpg.of.car = recode(Average.mpg.of.car,
                                     "< 15 miles per gallon" = "< 15",
                                     "> 60 miles per gallon" = "> 60",
                                     "15-30 miles per gallon" = "15-30",
                                     "30-45 miles per gallon" = "30-45",
                                     "45-60 miles per gallon" = "45-60"))

#shorten titles for totals.for.mpg now to match other shortened titles
totals.for.mpg <- totals.for.mpg %>%
  mutate(Average.mpg.of.car = recode(Average.mpg.of.car,
                                     "< 15 miles per gallon" = "< 15",
                                     "> 60 miles per gallon" = "> 60",
                                     "15-30 miles per gallon" = "15-30",
                                     "30-45 miles per gallon" = "30-45",
                                     "45-60 miles per gallon" = "45-60"))

#now we want to make a summary of the percentages of the causes for each of the car types
ggplot(knowledge.vs.car.mpg, aes(x = Average.mpg.of.car, fill = Causes.of.climate.change)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.mpg,
            aes(x = Average.mpg.of.car, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Average MPG of car", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\percentage of CC causes per mpg for percentage.png")





#doing this for knowledge of CC vs age
dat2 <- dat[, c("id", "Age", "Causes.of.climate.change")]
#use tidyverse and dplyr parse my data into new dataframe

#seperating by commas now
knowledge.vs.age <- dat2 %>%
  separate_rows(Causes.of.climate.change, sep = ",") %>%
  mutate(Causes.of.climate.change = trimws(Causes.of.climate.change))

#making totals for ages
totals.for.age <- dat %>%
  dplyr::count(Age)

#shortening title
knowledge.vs.age <- knowledge.vs.age %>%
  mutate(Age = recode(Age,
                                     "Under 20" = "< 20",
                                    "60+" = "> 60"))

#shortening title for totals
totals.for.age <- totals.for.age %>%
  mutate(Age = recode(Age,
                                     "Under 20" = "< 20",
                                    "60+" = "> 60"))

#final plot
ggplot(knowledge.vs.age, aes(x = Age, fill = Causes.of.climate.change)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.age,
            aes(x = Age, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Age", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\CC causes vs age for percentage.png")


#Doing this with education now
dat2 <- dat[, c("id", "Education", "Causes.of.climate.change")]
#use tidyverse and dplyr parse my data into new dataframe

#seperating by commas now
knowledge.vs.education <- dat2 %>%
  separate_rows(Causes.of.climate.change, sep = ",") %>%
  mutate(Causes.of.climate.change = trimws(Causes.of.climate.change))

#making totals for ages
totals.for.education <- dat %>%
  dplyr::count(Education)


#final plot
ggplot(knowledge.vs.education, aes(x = Education, fill = Causes.of.climate.change)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.education,
            aes(x = Education, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Education", fill = "Causes of climate change")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\CC causes vs education for percentage.png")



#
#
#
#
#
#
#knowledge graphs with knowledge of car impacts now


#trying to make the responses that aren't frequent become "Other" for the graph
library(forcats)


#Make new data frame of just id, car type, and What are the impacts cars can have
dat2<- dat[, c("id", "Car.type", "What.are.the.impacts.cars.can.have")]

#use tidyverse and dplyr parse my data into new dataframe
Car.impacts.vs.car.type <- dat2 %>%
  separate_rows(What.are.the.impacts.cars.can.have, sep = ",") %>%
  mutate(What.are.the.impacts.cars.can.have = trimws(What.are.the.impacts.cars.can.have))

#Making extraneous responses others
Car.impacts.vs.car.type <- Car.impacts.vs.car.type %>%
  mutate(What.are.the.impacts.cars.can.have = fct_lump(What.are.the.impacts.cars.can.have, n = 4, other_level = "Other"))

#new plots from dataframe of knowledge vs car type
g <- ggplot(Car.impacts.vs.car.type, aes(Car.type))
g + geom_bar(aes(fill = What.are.the.impacts.cars.can.have)) +
  labs(y = "Count", x = "Car Type", fill = "What impacts do cars have on the environment?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Car impacts vs car type for count.png")


#now we want to make a summary of the percentages of the causes for each of the car types
ggplot(Car.impacts.vs.car.type, aes(x = Car.type, fill = What.are.the.impacts.cars.can.have)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.car.type,
            aes(x = Car.type, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Car type", fill = "What impacts do cars have on the environment?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Car impacts vs car type for percentage.png")


#Make new data frame of just ID, car mpg, and What are the impacts cars can have
#again making uncommon responses other for this dataframe

dat2<- dat[, c("id", "Average.mpg.of.car", "What.are.the.impacts.cars.can.have")]
#use tidyverse and dplyr parse my data into new dataframe

Car.impacts.vs.car.mpg <- dat2 %>%
  separate_rows(What.are.the.impacts.cars.can.have, sep = ",") %>%
  mutate(What.are.the.impacts.cars.can.have = trimws(What.are.the.impacts.cars.can.have))

Car.impacts.vs.car.mpg <- Car.impacts.vs.car.mpg %>%
  mutate(What.are.the.impacts.cars.can.have = fct_lump(What.are.the.impacts.cars.can.have, n = 4, other_level = "Other"))

#Need to shorten titles in this dataframe, forgot to do that
Car.impacts.vs.car.mpg <- Car.impacts.vs.car.mpg %>%
  mutate(Average.mpg.of.car = recode(Average.mpg.of.car,
                                     "< 15 miles per gallon" = "< 15",
                                     "> 60 miles per gallon" = "> 60",
                                     "15-30 miles per gallon" = "15-30",
                                     "30-45 miles per gallon" = "30-45",
                                     "45-60 miles per gallon" = "45-60"))

#new plots from dataframe of knowledge vs car type
g <- ggplot(Car.impacts.vs.car.mpg, aes(Average.mpg.of.car))
g + geom_bar(aes(fill = What.are.the.impacts.cars.can.have)) +
  labs(y = "Count", x = "Average MPG of car", fill = "What impacts do cars have on the environment?")

ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Car impacts vs car mpg for count.png")


#now we want to make a summary of the percentages of the causes for each of the car types
ggplot(Car.impacts.vs.car.mpg, aes(x = Average.mpg.of.car, fill = What.are.the.impacts.cars.can.have)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = totals.for.mpg,
            aes(x = Average.mpg.of.car, y = 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE) +
  labs(y = "Percent", x = "Average MPG of car", fill = "What impacts do cars have on the environment?")


ggsave("C:\\Users\\shabo\\OneDrive\\Documents\\School\\IA work\\ESS IA 2026\\graphs\\Car impacts vs car mpg for percentage.png")

#3/21/26
#ALL DONE!!!
