library(ggplot2)
library(caret)
library(randomForest)
library(tidyverse)
library(ggridges)
library(rstatix)

# Load CSV file

UFC <-
  read.csv("/Users/mac/Documents/UFC_Project/ufc-master.csv", header = TRUE)
View(UFC)

# Filtering columns and splitting in order to remove red and blue segregation

fighter_RED <-
  UFC[, c(
    "R_fighter",
    "R_odds",
    "R_ev",
    "Winner",
    "weight_class",
    "gender",
    "no_of_rounds",
    "R_age",
    "B_age",
    "R_sig_str_landed_bout",
    "R_sig_str_pct_bout",
    "R_td_landed_bout",
    "R_td_attempted_bout",
    "R_td_pct_bout",
    "R_sub_attempts_bout",
    "R_Reach_cms",
    "B_Reach_cms"
  )]

fighter_BLUE <-
  UFC[, c(
    "B_fighter",
    "B_odds",
    "B_ev",
    "Winner",
    "weight_class",
    "gender",
    "no_of_rounds",
    "R_age",
    "B_age",
    "B_sig_str_landed_bout",
    "B_sig_str_pct_bout",
    "B_td_landed_bout",
    "B_td_attempted_bout",
    "B_td_pct_bout",
    "B_sub_attempts_bout",
    "R_Reach_cms",
    "B_Reach_cms"
  )]

# Renaming variables for red

colnames(fighter_RED)[1] <- "Fighter"
colnames(fighter_RED)[2] <- "Odds"
colnames(fighter_RED)[3] <- "Evens"
colnames(fighter_RED)[8] <- "FighterOneAge"
colnames(fighter_RED)[9] <- "FighterTwoAge"
colnames(fighter_RED)[10] <- "SignifigantStrikesLanded"
colnames(fighter_RED)[11] <- "SignifigantStrikePercentage"
colnames(fighter_RED)[12] <- "TakedownsLanded"
colnames(fighter_RED)[13] <- "TakedownAttempts"
colnames(fighter_RED)[14] <- "TakedownSuccessPercentage"
colnames(fighter_RED)[15] <- "SubmissionAttempts"
colnames(fighter_RED)[16] <- "FighterOneReach"
colnames(fighter_RED)[17] <- "FighterTwoReach"

#Renaming variables for blue

colnames(fighter_BLUE)[1] <- "Fighter"
colnames(fighter_BLUE)[2] <- "Odds"
colnames(fighter_BLUE)[3] <- "Evens"
colnames(fighter_BLUE)[8] <- "FighterOneAge"
colnames(fighter_BLUE)[9] <- "FighterTwoAge"
colnames(fighter_BLUE)[10] <- "SignifigantStrikesLanded"
colnames(fighter_BLUE)[11] <- "SignifigantStrikePercentage"
colnames(fighter_BLUE)[12] <- "TakedownsLanded"
colnames(fighter_BLUE)[13] <- "TakedownAttempts"
colnames(fighter_BLUE)[14] <- "TakedownSuccessPercentage"
colnames(fighter_BLUE)[15] <- "SubmissionAttempts"
colnames(fighter_BLUE)[16] <- "FighterOneReach"
colnames(fighter_BLUE)[17] <- "FighterTwoReach"

# Creating variable for age difference

fighter_RED$AgeDiff <-
  ((fighter_RED$FighterOneAge - fighter_RED$FighterTwoAge))
fighter_BLUE$AgeDiff <-
  ((fighter_BLUE$FighterOneAge - fighter_BLUE$FighterTwoAge))

# Creating variable for reach advantage

fighter_RED$ReachADV <-
  ((fighter_RED$FighterOneReach - fighter_RED$FighterTwoReach))
fighter_BLUE$ReachADV <-
  ((fighter_BLUE$FighterOneReach - fighter_BLUE$FighterTwoReach))

# Set winner as 1 or 0

RedFighterWin <- subset(fighter_RED, Winner == "Red")
RedFighterWin$Winner <- 1

RedFighterLoss <- subset(fighter_RED, Winner == "Blue")
RedFighterLoss$Winner <- 0

# Bind data back together

RedFighterNew <- rbind(RedFighterWin, RedFighterLoss)


# Rebinding whole dataframe

UFC_Final <- rbind(RedFighterNew, BlueFighterNew)

#Writing to csv as backup

write.csv(UFC_Final,
          "/Users/mac/Documents/UFC_Project/UFC_Final.csv",
          row.names = FALSE)

# Splitting male and female

Male_Fighters <- subset(UFC_Final, gender == "MALE")

# Singling out champions of each weight class men

MaleHeavyweightChampion <-
  subset(Male_Fighters, Fighter == "Francis Ngannou")
MaleLightHeavyweightChampion <-
  subset(Male_Fighters, Fighter == "Jan Blachowicz")
MaleMiddleweightChampion <-
  subset(Male_Fighters, Fighter == "Israel Adesanya")
MaleWelterweightChampion <-
  subset(Male_Fighters, Fighter == "Kamaru Usman")
MaleLightweightweightChampion <-
  subset(Male_Fighters, Fighter == "Charles Oliveira")
MaleFeatherweightChampion <-
  subset(Male_Fighters, Fighter == "Alexander Volkanovski")
MaleBantamweightChampion <-
  subset(Male_Fighters, Fighter == "Aljamain Sterling")
MaleFlyweightChampion <-
  subset(Male_Fighters, Fighter == "Brandon Moreno")

# Singling out champions of each weight class women

FemaleFlyweightChampion <-
  subset(Female_Fighters, Fighter == "Valentina Shevchenko")
FemaleStrawweightChampion <-
  subset(Female_Fighters, Fighter == "Rose Namajunas")

# Singling out Amanda Nunes, female champion of 2 weight classes, parsing her fights per division

ANChampion <-
  subset(Female_Fighters, Fighter == "Amanda Nunes")
FemaleFeatherweightChampion <-
  subset(ANChampion, weight_class == "Women's Featherweight")
FemaleBantamweightChampion <-
  subset(ANChampion, weight_class == "Women's Bantamweight")

# remove useless variables for calculating variable importance

Male_Fighters_New <- Male_Fighters [, -1:-3]

Female_Fighters_New <- Female_Fighters [, -1:-3]

# Dividing into male weight divisions for variable importance

MaleHeavyweight <-
  subset(Male_Fighters_New, weight_class == "Heavyweight")
MaleLightHeavyweight <-
  subset(Male_Fighters_New, weight_class == "Light Heavyweight")
MaleMiddleweight <-
  subset(Male_Fighters_New, weight_class == "Middleweight")
MaleWelterweight <-
  subset(Male_Fighters_New, weight_class == "Welterweight")
MaleLightweightweight <-
  subset(Male_Fighters_New, weight_class == "Lightweight")
MaleFeatherweight <-
  subset(Male_Fighters_New, weight_class == "Featherweight")
MaleBantamweight <-
  subset(Male_Fighters_New, weight_class == "Bantamweight")
MaleFlyweight <-
  subset(Male_Fighters_New, weight_class == "Flyweight")

# Dividing into female weight divisions

FemaleFeatherweight <-
  subset(Female_Fighters_New, weight_class == "Women's Featherweight")
FemaleBantamweight <-
  subset(Female_Fighters_New, weight_class == "Women's Bantamweight")
FemaleFlyweight <-
  subset(Female_Fighters_New, weight_class == "Women's Flyweight")
FemaleStrawweight <-
  subset(Female_Fighters_New, weight_class == "Women's Strawweight")

# Create new data frames for each gender with variables that won't interfere with Random Forest

Male_Fighters_New <-
  Male_Fighters[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

Female_Fighters_New <-
  Female_Fighters[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

# Create new data frames for each division with variables that won't interfere with Random Forest

MaleHeavyweight_New <-
  MaleHeavyweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleLightHeavyweight_New <-
  MaleLightHeavyweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleMiddleweight_New <-
  MaleMiddleweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleWelterweight_New <-
  MaleWelterweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleLightweightweight_New <-
  MaleLightweightweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleFeatherweight_New <-
  MaleFeatherweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleBantamweight_New <-
  MaleBantamweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

MaleFlyweight_New <-
  MaleFlyweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

FemaleFeatherweight_New <-
  FemaleFeatherweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

FemaleBantamweight_New <-
  FemaleBantamweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

FemaleFlyweight_New <-
  FemaleFlyweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]

FemaleStrawweight_New <-
  FemaleStrawweight[c(
    "SignifigantStrikesLanded",
    "ReachADV",
    "AgeDiff",
    "SignifigantStrikePercentage",
    "TakedownsLanded",
    "TakedownAttempts",
    "TakedownSuccessPercentage",
    "SubmissionAttempts",
    "Winner"
  )]


# Random forest model for all male fighters
# Ensure completeness of data (no NA's)

sum(is.na(Male_Fighters_New))
Male_Fighters_New_Complete <-
  subset(Male_Fighters_New, complete.cases(Male_Fighters_New))

# Build random forest model
Male_Fighters_New_RandomFor <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = Male_Fighters_New_Complete)

# Run variable importance
varImpPlot(Male_Fighters_New_RandomFor)


# Ensure completeness of data (no NA's)
sum(is.na(Female_Fighters_New))
Female_Fighters_New_Complete <-
  subset(Female_Fighters_New, complete.cases(Female_Fighters_New))

# Build random forest model
Female_Fighters_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = Female_Fighters_New_Complete)

# Run variable importance
varImpPlot(Female_Fighters_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleHeavyweight_New))
MaleHeavyweight_New_Complete <-
  subset(MaleHeavyweight_New, complete.cases(MaleHeavyweight_New))

# Build random forest model
MaleHeavyweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleHeavyweight_New_Complete)

# Run variable importance
varImpPlot(MaleHeavyweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleLightHeavyweight_New))
MaleLightHeavyweight_New_Complete <-
  subset(MaleLightHeavyweight_New,
         complete.cases(MaleLightHeavyweight_New))

# Build random forest model
MaleLightHeavyweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleLightHeavyweight_New_Complete)

# Run variable importance
varImpPlot(MaleLightHeavyweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleMiddleweight_New))
MaleMiddleweight_New_Complete <-
  subset(MaleMiddleweight_New, complete.cases(MaleMiddleweight_New))

# Build random forest model
MaleMiddleweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleMiddleweight_New_Complete)

# Run variable importance
varImpPlot(MaleMiddleweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleWelterweight_New))
MaleWelterweight_New_Complete <-
  subset(MaleWelterweight_New, complete.cases(MaleWelterweight_New))

# Build random forest model
MaleWelterweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleWelterweight_New_Complete)

# Run variable importance
varImpPlot(MaleWelterweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleLightweightweight_New))
MaleLightweightweight_New_Complete <-
  subset(MaleLightweightweight_New,
         complete.cases(MaleLightweightweight_New))

# Build random forest model
MaleLightweightweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleLightweightweight_New_Complete)

# Run variable importance
varImpPlot(MaleLightweightweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleFeatherweight_New))
MaleFeatherweight_New_Complete <-
  subset(MaleFeatherweight_New,
         complete.cases(MaleFeatherweight_New))

# Build random forest model
MaleFeatherweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleFeatherweight_New_Complete)

# Run variable importance
varImpPlot(MaleFeatherweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleBantamweight_New))
MaleBantamweight_New_Complete <-
  subset(MaleBantamweight_New, complete.cases(MaleBantamweight_New))

# Build random forest model
MaleBantamweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleBantamweight_New_Complete)

# Run variable importance
varImpPlot(MaleBantamweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(MaleFlyweight_New))
MaleFlyweight_New_Complete <-
  subset(MaleFlyweight_New, complete.cases(MaleFlyweight_New))

# Build random forest model
MaleFlyweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = MaleFlyweight_New_Complete)

# Run variable importance
varImpPlot(MaleFlyweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(FemaleFeatherweight_New))
FemaleFeatherweight_New_Complete <-
  subset(FemaleFeatherweight_New,
         complete.cases(FemaleFeatherweight_New))

# Build random forest model
FemaleFeatherweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = FemaleFeatherweight_New_Complete)

# Run variable importance
varImpPlot(FemaleFeatherweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(FemaleBantamweight_New))
FemaleBantamweight_New_Complete <-
  subset(FemaleBantamweight_New,
         complete.cases(FemaleBantamweight_New))

# Build random forest model
FemaleBantamweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = FemaleBantamweight_New_Complete)

# Run variable importance
varImpPlot(FemaleBantamweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(FemaleFlyweight_New))
FemaleFlyweight_New_Complete <-
  subset(FemaleFlyweight_New, complete.cases(FemaleFlyweight_New))

# Build random forest model
FemaleFlyweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = FemaleFlyweight_New_Complete)

# Run variable importance
varImpPlot(FemaleFlyweight_New_Complete)


# Ensure completeness of data (no NA's)
sum(is.na(FemaleStrawweight_New))
FemaleStrawweight_New_Complete <-
  subset(FemaleStrawweight_New,
         complete.cases(FemaleStrawweight_New))

# Build random forest model
FemaleStrawweight_New_Complete <-
  randomForest(Winner ~ .,
               mtry = 5,
               ntree = 1000,
               data = FemaleStrawweight_New_Complete)

# Run variable importance
varImpPlot(FemaleStrawweight_New_Complete)

# Remove catchweight
Male_Fighters <-
  subset(Male_Fighters, Male_Fighters$weight_class != 'Catch Weight')

# Split winners and losers (male)
Male_Fighters_win <-
  subset(Male_Fighters, Male_Fighters$Winner == 1)
Male_Fighters_loss <-
  subset(Male_Fighters, Male_Fighters$Winner == 0)

# Split winners and losers (female)
Female_Fighters_win <-
  subset(Female_Fighters, Female_Fighters$Winner == 1)
Female_Fighters_loss <-
  subset(Female_Fighters, Female_Fighters$Winner == 0)


# Remove outliers in flyweight
Male_Fighters <-
  subset(Male_Fighters, Male_Fighters$ReachADV != 187.96)

# Summary statistics of significant strikes by division (male) (Winner)
Summary_male_SigStrikes_winner <- Male_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_male_SigStrikes_winner <-
  Summary_male_SigStrikes_winner[order(-Summary_male_SigStrikes$mean),]

# Summary statistics of significant strikes by division (male) (Loser)
Summary_male_SigStrikes_loser <- Male_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_male_SigStrikes_loser <-
  Summary_male_SigStrikes_loser[order(-Summary_male_SigStrikes$mean),]


# plotting significant strikes by division male (Winner)

ggplot(data = Male_Fighters_win, aes(x = weight_class, y = SignifigantStrikesLanded)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Signifigant Strikes Landed")

# plotting significant strikes by division male (Loser)

ggplot(data = Male_Fighters_loss, aes(x = weight_class, y = SignifigantStrikesLanded)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Signifigant Strikes Landed")

# Summary statistics of significant strikes by division (female) (winner)

Summary_female_SigStrikes_win <- Female_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_female_SigStrikes_win <-
  Summary_female_SigStrikes_win[order(-Summary_female_SigStrikes$mean),]

# Summary statistics of significant strikes by division (female) (loser)

Summary_female_SigStrikes_loss <- Female_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_female_SigStrikes_loss <-
  Summary_female_SigStrikes_loss[order(-Summary_female_SigStrikes$mean),]

# plotting significant strikes by division female (Winner)

ggplot(data = Female_Fighters_win, aes(x = weight_class, y = SignifigantStrikesLanded)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Signifigant Strikes Landed")

# plotting significant strikes by division female (Loser)

ggplot(data = Female_Fighters_loss, aes(x = weight_class, y = SignifigantStrikesLanded)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Signifigant Strikes Landed")

# Summary statistics of Age Difference by division (male) (winner)
Summary_male_AgeDiff_win <- Male_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_male_AgeDiff_win <-
  Summary_male_AgeDiff_win[order(-Summary_male_AgeDiff_win$mean),]

# Summary statistics of Age Difference by division (male) (loser)
Summary_male_AgeDiff_loss <- Male_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_male_AgeDiff_loss <-
  Summary_male_AgeDiff_loss[order(-Summary_male_AgeDiff_loss$mean),]

# plotting Age Difference by division male (Winner)

ggplot(data = Male_Fighters_win, aes(x = weight_class, y = AgeDiff)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Age Difference")

# plotting Age Difference by division male (Loser)

ggplot(data = Male_Fighters_loss, aes(x = weight_class, y = AgeDiff)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Age Difference")

# Summary statistics of Age Difference by division (female) (Winner)

Summary_female_AgeDiff_win <- Female_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_female_AgeDiff_win <-
  Summary_female_AgeDiff_win[order(-Summary_female_AgeDiff_win$mean),]

# Summary statistics of Age Difference by division (female) (Loser)

Summary_female_AgeDiff_loser <- Female_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_female_AgeDiff_loser <-
  Summary_female_AgeDiff_loser[order(-Summary_female_AgeDiff_loser$mean),]

# plotting Age Difference by division Female (Winner)

ggplot(data = Female_Fighters_win, aes(x = weight_class, y = AgeDiff)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Age Difference")

# plotting Age Difference by division Female (Loser)

ggplot(data = Female_Fighters_loss, aes(x = weight_class, y = AgeDiff)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Age Difference")


# Summary statistics of Reach Difference by division (male) (Winner)
Summary_male_ReachADV_win <- Male_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_male_ReachADV_win <-
  Summary_male_ReachADV_win[order(-Summary_male_ReachADV_win$mean),]

# Summary statistics of Reach Difference by division (male) (Loser)
Summary_male_ReachADV_loss <- Male_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_male_ReachADV_loss <-
  Summary_male_ReachADV_loss[order(-Summary_male_ReachADV_loss$mean),]

# plotting Reach Advantage by division male (Winner)

ggplot(data = Male_Fighters_win, aes(x = weight_class, y = ReachADV)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Reach Advantage")

# plotting Reach Advantage by division male (Loser)

ggplot(data = Male_Fighters_loss, aes(x = weight_class, y = ReachADV)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Reach Advantage")


# Summary statistics of Reach Advantage by division (female) (Winner)

Summary_female_ReachADV_win <- Female_Fighters_win %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_female_ReachADV_win <-
  Summary_female_ReachADV_win[order(-Summary_female_ReachADV_win$mean),]

# Summary statistics of Reach Advantage by division (female) (Winner)

Summary_female_ReachADV_loss <- Female_Fighters_loss %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_female_ReachADV_loss <-
  Summary_female_ReachADV_loss[order(-Summary_female_ReachADV_loss$mean),]

# plotting Reach Advantage by division female (Winner)

ggplot(data = Female_Fighters_win, aes(x = weight_class, y = ReachADV)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Reach Advantage")

# plotting Reach Advantage by division female (Winner)

ggplot(data = Female_Fighters_loss, aes(x = weight_class, y = ReachADV)) +
  geom_boxplot(aes(color = weight_class),
               width = 0.3,
               show.legend = FALSE) +
  stat_summary(
    fun = mean,
    colour = "red",
    geom = "point",
    shape = 18,
    size = 3
  ) +
  geom_jitter(
    aes(color = weight_class),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  theme_minimal() +
  labs(x = "Weight Class",
       y = "Reach Advantage")

# Assigning all champions to their own dataframe (male)
Champions_male <- rbind(
  MaleHeavyweightChampion,
  MaleLightHeavyweightChampion,
  MaleMiddleweightChampion,
  MaleWelterweightChampion,
  MaleLightweightweightChampion,
  MaleFeatherweightChampion,
  MaleBantamweightChampion,
  MaleFlyweightChampion
)

# Remove catchweight
Champions_male <-
  subset(Champions_male, Champions_male$weight_class != 'Catch Weight')

# Summary statistics of champions significant strikes (male)
Summary_Champions_male_sig <- Champions_male %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_Champions_male_sig <-
  Summary_Champions_male_sig[order(-Summary_Champions_male_sig$mean),]

# Summary statistics of champions age difference (male)
Summary_Champions_AgeDiff <- Champions_male %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_Champions_AgeDiff <-
  Summary_Champions_AgeDiff[order(-Summary_Champions_AgeDiff$mean),]

# Summary statistics of champions reach advantage  (male)
Summary_Champions_Reach <- Champions_male %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_Champions_Reach <-
  Summary_Champions_Reach[order(-Summary_Champions_Reach$mean),]


# Assigning all champions to their own dataframe (female)
Champions_female <- rbind(
  FemaleFlyweightChampion,
  FemaleStrawweightChampion,
  FemaleFeatherweightChampion,
  FemaleBantamweightChampion
)

# Summary statistics of champions (female)
Summary_Champions_female_sig <- Champions_female %>%
  group_by(weight_class) %>%
  get_summary_stats(SignifigantStrikesLanded,
                    show = c("mean", "median", "sd"))
Summary_Champions_female_sig <-
  Summary_Champions_female_sig[order(-Summary_Champions_female_sig$mean),]

# Summary statistics of champions age difference (female)
Summary_Champions_female_AgeDiff <- Champions_female %>%
  group_by(weight_class) %>%
  get_summary_stats(AgeDiff,
                    show = c("mean", "median", "sd"))
Summary_Champions_female_AgeDiff <-
  Summary_Champions_female_AgeDiff[order(-Summary_Champions_female_AgeDiff$mean),]

# Summary statistics of champions reach advantage (female)
Summary_Champions_female_Reach <- Champions_female %>%
  group_by(weight_class) %>%
  get_summary_stats(ReachADV,
                    show = c("mean", "median", "sd"))
Summary_Champions_female_Reach <-
  Summary_Champions_female_Reach[order(-Summary_Champions_female_Reach$mean),]


# Heavyweight reach outliers
HeavyWeightReachOut <-
  Male_Fighters_loss[order(-Male_Fighters_loss$ReachADV), ]
HeavyWeightReachOut <-
  subset(HeavyWeightReachOut, weight_class == "Heavyweight")

# Print 'Stefan Struve' as example of huge outlier
HeavyWeightReachOutExample <-
  subset(HeavyWeightReachOut, Fighter == "Stefan Struve")
HeavyWeightReachOutExample <- HeavyWeightReachOutExample[c("Fighter",
                                                           "ReachADV",
                                                           "FighterOneReach")]


