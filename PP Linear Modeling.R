library(tidyverse)
library(dplyr)
library(corrplot) 
library(GGally)
library(gridExtra)
library(sjPlot)
library(ggResidpanel)

penguins<-read_csv("penguins.csv")

glimpse(penguins)
penguins %>% count(species, name="total")
total_penguins <- 151+68+123
total_penguins


penguins_islands <- penguins %>% count(island, name="penguins")
penguins_islands

flipper_mass <- ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
                geom_point(aes(color = species, shape = species), size = 3, alpha = 0.8) +
                labs(title = "Flipper Length and Body Mass", x = "Flipper Length (mm)", y = "Body Mass (g)", 
                     color = "Penguin Species", shape = "Penguin Species")
flipper_mass

flipper_bill <- ggplot(data = penguins, aes(x = flipper_length_mm, y = bill_length_mm)) +
                geom_point(aes(color = species, shape = species), size = 3, alpha = 0.8) +
                labs(title = "Flipper Length and Bill Length", x = "Flipper Length (mm)", y = "Bill Length (mm)",
                     color = "Penguin Species", shape = "Penguin Species")
flipper_bill

bill_len_dep <- ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm, group = species)) +
                geom_point(aes(color = species, shape = species), size = 3, alpha = 0.8) +
                geom_smooth(method = "lm", se = FALSE, aes(color = species)) + 
                labs(title = "Bill Length and Bill Depth", x = "Bill Length (mm)", y = "Bill Depth (mm)",
                     color = "Penguin species", shape = "Penguin species") 
bill_len_dep

bill_no_species <- ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
                  geom_point() +
                  labs(title = "Bill Dimensions (Omit Species)", x = "Bill Length (mm)", y = "Bill Depth (mm)") +
                  geom_smooth(method = "lm", se = FALSE, color = "gray50")
bill_no_species

penguins_per_island <- ggplot(penguins)+
                      geom_point(aes(island, species, color=species, shape=species))+
                      labs(title="Penguin Species Per Island")
penguins_per_island

hbarpenguins <- ggplot(penguins) +
                geom_bar(aes(island, fill=species)) +
                coord_flip() +
                labs(title="Types of Penguins per Island", x="Island", y="Amount of Penguins") 
hbarpenguins

penguinshisto <- ggplot(penguins) + 
                geom_histogram(aes(flipper_length_mm, fill=species), position='identity', alpha=0.5) + 
                labs(title="Frequency of Penguin Flipper Lengths per Species", x="Length (mm)", y="Frequency")
penguinshisto

size_per_penguin <- ggplot(penguins) +
                  geom_point(aes(flipper_length_mm, body_mass_g, color=species, shape=species)) + 
                  labs(title="Body Mass vs. Flipper Length")
size_per_penguin

species_per_island <- ggplot(data = penguins) +
                      geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species))+
                      labs(title="Body Mass vs. Flipper Length")+ 
                      geom_smooth(aes(x=flipper_length_mm, y = body_mass_g)) + 
                      facet_wrap(~island)
species_per_island

penguins %>% select(species, body_mass_g, ends_with("_mm")) %>% GGally::ggpairs(aes(color = species),
             columns = c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm"))


penguins$species <- as.factor(penguins$species)
penguins$island <- as.factor(penguins$island)
penguins$sex <- as.factor(penguins$sex)

set.seed(1234)
index <- sample(1:nrow(penguins), size = nrow(penguins)/2)

speciesnumeric <- as.numeric(penguins$species)
penguins$speciesnumeric <- speciesnumeric
train <- penguins[index,]
test <- penguins[-index,]
fit <- lm(speciesnumeric ~ flipper_length_mm + bill_length_mm + bill_depth_mm + body_mass_g, train)
summary(fit) 

predRes <- round(predict(fit, test))
predRes[which(predRes>3)] <- 3
predRes <- sort(names(pCol))[predRes]
test$predRes <- predRes
ggplot(test, aes(x = species, y = predRes, color = species)) + geom_jitter(size = 3)
table(test$predRes, test$species)

model1 <- lm(body_mass_g ~ bill_length_mm, penguins)
summary(model1)

model2 <- lm(body_mass_g ~ bill_depth_mm, penguins)
summary(model2)

model3 <- lm(body_mass_g ~ flipper_length_mm, penguins)
summary(model3)

model4 <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm, penguins)
summary(model4)

model5 <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, penguins)
summary(model5)

model6 <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, penguins)
summary(model6)

tab_model(model1, model2, model3, model4, model5, model6, dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"))

resid_panel(model1,plots=c("resid","qq"))
resid_panel(model2,plots=c("resid","qq"))
resid_panel(model3,plots=c("resid","qq"))
resid_panel(model4,plots=c("resid","qq"))
resid_panel(model5,plots=c("resid","qq"))
resid_panel(model6,plots=c("resid","qq"))
