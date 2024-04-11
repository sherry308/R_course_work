## read data
mouse.data=read.csv("~/projects/computational_biology_course/mouse.data.csv")
# Display the first few rows of the dataset
head(mouse.data)

## plot a x/y scatter plot with the data
plot(mouse.data$Weight, mouse.data$Size)

## create a "linear model" - that is, do the regression
mouse.regression <- lm(size ~ weight, data=mouse.data)
## generate a summary of the regression
summary(mouse.regression)

## add the regression line to our x/y scatter plot
abline(mouse.regression, col="blue")

library(ggfortify)
autoplot(mouse.regression, which = 1:2, label.size = 3, data = mouse.data)

##### Addictive and interactive linear models
library(tidyverse)
library(gapminder) # dataset
library(broom)

dim(gapdata)
head(gapdata)
theme_set(theme_bw())
gapdata <- gapminder
glimpse(gapdata)
missing_glimpse(gapdata) 
ff_glimpse(gapdata)
gapdata %>%                        
  filter(continent == "Europe") %>%    # Europe only
  ggplot(aes(x = year, y = lifeExp)) + # lifeExp~year  
  geom_point() +                       # plot points
  facet_wrap(~ country) +              # facet by country
  scale_x_continuous(
    breaks = c(1960, 2000)) +          # adjust x-axis 
  geom_smooth(method = "lm")           # add regression lines

#let's plot two countries to compare, Turkey and United Kingdom
gapdata %>% 
  filter(country %in% c("Turkey", "United Kingdom")) %>% 
  ggplot(aes(x = year, y = lifeExp, colour = country)) + 
  geom_point()


# United Kingdom
fit_uk <- gapdata %>%
  filter(country == "United Kingdom") %>% 
  lm(lifeExp~year, data = .)

fit_uk %>% 
  summary()




# Turkey
fit_turkey <- gapdata %>%
  filter(country == "Turkey") %>% 
  lm(lifeExp~year, data = .)

fit_turkey %>% 
  summary()

min(gapdata$year)

#To make the intercepts meaningful, we will add in a new column called year_from1952 and re-run fit_uk and fit_turkey using year_from1952 instead of year.
gapdata <- gapdata %>% 
  mutate(year_from1952 = year - 1952)

fit_uk <- gapdata %>%
  filter(country == "United Kingdom") %>% 
  lm(lifeExp ~ year_from1952, data = .)
summary(fit_uk)
head(gapdata)
fit_turkey <- gapdata %>%
  filter(country == "Turkey") %>% 
  lm(lifeExp ~ year_from1952, data = .)
summary(fit_turkey)

# compare addictive and interactive models
#addictive model
# UK and Turkey dataset
gapdata_UK_T <- gapdata %>%
  filter(country %in% c("Turkey", "United Kingdom"))

fit_both1 <- gapdata_UK_T %>% lm(lifeExp ~ year_from1952, data = .)
fit_both1
myfit = lm(lifeExp ~ year, data = gapdata)
summary(myfit)
myfit = lm(lifeExp ~ year + country, data = gapdata)
summary(myfit)
#Interactive model
myfit = lm(lifeExp ~ year * country, data = gapdata)
summary(myfit)
### take a note which of them have higher R2, that model is better

#### now lets try with meaningful column of the year 
gapdata_UK_T <- gapdata %>% 
  filter(country %in% c("Turkey", "United Kingdom"))

fit_both1 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952, data = .)
fit_both1

     
#### use below to show single variable model  ### focus on what predict function is doing
gapdata_UK_T %>%
  mutate(pred_lifeExp = predict(fit_both1)) %>%
  ggplot() +
  geom_point(aes(x = year, y = lifeExp, colour = country)) + geom_line(aes(x = year, y = pred_lifeExp))

### additive model
fit_both2 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952 + country, data = .)
fit_both2
summary(fit_both2)




gapdata_UK_T %>% 
  mutate(pred_lifeExp = predict(fit_both2)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(aes(x = year, y = pred_lifeExp, colour = country))


## interaction model 

fit_both3 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952 * country, data = .)
fit_both3
summary(fit_both3)
gapdata_UK_T %>% 
  mutate(pred_lifeExp = predict(fit_both3)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(aes(x = year, y = pred_lifeExp, colour = country))



