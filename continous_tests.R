# Load packages
library(tidyverse)
library(finalfit) 
library(gapminder)
# Create object gapdata from object gapminder
gapdata <- gapminder

glimpse(gapdata)
missing_glimpse(gapdata) # missing data for each variable
ff_glimpse(gapdata) # summary statistics for each variable

######
#Always plot your data first. Never skip this step!
gapdata %>%
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(x = lifeExp)) + geom_histogram(bins = 20) + facet_grid(year ~ continent)
# remember aes()
# histogram with 20 bars
# optional: add scales="free"

gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(sample = lifeExp)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue") +      # add the theoretical line
  facet_grid(year ~ continent)

#Boxplots are our preferred method for comparing a continuous variable such as life expectancy across a categorical explanatory variable.
gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  facet_wrap(~ year)
## ggboxplots
gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
 ggboxplot( x = "continent", y = "lifeExp", color = "continent", palette = "jco",add = "jitter",
                 facet.by = "year", short.panel.labs = FALSE) 

### lets add p values to plots 
## ggboxplots
sel_data=gapdata %>% 
  filter(year %in% c(2002)) %>%
  filter(continent %in% c("Africa","Americas"))

ggboxplot( sel_data,x = "continent", y = "lifeExp", color = "continent", palette = "jco",add = "jitter") 

compare_means(lifeExp ~ continent, data = sel_data)
p=ggboxplot( sel_data,x = "continent", y = "lifeExp", color = "continent", palette = "jco",add = "jitter") 
#  Add p-value
p + stat_compare_means()
# Change method
compare_means(lifeExp ~ continent, data = sel_data,method = "t.test")
p + stat_compare_means(method="t.test")
p + stat_compare_means(method="t.test",aes(label = ..p.signif..),label.x = 1.5, label.y = 100)

#### Lets put p values on faceted plots and apply statistical tests

sel_data=gapdata %>%  
  filter(year %in% c(2002, 2007))
head(sel_data)
stat_compare_means
?compare_means
compare_means(lifeExp ~ year, data = sel_data,group.by="continent",method = "t.test")
p <- ggboxplot(sel_data, x = "year", y = "lifeExp",
               color = "continent", palette = "jco",
               add = "jitter",
               facet.by = "continent", short.panel.labs = TRUE) + ylim(40,100)
p
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format",method="t.test",label.x = 1.5,label.y=85)
#####

# Calculate means
mean_values <- gapdata %>%  
  filter(year %in% c(2002, 2007)) %>%
  filter(continent %in% c("Europe", "Africa")) %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp))
table(gapdata$continent)
# Create density plot


gapdata %>%  
  filter(continent %in% c("Europe", "Africa")) %>% 
  ggplot( aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_values, aes(xintercept = mean, color = continent), linetype = "dashed") +
  labs(title = "Density Plot of Two Variables with Means",
       x = "Value", y = "Density") +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal()

# Create density plot


mean_values <- gapdata %>%  
  filter(year %in% c(2002, 2007)) %>%
  filter(continent %in% c("Europe", "Americas")) %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp))
gapdata %>%  
  filter(year %in% c(2002, 2007)) %>%
  filter(continent %in% c("Europe", "Americas")) %>% 
  ggplot( aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_values, aes(xintercept = mean, color = continent), linetype = "dashed") +
  labs(title = "Density Plot of Two Variables with Means",
       x = "Value", y = "Density") +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal()

###


### Two-sample *t*-tests
#let's compare life expectancy between Asia and Europe for 2007. 

ttest_data <- gapdata %>%                    # save as object ttest_data                    # 2007 only
  filter(continent %in% c("Europe", "Africa")) # Asia/Europe only

ttest_result <- ttest_data %>%               # example using pipe
  t.test(lifeExp ~ continent, data = .)      # note data = ., see below
ttest_result

table(gapdata$country)

### check between America and Europe

ttest_data <- gapdata %>%                    # save as object ttest_data                    # 2007 only
  filter(continent %in% c("Europe", "Americas")) # Asia/Europe only

ttest_result <- ttest_data %>%               # example using pipe
  t.test(lifeExp ~ continent, data = .)      # note data = ., see below
ttest_result

library(broom)
tidy(ttest_result)

table(ttest_data$continent)

# 
gapdata %>% 
filter(year == 2007) %>%          # 2007 only
  group_by(continent) %>%           # split by continent
  do(                               # dplyr function
    t.test(.$lifeExp, mu = 77) %>%  # compare mean to 77 years 
      tidy()                        # tidy into tibble
  )


gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% 
           c("Americas", "Europe", "Asia")) %>% 
  ggplot(aes(x = continent, y=lifeExp)) +
  geom_boxplot()
### ANOVA
aov_data <- gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia"))

fit = aov(lifeExp ~ continent, data = aov_data) 
summary(fit)

library(broom)
gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp~continent, data = .) %>% 
  tidy()
pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "bonferroni")
pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "fdr")
##### Anova and Kruskal–Wallis test ggboxplots
sel_data=gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia"))

# Default method = "kruskal.test" for multiple groups
ggboxplot(sel_data, x = "continent", y = "lifeExp",
          color = "continent", palette = "jco")+
  stat_compare_means()
# Change method to anova
ggboxplot(sel_data, x = "continent", y = "lifeExp",
          color = "continent", palette = "jco")+
  stat_compare_means(method = "anova")

compare_means(lifeExp ~ continent,  data = sel_data)

####

# Visualize: Specify the comparisons you want
my_comparisons <- list( c("Americas", "Asia"), c("Asia", "Europe"),  c("Americas", "Europe") )
ggboxplot(sel_data,  x = "continent", y = "lifeExp",
          color = "continent", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)     # Add global p-value


compare_means(lifeExp ~ continent,  data = sel_data, ref.group = "Asia",
              method = "t.test")
ggboxplot(sel_data,  x = "continent", y = "lifeExp",
          color = "continent", palette = "jco")+
  stat_compare_means(method = "anova", label.y = 40)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "Asia")                    # Pairwise comparison against reference


####
