# RegressionModels
---
title: "Regression Coursework"
author: "Hoe Keng Yew"
date: "15/10/2020"
output: pdf_document
---
# Summary
This report analyzed the relationship between transmission types and miles per gallon (MPG). The aim of this investigation is to determine which transmission type is more fuel economical (higher MPG value). After the analysis was done, manual transmission cars generally have a higher MPG value however the difference in MPG between automatic and manual transmission is only 1.81. Other factors such as number of cylinders, horsepower and weight of the car have a more significant effect on the MPG value.

# Loading the data
```{r}
# loading the R Built-in dataset
library(ggplot2)
data(mtcars)
mtcars$mpg <- as.numeric(mtcars$mpg)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- ifelse(as.integer(mtcars$am == 1), "Manual", "Auto")
head(mtcars)
```
# Exploratory Analysis
```{r, echo = TRUE, include=FALSE}
# Results are in appendix
# Finding the average mpg for auto and manual transmission cars based on cylinders
transmissionMPG <- aggregate(mpg ~ am + cyl, mtcars, FUN = mean )
head(transmissionMPG)
p <- ggplot(transmissionMPG, aes(x = am,  y = mpg)) + geom_bar(stat = "identity") +facet_grid(scales="free", space="free", .~cyl) +
  guides(fill=FALSE) + theme_bw() +
  scale_fill_brewer(palette="Blues")+labs(x="Transmission", y=expression("Average MPG")) + 
  labs(title=expression("Average MPG based on Transmission type for number of cylinders")) + scale_y_continuous(name = "Average MPG",c(0,25))
p
```
## Regression model
```{r, results="hide"}
fullModel <- lm(mpg~., mtcars)
optimalModel <- step(fullModel, direction = "backward")
```
```{r,echo=TRUE, include = FALSE}
# Results in appendix
# Summary of the best model 
summary(optimalModel)
par(mfrow=c(2,2))
plot(optimalModel)
```
This procedure determines that the best model includes the cyl6, cyl8, hp, wt, and amManual variables (overall p-value<0.001). The adjusted R-squared indicates that about 84% of the variance is explained by the final model. Moreover, the output of this model suggests that mpg decreases with respect to cylinders (-3.03 and -2.16 for cyl6 and cyl8, respectively), horsepower (-0.03), and weight (for every 1,000lb, by -2.5). On the other hand, mpg increases with respect to having a manual transmission (by 1.8). Residual plots (see appendix) suggest that some transformation may be necessary to achieve linearity.

## Statistical Inference
```{r}
t.test(mpg~am,mtcars)
```
The confidence intervals excludes 0 and the p-value is greater than the threshold of 0.05.  The null hypothesis can be rejected.
```{r, echo = TRUE, include = FALSE}
# Results in appendix 
# Box Plot of MPG based on transmission type
b<-ggplot(data = mtcars,aes(x=am,y=mpg)) + geom_boxplot(col = "green", fill = "darkgreen") + theme_bw() + xlab("Transmission Type") + ylab("Miles Per Gallon") + labs(title=expression("Box Plot of MPG based on Transmission type"))
b
```
## Conclusion
According to the data analysis, cars with manual transmission have better fuel efficiency as they have a higher MPG value. The hypothesis testing done for the condition where MPG is the same for automatic transmission and manual transmission are rejected. Cars with a lower number of cylinders have a higher MPG value compared to cars with higher number of cylinders. The difference between MPG values for Automatic and Manual transmission cars also decreases as umber of cylinders increase.
\newpage

## Appendix
### Graphs and Summaries of tables
```{r}
# Finding the average mpg for auto and manual transmission cars based on cylinders
transmissionMPG <- aggregate(mpg ~ am + cyl, mtcars, FUN = mean )
#head(transmissionMPG)
# Plotting the histogram
p <- ggplot(transmissionMPG, aes(x = am,  y = mpg)) + geom_bar(stat = "identity") +facet_grid(scales="free", space="free", .~cyl) +
  guides(fill=FALSE) + theme_bw() +
  scale_fill_brewer(palette="Blues")+labs(x="Transmission", y=expression("Average MPG")) + 
  labs(title=expression("Average MPG based on Transmission type for number of cylinders")) + scale_y_continuous(name = "Average MPG",c(0,25))
p
# Summary of the best model 
summary(optimalModel)
par(mfrow=c(2,2))
plot(optimalModel)
# Box Plot of the MPG based on transmission type
b<-ggplot(data = mtcars,aes(x=am,y=mpg)) + geom_boxplot(col = "green", fill = "darkgreen") + theme_bw() + xlab("Transmission Type") + ylab("Miles Per Gallon") + labs(title=expression("Box Plot of MPG based on Transmission type"))
b
```
