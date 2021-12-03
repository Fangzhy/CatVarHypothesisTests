# Hypothesis Tests Involving Categorical Vairables
__Fang Yu | yufzhy@gmail.com__

This repository hosts codes to perform and visualize statistic tests involving at least one categorical variable. Chi Square test is performed if it is between two categorical variables. Tukey HSD test is performed if it is between one categorical and one continuous variable. I did not include the test between two continuous variables as it is really easy to perform using all kinds of tests such as T test.  

The running examples here are based a public dataset called cars93.

# Examples using a public dataset cars93
This section shows examples of how to use functions in this repository to perform and visualizae the tests using a public dataset called cars93. 
```{r}
### Quick test using public dataset
data("Cars93")

```
## Chi Square test of two categorical variables
Do different types of cars have different drive trains? If so, whether the difference are significant or not?

```{r, message=FALSE}
# Chi Square test of two categorical variables
data1=data.frame(x=Cars93$Type,y=Cars93$DriveTrain)
test2CatVar(data = data1, xlabText='Type of Car', ylabText = 'Drive Train',xlabTickAngle = 15, titleText='Car Type VS Drive Train', testType = 'ChiSquared')

```

## Tukey HSD test of one categorical variable and one continous variable.
Do different types of cars have different metrics of Mileage Per Hour (MPG) ? If so, whether the difference are significant or not?

```{r, message=FALSE}
# Tukey HSD test of one categorical variable and one continuous variable
data2=data.frame(x=Cars93$Type,y=Cars93$MPG.city)
test2CatVar(data = data2, xlabText='Type of Car', ylabText = 'MPG',xlabTickAngle = 15, titleText='Car Type VS MPG', testType = 'TukeyHSD')

```
