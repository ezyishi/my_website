---
categories:
- ""
- ""
date: "2020-10-16"
description: The question of pay discrimination
draft: false
image: equality2.jpg
keywords: ""
slug: blog10
title: Omega Group's pay discrimination
---

The pay discrimination among gender and many other factors has been a long-existing problem in the industries. Is there really a systematic pay discrimination among the industries, or the phenomenon is simply a statistical coincidence? To understand more about this, we conduct a analysis of Omega Group as an example and take in consideration of factors like gender and experience to figure it out. 

**We first load the data set**

```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

**The data frame `omega`  contains the salaries for the sample of 50 executives in the company. We want to conclude that if the difference between the salaries of executives is caused by a gender difference.**

#Divide omega by male and female

```
omega_male <- omega %>%
+     filter(gender == "male")

omega_female <- omega %>%
+     filter(gender == "female")
```

**Since we want to exclude the probability that our data is just one of the "coincidence", we repeat the calculation for bootstrap distribution for 1000 times and get the 95% confidence interval**

#Build a bootstrap distribution for male and female

```
bootstrap_distribution_male <- omega_male%>%
+ specify(response = salary) %>%
+ generate(reps = 1000, type = "bootstrap") %>%
+ calculate(stat = "mean")

bootstrap_distribution_female <- omega_female%>%
+ specify(response = salary) %>%
+ generate(reps = 1000, type = "bootstrap") %>%
+ calculate(stat = "mean")
```

#Low/high end point of 95% CI

```
x_bar_male <- omega_male %>%
+ specify(response = salary) %>%
+ calculate(stat = "mean")

x_bar_female <- omega_female %>%
+ specify(response = salary) %>%
+ calculate(stat = "mean")

standard_error_ci_male <- bootstrap_distribution_male %>%
+ get_ci(type = "se", point_estimate = x_bar_male)

standard_error_ci_female <- bootstrap_distribution_female %>%
+ get_ci(type = "se", point_estimate = x_bar_female)
```

#The table

```
summary <- omega %>%
+ group_by(gender) %>%
+ summarize(min = min(salary, na.rm = TRUE), q1 = quantile(salary, 0.25, na.rm = TRUE), median = quantile(salary, 0.5, na.rm = TRUE), q3 = quantile(salary, 0.75, na.rm = TRUE), max = max(salary, na.rm = TRUE), mean = mean(salary, na.rm = TRUE), sd = sd(salary, na.rm = TRUE), n = n(), missing = sum(is.na(salary)))
```

**We also analyse the standard error of our data, which indicates how far our sample mean is away from the population mean, and margin error that gives us a range of where our data are expected to be in.**

#Now add in SE, 95%ci, t-critical and marginal error

```
mutation_table <- mosaic::favstats (salary ~ gender, data=omega) %>%
  select(gender, mean, sd, n)

mutation_table_2 <- mutation_table %>%
  mutate(SE = sd/sqrt(n),
         t_critical = qt(0.975, n-1),
         margin_error = t_critical * SE,
         low_95_ci = mean - margin_error,
         high_95_ci = mean + margin_error)
```

**Then, we would like to run a hypothesis testing, assuming as a null hypothesis that the mean difference in salaries is zero, or that, on average, men and women make the same amount of money. The hypothesis testing could give us a hindset that if our data manage to reject the null assumption that men and women make the same amount of money.**

#Hypothesis testing using t.test() 

```
salary_male <- omega_male %>%
  select(salary)

salary_female <- omega_female %>%
  select(salary)

t.test(salary_male, salary_female, "two.sided", var.equal = FALSE)
```

#Hypothesis testing using infer package

```
infer_gender <- omega %>% 
  filter(gender == "male" | gender == "female") %>%
  specify(salary ~ gender) %>%  
  hypothesize( null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("male", "female"))
```

**The above analysis helps us to conclude if the difference in salaries between men and women executives in Omega Group is caused by a gender difference, but we would also like to analyse the relationship between their salaries and the actual years of experience each of the executives has. We want to see if that also plays a major role instead of gender alone.**


#Summary Statistics of salary by gender

```
favstats (experience ~ gender, data=omega)

bootstrap_distribution_male_ex <- omega_male%>%
+ specify(response = experience) %>%
+ generate(reps = 1000, type = "bootstrap") %>%
+ calculate(stat = "mean")

bootstrap_distribution_female_ex <- omega_female%>%
+ specify(response = experience) %>%
+ generate(reps = 1000, type = "bootstrap") %>%
+ calculate(stat = "mean")
```

#Low/high end point of 95% CI

```
x_bar_male_ex <- omega_male %>%
+ specify(response = experience) %>%
+ calculate(stat = "mean")

x_bar_female_ex <- omega_female %>%
+ specify(response = experience) %>%
+ calculate(stat = "mean")

standard_error_ci_male_ex <- bootstrap_distribution_male_ex %>%
+ get_ci(type = "se", point_estimate = x_bar_male_ex)

standard_error_ci_female_ex <- bootstrap_distribution_female_ex %>%
+ get_ci(type = "se", point_estimate = x_bar_female_ex)
```

#The table

```
summary_ex <- omega %>%
+ group_by(gender) %>%
+ summarize(min = min(experience, na.rm = TRUE), q1 = quantile(experience, 0.25, na.rm = TRUE), median = quantile(experience, 0.5, na.rm = TRUE), q3 = quantile(experience, 0.75, na.rm = TRUE), max = max(experience, na.rm = TRUE), mean = mean(experience, na.rm = TRUE), sd = sd(experience, na.rm = TRUE), n = n(), missing = sum(is.na(experience)))
```

#Now add in SE, 95%ci, t-critical and marginal error

```
mutation_table_ex <- favstats (experience ~ gender, data=omega) %>%
  select(gender, mean, sd, n)

mutation_table_ex2 <- mutation_table_ex %>%
  mutate(SE = sd/sqrt(n),
         t_critical = qt(0.975, n-1),
         margin_error = t_critical * SE,
         low_95_ci = mean - margin_error,
         high_95_ci = mean + margin_error)
```

**It can be concluded from the above new analysis that the difference in experience also contribute a lot towards the difference in payments between genders.**

**Therefore, within this sample we selected, we could comfortably claim that even pay discrimination indeed exists, it is not solely due to the gender difference, but the year of working experience also manages to cause such discrimination.**