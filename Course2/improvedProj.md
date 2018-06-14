---
title: "Education and the GSS data"
author: "DocOfi"
date: "October 9, 2016"
output:
   html_document:
    fig_height: 4
    highlight: pygments
    theme: spacelab
    keep_md: true
---





###Introduction

Since 1972, the General Social Survey (GSS) has been monitoring societal change and studying the growing complexity of American society. The GSS aims to gather data on contemporary American society in order to monitor and explain trends and constants in attitudes, behaviors, and attributes; to examine the structure and functioning of society in general as well as the role played by relevant subgroups; to compare the United States to other societies in order to place American society in comparative perspective and develop cross-national models of human society; and to make high-quality data easily accessible to scholars, students, policy makers, and others, with minimal cost and waiting. [http://www.norc.org/Research/Projects/Pages/general-social-survey.aspx](http://www.norc.org/Research/Projects/Pages/general-social-survey.aspx)

The GSS is an area-probability sample that uses the National Opinion Research Center (NORC) National Sampling Frame for an equal probability multi-stage cluster sample of housing units for the entire United States. The results of the survey can therefore be generalized to the general population. 

We must be mindful however, that a survey is an obsevational study. Causality cannot be inferred as the conditions present does not afford an experimental design that create equal conditions for a control group and an experimental group. 

The data we will be using is an extract of the General Social Survey (GSS) Cumulative File 1972-2012 set aside for the goal of providing a convenient data resource for students learning statistical reasoning using the R language. The data is available for download for students of coursera taking the inferenctial statistics course.


```r
load("gss.RData")
```

###Research Question 1

Is confidence in educational institutions associated with opinions  regarding Government spending for education? 

Does having the opinion that there is too little spending for education associated with having hardly any confidence in educational institutions?

Cost of educaton has grown exponentially as more attention has been given to other problems like health. When access to good education is restricted, it becomes yet another structure that promotes oppression and inequality in society. This is why the efforts of those behind Coursera is praiseworthy because of the almost quixotic ideals that it fosters and the unbelievable courage it has for taking the challenge of reversing the trend in education not only in the United States but in the whole world. 

I want to know the prevailing opinion regarding spending for education in America and how or if this related to confidence in educational institutions.




```r
by_coneduc <- gss %>% filter(year != "NA", coneduc != "NA", nateduc != "NA") %>% group_by(year,coneduc) %>% summarise(n_coneduc = n())
byconeduc_year <- gss %>% filter(year != "NA", coneduc != "NA") %>% group_by(year) %>% select(year, coneduc) %>% summarise(total_confid = n())
merged_coneduc <- merge(byconeduc_year, by_coneduc) %>% filter(year >= 2000) %>% group_by(coneduc) %>% summarise(n = sum(n_coneduc)) %>% mutate(total = sum(n)) %>% mutate(perc_conf = n/total) %>% mutate(perc_conf = round(perc_conf*100, 1)) %>% mutate(category = rep("confidence", 3)) %>% rename(response = coneduc)
by_nateduc <- gss %>% filter(year != "NA", nateduc != "NA") %>% group_by(year,nateduc) %>% summarise(n_nateduc = n())
bynateduc_year <- gss %>% filter(year != "NA", nateduc != "NA") %>% group_by(year) %>% select(year, nateduc) %>% summarise(total_confid = n())
merged_nateduc <- merge(bynateduc_year, by_nateduc) %>% filter(year >= 2000) %>% group_by(nateduc) %>% summarise(n = sum(n_nateduc)) %>% mutate(total = sum(n)) %>% mutate(perc_conf = n/total) %>% mutate(perc_conf = round(perc_conf*100, 1)) %>% mutate(category = rep("spending", 3)) %>% rename(response = nateduc)
educ_df <-  rbind(merged_coneduc, merged_nateduc)
educ_df %>% group_by(category) %>% ggplot(aes(y = perc_conf, x = response, fill = response)) + geom_bar(stat = "identity") + ylab("Percent") + xlab("Response") + ggtitle("Confidence and Spending in Education 2000-2012") +  scale_fill_hue(name="Response") + facet_wrap( ~ category) + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
```

![](improvedProj_files/figure-html/sumstat-1.png)<!-- -->

The plot on the right shows the overwhelming percentage of respondents who believe the extreme view that too little money is spent by government for education. More than 70% of the respondents belong to this group. 

In terms of confidence in edcuational institutions, the plot o the left shows the moderates (About Right), or those who are in the middle of the two ends, dominate the extreme views with regard to confidence in educational institutions.  Its dominance is not as marked however.

We will focus our analysis on the relationship between Government spending on education, income, sex, and educational institutions.  The variables we are interested in are:

### Table 1. selected variables

|Variable         |Question                                                                                 |DataType |
|:----------------|:----------------------------------------------------------------------------------------|:--------|
|`year`           |GSS year for this respondent                                                             |Numeric  |
|`coninc`         |Total family income in constant dollars                                                  |Numeric  |
|`coneduc`        |confidence in educational  institutions                                                  |Factor   |
|`nateduc`        |spending to improve nation's education system                                            |Factor   |
|`sex`            |sex                                                                                      |Factor   |

To find out whether confidence in educational institutions is associated with an opinion regarding Government spending for education we will perform a hypothesis test using a chi-squared test of independence. 

###The conditions for the chi-squared test are:

1 Independence - Since random sampling was performed and sampling without replacement was performed, we can assume the requirement for independence is met. Each observation contributes to only one cell in the table.

2 Sample size - Each cell contains more than 5 expected observations as seen below. 


```r
data_2000 <-gss %>% filter(year != "NA", nateduc != "NA", coninc != "NA", coneduc != "NA", sex != "NA") %>% filter(year >= 2000) %>% select(coneduc, nateduc, coninc, year, sex)
table(data_2000$coneduc, data_2000$nateduc)
```

```
##               
##                Too Little About Right Too Much
##   A Great Deal        723         332       42
##   Only Some          1851         466      112
##   Hardly Any          485          67       93
```
This condition is also met.

### Hypothesis test for chi-squared test

null hypothesis: Levels of confidence in educational institutions are independent of opinions on whether enough money is spent on education. 

alternative hypothesis: Levels of Confidence in educational institutions are not independent of opinions on whether enough money is spent on education.

We know perform a chi-squared test using the inference function.


```r
inference(data = data_2000, y = coneduc, x = nateduc, type = "ht", statistic = "proportion", method = "theoretical", alternative = "greater")
```

```
## Response variable: categorical (3 levels) 
## Explanatory variable: categorical (3 levels) 
## Observed:
##              y
## x             A Great Deal Only Some Hardly Any
##   Too Little           723      1851        485
##   About Right          332       466         67
##   Too Much              42       112         93
## 
## Expected:
##              y
## x             A Great Deal Only Some Hardly Any
##   Too Little      804.5368 1781.4220  473.04124
##   About Right     227.5006  503.7365  133.76289
##   Too Much         64.9626  143.8415   38.19588
## 
## H0: nateduc and coneduc are independent
## HA: nateduc and coneduc are dependent
## chi_sq = 189.2322, df = 4, p_value = 0
```

![](improvedProj_files/figure-html/chisq1-1.png)<!-- -->

### Conclusion for the Chi-Squared Test

Since our p-value is less than 0.05, we reject the null hypothesis. The data provide convincing evidence that the observed counts are different from the expected counts.

Confidence in educational institutions and opinions whether enough money is spent on education are dependent.

###Research Question 2

Can the difference in opinions regarding spending for education among those with hardly any confidence in educational institutions be explained by differences in income?  Can low or high average income influence the respondents to believe that too much is spent for education? 


```r
inc <- data_2000 %>% filter(coneduc == "Hardly Any")
inc %>% group_by(nateduc) %>%summarise(sum_inc = sum(coninc), mean_inc = mean(coninc), sd_inc = sd(coninc), max_inc = max(coninc), iqr_inc = IQR(coninc), n_inc = n(), median_inc = median(coninc)) 
```

```
## # A tibble: 3 x 8
##   nateduc      sum_inc mean_inc sd_inc max_inc iqr_inc n_inc median_inc
##   <fct>          <int>    <dbl>  <dbl>   <dbl>   <dbl> <int>      <int>
## 1 Too Little  22738979   46884. 40363.  178712   46398   485      36135
## 2 About Right  3027561   45187. 42736.  178712   40702    67      31618
## 3 Too Much     5237873   56321. 46874.  178712   47794    93      42130
```

Looking at the summary statistics we see that, among those who have hardly any confidence in educational institutions, the group Too Little  and the group About Right has mean incomes that are nearly similar. The group Too Much has a higher mean income and is the highest among the groups. 

<img src="improvedProj_files/figure-html/pltms-1.png" style="display: block; margin: auto;" />

The confidence interval for the point estimates show considerable overlap. Based on the plot it is not easy to tell whether the differences are not due to random difference due to sampling variability.

Since we are dealing with a numeric response variable and a categorical dependent variable with 3 levels, we will use anova or analysis of variance to test our hypothesis. The ANOVA is the preferred approach for our data because it controls for so-called "study-wide error rate". Comparing the different groups with each other will require 3 separate t-tests.  Multiple testing increase the chance of finding a statistically significant finding by chance.

### Hypothesis testing for Anova

Our null hypothesis: The population mean total family income of those with hardly any confidence in educational institutions is the same among those who believe that there is: too little, about right, and too much spending for education. 

Our alternative hypothesis: The population mean total constant family income of those with hardly any confidence in educational institutions is different in at least one group among those who believe that there is: too little, about right, and too much spending for education. 

let us see if the conditions for performing anova are first met.

### Conditions for Anova

1. Independence - Since random sampling without replacement was performed, we can assume the requirement for independence both within and between groups are met.  The data is not paired. 
2. Approximate normality- distributions should be nearly normal within each group.  


```r
mean_income <- inc %>% select(coninc, nateduc) %>% group_by(nateduc) %>% summarise(mean_inc = mean(coninc))
inc %>% ggplot(aes(x = coninc, fill = nateduc)) + geom_histogram(binwidth= 18500, colour = "black", aes(y = ..density..)) + facet_wrap( ~ nateduc) + geom_vline(aes(xintercept = mean_inc), mean_income, colour = "red") + scale_colour_discrete(name="Response", breaks= c("Too Little", "About Right", "Too Much"), labels= c("Too Little", "About Right", "Too Much")) + scale_shape_discrete(name="Response", breaks= c("Too Little", "About Right", "Too Much"), labels= c("Too Little", "About Right", "Too Much")) + scale_fill_hue(name="Response")
```

![](improvedProj_files/figure-html/gghistog-1.png)<!-- -->

We see that constant income seems to have roughly the same right skewed distribution among the different opinions on spending for education. The red line representing the respective means of each category. Our sample size is comparatively adequate to overcome the skewness of the data, except for the group Too Much with only 67 oservations. We will however proceed with the test for the sake of this exercise.

3. Equal variance - Groups should have roughly equal variability. 


```r
inc %>% ggplot(aes(x=nateduc, y=coninc, fill=nateduc)) + geom_boxplot() + ylab("Total family income in constant dollars") + xlab("Response") + ggtitle("Family Income VS Spending for Education") +  scale_fill_hue(name="Response")
```

![](improvedProj_files/figure-html/bxplt-1.png)<!-- -->

Based on the boxplots of the different groups above, this condition is roughly met.

We use the inference function in `statsr` to perform the anova test.


```r
inc %>% inference(y = coninc, x = nateduc, statistic = "mean", type = "ht", method = "theoretical", alternative = "greater")
```

```
## Response variable: numerical
## Explanatory variable: categorical (3 levels) 
## n_Too Little = 485, y_bar_Too Little = 46884.4928, s_Too Little = 40363.2
## n_About Right = 67, y_bar_About Right = 45187.4776, s_About Right = 42736.0984
## n_Too Much = 93, y_bar_Too Much = 56321.2151, s_Too Much = 46874.4055
## 
## ANOVA:
##            df           Sum_Sq         Mean_Sq      F p_value
## nateduc     2  7570007231.2374 3785003615.6187 2.1868  0.1131
## Residuals 642 1111210951229.64 1730858179.4854               
## Total     644 1118780958460.88
```

![](improvedProj_files/figure-html/infanov-1.png)<!-- -->

### Conclusion for the Anova Test

The resulting pvalue of our anova test is greater than the 0.05% significance level.  We therefore  fail to reject the null hypothesis. 

Conclusion: The data does not provide convincing evidence that the population mean total family income of those with hardly any of confidence in educational institutions is different among those who believe that there is: too little, about right, and too much spending for education.

It seems that mean total family income is the same among the different groups. The difference in income probably does not influence the respondents belief regarding Government spending for education? 

###Research Question 3

Women tend to do better in school. Could gender be associated with belief regarding Government spending for education? 

Does the proportion of males and females differ in the groups too little and about right with regard to Government spending for education.


```r
gndr <- inc %>% filter(nateduc == "Too Little" | nateduc == "About Right") %>% droplevels() %>% group_by(sex) %>% summarise(count = n()) %>% mutate(total = sum(count), perc = count/total) %>% mutate(perc = round(perc,2))
gndr
```

```
## # A tibble: 2 x 4
##   sex    count total  perc
##   <fct>  <int> <int> <dbl>
## 1 Male     261   552  0.47
## 2 Female   291   552  0.53
```

There is slightly greater percentage of females in our data.


```r
tlar <- inc %>% filter(nateduc == "Too Little" | nateduc == "About Right") %>% droplevels() %>% group_by(nateduc) %>% summarise(count = n()) %>% mutate(total = sum(count), perc = count/total) %>% mutate(perc = round(perc,2))
tlar
```

```
## # A tibble: 2 x 4
##   nateduc     count total  perc
##   <fct>       <int> <int> <dbl>
## 1 Too Little    485   552  0.88
## 2 About Right    67   552  0.12
```

Almost 90% of the respondents in our data believe that Government spending for education is too little.  

### Conditions for the difference in proportion

1. Independence 

- within groups: sampled observations must be independent within each group. Since random sampling without replacement was performed and our sample is less tha 10% of the total population, we can assume the requirement for independence are met. 

- between groups: the two groups must be independent of each other (non-paired). This condition is met.

2. Sample size/skew 
- Whether the distribution of the difference in proportion follows a normal distriution.
- Each sample should meet the success-failure condition, at least 10 for the data to follow a normal distribution.

`n1p1 is greater than or equal to 10 and n1(1-p1) is greater than or equal to 10`

`n2p2 is greater than or equal to 10 and n2(1-p2) is greater than or equal to 10`

###Checking the Success Failure condition for a confidence interval


```r
genprop_tl <- inc %>% filter(nateduc == "Too Little") %>% droplevels() %>% group_by(sex) %>% summarise(count = n()) %>% mutate(total = sum(count), perc = count/total) %>% mutate(perc = round(perc, 2))
genprop_ar <- inc %>% filter(nateduc == "About Right") %>% droplevels() %>% group_by(sex) %>% summarise(count = n()) %>% mutate(total = sum(count), perc = count/total) %>% mutate(perc = round(perc, 2))
genprop_tl
```

```
## # A tibble: 2 x 4
##   sex    count total  perc
##   <fct>  <int> <int> <dbl>
## 1 Male     227   485  0.47
## 2 Female   258   485  0.53
```

```r
genprop_ar
```

```
## # A tibble: 2 x 4
##   sex    count total  perc
##   <fct>  <int> <int> <dbl>
## 1 Male      34    67  0.51
## 2 Female    33    67  0.49
```

The number of respondents in Too Little group is *485* and the proportion of females are *0.53*. The number of successes or the number of females are *258*. The  number of failures or the number of males are *227*.

The number of respondents in About Right group is *67* and the proportion of females are *0.49*. The number of successes or the number of females are *33*. The  number of failures or the number of males are *34*.

The success failure condition is met.

###Create a 95% confidence interval


```r
confI <- (genprop_tl$perc[2] - genprop_ar$perc[2]) + c(-1,1) * 1.96 * sqrt(((genprop_tl$perc[2]*(1-genprop_tl$perc[2]))/genprop_tl$count[2]) + ((genprop_ar$perc[2]*(1-genprop_ar$perc[2]))/genprop_ar$count[2]))
```

The proportion of women in the group too little is 53% while on the group about right the proportion is 49%,  a difference of 4%. Our 95% confidence interval for this point estimate is **-0.141109, 0.221109**.  The interval contains the null value of 0, which means that we cannot discount the possiility that there is no difference between the two proportions.

### Hypothesis testing for difference in proportion

null hypothesis: The proportion of females among those who believe that Government spending for education is too little and about right are the same. 

alternative hypothesis: The proportion of females among those who believe that Government spending for education is too little and about right  are not the same.

###Checking the Success Failure condition for a Hypothesis Test
 
When conducting a hypothesis test, we state that in null hypothesis that the two proportions are equal.  We need to use a pooled proportion to state that this is the value to which the two proportions are equal to.

We use our pooled proportion to check whther our success failure conditionns is met

##Pooled proportion

```r
pooled_prop <- (genprop_tl$count[2] + genprop_ar$count[2])/(genprop_tl$total[2] + genprop_ar$total[2])
pooled_prop
```

```
## [1] 0.5271739
```

The number of respondents in the group too little are *485* and the  pooled proportion *0.5271739*. The  number of successes are *255.7*. The  number of failures are *229.3*.

The number of respondents in the group about right are *67* and the  pooled proportion *0.5271739*. The  number of successes are *35.3*. The  number of failures are *31.7*.

The success failure condition is met.  We proceed to perform the difference in proportions test using the inference function.

##Standard Error


```r
SE <- sqrt(((pooled_prop*(1-pooled_prop))/genprop_tl$count[2]) + ((pooled_prop*(1-pooled_prop))/genprop_ar$count[2]))
null_v <- 0
pt_est <- genprop_tl$perc[2] - genprop_ar$perc[2]
z_score <- (pt_est-0)/SE
SE
```

```
## [1] 0.0923012
```

```r
pt_est
```

```
## [1] 0.04
```

```r
z_score
```

```
## [1] 0.4333638
```

```r
2*pnorm(z_score, lower.tail = FALSE)
```

```
## [1] 0.6647505
```



```r
inc %>% filter(nateduc == "Too Little" | nateduc == "About Right") %>% droplevels() %>% inference(y = sex, x = nateduc, statistic = "proportion", type = "ht",null = 0, method = "theoretical", alternative="twosided",success = "Female")
```

```
## Response variable: categorical (2 levels, success: Female)
## Explanatory variable: categorical (2 levels) 
## n_Too Little = 485, p_hat_Too Little = 0.532
## n_About Right = 67, p_hat_About Right = 0.4925
## H0: p_Too Little =  p_About Right
## HA: p_Too Little != p_About Right
## z = 0.6058
## p_value = 0.5446
```

![](improvedProj_files/figure-html/proptest-1.png)<!-- -->


### Conclusion for the difference in proportion

The proportion of women in the group too little is 53% while on the group about right the proportion is 49%,  a difference of 4%. Given that the pvalue of our test for difference in proportion is 0.54 which is greater than the our significance level of 0.05%,  we therefore fail to reject the null hypothesis. 

Conclusion: The data does not provide convincing evidence that the proportion of females among those believe that Government spending for education is too little and about right  are not the same among those who have hardly any confidence in educational institutions. It seems that gender difference is not associated with beliefs regarding Government spending for education.

### Keypoints

- About 15% of the respondents in the GSS survey between the year 2000 and 2012 answered hardly any when asked "would you say you have a great deal of confidence in educational institutions?" 

- The greatest proportion (75%) of those who have hardly any confidence in educational institutions believe that there is too little government spending for education.  

- The belief that there is too little, about right, or too much spending for education among those who hardly have any confidence in educational institutions is not associated with mean family income. The mean family income among the different groups are statistically similar.

- Gender proportion among the group too little and about right are statistically the same. Gender is not associated between choosing whether spending for education is too little or about right. 


```r
sessionInfo()
```

```
## R version 3.4.1 (2017-06-30)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7600)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] bindrcpp_0.2.2     statsr_0.0.1       dplyr_0.7.4       
## [4] gplots_3.0.1       ggplot2_2.2.1.9000
## 
## loaded via a namespace (and not attached):
##  [1] progress_1.1.2.9002    gtools_3.5.0           reshape2_1.4.3        
##  [4] rematch2_2.0.1         colorspace_1.3-2       htmltools_0.3.6       
##  [7] yaml_2.1.19            utf8_1.1.3             rlang_0.2.0.9000      
## [10] pillar_1.2.2           later_0.7.2            glue_1.2.0            
## [13] withr_2.1.2            selectr_0.4-1          bindr_0.1.1           
## [16] plyr_1.8.4             stringr_1.3.0          munsell_0.4.3         
## [19] gtable_0.2.0           caTools_1.17.1         evaluate_0.10.1       
## [22] labeling_0.3           knitr_1.20             httpuv_1.4.1          
## [25] ansistrings_1.0.0.9000 Rcpp_0.12.16           KernSmooth_2.23-15    
## [28] xtable_1.8-2           scales_0.5.0.9000      backports_1.1.2       
## [31] promises_1.0.1         gdata_2.18.0           mime_0.5              
## [34] gridExtra_2.3          hms_0.4.2              digest_0.6.15         
## [37] stringi_1.1.7          shiny_1.0.5            grid_3.4.1            
## [40] rprojroot_1.3-2        cli_1.0.0.9001         tools_3.4.1           
## [43] bitops_1.0-6           magrittr_1.5           lazyeval_0.2.1        
## [46] tibble_1.4.2           crayon_1.3.4           pkgconfig_2.0.1       
## [49] xml2_1.2.0             prettyunits_1.0.2      assertthat_0.2.0      
## [52] rmarkdown_1.9.8        rstudioapi_0.7.0-9000  R6_2.2.2              
## [55] compiler_3.4.1
```
