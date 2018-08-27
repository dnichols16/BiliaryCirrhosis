---
title: "Biliary Cirrhosis"
author: "David P. Nichols, MD, MPH"
date: "August 22, 2018"
output: 
  html_document: 
    keep_md: yes
---



This data is derived from the Mayo Clinic 
trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 
and 1984. A description of the clinical background for the trial and the 
covariates recorded here can be found in Dickson, et al., Hepatology 10:1-7 (1989) 
and in Markus, et al., N Eng J of Med 320:1709-13 (1989). 
"A total of 424 PBC patients, referred to Mayo Clinic during that ten-year 
interval, met eligibility criteria for the randomized placebo controlled 
trial of the drug D-penicillamine. The first 312 cases in the data set 
participated in the randomized trial and contain largely complete data." 
Missing data items are denoted by "." <br>

Primary biliary cirrhosis (now more accurately termed primary biliary cholangitis) is a chronic disease in which the bile ducts in the liver are slowly destroyed. When the bile ducts are damaged, bile can back up in the liver and sometimes lead to irreversible scarring of liver tissue (cirrhosis). Primary biliary cholangitis is considered an autoimmune disease, which means your body's immune system is mistakenly attacking healthy cells and tissue. Researchers think a combination of genetic and environmental factors triggers the disease. It usually develops slowly. Medication can slow liver damage, especially if treatment begins early. <br>

We will treat this data as a survival analysis problem. In addition to looking at the treatment's efficacy, we can look at the effectiveness of the various physical signs included as covariates which may help to indicate a fatal outcome for this disease. <br>

I have stored the data on Google Sheets. It can be obtained by:


```r
set.seed(123)
library(tidyverse)
library(survival)
library(tableone)
library(Matching)
library(survminer)
library(survMisc)
library(survcomp)
library(broom)
library(tidyr)
library(flexsurv)
library(GGally)
library(e1071)
library(rms)
library(purrr)
library(stringr)
library(gridExtra)
library(vcd)
library(ggmosaic)
```

```r
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTvuWL7vHEYFv9N0KV1C3ZVXDYhTMOS6n4PCjop8vaeK1IJQFsFTuh4bJYkdoYACZbc1upKeaykwqnK/pub?output=csv"

bcir <- read_csv(url(myurl))
```

```
## Warning: Duplicated column names deduplicated: '1' => '1_1' [4], '1' =>
## '1_2' [6], '1' => '1_3' [7], '1' => '1_4' [8], '1' => '1_5' [9], '1' =>
## '1_6' [10]
```

```
## Parsed with column specification:
## cols(
##   .default = col_integer(),
##   `1_6` = col_double(),
##   `14.5` = col_double(),
##   `261` = col_character(),
##   `2.6` = col_double(),
##   `156` = col_character(),
##   `1718` = col_double(),
##   `137.95` = col_double(),
##   `172` = col_character(),
##   `190` = col_character(),
##   `12.2` = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

```r
colnames(bcir) <- c("id", "time", "event", "rx", "age",
                    "sex","ascites", "hepmeg", "spiders",
                    "edema", "bili", "chol", "alb", "cu",
                    "alkphos", "sgot", "trig", "plat", "ptt",
                    "stage")
glimpse(bcir)
```

```
## Observations: 311
## Variables: 20
## $ id      <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17...
## $ time    <int> 4500, 1012, 1925, 1504, 2503, 1832, 2466, 2400, 51, 37...
## $ event   <int> 0, 2, 2, 1, 2, 0, 2, 2, 2, 2, 2, 0, 2, 2, 0, 2, 2, 0, ...
## $ rx      <int> 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, ...
## $ age     <int> 20617, 25594, 19994, 13918, 24201, 20284, 19379, 15526...
## $ sex     <int> 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, ...
## $ ascites <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, ...
## $ hepmeg  <int> 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, ...
## $ spiders <int> 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, ...
## $ edema   <dbl> 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,...
## $ bili    <dbl> 1.1, 1.4, 1.8, 3.4, 0.8, 1.0, 0.3, 3.2, 12.6, 1.4, 3.6...
## $ chol    <chr> "302", "176", "244", "279", "248", "322", "280", "562"...
## $ alb     <dbl> 4.14, 3.48, 2.54, 3.53, 3.98, 4.09, 4.00, 3.08, 2.74, ...
## $ cu      <chr> "54", "210", "64", "143", "50", "52", "52", "79", "140...
## $ alkphos <dbl> 7394.8, 516.0, 6121.8, 671.0, 944.0, 824.0, 4651.2, 22...
## $ sgot    <dbl> 113.52, 96.10, 60.63, 113.15, 93.00, 60.45, 28.38, 144...
## $ trig    <chr> "88", "55", "92", "72", "63", "213", "189", "88", "143...
## $ plat    <chr> "221", "151", "183", "136", ".", "204", "373", "251", ...
## $ ptt     <dbl> 10.6, 12.0, 10.3, 10.9, 11.0, 9.7, 11.0, 11.0, 11.5, 1...
## $ stage   <int> 3, 4, 4, 3, 3, 3, 3, 2, 4, 4, 4, 3, 4, 3, 3, 4, 4, 3, ...
```

Change the treatment to 0's and 1's

```r
bcir$rx  <- bcir$rx -1
```

Include patients who had liver transplants in the censored group

```r
   #original study event was 0 = censored, 1 = transplant, 2 = death
   #will count censored and transplant as censored
bcir <- bcir %>% mutate(event = if_else(event == 0 | event == 1, 0, 1))
bcir <- bcir %>% mutate(rx = as.integer(rx))
bcir <- bcir %>% mutate(age_yr = round(age/365))
```

Extract the numeric variables to work on them separately

```r
   #extract numerics
nums <- bcir %>% dplyr::select(bili, chol, alb, cu, alkphos, sgot,
                               trig, plat, ptt, age_yr) 
```

Change the missing data "." to NA so it can be imputed

```r
   #changes "." to NA by coercion
nums <- nums %>% map_at(c("chol", "cu", "trig", "plat"), as.numeric)
```

A data frame is more manageable than a list. 

```r
   #change list to data frame
nums <- do.call(bind_cols, nums)
```

Inspect the distributions of the numeric variables

```r
   #visualize(numerics)
p1 <- ggplot(nums) +
  geom_density(aes(x = bili)) 
p2 <- ggplot(nums) +
  geom_density(aes(x = chol)) 
p3 <- ggplot(nums) +
  geom_density(aes(x = alb))
p4 <- ggplot(nums) +
  geom_density(aes(x = cu)) 
p5 <- ggplot(nums) +
  geom_density(aes(x = alkphos)) 
p6 <- ggplot(nums) +
  geom_density(aes(x = sgot)) 
p7 <- ggplot(nums) +
  geom_density(aes(x = trig)) 
p8 <- ggplot(nums) +
  geom_density(aes(x = plat))
p9 <- ggplot(nums) +
  geom_density(aes(x = ptt)) 
p10 <- ggplot(nums) +
  geom_density(aes(x = age_yr)) 
gridExtra::grid.arrange(p1, p2, p3, p4, p5,
                        p6, p7, p8, p9, p10,
                        nrow = 5)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Taking logs will improve the distributions (more normal appearing) for analysis

```r
   #bili chol alkphos cu have large right skews, get logs
nums <- nums %>% mutate(lbili = log(bili))
nums <- nums %>% mutate(lchol = log(chol))
nums <- nums %>% mutate(lalkphos = log(alkphos))
nums <- nums %>% mutate(lcu = log(cu))
nums <- nums %>% mutate(lsgot = log(sgot))
nums <- nums %>% mutate(lptt = log(ptt))
nums <- nums %>% mutate(ltrig = log(trig))
   #discard nums which have been logged
nums1 <- nums %>% dplyr::select(lbili, lchol, lalkphos, lcu, lsgot,
                         lptt, ltrig, alb, plat, age_yr)
```

I will deal with missing data by random imputation, Please see my previous blog posts for an explanation of this method: [Random Imputation]("https://epi2020datascience.blogspot.com/). We will need to obtain the means and standard deviations of the variable that we will impute.

```r
   #Random impute the NA's
   #first confirm that we have NA's
nums1 %>% summarize_all(mean, na.rm = FALSE)
```

```
## # A tibble: 1 x 10
##   lbili lchol lalkphos   lcu lsgot  lptt ltrig   alb  plat age_yr
##   <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 0.569    NA     7.27    NA  4.71  2.37    NA  3.52    NA   50.0
```

```r
   #then get the means of each variable
means <- nums1 %>% summarize_all(mean, na.rm = TRUE)
means
```

```
## # A tibble: 1 x 10
##   lbili lchol lalkphos   lcu lsgot  lptt ltrig   alb  plat age_yr
##   <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 0.569  5.80     7.27  4.26  4.71  2.37  4.72  3.52  262.   50.0
```

```r
   #then get the standard deviations of each variable
sds <- nums1 %>% summarize_all(sd, na.rm = TRUE)
sds
```

```
## # A tibble: 1 x 10
##   lbili lchol lalkphos   lcu lsgot   lptt ltrig   alb  plat age_yr
##   <dbl> <dbl>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1  1.03 0.437    0.723 0.825 0.449 0.0881 0.447 0.417  95.7   10.6
```

Using random imputation, impute the variables with NA's: lchol, lcu, ltrig, plat

```r
   #random impute the NA's
   #lchol
nums1$lchol <- if_else(is.na(nums1$lchol),
                      rnorm(1, 5.80, 0.437),
                      nums1$lchol)
   #lcu
nums1$lcu <- if_else(is.na(nums1$lcu),
                      rnorm(1, 4.26, 0.825),
                      nums1$lcu)
   #ltrig
nums1$ltrig <- if_else(is.na(nums1$ltrig),
                      rnorm(1, 4.72, 0.447),
                      nums1$ltrig)
   #plat
nums1$plat <- if_else(is.na(nums1$plat),
                      rnorm(1, 262, 95.6),
                      nums1$plat)
   #now summarize again to check for any NA's left
nums1 %>% summarize_all(mean, na.rm = FALSE)
```

```
## # A tibble: 1 x 10
##   lbili lchol lalkphos   lcu lsgot  lptt ltrig   alb  plat age_yr
##   <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 0.569  5.78     7.27  4.26  4.71  2.37  4.79  3.52  262.   50.0
```

Prepare a pairs plot, looking for multicollinarity problems:

```r
#pairs plot
GGally::ggpairs(nums1,
        lower = list(continuous = wrap("points",
        alpha = 0.2,
        color = "blue",
        size = .3)))
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
#plot variables
   #connect the data frames back up
   #first break off the non_numerics from bcir
bcir_non_numeric <- bcir %>% dplyr::select(id, time, event, rx, sex, ascites,
                                    hepmeg, spiders, edema, stage)
   #fix edema to get rid of decimal .5 value
bcir_non_numeric <- bcir_non_numeric %>% 
  mutate(edema1 = case_when(
    edema == 0 ~ 0,
    edema == 0.5 ~ 1,
    edema == 1 ~ 2,
    TRUE ~ 999))
bcir_non_numeric <- bcir_non_numeric %>%
  dplyr::select(-edema) %>% 
  rename(edema = "edema1") %>% 
  mutate(edema = as.integer(bcir_non_numeric$edema))
   #connect numeric and non-numeric
bcir1 <- bcir_non_numeric %>% bind_cols(nums1)
   #check for NA's
sum(is.na(bcir1))
```

```
## [1] 0
```

Next, we will look for evidence of selection bias in this study. Might the treatment have been assigned based on the patient's physical signs rather than on a randomized schedule?

```r
   #Could treament be assigned based on presence of symptoms?
   #break off categoricals as a matrix
cats <- bcir1 %>% dplyr::select(rx, sex, ascites,
                                hepmeg, spiders, edema, stage)
cats1 <- cats %>% map(as.numeric)  
#cats3 <- data.frame(cats3)
#cats3 <- as.tibble(cats3)
tab1 <- xtabs(~ cats$rx + cats$sex)
tab2 <- xtabs(~ cats$rx + cats$ascites)
tab3 <- xtabs(~ cats$rx + cats$hepmeg)
tab4 <- xtabs(~ cats$rx + cats$spiders)
tab5 <- xtabs(~ cats$rx + cats$edema)
tab6 <- xtabs(~ cats$rx + cats$stage)
p1 <- vcd::mosaic(tab1)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
p2 <- vcd::mosaic(tab2)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
p3 <- vcd::mosaic(tab3)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

```r
p4 <- vcd::mosaic(tab4)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-4.png)<!-- -->

```r
p5 <- vcd::mosaic(tab5)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-5.png)<!-- -->

```r
p6 <- vcd::mosaic(tab6)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-14-6.png)<!-- -->

The mosaic plots show that the treatment and physical signs are evenly matched up. The areas of the rectangles are about the same for all the variables when split into treated and non-treated groups.


```r
   #Could treament have been assigned based on presence of abnormal labs?
p1 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lbili), fill = "#6588a7") +
  labs(x = "rx")
p2 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lchol), fill = "#6588a7") +
  labs(x = "rx")
p3 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lalkphos), fill = "#6588a7") +
  labs(x = "rx")
p4 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lcu), fill = "#6588a7") +
  labs(x = "rx")
p5 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lsgot), fill = "#6588a7") +
  labs(x = "rx")
p6 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lptt), fill = "#6588a7") +
  labs(x = "rx")
p7 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = ltrig), fill = "#6588a7") +
  labs(x = "rx")
p8 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = alb), fill = "#6588a7") +
  labs(x = "rx")
p9 <-ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = plat), fill = "#6588a7") +
  labs(x = "rx")
grid.arrange(p1, p2, p3, p4, p5, p6, p6, p7, p8, p9,
             ncol = 4)
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

It looks as though the labs are evenly divided between treatment and non-treatment groups. So far there is no evidence of selection bias. <br>

For a final check on selection bias we can see if there could be any improvement on the matching of treatment and non-treatment groups. The first thing is to create a vector of propensity scores by using a glm model using our data with treatment as the dependent variable.


```r
psmodel <- glm(rx ~ sex + ascites + hepmeg + spiders + edema + 
                 lbili + lchol + lalkphos + lcu + lsgot + lptt +
                 ltrig + alb + plat + age_yr + stage,
               family = "binomial",
               data = bcir1) 
pscore <- psmodel$fitted.values  #propensity scores
bcir1$pscore <- pscore
ps <- bcir1 %>% dplyr::select(rx, pscore)
table(ps$rx)
```

```
## 
##   0   1 
## 157 154
```

Next, we'll make a comparison table showing the covariates in the two treatment groups. We want the standardized differences (SMD's) to be less than 0.1 for the best match.


```r
xvars <- c( "sex", "ascites", "hepmeg", "spiders", "edema",
            "lbili", "lchol", "lalkphos", "lcu", "lsgot",
           "lptt", "ltrig", "alb", "plat", "age_yr", "stage")
unmatchedtab1 <- tableone::CreateTableOne(vars = xvars,
                              strata = "rx",
                              data = bcir1,
                              test = FALSE)
print(unmatchedtab1, smd = TRUE)
```

```
##                       Stratified by rx
##                        0              1              SMD   
##   n                       157            154               
##   sex (mean (sd))        0.87 (0.34)    0.90 (0.30)   0.114
##   ascites (mean (sd))    0.08 (0.28)    0.06 (0.25)   0.068
##   hepmeg (mean (sd))     0.46 (0.50)    0.56 (0.50)   0.213
##   spiders (mean (sd))    0.28 (0.45)    0.29 (0.46)   0.026
##   edema (mean (sd))      0.06 (0.23)    0.06 (0.25)   0.032
##   lbili (mean (sd))      0.52 (0.95)    0.61 (1.10)   0.088
##   lchol (mean (sd))      5.78 (0.39)    5.78 (0.45)   0.005
##   lalkphos (mean (sd))   7.28 (0.74)    7.27 (0.71)   0.018
##   lcu (mean (sd))        4.24 (0.82)    4.27 (0.83)   0.030
##   lsgot (mean (sd))      4.69 (0.45)    4.73 (0.45)   0.086
##   lptt (mean (sd))       2.36 (0.08)    2.37 (0.10)   0.143
##   ltrig (mean (sd))      4.79 (0.49)    4.78 (0.45)   0.009
##   alb (mean (sd))        3.52 (0.44)    3.52 (0.40)   0.004
##   plat (mean (sd))     259.32 (99.86) 265.25 (90.14)  0.062
##   age_yr (mean (sd))    51.43 (11.03)  48.61 (9.95)   0.268
##   stage (mean (sd))      2.97 (0.94)    3.09 (0.81)   0.140
```

For the most part, this is a pretty good match. Let's see if we can improve the match by matching on the propensity scores.

```r
psmatch <- Matching::Match(Tr = bcir1$rx,
                 M = 1, 
                 X = pscore,
                 replace = FALSE)
matched <- bcir1[unlist(psmatch[c("index.treated",
                                 "index.control")]),]
matchedtab1 <- tableone::CreateTableOne(vars = xvars,
                              strata = "rx",
                              data = matched,
                              test = FALSE)
print(matchedtab1, smd = TRUE)
```

```
##                       Stratified by rx
##                        0              1              SMD   
##   n                       154            154               
##   sex (mean (sd))        0.88 (0.33)    0.90 (0.30)   0.083
##   ascites (mean (sd))    0.08 (0.27)    0.06 (0.25)   0.050
##   hepmeg (mean (sd))     0.46 (0.50)    0.56 (0.50)   0.208
##   spiders (mean (sd))    0.27 (0.45)    0.29 (0.46)   0.043
##   edema (mean (sd))      0.06 (0.24)    0.06 (0.25)   0.027
##   lbili (mean (sd))      0.51 (0.95)    0.61 (1.10)   0.102
##   lchol (mean (sd))      5.77 (0.40)    5.78 (0.45)   0.009
##   lalkphos (mean (sd))   7.26 (0.73)    7.27 (0.71)   0.002
##   lcu (mean (sd))        4.23 (0.81)    4.27 (0.83)   0.050
##   lsgot (mean (sd))      4.69 (0.45)    4.73 (0.45)   0.096
##   lptt (mean (sd))       2.36 (0.08)    2.37 (0.10)   0.149
##   ltrig (mean (sd))      4.78 (0.49)    4.78 (0.45)   0.002
##   alb (mean (sd))        3.52 (0.44)    3.52 (0.40)   0.002
##   plat (mean (sd))     260.87 (99.86) 265.25 (90.14)  0.046
##   age_yr (mean (sd))    51.08 (10.81)  48.61 (9.95)   0.238
##   stage (mean (sd))      2.97 (0.94)    3.09 (0.81)   0.141
```

We can compare the list of SMD's to see if there has been any beneit from matching.

```r
getsmd <- function(df) { 
  options(digits = 3)
  print("SMD")
  x <- tableone::ExtractSmd(df)
  print(x)
  paste("sum of SMD's = ", round(sum(x[1:length(x)]), 3))
}
getsmd(matchedtab1)
```

```
## [1] "SMD"
##           1 vs 2
## sex      0.08269
## ascites  0.05028
## hepmeg   0.20831
## spiders  0.04314
## edema    0.02690
## lbili    0.10216
## lchol    0.00888
## lalkphos 0.00206
## lcu      0.05045
## lsgot    0.09636
## lptt     0.14908
## ltrig    0.00191
## alb      0.00171
## plat     0.04601
## age_yr   0.23758
## stage    0.14062
```

```
## [1] "sum of SMD's =  1.248"
```

```r
getsmd(unmatchedtab1)
```

```
## [1] "SMD"
##           1 vs 2
## sex      0.11353
## ascites  0.06813
## hepmeg   0.21326
## spiders  0.02636
## edema    0.03167
## lbili    0.08757
## lchol    0.00502
## lalkphos 0.01759
## lcu      0.03004
## lsgot    0.08580
## lptt     0.14295
## ltrig    0.00891
## alb      0.00414
## plat     0.06239
## age_yr   0.26821
## stage    0.14010
```

```
## [1] "sum of SMD's =  1.306"
```

There really isn't much difference between the unmatched and matched groups so it looks as if the reseachers did the best possible job of matching through randomization of treatment. <br>




```r
#KM Curve
m1<- survfit(Surv(time, event) ~ rx, data = bcir1)
m2<- survfit(Surv(time, event) ~ sex, data = bcir1)
m3<- survfit(Surv(time, event) ~ stage, data = bcir1)
m4<- survfit(Surv(time, event) ~ ascites, data = bcir1)
m5<- survfit(Surv(time, event) ~ hepmeg, data = bcir1)
m6<- survfit(Surv(time, event) ~ spiders, data = bcir1)
m7<- survfit(Surv(time, event) ~ edema, data = bcir1)
ggsurvplot(m1, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
ggsurvplot(m2, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
ggsurvplot(m3, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-3.png)<!-- -->

```r
ggsurvplot(m4, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-4.png)<!-- -->

```r
ggsurvplot(m5, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-5.png)<!-- -->

```r
ggsurvplot(m6, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-6.png)<!-- -->

```r
ggsurvplot(m7, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
```

![](BiliaryCirrhosis_files/figure-html/unnamed-chunk-20-7.png)<!-- -->





