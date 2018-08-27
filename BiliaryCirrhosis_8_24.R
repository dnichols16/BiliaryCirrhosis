library(tidyverse)
library(survival)
library(tableone)
library(Matching)
library(survminer)
library(car)
library(survMisc)
library(survcomp)
library(eha)
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
library(googlesheets)
library(ggmosaic)

#load data
bcir <- read_table2("BiliaryCirrhosis.txt", col_names = FALSE)

#cleaning
colnames(bcir) <- c("id", "time", "event", "rx", "age",
                    "sex","ascites", "hepmeg", "spiders",
                    "edema", "bili", "chol", "alb", "cu",
                    "alkphos", "sgot", "trig", "plat", "ptt",
                    "stage")
bcir <- bcir[, -length(bcir)]
bcir$rx  <- bcir$rx -1
   #original study event was 0 = censored, 1 = transplant, 2 = death
   #will count censored and transplant as censored
bcir <- bcir %>% mutate(event = if_else(event == 0 | event == 1, 0, 1))
glimpse(bcir)
bcir <- bcir %>% mutate(rx = as.integer(rx))
bcir <- bcir %>% mutate(age_yr = round(age/365))
   #extract numerics
nums <- bcir %>% dplyr::select(bili, chol, alb, cu, alkphos, sgot,
                               trig, plat, ptt, age_yr) 
  
summary(nums)
glimpse(nums)
   #changes "." to NA by coercion
nums <- nums %>% map_at(c("chol", "cu", "trig", "plat"), as.numeric)
   #change list to data frame
nums <- do.call(bind_cols, nums)
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
   #Random impute the NA's
   #first find which variables have the NAS's
nums1 %>% summarize_all(mean, na.rm = FALSE)

   #then get the means of each variable
means <- nums1 %>% summarize_all(mean, na.rm = TRUE)
means
   #then get the standard deviations of each variable
sds <- nums1 %>% summarize_all(sd, na.rm = TRUE)
sds
   #random impute the NA's
   #lchol
nums1$lchol <- ifelse(is.na(nums1$lchol),
                      rnorm(1, 5.80, 0.437),
                      nums1$lchol)
   #lcu
nums1$lcu <- ifelse(is.na(nums1$lcu),
                      rnorm(1, 4.26, 0.825),
                      nums1$lcu)
   #ltrig
nums1$ltrig <- ifelse(is.na(nums1$ltrig),
                      rnorm(1, 4.72, 0.447),
                      nums1$ltrig)
   #plat
nums1$plat <- ifelse(is.na(nums1$plat),
                      rnorm(1, 262, 95.6),
                      nums1$plat)
   #now summarize again to check for any NA's left
nums1 %>% summarize_all(mean, na.rm = FALSE)
#*********************************************************
#pairs plot
ggpairs(nums1,
        lower = list(continuous = wrap("points",
        alpha = 0.2,
        color = "blue",
        size = .3)))
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
summary(bcir1)
   #Could treament be assigned based on presence of symptoms?
   #break off categoricals as a matrix
cats <- bcir1 %>% dplyr::select(rx, sex, ascites,
                                hepmeg, spiders, edema, stage)
#tab1 <- xtabs(~ cats$rx + cats$sex)
#tab2 <- xtabs(~ cats$rx + cats$ascites)
#tab3 <- xtabs(~ cats$rx + cats$hepmeg)
#tab4 <- xtabs(~ cats$rx + cats$spiders)
#tab5 <- xtabs(~ cats$rx + cats$edema)
#tab6 <- xtabs(~ cats$rx + cats$stage)
#p1 <- mosaic(tab1)
#p2 <- mosaic(tab2)
#p3 <- mosaic(tab3)
#p4 <- mosaic(tab4)
#p5 <- mosaic(tab5)
#p6 <- mosaic(tab6)
#function to create mosaic plots as ggplot objects
makeplot_mosaic <- function(data, x, y, ...){
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
  
  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  
  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable));
  
  ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_rect(color="black", aes_string(fill=yvar)) +
    xlab(paste(xvar, "(count)")) + ylab(paste(yvar, "(proportion)"));
}

cats3 <- cats1 %>% map(factor)  
cats3 <- data.frame(cats3)
cats3 <- as.tibble(cats3)
glimpse(cats3)

#Could treament have been assigned based on presence of abnormal labs?
p1 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lbili), fill = "#6588a7")
p2 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lchol))
p3 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lalkphos))
p4 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lcu))
p5 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lsgot))
p6 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = lptt))
p7 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = ltrig))
p8 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = alb))
p9 <- ggplot(bcir1) +
  geom_boxplot(aes(x = factor(rx), y = plat))
grid.arrange(p1, p2, p3, p4, p5, p6, p6, p7, p8, p9,
             ncol = 4)
#propensity matching
psmodel <- glm(rx ~ sex + ascites + hepmeg + spiders + edema + 
                 lbili + lchol + lalkphos + lcu + lsgot + lptt +
                 ltrig + alb + plat + age_yr + stage,
               family = "binomial",
               data = bcir1) 
pscore <- psmodel$fitted.values 
bcir1$pscore <- pscore
ps <- bcir1 %>% dplyr::select(rx, pscore)
table(ps$rx)
xvars <- c( "sex", "ascites", "hepmeg", "spiders", "edema",
            "lbili", "lchol", "lalkphos", "lcu", "lsgot",
           "lptt", "ltrig", "alb", "plat", "age_yr", "stage")
unmatchedtab1 <- CreateTableOne(vars = xvars,
                              strata = "rx",
                              data = bcir1,
                              test = FALSE)
print(unmatchedtab1, smd = TRUE)

psmatch <- Matching::Match(Tr = bcir1$rx,
                 M = 1, 
                 X = pscore,
                 replace = FALSE)
matched <- bcir1[unlist(psmatch[c("index.treated",
                                 "index.control")]),]
matchedtab1 <- CreateTableOne(vars = xvars,
                              strata = "rx",
                              data = matched,
                              test = FALSE)
print(matchedtab1, smd = TRUE)

getsmd <- function(df) { 
  options(digits = 3)
  print("SMD")
  x <- tableone::ExtractSmd(df)
  print(x)
  paste("sum of SMD's = ", round(sum(x[1:length(x)]), 3))
}
getsmd(matchedtab1)

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
ggsurvplot(m2, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
ggsurvplot(m3, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
ggsurvplot(m4, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
ggsurvplot(m5, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
ggsurvplot(m6, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
ggsurvplot(m7, data = bcir1,
           surv.median.line = "hv",
           conf.int = TRUE) # Add medians survival
survdiff(Surv(time, event) ~ rx, data = bcir1)
survdiff(Surv(time, event) ~ sex, data = bcir1)
survdiff(Surv(time, event) ~ ascites, data = bcir1)
survdiff(Surv(time, event) ~ hepmeg, data = bcir1)
survdiff(Surv(time, event) ~ spiders, data = bcir1)
survdiff(Surv(time, event) ~ edema, data = bcir1)
survdiff(Surv(time, event) ~ stage, data = bcir1)

#Categorize the numerics for K-M estimator
   #bili - not the log value
numcat_km <- function(var) {
  var_cut <- cut(var,
                 quantile(var, c( 0.00, 0.50, 1.00),
                          na.rm = TRUE))
  bcir1$var_cut <- var_cut
  m8<- survfit(Surv(time, event) ~ var_cut, data = bcir1)
  ggsurvplot(m8, data = bcir1,
             surv.median.line = "hv",
             conf.int = TRUE)
}
numcat_diff <- function(var) {
  var_cut <- cut(var,
                 quantile(var, c( 0.00, 0.50, 1.00),
                          na.rm = TRUE))
  bcir1$var_cut <- var_cut
  survdiff(Surv(time, event) ~ var_cut, data = bcir1)
}
   #bili
print("Bilirubin")
numcat_km(nums$bili)
numcat_diff(nums$bili)
   #chol
print("Cholesterol")
numcat_km(nums$chol)
numcat_diff(nums$chol)
#alb
print("Albumin")
numcat_km(nums$alb)
numcat_diff(nums$alb)
#cu
print("Copper")
numcat_km(nums$cu)
numcat_diff(nums$cu)
#alkphos
print("Alkaline Phosphatase")
numcat_km(nums$alkphos)
numcat_diff(nums$alkphos)
#sgot
print("SGOT")
numcat_km(nums$sgot)
numcat_diff(nums$sgot)
#trig
print("Triglycerides")
numcat_km(nums$trig)
numcat_diff(nums$trig)
#chol
print("Platelets")
numcat_km(nums$plat)
numcat_diff(nums$plat)
#ptt
print("PTT")
numcat_km(nums$ptt)
numcat_diff(nums$ptt)

#ph model
m2 <- coxph(Surv(time, event) ~ rx + sex + ascites +
            hepmeg + spiders + edema +
            lbili + lchol + lalkphos + lcu + lsgot +
            lptt + ltrig + alb + plat + age_yr + stage, 
            data = bcir1)
m2
#parametric model


#prediction



