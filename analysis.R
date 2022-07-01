
# load data
d = read.csv("0604newdata.csv")

# load packages
library(tidyverse)
library(summarytools)
library(prettyR)
library(modelsummary)
library(reshape2)
library(sjPlot)
library(hrbrthemes)
library(likert)
library(stargazer)

#age
d = d %>%
  mutate(age = ifelse(Q1>100,2022-Q1,Q1))

d = d %>%
  mutate(agegroup = case_when(age<25&age>17 ~ "Youth",
                              age>24&age<55 ~"Adults",
                              age>54 ~"Seniors"))

# Empathy

d$Q27_1 = as.numeric(d$Q27_1)
d$Q27_2 = as.numeric(d$Q27_2)
d$Q27_3 = as.numeric(d$Q27_3)  

d = d %>%
  mutate(empathy = (Q27_1 + Q27_2 + Q27_3)/3) 

#Empathy on Prevention

d %>%
  lm(Q18_1 ~ empathy,.) %>%
  ggplot(aes(x=empathy, y=Q18_1)) + 
  ggtitle("Empathy on Watch Hands") +
  geom_point(size = 0.05, color = "black", alpha = 0.5) +
  geom_smooth(method = lm, color = "black") 

d =d %>%
  mutate(gender = Q2, 
         education = Q6,
         income = Q7)

## empathy on Wash hands
r1 = lm(Q18_1 ~ empathy,data = d)
r2 = lm(Q18_1 ~ empathy +age +gender + education +income,data = d)


stargazer(r1,r2,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Wash Hands",
          model.names = T,
          single.row = T)

## Empathy on stop shaking hands

r3 = lm(Q18_2 ~ empathy,data = d)
r4 = lm(Q18_2 ~ empathy +age +gender + education +income,data = d)


stargazer(r3,r4,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Stop shaking hands",
          model.names = T,
          single.row = T)

## Empathy on Stop hugging
r5 = lm(Q18_3 ~ empathy,data = d)
r6 = lm(Q18_3 ~ empathy +age +gender + education +income,data = d)


stargazer(r5,r6,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Stop shaking hands",
          model.names = T,
          single.row = T)

## Empathy on Stop touching faces

r7 = lm(Q18_4 ~ empathy,data = d)
r8 = lm(Q18_4 ~ empathy +age +gender + education +income,data = d)


stargazer(r7,r8,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Stop Touching Faces",
          model.names = T,
          single.row = T)

## Empathy on Stay in home
r9 = lm(Q18_5 ~ empathy,data = d)
r10 = lm(Q18_5 ~ empathy +age +gender + education +income,data = d)


stargazer(r9,r10,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Stay in home",
          model.names = T,
          single.row = T)

## See a doctor
r11 = lm(Q18_6 ~ empathy,data = d)
r12 = lm(Q18_6 ~ empathy +age +gender + education +income,data = d)


stargazer(r11,r12,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on See a Doctor",
          model.names = T,
          single.row = T)

##Stay in home well
r13 = lm(Q18_7 ~ empathy,data = d)
r14 = lm(Q18_7 ~ empathy +age +gender + education +income,data = d)


stargazer(r13,r14,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Stay in home well",
          model.names = T,
          single.row = T)

## Cover mouths
r15 = lm(Q18_8 ~ empathy,data = d)
r16 = lm(Q18_8 ~ empathy +age +gender + education +income,data = d)


stargazer(r15,r16,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Cover Mouths",
          model.names = T,
          single.row = T)

## Foods Medicine Reserve
r17 = lm(Q18_9 ~ empathy,data = d)
r18 = lm(Q18_9 ~ empathy +age +gender + education +income,data = d)


stargazer(r17,r18,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Foods Medicine Reserve",
          model.names = T,
          single.row = T)
## cleaning supplies reserves
r19 = lm(Q18_10 ~ empathy,data = d)
r20 = lm(Q18_10 ~ empathy +age +gender + education +income,data = d)


stargazer(r19,r20,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Cleaning Supplies Reserves",
          model.names = T,
          single.row = T)

## Wear Masks
r21 = lm(Q18_11 ~ empathy,data = d)
r22 = lm(Q18_11 ~ empathy +age +gender + education +income,data = d)


stargazer(r21,r22,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Wear Masks",
          model.names = T,
          single.row = T)

## avoid crowded places
r23 = lm(Q18_12 ~ empathy,data = d)
r24 = lm(Q18_12 ~ empathy +age +gender + education +income,data = d)


stargazer(r23,r24,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on avoid crowded places",
          model.names = T,
          single.row = T)

## keep away from other in public
r25 = lm(Q18_13 ~ empathy,data = d)
r26 = lm(Q18_13 ~ empathy +age +gender + education +income,data = d)


stargazer(r25,r26,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on keep away from other in public",
          model.names = T,
          single.row = T)

## cover the toilet board
r27 = lm(Q18_14 ~ empathy,data = d)
r28 = lm(Q18_14 ~ empathy +age +gender + education +income,data = d)


stargazer(r27,r28,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on cover the toilet board",
          model.names = T,
          single.row = T)

## help family members
r29 = lm(Q18_15 ~ empathy,data = d)
r30 = lm(Q18_15 ~ empathy +age +gender + education +income,data = d)


stargazer(r29,r30,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on help family members",
          model.names = T,
          single.row = T)

## care about the elders
r31 = lm(Q18_16 ~ empathy,data = d)
r32 = lm(Q18_16 ~ empathy +age +gender + education +income,data = d)


stargazer(r31,r32,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on care about the elders",
          model.names = T,
          single.row = T)

# Empathy on Overall prevention

d = d %>%
  as_tibble()%>%
  rowwise()%>%
  mutate(mean_prevention = sum(c_across(Q18_1:Q18_16)/16)) 

r33 = lm(mean_prevention ~ empathy,data = d)
r34 = lm(mean_prevention ~ empathy +age +gender + education +income,data = d)


stargazer(r33,r34,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on overall prevention",
          model.names = T,
          single.row = T)

# Resilience and Anxiety
##recode resilience items
a = d %>%
  mutate(re1_po = Q25_1,
         re1_ng = Q25_2,
         re2_po = Q25_3,
         re2_ng = Q25_4,
         re3_po = Q25_5,
         re3_ng = Q25_6) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_at(c("re1_ng","re2_ng","re3_ng") ,
            funs(dplyr::recode(.,
                           "1" = 7,
                           "2" =6,
                          "3" =5,
                           "4"=4,
                          "5"=3,
                          "6"=2,
                          "7"=1))) 
## mean of resilience Q25
a= a %>%
  as_tibble()%>%
  rowwise()%>%
  mutate(mean_resilience = sum(c_across(re1_po:re3_ng)/6))

##Anxiety GADscore Q22
a = a %>%
  rowwise() %>%
  mutate(GAD = sum(c_across(Q22_1:Q22_7)))

a = a %>%
  mutate(gender = Q2, 
           education = Q6,
           income = Q7)

### regression 
r35 = lm(GAD~ mean_resilience, data = a)
r36 = lm(GAD~ mean_resilience+age +gender + education +income, data = a)

stargazer(r35,r36,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Empathy on Anxiety",
          model.names = T,
          single.row = T)

# Support and Anxiety Q22

## Support calculation
a= a %>%
  as_tibble()%>%
  rowwise()%>%
  mutate(mean_support = sum(c_across(Q26_1:Q26_6)/6))
### regression support on anxiety
r37 = lm(GAD ~ mean_support, data = a)
r38 = lm(GAD ~ mean_support+age +gender + education +income, data = a)

stargazer(r37,r38,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Support on Anxiety",
          model.names = T,
          single.row = T)

## support from friends, neigbors and family members cannot release
## anxiety
summary(lm(GAD ~ Q26_3+age +gender + education +income, data = a))
summary(lm(GAD ~ Q26_5+age +gender + education +income, data = a))


# resilience and support
r39 = lm(mean_support ~ mean_resilience, data = a)
r40 = lm(mean_support ~ mean_resilience+age +gender + education +income, data = a)

stargazer(r39,r40,
          type="latex",
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Support on Resilience",
          model.names = T,
          single.row = T)

## sem support resilience and anxiety
install.packages("lavaan")
install.packages("semPlot")
library(lavaan)
library(semPlot)

model = 'GAD ~ mean_support + mean_resilience +age +gender + education +income
         mean_resilience ~ mean_support+age +gender + education +income
         mean_support ~ age +gender + education +income
         '


install.packages("semTable")
library(semTable)


smodel = sem(model, data = a,
             std.lv = TRUE , meanstructure = TRUE )


semTable (smodel, columns = c("est", "estsestars" ), paramSets = c("intercepts","slopes"), 
          fits = c("chisq","rmsea"),
          type = "latex") 

semPaths(smodel,"std",edge.label.cex = 0.8,
         fade = FALSE, layout = "spring",
         optimizeLatRes = FALSE,residuals = FALSE)







