
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
library(lavaan)
library(kableExtra)


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
  mutate(age = ifelse(Q1>100,2022-Q1,Q1))
a = a %>%
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

# Demographic variables and Anxiety Q22 
## age and anxiety
r41 = lm(GAD ~ age, data = a)
r42 = lm(GAD ~ gender, data = a)
r43 = lm(GAD ~ education, data = a)
r44 = lm(GAD ~ income, data = a)
r45 = lm(GAD ~ age +gender + education +income, data = a)

stargazer(r41,r42,r43,r44,r45,
          type="latex",
          font.size = "small",
          align = TRUE,
          intercept.bottom = F,
          intercept.top = T,
          ci = F, digits=2,
          notes = "Demographics on Anxiety",
          column.sep.width = "-25pt", 
          model.names = T,
          single.row = T)

# Mediation of media use

a = a %>%
  mutate(Use_phone = Q15_1,
         Socialmedia_chat = Q15_2,
         Socialmedia_post = Q15_3,
         Socialmedia_scan = Q15_4,
         Socialmedia_news = Q15_5,
         Online_search = Q15_6,
         Email = Q15_7,
         Media_share = Q15_8,
        Short_message = Q15_9,
        Digital_game = Q15_10,
        Whatsapp_wechat_line = Q15_11,
        Skype_FT_zoom = Q15_12,
        Online_friends = Q15_13,
        Phone_call = Q15_14,
        Watch_TV = Q15_5,
        Radio = Q15_6,
        Newspaper = Q15_17)

#Mediation of Use-phone

usephone.mod = '
  GAD ~ a1* Use_phone +b1 *age + b2*gender +b3*education + b4* income
  Use_phone~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
usephone.model = sem(usephone.mod, data = a,
                    std.lv = TRUE , meanstructure = TRUE )

parameterestimates(usephone.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

##Mediation of Social media Chat
smc.mod = '
  GAD ~ a1* Socialmedia_chat +b1 *age + b2*gender +b3*education + b4* income
  Socialmedia_chat~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
smc.model = sem(smc.mod, data = a,
                     std.lv = TRUE , meanstructure = TRUE )

parameterestimates(smc.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 


##Mediation of Social media Post
smp.mod = '
  GAD ~ a1* Socialmedia_post +b1 *age + b2*gender +b3*education + b4* income
  Socialmedia_post~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
smp.model = sem(smp.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(smp.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Social media News
smn.mod = '
  GAD ~ a1* Socialmedia_news +b1 *age + b2*gender +b3*education + b4* income
  Socialmedia_news~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
smn.model = sem(smn.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(smn.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Online Search
os.mod = '
  GAD ~ a1* Online_search +b1 *age + b2*gender +b3*education + b4* income
  Online_search~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
os.model = sem(os.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(os.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 


## Mediation of Email
eml.mod = '
  GAD ~ a1* Email +b1 *age + b2*gender +b3*education + b4* income
  Email~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
eml.model = sem(eml.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(eml.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 


## Mediation of Mediashare
ms.mod = '
  GAD ~ a1* Media_share +b1 *age + b2*gender +b3*education + b4* income
  Media_share~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
ms.model = sem(ms.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(ms.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 



## Mediation of Short message
smg.mod = '
  GAD ~ a1* Short_message +b1 *age + b2*gender +b3*education + b4* income
  Short_message~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
smg.model = sem(smg.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(smg.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Digital Game
dg.mod = '
  GAD ~ a1* Digital_game +b1 *age + b2*gender +b3*education + b4* income
  Digital_game~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
dg.model = sem(dg.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(dg.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Whatsapp
wapp.mod = '
  GAD ~ a1* Whatsapp_wechat_line +b1 *age + b2*gender +b3*education + b4* income
  Whatsapp_wechat_line~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
wapp.model = sem(wapp.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(wapp.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Skype
sky.mod = '
  GAD ~ a1* Skype_FT_zoom +b1 *age + b2*gender +b3*education + b4* income
  Skype_FT_zoom~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
sky.model = sem(sky.mod, data = a,
                 std.lv = TRUE , meanstructure = TRUE )

parameterestimates(sky.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Online friends
of.mod = '
  GAD ~ a1* Online_friends +b1 *age + b2*gender +b3*education + b4* income
  Online_friends ~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
of.model = sem(of.mod, data = a,
                std.lv = TRUE , meanstructure = TRUE )

parameterestimates(of.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 



## Mediation of TV
TV.mod = '
  GAD ~ a1* Watch_TV +b1 *age + b2*gender +b3*education + b4* income
  Watch_TV ~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
TV.model = sem(TV.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(TV.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Radio
Rd.mod = '
  GAD ~ a1* Radio +b1 *age + b2*gender +b3*education + b4* income
  Radio~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
Rd.model = sem(Rd.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(Rd.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## Mediation of Newspaper
Npr.mod = '
  GAD ~ a1* Newspaper +b1 *age + b2*gender +b3*education + b4* income
  Newspaper~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 := a1*c3
  Indirect4 := a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
Npr.model = sem(Npr.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )

parameterestimates(Npr.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 


# Mediation of source assessment

a = a %>%
  mutate(certainty = Q17_1,
         overload = Q17_2,
         anxiety = Q17_3,
         confusion = Q17_4)

## mediation effects analysis
library(mediation)

#M:certainty age
certainty.mediator = lm(certainty ~ age,a)
certainty.dv = lm(GAD ~ age+ certainty,a)
m1 = mediate(certainty.mediator, certainty.dv, treat='age', mediator='certainty', boot=T)
summary(m1)

## all source assessment 
source.mod = '
  GAD ~ a1* certainty +a2* overload + a3* anxiety + a4* confusion +b1 *age + b2*gender +b3*education + b4* income
  certainty ~ c1 *age + c2*gender +c3*education + c4* income
  overload ~ d1 *age + d2*gender +d3*education + d4* income
  anxiety ~  e1 *age + e2*gender +e3*education + e4* income
  confusion ~  f1 *age + f2*gender +f3*education + f4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 :=a1*c3
  Indirect4 :=a1*c4
  Indirect5 := a2*d1
  Indirect6 := a2*d2
  Indirect7 :=a2*d3
  Indirect8 :=a2*d4
  Indirect9 := a3*e1
  Indirect10 := a3*e2
  Indirect11 :=a3*e3
  Indirect12 :=a3*e4
  Indirect13 := a4*f1
  Indirect14:= a4*f2
  Indirect15 :=a4*f3
  Indirect16 :=a4*f4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'

## sem all model
library(lavaan)
sa.model = sem(source.mod, data = a,
             std.lv = TRUE , meanstructure = TRUE )
library(semTable)
library(semPlot)
semTable (sa.model, columns = c("est", "estsestars" ), paramSets = c("intercepts","slopes"), 
          fits = c("chisq","rmsea"),
          type = "latex") 

summary(sa.model, standardized = TRUE)
library(kableExtra)
parameterestimates(sa.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

semPaths(sa.model,"std","est",
         style = "lisrel",
         residScale = 8,
         theme = "colorblind",
         nCharNodes = 0,
         rotation = 2,
         layout = "tree3")

#Mediation of support
support.mod = '
  GAD ~ a1* mean_support +b1 *age + b2*gender +b3*education + b4* income
  mean_support~ c1 *age + c2*gender +c3*education + c4* income
  Indirect := a1*c1
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'
support.model = sem(support.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )
library(kableExtra)

parameterestimates(support.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

# mediation of resilience
resilience.mod = '
  GAD ~ a1* mean_resilience +b1 *age + b2*gender +b3*education + b4* income
  mean_resilience~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 :=a1*c3
  Indirect4 :=a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'

resilience.model = sem(resilience.mod, data = a,
               std.lv = TRUE , meanstructure = TRUE )
parameterestimates(resilience.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## mediation of empathy
a$Q27_1 = as.numeric(d$Q27_1)
a$Q27_2 = as.numeric(d$Q27_2)
a$Q27_3 = as.numeric(d$Q27_3)  

a = a %>%
  mutate(empathy = (Q27_1 + Q27_2 + Q27_3)/3) 

empathy.mod = '
  GAD ~ a1* empathy +b1 *age + b2*gender +b3*education + b4* income
  empathy~ c1 *age + c2*gender +c3*education + c4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 :=a1*c3
  Indirect4 :=a1*c4
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'

resilience.model = sem(empathy.mod, data = a,
                       std.lv = TRUE , meanstructure = TRUE )
parameterestimates(resilience.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 

## mediation of support, resilience and empathy in one model

catchall.mod = '
  GAD ~ a1* mean_support +a2* mean_resilience + a3 * empathy +b1 *age + b2*gender +b3*education + b4* income
  mean_support~ c1 *age + c2*gender +c3*education + c4* income
  mean_resilience~ d1 *age + d2*gender +d3*education + d4* income
  empathy~ e1 *age + e2*gender +e3*education + e4* income
  Indirect1 := a1*c1
  Indirect2 := a1*c2
  Indirect3 :=a1*c3
  Indirect4 :=a1*c4
  Indirect5 := a2*d1
  Indirect6 := a2*d2
  Indirect7 :=a2*d3
  Indirect8 :=a2*d4
  Indirect9 := a3*e1
  Indirect10 := a3*e2
  Indirect11 :=a3*e3
  Indirect12 :=a3*e4
  mean_support ~~ mean_resilience
  mean_resilience ~~ empathy
  mean_support ~~ empathy
  age ~~ gender
  gender ~~ education
  education ~~ income
  age ~~ education
  age ~~ income
  gender ~~ income'

catchall.model = sem(catchall.mod, data = a,
                       std.lv = TRUE , meanstructure = TRUE )
parameterestimates(catchall.model, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kbl(.,booktab = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) 








