
#### Andersen Replication and extension
library(nnet)
library(ggeffects)
ces %>% 
  filter(election!=2000&quebec==0&election!=2021) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))->roc_models_2019

#Start with where the predicted values are stored

# roc_models_2019 %>% 
#   unnest_wider(predicted) %>%
#   unnest(occupation2) %>%
#   filter(response.level!="Green") %>% 
#   ggplot(., aes(x=election, y=predicted, group=x, col=x))+geom_line()+facet_grid(~response.level)+labs(title="Class Voting In ROC")+theme(axis.text.x=element_text(angle=90))

#ggsave(here("Plots", "class_voting_roc_2019.png"), width=10, height=3)
#Now QC 2019

ces %>% 
  filter(election!=2000&quebec==1&election!=2021) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2~occupation2, data=x)), 
         predicted=map(model, ggpredict))-> qc_models_2019

#Provide variables to state where the models come frome e.g. Quebec or ROC
qc_models_2019$region<-rep("Quebec", nrow(qc_models_2019))
roc_models_2019$region<-rep("Rest of Canada", nrow(roc_models_2019))
qc_models_2019
#Visualize
qc_models_2019 %>% 
  #bind the two sets of models together
  bind_rows(roc_models_2019) %>% 
  #Unnest the predicted values
  unnest_wider(predicted) %>% 
  #Now unnest the class variable
  unnest(occupation2) %>% 
  #Don't care about visualizing the greens
  filter(response.level!="Green") %>%
  #Rename some variables
  rename(Class=x, Election=election) %>% 
  #Gfaph election on x, yas predicted
  ggplot(., aes(x=Election, y=predicted, group=Class, linetype=Class))+
  geom_line()+
  facet_grid(region~response.level)+
  scale_linetype_manual(values=c(2,3,6, 1))+
  theme(axis.text.x = element_text(angle = 90))+labs(y="Predicted Probability", title=str_wrap("Effect of Social Class on Vote, 1965-2019, without Self-Employed", 35))
ggsave(filename=here("Plots", "canada_class_voting_1965_2019.png"), dpi=300,width=12, height=4)

#### This section addresses the concerns about the interaction between class and time
### It closely follows Andersen p. 174
#This removes pre-1979 elections, The Green Party and the year 2000 for multinomial logit modelling. 
ces %>% 
  filter(election> 1974&election!=2000&election!=2021  & vote2!="Green")->ces.out

# This model is no class and election as a continuous covariate
model1roc<-multinom(vote2 ~ age+male+as.factor(religion2)+degree+as.numeric(election), data=subset(ces.out, quebec!=1))
model1qc<-multinom(vote2 ~ age+male+as.factor(religion2)+degree+as.numeric(election), data=subset(ces.out, quebec!=0))
#This model includes
model2roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election), data=subset(ces.out, quebec!=1))
model2qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election), data=subset(ces.out, quebec!=0))
model3roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+as.numeric(election)*occupation4, data=subset(ces.out, quebec!=1))
model3qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+as.numeric(election)*occupation4, data=subset(ces.out, quebec!=0))
model4roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+poly(as.numeric(election), 2)*occupation4, data=subset(ces.out, quebec!=1))
model4qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+poly(as.numeric(election), 2)*occupation4, data=subset(ces.out, quebec!=0))
model5roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+poly(as.numeric(election), 3)*occupation4, data=subset(ces.out, quebec!=1))
model5qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+poly(as.numeric(election), 3)*occupation4, data=subset(ces.out, quebec!=0))
model6roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+
                      occupation4*`1979`+
                      occupation4*`1980`+
                      occupation4*`1984`+
                      occupation4*`1988`+
                      occupation4*`1993`+
                      occupation4*`1997`+
                      occupation4*`2004`+
                      occupation4*`2006`+
                      occupation4*`2008`+
                      occupation4*`2011`+
                      occupation4*`2015`+
                      occupation4*`2019`, data=subset(ces.out, quebec!=1))

model6qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.numeric(election)+
                     occupation4*`1979`+
                     occupation4*`1980`+
                     occupation4*`1984`+
                     occupation4*`1988`+
                     occupation4*`1993`+
                     occupation4*`1997`+
                     occupation4*`2004`+
                     occupation4*`2006`+
                     occupation4*`2008`+
                     occupation4*`2011`+
                     occupation4*`2015`+
                     occupation4*`2019`, data=subset(ces.out, quebec!=0))
model7roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+Period*occupation4, data=subset(ces.out, quebec!=1))
model7qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+Period*occupation4, data=subset(ces.out, quebec!=0))

model.list<-list(model1qc, model2qc, model3qc, model4qc,model5qc,model6qc, model7qc, model1roc, model2roc, model3roc, model4roc, model5roc, model6roc, model7roc)
library(kableExtra)
model.list %>% 
  map(., AIC) %>% 
  matrix(., nrow=7, ncol=2) ->AIC.matrix
rownames(AIC.matrix)<-c("No Class", "Stable Class",
                        "Class x Time Linear", 
                        "Class X Time Quadratic", 
                        "Class x Time Cubic", 
                        "Class x Time Dichotomous",
                        "Class x Time Pre-Post 2004")
colnames(AIC.matrix)<-c("QC", "ROC")
library(xtable)
print(xtable(AIC.matrix), type="html", file=here("Tables/AIC.html"))
library(stargazer)
#### This makes Table 1
### It is a slightly modified version of model2 above.
### That model treated time as a continuous covariate
### This does basically the same thing, it just treats election as a factor
### 1979 as the reference level for ROC and 1993 as the reference level for QC
### The bottom line is that it is reporting the stable effects of class from 1979 to 2021
### Controlling for fixed (demographic) and random (electoral) effects
model8roc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+as.factor(election)+as.factor(region3), data=subset(ces.out, quebec!=1))
model8qc<-multinom(vote2 ~ occupation4+age+male+as.factor(religion2)+degree+fct_relevel(election, "1993"), data=subset(ces.out, quebec!=0))

stargazer(model8qc, model8roc, digits=2,out=here("Tables", "andersen_replication_extension_1979_2019_stable_class.html"),  type="html", 
          column.labels=c("QC", "ROC"), column.separate = c(3,2), 
          title="Multinomial Logistic Regression of Left Vote On Class, 1979-2019", 
          #covariate.labels=c('Age', 'Gender (Male)', '(Religion) Catholic', '(Religion) Protestant', '(Religion) Other','(Education) Degree', '(Social Class) Managers', '(Social Class) Professionals','(Social Class) Self-Employed', '(Social Class) Routine Non Manual',  '1979','1980' ,'1984','1988','1997', '2004', '2006','2008','2011','2015','2019','1980','1984' ,'1988','1993','1997', '2004', '2006','2008','2011','2015','2019'),
          add.lines=list(c("N"," ", nrow(model2qc$fitted.values), "", nrow(model2roc$fitted.values), " ")), 
          single.row = T, 
          dep.var.labels=c("Right/Liberal", "Right/NDP", "Right/BQ", "Right/Liberal", "Right/NDP"), 
          star.cutoffs = c(0.05, 0.01, 0.001))
#### Visualizing the impact of class over time with controls
#### This fits the multinomial model between 1979 and 2019 with controls
### Separately for each election
ces.out%>% 
  filter(quebec==0) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2 ~ age+male+as.factor(religion2)+degree+occupation4+as.factor(region3), data=x)), 
         predicted=map(model, ggpredict, terms=c("occupation4")))->roc_models_2019_with_controls

  
ces.out %>% 
  filter(quebec==1) %>% 
  nest(-election) %>% 
  mutate(model=map(data, function(x) multinom(vote2 ~ age+male+religion2+degree+occupation4, data=x)), 
         predicted=map(model, ggpredict,terms=c("occupation4")))->qc_models_2019_with_controls

#Start with where the predicted values are stored

#Now QC 2019

qc_models_2019_with_controls$region<-rep("Quebec", nrow(qc_models_2019_with_controls))
roc_models_2019_with_controls$region<-rep("Rest of Canada", nrow(roc_models_2019_with_controls))

qc_models_2019_with_controls %>% 
  bind_rows(roc_models_2019_with_controls) %>% 
 # select(-data:-model) %>% 
  unnest(predicted) %>% 
  unnest(x) %>% 
#  distinct() %>% 
  rename(Class=x, Election=election) %>% 
  ggplot(., aes(x=as.factor(Election), y=predicted,linetype=Class, group=Class))+
  geom_line()+
  facet_grid(region~response.level)+
  scale_linetype_manual(values=c(1, 2,3,6, 5))+
  theme(axis.text.x = element_text(angle = 90))+labs(y="Predicted Probability", title=str_wrap("Effect of Social Class on Vote, 1979-2019, With Self-Employed", 35))
ggsave(filename=here("Plots", "canada_class_voting_1979_2019.png"), width=12, height=4,dpi=300 )

#### OLS model of party vote
#Load broom
library(broom)
#1979 -2019
ces %>% 
  select(election, working_class3) %>%
  group_by(election) %>% 
  summarize(sum(!is.na(working_class3)))
ces %>%
  filter(election> 1974 & election!=2000&election!=2021) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~region2+male+age+income+degree+as.factor(religion2)+working_class3+union_both, data=x)),
         tidied=map(model, tidy),
         vote=rep('NDP', nrow(.)))->ndp_models_complete1

ces %>%
  filter(election> 1974 & election!=2000&election!=2021) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(conservative~region2+male+age+income+degree+as.factor(religion2)+working_class3+union_both, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))
  )->conservative_models_complete1

ces %>%
  filter(election> 1974 & election!=2000&election!=2021) %>%
  nest(variables=-election) %>%
  mutate(model=map(variables, function(x) lm(liberal~region2+male+age+income+degree+as.factor(religion2)+working_class3+union_both, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))
  )->liberal_models_complete1

#Join all parties and plot Class coefficients
ndp_models_complete1 %>%
  bind_rows(., liberal_models_complete1) %>%
  bind_rows(., conservative_models_complete1) %>%
  unnest(tidied) %>% 
  filter(term=="union_both"|term=="working_class3") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'; 'union_both'='Union Household'")) %>%
  ggplot(., aes(x=election, y=estimate, col=vote, shape=term))+
  geom_point(color="black")+
  labs(title="OLS Coefficients of Working Class and Union Status \non Party Vote 1979-2019", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0, color="black")+
  ylim(c(-0.3,0.3))+
  #scale_color_manual(values=c("navy blue", "red", "orange"))+
  #Turn to greyscale for printing in the journal; also we don't actually need the legend because the labels are on the side
scale_shape_discrete(guide='none')+
  facet_grid(rows=vars(vote),cols=vars(term), switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_union_all_parties_all_canada.png"), dpi=300)

# 1979 -2019 without union + no degree plot (Quebec)
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  filter(quebec==1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete_QC1
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  filter(quebec==1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative ~male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete_QC1

ces %>% 
  filter(quebec==1) %>% 
  filter(election> 1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal ~male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete_QC1

ces %>% 
  filter(election> 1984 & election!=2000) %>%
  filter(quebec==1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(bloc ~male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)),
         tidied=map(model, tidy),
         vote=rep('Bloc', nrow(.))  
  )->bloc_models_complete_QC1

# 1979 -2019 without union + no degree plot (ROC)
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  filter(quebec!=1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(ndp~region+male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_models_complete_ROC1
ces %>% 
  filter(election> 1974 & election!=2000) %>%
  filter(quebec!=1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(conservative~region+male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Conservative', nrow(.))  
  )->conservative_models_complete_ROC1

ces %>% 
  filter(election> 1974 & election!=2000) %>%
  filter(quebec!=1) %>% 
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) lm(liberal~region+male+age+income+degree+as.factor(religion2)+union+working_class3, data=x)), 
         tidied=map(model, tidy),
         vote=rep('Liberal', nrow(.))  
  )->liberal_models_complete_ROC1

#Join all parties and plot Class coefficients  by QC
ndp_models_complete_QC1 %>% 
  bind_rows(., liberal_models_complete_QC1) %>% 
  bind_rows(., conservative_models_complete_QC1) %>%
  bind_rows(., bloc_models_complete_QC1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class3") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote 1979-2019 for QC", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.3,0.3))+
  scale_color_manual(values=c("sky blue", "navy", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_all_parties2_QC.png"))

#Join all parties and plot Class coefficients by ROC
ndp_models_complete_ROC1 %>% 
  bind_rows(., liberal_models_complete_ROC1) %>% 
  bind_rows(., conservative_models_complete_ROC1) %>%
  unnest(tidied) %>% 
  filter(term=="working_class3") %>% 
  mutate(term=Recode(term, "'working_class3'='Working Class'")) %>% 
  ggplot(., aes(x=election, y=estimate, col=vote))+
  geom_point()+
  labs(title="OLS Coefficients of Working Class on Party Vote 1979-2019 for ROC", alpha="Variable", color="Vote", x="Election", y="Estimate")+
  geom_errorbar(aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)), width=0)+
  ylim(c(-0.3,0.3))+
  scale_color_manual(values=c("navy", "red", "orange"))+
  facet_grid(vote~term, switch="y")+geom_hline(yintercept=0, alpha=0.5)+theme(axis.text.x=element_text(angle=90))

ggsave(here("Plots", "Vote_Coefficents_WC_all_parties_ROC2.png"))

#### Average Scores For Working Class Versus Average ####

ces %>% 
  select(election,  working_class4,  redistribution, immigration_rates, market_liberalism,traditionalism2) %>%
  rename(Group=working_class4, Redistribution=redistribution, `Immigration Rates`=immigration_rates, `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2) %>% 
  mutate(Redistribution=skpersonal::revScale(Redistribution, reverse=T)) %>% 
  as_factor() %>% 
  pivot_longer(cols=3:6, names_to="Variable", values_to="Score") %>% 
  #pivot_longer(cols=2:3, names_to="Variable", values_to="Group") %>% 
  filter(election>1988 &election!=2000) %>% 
  group_by(election, Group, Variable) %>% 
  summarize(Average=mean(Score, na.rm=T), sd=sd(Score, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(y=election, x=Average, group=Variable, col=`Group`))+geom_point()+
    facet_wrap(~fct_relevel(Variable, "Immigration Rates","Moral Traditionalism", "Market Liberalism", "Redistribution"), nrow=2)+
  theme(axis.text.x=element_text(angle=90))+scale_color_discrete(type=c("grey", "black"))+
  scale_y_discrete(limits=rev)+geom_errorbar(aes(xmin=Average-(1.96*se),xmax=Average+(1.96*se) ), width=0)+
  geom_vline(xintercept=0.5, linetype=2)+labs(y="Election", x="Average")
ggsave(here("Plots", "average_scores_raw_class_population_with_errorbars.png"), width=6, height=4)

#### Poooled Models

## These are some mslight modifications necessary; nothing huge.
## The one thing is to set Ontario to the reference category for the region2; it helps for predicted probabilities
ces$region2<-factor(ces$region2, levels=c("Ontario", "Atlantic", "Quebec", "West"))
ces$degree<-as_factor(ces$degree)
ces$male<-as_factor(ces$male)
ces$redistibution<-as.numeric(ces$redistribution)
ces$redistribution<-zap_labels(ces$redistibution)
ces$income<-as.numeric(ces$income)

#What we need is just 1988-2000 and 2004-2019, minus the BQ and the Greens
ces %>% 
  filter(election<1998 &election> 1984&vote2!="BQ"&vote2!="Green")->ces.1
ces %>% 
  filter(election>2003 &vote2!="BQ"&vote2!="Green")->ces.2
library(stargazer)
#Relevel region2

#### Pooled OLS Models####
# NDP Models
m19<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m28<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m22<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m31<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m25<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m34<-lm(ndp~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
#NDP

# Liberal models
m20<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m29<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m23<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m32<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m26<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m35<-lm(liberal~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)

#Conservative Models
m21<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.1)
m30<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates, data=ces.2)
m24<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.1)
m33<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:redistribution, data=ces.2)
m27<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.1)
m36<-lm(conservative~region2+age+male+religion2+degree+income+occupation4+redistribution+market_liberalism+traditionalism2+immigration_rates+degree:traditionalism2, data=ces.2)
# 

#Storing these here to avoid having to retype. 
#"Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", 
summary(m19)
stargazer(m19, m28, m20, m29, m21, m30, type="html", 
          omit=c(1:8), out=here("Tables", "pooled_party_vote_choice.html"),  
          column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), 
          covariate.labels=c("Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),
          star.cutoffs = c(0.05, 0.01, 0.001), 
          single.row=F, font.size="small", digits=2)

# For Appendix (full controls displayed)
stargazer(m19, m28, m20, m29, m21, m30, type="html", out=here("Tables", "pooled_party_vote_choice_full.html"),  column.labels = c('1988-1997', '2004-2019', '1988-1997', '2004-2019', '1988-1997', '2004-2019'), covariate.labels=c("Region (East", "Region (Ontario)", "Region (West)", "Age", "Male", "Religion (Catholic)", "Religion (Protestant)", "Religion (Other)", "Degree", "Income", "Class (Managers)", "Class (Professionals)", "Class (Self-Employed)", "Class (Routine Non-Manual)", "Redistribution", "Market Liberalism", "Moral Traditionalism", "Immigration Rates"), dep.var.labels =c("NDP" ,"Liberals", "Conservatives"),star.cutoffs = c(0.05, 0.01, 0.001), single.row=F, font.size="small", digits=2)

#### ###
#

interaction.models<-list(m22, m31, m25, m34, 
                         m23, m32, m26, m35, 
                         m24, m33, m27, m36)
#get length of coefficients
interaction.models %>% 
  map(., function(x) length(x$coefficients))

#names(interaction.models)<-c(rep("NDP", 2), rep("Liberals", 2), rep("Conservative", 2))
interaction.models %>% 
  map_dfr(., tidy, .id='model') %>% 
  mutate(term=recode_factor(term, "degreeDegree:redistribution"="Degree:Redistribution",
                            "degreeDegree:traditionalism2"="Degree:Traditionalism")) %>% 
  mutate(Party=c(rep("NDP", 80), rep("Liberal", 80), rep("Conservative", 80)),
                  Period=c(rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20),
         rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20),rep("1988-2000", 20 ), rep("2004-2019", 20), rep("1988-2000", 20 ), rep("2004-2019", 20))) %>% filter(str_detect(term, ":")) %>% 
  ggplot(., aes(x=Period, y=estimate, col=Period))+geom_point()+facet_grid(term~fct_relevel(Party, "NDP", "Liberal"), scales="free")+scale_color_grey(start=0.8, end=0.2) +geom_errorbar(width=0, aes(ymin=estimate-(1.96*std.error), ymax=estimate+(1.96*std.error)))+   geom_hline(yintercept=0,  linetype=2)+theme(strip.text.y.right = element_text(angle = 0))+labs(y="Coefficient")
ggsave(filename=here("Plots", "interaction_terms.png"), width=8, height=4)


  ## The models are re-created above and found in the word document.
  #Make a list of the models
summary(m22)
summary(m31)
summary(m23)
summary(m32)
summary(m24)
summary(m33)
model.list<-list(m22, m31, m24, m33)
#Run the ggeffects command for each. 
map(model.list, ggeffect, terms=c("redistribution", "degree")) %>% 
  #Turn each into a dataframe
  map(data.frame) ->model.list
#Provide names for each model
names(model.list)<-c(rep("NDP", 2),  rep("Conservative", 2)) 
#
model.list %>% 
  #Combine these things
  bind_rows() %>% 
  #PRovide a variable showing the period 
  mutate(Period=rep(c(rep("1988-2000", 10), rep("2004-2019", 10)), 2)) %>% 
  #Define variables for party
  mutate(Party=c(rep("NDP", 20),  rep("Right",20) )) %>% 
  #Plot
  ggplot(., aes(x=x, y=predicted, col=group))+geom_point()+geom_line()+facet_grid(Party~Period)+labs(x="Redistribution", y="Predicted")+scale_color_grey(name="Degree\nStatus")


ggsave(filename=here("Plots", "degree_redistribution_interaction_ndp_conservative.png"), width=6, height=4 )

#### Party Vote Share
ces$traditionalism2
ces$traditionalism
table(ces$working_class3, ces$occupation4)
table(ces$occupation4, ces$working_class4)
ces %>% 
  select(Election=election,working_class3, vote,  `Market Liberalism`=market_liberalism, `Moral Traditionalism`=traditionalism2, `Redistribution`=redistribution, `Immigration Rates`=immigration_rates) %>%
  pivot_longer(cols=`Market Liberalism`:`Immigration Rates`) %>% 
  group_by(name) %>% 
  mutate(pro=case_when(
    value>0.5~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(Election, working_class3, name, pro, vote) %>% 
  filter(Election>1988 &Election!=2000) %>% 
  filter(!is.na(vote)) %>% 
  filter(vote > 0 &vote<5) %>% 
  summarize(n=n()) %>% 
  mutate(percent=n/sum(n)) %>% 
 # filter(Election==2015) %>% 
  filter(working_class3==1) %>% 
  filter(pro==1) %>% 
  ggplot(., aes(x=Election, y=percent, fill=as_factor(vote)))+geom_col(position="dodge")+
  facet_wrap(~fct_relevel(name, "Moral Traditionalism", ))+labs(y="Percent")+
  #scale_fill_grey(name="Vote") 
  scale_fill_manual(values=c('red', 'darkblue', 'orange', 'lightblue' ), name="Vote")+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle=90))  
ggsave(filename=here("Plots", "party_vote_shares_issues_1993_2019.png"), width=10, height=8, dpi=300) 

