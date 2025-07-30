### this is the analysis for the book chapter
theme_set(theme_bw(base_size=20))
table(ces$working_class, ces$working_class3)
table(ces$working_class, ces$occupation3)
#### Voting Shares of Working Class
library(lubridate)
ces %>% 
  select(election, working_class3, vote2) %>% 
  #filter(is.na(election))
  group_by(election, working_class3, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class3==1) %>% 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election", linetype="Vote")+scale_linetype_manual(values=c(1,3,5,6))+
  scale_x_date(breaks= seq.Date(from=as.Date("1965-01-01"), to=as.Date("2021-01-01"), by="5 years"), date_labels="%Y")


ggsave(here("Plots", "book_chapter_working_class_votes_for_parties.png"))

ces %>% 
  select(election, working_class3, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  group_by(election,  vote2, working_class3) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=(n/sum(n))*100) %>% 
  filter(working_class3==1) %>% 
  filter(vote2!="Green") %>% 
  ggplot(., aes(x=election, y=pct, linetype=fct_relevel(vote2, "Liberal"), shape=fct_relevel(vote2, "Liberal"),group=vote2))+
  geom_point()+
  geom_line()+
  labs(y="Percent", x="Election", linetype="Vote", shape="Vote") +
scale_x_date(breaks= seq.Date(from=as.Date("1965-01-01"), to=as.Date("2021-01-01"), by="4 years"), date_labels="%Y")
ggsave("book_chapter_parties_share_working_class_votes.png")
table(ces$occupation3, ces$occupation4)
ces$Occupation<-Recode(ces$occupation4, "'Working_Class'='Working Class' ; 
                       'Routine_Nonmanual'='Routine Non-Manual'", levels=c(
                         "Managers", "Professionals", "Self-Employed", "Routine Non-Manual", "Working Class"
                       ))
ces %>% 
  select(election, Occupation, vote2) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  filter(election>1978&election!=2000&election!=2021) %>% 
  group_by(election, Occupation, vote2) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=Occupation, size=Occupation))+
  geom_line()+
  facet_wrap(~vote2)+
  scale_linetype_manual(values=c(3,4,2,5,1))+
  scale_size_manual(values=c(1,1,1,1.5,1.5))+
  labs(y="Percent", x="Election", title="Raw Support For Parties By Class", linetype="Class", size="Class") +
scale_x_date(breaks= seq.Date(from=as.Date("1980-01-01"), to=as.Date("2021-01-01"), by="4 years"), date_labels="%Y")+
  theme(axis.text.x=element_text(angle=90), legend.position = "bottom")+
  guides(linetype=guide_legend(keywidth=5, ncol=2))
 ggsave(here("Plots", "book_occupation_share_per_party.png"), width=8, height=8)


 ces %>% 
   select(election, Occupation, vote2) %>% 
   #mutate(election=ymd(election, truncated=2L)) %>% 
   filter(election>1978&election!=2000&election!=2021) %>% 
   group_by(election, Occupation, vote2) %>% 
   summarize(n=n()) %>% 
   filter(!is.na(vote2)) %>% 
   mutate(pct=n/sum(n)) %>% 
   filter(vote2!="Green") %>% 
   filter(!is.na(Occupation)) %>% 
   ggplot(., aes(x=as.factor(election), y=pct*100, fill=Occupation))+
   geom_col(position="dodge")+
   scale_fill_grey(start=0.8, end=0.2)+
   facet_wrap(~vote2)+
   labs(y="Percent", x="Election", title="Raw Support For Parties By Class", linetype="Class", size="Class") +
   #scale_x_date(breaks= seq.Date(from=as.Date("1980-01-01"), to=as.Date("2021-01-01"), by="4 years"), date_labels="%Y")+
   theme(axis.text.x=element_text(angle=90), legend.position = "bottom")+
   guides(fill=guide_legend(nrow=2))
 ggsave(here("Plots", "book_occupation_share_per_party_bar.png"), width=10, height=8)
 
ces %>% 
  select(election, Occupation, vote2) %>% 
  filter(election>1978& election!=2000& election!=2021) %>% 
  mutate(election=ymd(election, truncated=2L)) %>% 
  group_by(election, vote2, Occupation) %>% 
  summarize(n=n()) %>% 
  #filter(!is.na(vote2)) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(vote2!="Green") %>% 
  filter(!is.na(Occupation)) %>% 
  ggplot(., aes(x=election, y=pct*100, linetype=vote2, group=vote2))+
  geom_line()+
  facet_wrap(~Occupation)+scale_linetype_manual(values=c(1,2,3,4))+
  labs(y="Percent", x="Election", title="Share Of Each Party's Electorate By Class",linetype="Vote")+
  theme(axis.text.x=element_text(angle=90))+
  scale_x_date(breaks= seq.Date(from=as.Date("1980-01-01"), to=as.Date("2021-01-01"), by="5 years"), date_labels="%Y")
  
ggsave(here("Plots", "book_party_share_by_occupation.png"), width=10, height=8)

ces$region2<-factor(ces$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
table(ces$election)
table(ces$election, ces$Period)
ces %>% 
  filter(election>1987&election<2004)->ces.1

ces %>% 
  filter(Period==1&election<2021)->ces.2
#Check for missing values on traditionalism
ces %>% 
  group_by(election) %>% 
  select(election,  traditionalism2, working_class3, region2) %>% 
summarize_all(function(x) sum(!is.na(x))) 

#Fit models
ndp1<-lm(ndp~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`1988`+`1993`+`1997`, data=subset(ces.1, working_class3==1))
ndp2<-lm(ndp~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=subset(ces.2, working_class3==1))

lib1<-lm(liberal~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`1988`+`1993`+`1997`, data=subset(ces.1, working_class3==1))
lib2<-lm(liberal~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=subset(ces.2, working_class3==1))

con1<-lm(conservative~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`1988`+`1993`+`1997`, data=subset(ces.1, working_class3==1))
con2<-lm(conservative~region2+age+male+income+degree+religion2+union_both+redistribution+market_liberalism+immigration_rates+traditionalism2+`2004`+`2006`+`2008`+`2011`+`2015`+`2019`, data=subset(ces.2, working_class3==1))
model.list<-list(ndp1, ndp2, lib1, lib2, con1, con2)
#Use modelsummary() to print models
library(modelsummary)
modelsummary(model.list)
names(model.list)<-c("NDP 1988-2000", "NDP 2004-2019", 
                     "Liberal 1988-2000", "Liberal 2004-2019",
                     "Conservative 1988-2000", "Conservative 2004-2019")
modelsummary(model.list,coef_map=c(
  "(Intercept)"="Intercept" ,
  "region2Quebec"="Region (Quebec)",
  "region2Ontario"="Region (Ontario)",
  "region2West"="Region (West)",
  "age"="Age", 
  "male"="Male", 
  "income"="Income (Quintiles)",
  "degree"="Degree", 
  "religion2Catholic"="Religion (Catholic)",
  "religion2Protestant"="Religion (Protestant)",
  "union_both"="Union", 
  "redistribution"="Redistribution", 
  "market_liberalism"="Market Liberalism", 
  "immigration"="Immigration",
  "traditionalism2"="Traditionalism"),
  fmt=2,stars = c('*' = .05, '**' = .01, '***'=0.01),
  gof_omit=c("Log.Lik.|RMSE|BIC|AIC"), output=here("Tables", "book_ols_pre_post_2004.html"))



ces %>% 
  select(election, working_class3, traditionalism2, redistribution, immigration_rates, market_liberalism) %>%
  rename(Traditionalism=3, Redistribution=4, `Immigration Rates`=5, `Market Liberalism`=6) %>% 
  mutate(across(c(Traditionalism:6), .fns=function(x) Recode(x, "0.51:1=1; 0:0.5=0; else=NA"))) %>% 
  mutate(`Working Class`=Recode(working_class3, "0='Other'; 1='Working Class'", levels=c('Working Class', 'Other'))) %>% 
filter(election>1992) %>% 
  pivot_longer(3:6, names_to=c("Item"), values_to=c("Support")) %>% 
 group_by(election, `Working Class`, Item, Support) %>% 
  filter(!is.na(`Working Class`)) %>% 
summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
filter(Support==1) %>% 
  #summarize(Average=mean(traditionalism2, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=Percent, fill=`Working Class`))+labs(fill="Class",x="Election" ,y="Percent Agreeing")+
  geom_col(position="dodge")+
  facet_wrap(~fct_relevel(Item, "Immigration Rates", "Traditionalism"))+
  scale_fill_grey(start=0.9, end=0.2)+
  theme(legend.position="bottom")
ggsave(here("Plots", "Proportion_working_class_supporting_measures.png"), width=10, height=8)

ces %>% 
  filter(election==2015|election==2019) %>% 
  select(working_class4, election, ndp, conservative, region2, age, male, union_both, income, degree, religion2) %>% 
  nest(-election) %>% 
  mutate(ndp=map(data, function(x) lm(ndp~working_class4+region2+age+male+income+degree+religion2+union_both,data=x)),
 conservative=map(data, function(x) lm(conservative~working_class4+region2+age+male+income+degree+religion2+union_both,data=x)))->ndp_conservative_models2
#Check missing values for all variables here.
ndp15<-lm(ndp~working_class4+region2+age+male+income+degree+religion2+union_both,data=subset(ces, election==2015))
ndp19<-lm(ndp~working_class4+region2+age+male+income+degree+religion2+union_both,data=subset(ces, election==2019))
conservative15<-lm(conservative~working_class4+region2+age+male+income+degree+religion2+union_both,data=subset(ces, election==2015))
conservative19<-lm(conservative~working_class4+region2+age+male+income+degree+religion2+union_both,data=subset(ces, election==2019))

modelsummary(models=list(ndp15, ndp19, conservative15, conservative19) ,
             coef_rename=c("(Intercept)"="Intercept", 
                           "working_class3"="Working Class", 
                           "region2Quebec"="Region (Quebec)", 
                           "region2Ontario"="Region (Ontario)", 
                           "region2West"="Region (West)", 
                           "age"="Age", 
                           "male"="Male", 
                           "income"="Income",
                           "degree"="Degree",
                           "religion2Catholic"="Religion (Catholic)", 
                           "religion2Protestant"="Religion (Protestant)", 
                           "religion2Other"="Religion (Other)", 
                           "union_both"="Union"), gof_omit = c("AIC|BIC|RMSE|Log.Lik."), 
             fmt=2,stars = c('*' = .05, '**' = .01, '***'=0.01), output=here("Tables", "book_ndp_conservative_2015_2019.html"))

ces %>% 
  filter(election==2015|election==2019) %>% 
  select(working_class3, election, ndp, conservative, region2, age, male, union_both, income, degree, religion2) %>% 
  group_by(election) %>% 
  summarize_all(function(x) sum(!is.na(x)))
ces %>% 
  filter(election==2015|election==2019) %>% 
  select(working_class3, election, ndp, conservative, region2, age, male, union_both, income, degree, religion2) %>% 
  group_by(election) %>% 
  summary()

#### Make MIP PErcent cnage
ces19phone %>% 
  select(occupation3, mip)->out19
ces15phone %>% 
  select(occupation3, mip)->out15
out15$Election<-rep("2015",nrow(out15))
out19$Election<-rep("2019", nrow(out19))
out15 %>% 
  bind_rows(out19) %>% 
  as_factor() %>% 
  mutate(occupation3=Recode(occupation3, "'Routine_Nonmanual'='Routine Non-Manual'; 'Skilled'='Working Class';
                            'Unskilled'='Working Class'; 
                            'Self_employed'='Self-Employed'", 
                            levels=c("Managers", "Professional", "Self-Employed", "Routine Non-Manual", "Working Class")) ) %>% 
mutate(Issue=Recode(mip, "'Jobs'='Jobs and Economy' ;
                    'Economy'='Jobs and Economy'"))  %>% 
  group_by(Election, occupation3, Issue) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  arrange(Issue, occupation3) %>% 
  group_by(Issue, occupation3) %>% 
  mutate(Change=Percent-lag(Percent)) %>% 
  #filter(Election==2019) %>% 
  filter(str_detect(Issue, "Jobs|Environment|Immigration|Health|Energy")) %>% 
  filter(!is.na(occupation3)) %>% 
  ggplot(., aes(y=fct_relevel(occupation3, "Working Class", "Routine Non-Manual", "Self-Employed", "Professional", "Managers"), x=Percent, fill=fct_relevel(Issue, "Environment", "Energy")))+
  facet_grid(~Election)+geom_col(position="dodge")+labs(y="Class", fill="Issue")+scale_fill_grey(guide=guide_legend(reverse=T))
ggsave(here("Plots", "book_mip_change.png"))
levels(ces$occupation4)
ces$working_class5<-Recode(ces$occupation4,"'Working_Class'='Working Class' ; 
                           'Routine_Nonmanual'='Routine Non-manual';NA=NA;
                           else='Other'", 
                           levels=c("Other", "Routine Non-manual", "Working Class"))
#Do the moral traditionalism interaction
ces %>% 
  filter(election==2015| election==2019) %>% 
  select(working_class3,vote2, traditionalism2, election) %>% 
  group_by(election,vote2, working_class3) %>% 
 filter(!is.na(working_class3)) %>% 
 # filter(working_class3==1) %>% 
  filter(vote2=="Conservative"|vote2=="NDP") %>% 
  summarize(avg=mean(traditionalism2, na.rm=T), 
            n=n(), 
            sd=sd(traditionalism2, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=as.factor(working_class3), y=avg, col=vote2))+geom_point()+ylim(c(0,1))+facet_grid(~election)

#### This the moral traditionalism interaction that we are dropping
# ces %>% 
#   #Keep only 2015 and 2019
#   filter(election==2015|election==2019) %>% 
#   #Filter working_class 
#  #filter(working_class4==1) %>% 
#   #Select relevenat variables note no controls
#   #select(working_class5, market_liberalism, traditionalism2, election, conservative, ndp) %>% 
#   #Nest the the data for the elections
#   nest(data=-election) %>% 
#   #we are mapping the model function onto the column data
#   #fitting first the model ndp ~ traditionalism and market liberalism
#   mutate(ndp=map(data, function(x) lm(ndp~working_class3+traditionalism2+working_class3:traditionalism2, data=x)),
#          #Then doing the same for consservatives
#          conservative=map(data, function(x) lm(conservative~working_class3+traditionalism2+working_class3:traditionalism2, data=x))) ->ndp_conservative_models
#### valence
ces %>% 
  filter(election>2014 &election<2020) %>% 
  select(manage_economy)
ces15phone %>% 
  select(manage_economy, occupation3, vote) %>% 
  mutate(Election=rep(2015, nrow(.)))->out15

ces19phone %>% 
  select(manage_economy, occupation3, vote) %>% 
  mutate(Election=rep(2019, nrow(.)))->out19
out15 %>% 
  bind_rows(out19) %>% 
  as_factor() %>% 
rename(`Manage Economy`=1, Class=2, Vote=3) %>% 
  mutate(Election=factor(Election, levels=c("2019", "2015")), Class=Recode(Class, "
                      'Unskilled'='Working Class' ; 'Skilled'='Working Class' ;
                      'Self_employed'='Self-Employed'; 
                      'Routine_Nonmanual'='Routine Non-Manual'", 
                      levels=c(NA,"Working Class" , "Routine Non-Manual", "Self-Employed", "Professional", "Managers"))) %>% 
  group_by(Election, Class, Vote) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  filter(Vote!="Other"&Vote!="Green"& Vote!="Bloc") %>% 
  ggplot(., aes(x=Percent, y=Class, fill=Election))+geom_col(position="dodge")+facet_grid(vars(Vote))+
  scale_fill_grey(start=0.2, end=0.8)+labs(fill="Election")+
  guides(fill=guide_legend(reverse=T))
ggsave(here("Plots", "book_valence_change.png"))

