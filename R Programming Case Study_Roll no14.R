library(dplyr)
library(tidyverse)
library(ggplot2)
library(unvotes)
library(lubridate)

############
  
# Q1. Prepare an appropriate graph for listing all the issues 
# discussed at the UN along with their frequency
# and comment on the above charge

q_1 = function(){
issues_freq = un_roll_call_issues %>% count(issue, sort=TRUE)
issues_freq

issues_freq1 = issues_freq %>% 
  rename(count = n)

ggplot(issues_freq1, aes(x=count, y=issue)) +
  geom_bar(stat='identity')


message("According to the graph plotted: 
the issue of 'Arms control and disarmament' was discussed the most while
the issue of 'Economic development' was discussed the least")
}
q_1()


#########

# Q2. on what date was the issue of Nuclear weapons and 
# nuclear material first discussed at the UN

q_2 = function(){
  df = merge(un_roll_call_issues, un_roll_calls, by = 'rcid')
  df1 = df %>% select(date, issue)
  df2 = df1[match(unique(df1$issue), df1$issue),]
  
  x = df2[df2$issue=='Nuclear weapons and nuclear material',]
# date when issue of Nuclear weapons and 
# nuclear material was first discussed at the UN
print(x)

# Do you think a relevant topic like Nuclear weapons was 
# discussed too late?

message("According to the dataset, the issue of 'Nuclear weapons and nuclear material' 
        was first discussed on 7th November, 1948 which is almost 2 years after 
        the formation of the UN. The attacks on Hiroshima and Nagasaki took place in 1945 
        so yes, according to me the issue of 'Nuclear weapons and nuclear material' 
        should have been given more importance and discussed earlier")
}
q_2()



#############

# Q3. find out for which listed issue have the countries 
# abstained themselves from voting the most

q_3 = function(){
votes_issues <- un_votes %>%
  inner_join(un_roll_call_issues, by = "rcid")
votes_issues

by_issue <- votes_issues %>%
  group_by(issue) %>%
  summarize(n_votes = n(),
            n_abs = sum(vote == "abstain"),
            prct_abs = (n_abs/ n_votes)*100) %>% 
  arrange(desc(prct_abs))

by_issue

message("issue from which the countries have abstained themselves from voting the most is:")
by_issue[which.max(by_issue$n_abs),]
}
q_3()


#############

# Q4. Which country/ countries has casted the greatest 
# number of votes (ONLY as Yes or No) across all the voting

q_4 = function(){
casting <- un_votes %>%
  inner_join(un_roll_calls, by = "rcid")
casting

cast = subset(casting, casting$vote=="yes" | casting$vote=="no") %>% group_by(country)%>%
  summarize( votes = n()) %>% arrange(desc(votes))
cast

message("Country that has casted the greatest number of votes (ONLY as Yes or No) across all the voting is:")
cast[which.max(cast$votes),]
}
q_4()


############

# Q5. Some countries have reported two most important issues facing the 
# world in the 21st century as the Human rights and Economic development. 
# Post 1st January 2000, how many voting have taken place for
# these two issues separately. What is the combined proportion of these two issues 
# out of all the discussions that have taken place? 
# They expected at least one third of the debates should have taken
# place on these two issues. What does the data indicate ? 


q_5 = function(){
  post_date = un_roll_calls[(which(un_roll_calls$date >= as.Date("2000-01-01"))), ]
  samp = (merge(x = post_date, y = un_roll_call_issues, by = "rcid", all.X = TRUE))
  samp = subset(samp, select = c("rcid", "importantvote", "date", "issue"))
  
  message("Number of voting taken place for Human Rights are: ",nrow(samp[which(samp$issue == "Human rights"), ]))
  message("Number of voting taken place for Economic development are: ",nrow(samp[which(samp$issue == "Economic development"), ]))
  
  hred = nrow(samp[which(samp$issue == "Human rights" | samp$issue == "Economic development"), ])
  prop = hred/(nrow(samp))
  message("Proportion of these issues discussed is: ", prop)
  one_third = nrow(samp)*1/3
  message("No. of times these two issues being debated: ", hred)
  message("One Third of the total debates post 1st Jan 2000: ", one_third)
  message("The proportion is expected to be one third of all the debates. 
Although the debates are less than 1/3rd there isn't a significant difference.
We can see that these two issues were discussed in almost one third of the debates post 1st Jan 2000.")  
}
q_5()
  


############

# Q6. Pakistan keeps raising issues of Human right in Kashmir 
# which India has openly refuted. It refuses to participate in any such debates
# saying that Kashmir has been an internal matter for India. Because of
# this it has to abstain itself from voting on issues related to Human rights. 
# How many times did India abstain from the voting where Human rights issues 
# were being discussed? What is the proportion?


q_6 = function()
{
  
  ind1 = (un_votes[which(un_votes$country_code == "IN"), ])
  ind1 = merge(x = un_votes, y = un_roll_call_issues, by = "rcid", all.x = TRUE)
  count_hr = nrow(ind[which(ind1$issue == "Human rights" & ind1$vote == "abstain" & ind1$country == "India"), ])
  count_all = nrow(ind[which(ind1$issue == "Human rights" & ind1$country == "India"), ])
  prop = count_hr/ (count_all)
  message("The number of times India abstained from voting where Human Rights issues were 
          discussed is: ", count_hr)
  message("The proportion of votes abstained from voting where human rights were discussed 
          than other issues were discusse are: ", prop)
}
q_6()

############
  

# Q7. In general, how has been India's track record in taking a specific position 
# in the debates carried out at the UN. Specially for the issues that have been flagged 
# as important. In what proportion of voting rated as important 
# (importantvote value is specified as 1) did India abstain from voting? How does the
# proportion change if the votes where importance has not been specified are included 
# as important? Compare the proportion for India against the proportion of absenteeism 
# of US from important issues and draw your conclusions
  

q_7 = function()
{
d2= un_votes %>% inner_join(un_roll_calls, by = 'rcid')
imp_1=nrow(subset(d2,d2$country=="India" & d2$importantvote==1 & d2$vote=="abstain"))
imp_not1=nrow(subset(d2,d2$country=="India" & d2$importantvote!=1 & d2$vote=="abstain"))
all_imp=length(subset(un_roll_calls$rcid,un_roll_calls$importantvote==1))
imp_na=nrow(subset(d2,d2$country=="India" & (d2$importantvote==1 | d2$importantvote==is.na(d2$importantvote)) 
                   & d2$vote=="abstain"))
india_prop_imp1=imp_1/all_imp
message("1. proportion of voting where India abstained from voting on issues rated as important", 
        india_prop_imp1)

include_na=length(which(is.na(un_roll_calls$importantvote)))+all_imp
india_prop_imp_na=imp_na/include_na
message("2. proportion change if the votes where importance has not been specified 
   are included as important ", india_prop_imp_na)


ind_abs=length(subset(d2$rcid,d2$country=="India" & d2$importantvote==1 & d2$vote=="abstain"))

us_abs=length(subset(d2$rcid,d2$country=="United States" & d2$importantvote==1 & d2$vote=="abstain"))
prop_ind_abs=ind_abs/all_imp
prop_us_abs=us_abs/all_imp

message("3. proportion of absenteeism for India ", prop_ind_abs)
message("   proportion of absenteeism for US ", prop_us_abs)

message("We can conclude that India was absent more for important votes than USA")
}
q_7()


############
  
# Q8. Because of the improvements in the ties between US and India over the last few years, 
# Indian governments have been receiving flaks from various sections on supporting US 
# blindly to further their ties. Calculate the proportion of voting when India and US have 
# casted the same vote. Compare the proportions before and after 2001 and 
# draw your conclusions.
  
  
q_8 = function()
{
  india1 = subset(un_votes,un_votes$country=="India")
  india2 = merge(x = india1, y = un_roll_calls, by = "rcid", all.x = TRUE)
  india2 = subset(india2, select = c("rcid", "country", "vote", "date"))
  india_post_2001 = subset(india2, (india2$date > as.Date("2001-01-01")))
  india_pre_2001 = subset(india2, (india2$date <= as.Date("2001-01-01")))
  
  india_us_all = nrow(india2)
  pre_india_count=nrow(india_pre_2001)
  post_india_count=nrow(india_post_2001)
  
  us1 = subset(un_votes,un_votes$country=="United States")
  us2 = merge(x = us1, y = un_roll_calls, by = "rcid", all.x = TRUE)
  us2 = subset(us2, select = c("rcid", "country", "vote", "date"))
  us_post_2001 = subset(us2, (us2$date > as.Date("2001-01-01")))
  us_pre_2001 = subset(us2, (us2$date <= as.Date("2001-01-01")))
  
  pre_india_us_count=0
  for(i in 1:nrow(india_pre_2001))
  {
    for(j in 1:nrow(us_pre_2001)){
      if(india_pre_2001$rcid[i]==us_pre_2001$rcid[j] && india_pre_2001$vote[i]==us_pre_2001$vote[j]){
        pre_india_us_count=pre_india_us_count+1
      }
    }
  }
  
  post_india_us_count=0
  for(i in 1:nrow(india_post_2001))
  {
    for(j in 1:nrow(us_post_2001)){
      if(india_post_2001$rcid[i]==us_post_2001$rcid[j] && india_post_2001$vote[i]==us_post_2001$vote[j]){
        post_india_us_count=post_india_us_count+1
      }
    }
  }
  
  india_us_count = pre_india_us_count + post_india_us_count
  india_us_prop = india_us_count/india_us_all
  pre_prop_iu=pre_india_us_count/ pre_india_count
  post_prop_iu=post_india_us_count/ post_india_count
  message("The proportion of voting when India and US have casted the same vote: ", india_us_prop)
  message("Before and After Proportion: ")
  message("Before 2001: ", pre_prop_iu)
  message("After 2001: ", post_prop_iu)
  message("We can conclude that there is drop in the proportion of votes 
where India and USA have casted the same vote")
}
q_8()

###############


# Q9. It is also said that while in the past India was siding with Russia more often 
# than with US, recently in cases where US and Russia have voted oppositely, India has 
# sided with US more often. Compare and contrast the proportions before and after 2001 
# to check this claim and draw your conclusions.


q_9 = function(){
  india1 = subset(un_votes,un_votes$country=="India")
  india2 = merge(x = india1, y = un_roll_calls, by = "rcid", all.x = TRUE)
  india2 = subset(india2, select = c("rcid", "country", "vote", "date"))
  india_post_2001 = subset(india2, (india2$date > as.Date("2001-01-01")))
  india_pre_2001 = subset(india2, (india2$date <= as.Date("2001-01-01")))
  
  pre_india_count=nrow(india_pre_2001)
  post_india_count=nrow(india_post_2001)
  
  us1 = subset(un_votes,un_votes$country=="United States")
  us2 = merge(x = us1, y = un_roll_calls, by = "rcid", all.x = TRUE)
  us2 = subset(us2, select = c("rcid", "country", "vote", "date"))
  us_post_2001 = subset(us2, (us2$date > as.Date("2001-01-01")))
  us_pre_2001 = subset(us2, (us2$date <= as.Date("2001-01-01")))
  
  pre_india_us_count=0
  for(i in 1:nrow(india_pre_2001))
  {
    for(j in 1:nrow(us_pre_2001)){
      if(india_pre_2001$rcid[i]==us_pre_2001$rcid[j] && india_pre_2001$vote[i]==us_pre_2001$vote[j]){
        pre_india_us_count=pre_india_us_count+1
      }
    }
  }
  
  post_india_us_count=0
  for(i in 1:nrow(india_post_2001))
  {
    for(j in 1:nrow(us_post_2001)){
      if(india_post_2001$rcid[i]==us_post_2001$rcid[j] && india_post_2001$vote[i]==us_post_2001$vote[j]){
        post_india_us_count=post_india_us_count+1
      }
    }
  }
  
  
  pre_prop_u=pre_india_us_count/ pre_india_count
  post_prop_u=post_india_us_count/ post_india_count
  
  russia1 = subset(un_votes,un_votes$country=="Russia")
  russia2 = merge(x = russia1, y = un_roll_calls, by = "rcid", all.x = TRUE)
  russia2 = subset(russia2, select = c("rcid", "country", "vote", "date"))
  russia_post_2001 = subset(russia2, (russia2$date > as.Date("2001-01-01")))
  russia_pre_2001 = subset(russia2, (russia2$date <= as.Date("2001-01-01")))
  
  pre_india_russia_count=0
  for(i in 1:nrow(india_pre_2001))
  {
    for(j in 1:nrow(russia_pre_2001)){
      if(india_pre_2001$rcid[i]==russia_pre_2001$rcid[j] && india_pre_2001$vote[i]==russia_pre_2001$vote[j]){
        pre_india_russia_count=pre_india_russia_count+1
      }
    }
  }
  
  post_india_russia_count=0
  for(i in 1:nrow(india_post_2001))
  {
    for(j in 1:nrow(russia_post_2001)){
      if(india_post_2001$rcid[i]==russia_post_2001$rcid[j] && india_post_2001$vote[i]==russia_post_2001$vote[j]){
        post_india_russia_count=post_india_russia_count+1
      }
    }
  }
  
  
  
  pre_prop_r=pre_india_russia_count/ pre_india_count
  post_prop_r=post_india_russia_count/ post_india_count
  
  message("Proportion for same votes casted by US and India: ")
  message("Proportion of votes casted Before 2001: ", pre_prop_u)
  message("Proportion of votes casted After 2001: ", post_prop_u) 
  
  message("Proportion for same votes casted by Russia and India: ")
  message("Proportion of votes casted Before 2001: ", pre_prop_r)
  message("Proportion of votes casted After 2001: ", post_prop_r)
  message("We can conclude that India has voted the same as Russia both before and after 2001.
This proportion is more than that of India and US having voted the same
Note : 2001 has been included in the category 'before 2001'")
}
q_9()



############

# Q10. Make an appropriate graph to showcase how the proportion of voting where 
# India votes as Yes has varied over the time

q_10 = function()
{
join1 <- un_votes %>% inner_join(un_roll_calls, by = 'rcid')
by_country_year <- join1 %>% group_by(country, year=year(date)) %>% 
  summarise(votes=n(), 
            pct_yes = mean(vote=='yes'))
arrange(by_country_year, desc(pct_yes))

countries <- c('India')
by_country_year %>% filter(country %in% countries) %>% 
  ggplot(aes(x=year, y=pct_yes, color=country)) + geom_line() + 
  ylab("% of votes are 'Yes'") + ggtitle("Trend in percentage Yes Votes of India") + theme_bw()
}
q_10()







