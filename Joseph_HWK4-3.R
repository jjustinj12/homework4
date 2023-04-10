if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(tidyverse)
library(scales)
library(knitr)
library(rdrobust)
library(cobalt)
library(modelsummary)
library(rdd)
library(broom)



#full.ma.data <- readRDS('data/output/full_ma_data.rds')
ma.data <- readRDS('data/output/final_ma_data.rds')

#1. 

f<- function(x) {
  r <- quantile (x, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(r) <-  c("ymin", "lower", "middle", "upper", "ymax")
  r
}

Final.plan.plot <- ma.data %>%
  group_by (fips, year) %>%
  select(fips, year) %>% summarize (plan_count=n()) %>%
  ggplot(aes(x=as.factor(year), y=plan_count)) + 
  stat_summary(fun.data=f, geom="boxplot")+
  labs( 
    x="Year", 
    y="Number of Plans", 
    title="Plan Counts by county from 2007-2015 "
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()
Q1<-Final.plan.plot

# Final.plan.plot1 <- ma.data %>%
#   group_by (fips, year) %>%
#   select(fips, year) %>% summarize (plan_count=n()) %>%
#   ggplot(aes(x=as.factor(year), y=plan_count)) +
#   geom_boxplot()
# Final.plan.plot1

#2 Provide bar graphs showing the distribution of star ratings in 2009, 2012, and 2015. How has this distribution changed over time?

rating.years <- ma.data %>%
  filter(year==2009 | year==2012 | year==2015)%>%
  filter(!is.na(Star_Rating))%>%
  ggplot(aes(x=as.factor(Star_Rating)))+
  geom_bar(aes(fill=as.factor(year)), position=position_dodge()) + 
  scale_fill_grey() + 
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Distribution of star ratings in 2009, 2012, and 2015",
    fill="Year" 
  ) + 
  theme_bw() + 
  scale_y_continuous(labels=comma)
Q2<-rating.years

#Q3
benchmark_payment <- ma.data %>%
  group_by(fips, year) %>%
  filter(year>=2009 & year<=2015) %>%
  summarize(payment=mean(ma_rate, na.rm=TRUE))%>%
  ggplot(aes(x=as.factor(year), y=payment))+ 
  stat_summary(fun="mean", geom = "bar")  +
  labs(
    x="Year",
    y="Benchmark Payment",
    title="Average Benchmark Payment from 2009 to 2015"
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()

Q3<-benchmark_payment

#Q4
# Q4<-ma.data%>%
#   group_by(fips, year) %>%
#   filter(year>=2009 & year<=2015) %>%
#   summarize(shares=(avg_enrolled/avg_eligibles)) %>%
#   ggplot(aes(x=as.factor(year), y=shares))+ 
#   stat_summary(geom = "bar")  +
#   labs(
#     x="Year",
#     y="Average Share of MA",
#     title="Average share of Medicare Advantage 
#     (relative to all Medicare eligibles) over time from 2009 through 2015"
#   ) + scale_y_continuous(labels=comma) + 
#   theme_bw()
# Q4


ma.mkt.data<- ma.data %>% group_by(fips,year)%>%
  summarize(enroll=first(avg_enrolled), medicare=first(avg_eligibles), bench=mean(ma_rate, na.rm=1))%>%
  mutate(mkt_share=enroll/medicare)
ma.share<-ma.mkt.data %>% filter(year>=2009 & year<=2015) %>%
  ggplot(aes(x=as.factor(year), y=mkt_share, group=1))+ 
  stat_summary(fun.y="mean", geom = "bar", na.rm=TRUE)  +
  labs(
    x="Year",
    y="Average Share of MA",
    title="Average share of Medicare Advantage 
    (relative to all Medicare eligibles) over time from 2009 through 2015"
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()
Q4<-ma.share
#Q5


ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score))

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, first_enrolled,
         last_enrolled, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, partd, plan_type)

ma.rounded<-ma.data.clean %>% 
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3,1,0),
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1, 0),
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5,1, 0)) %>%
  group_by(Star_Rating)%>% filter(Star_Rating %in% c(3,3.5,4,4.5,5)) %>%
  summarize(count_30=sum(rounded_30),
            count_35=sum(rounded_35),
            count_40=sum(rounded_40),
            count_45=sum(rounded_45),
            count_50=sum(rounded_50)) %>%
  mutate(rounded=count_30+count_35+count_40+count_45+count_50)%>%
  select(Star_Rating, rounded)


Q5<-ma.rounded
Q5


#Q6


ma.data.clean <- ma.data.clean %>%
  mutate(
    mkt_share = avg_enrollment/avg_eligibles,
    ln_share = log(mkt_share))



star30 <- lm(mkt_share ~ treat + score,
             data=(ma.data.clean %>%
                     filter(raw_rating>=(2.75-0.125),
                            raw_rating <= (2.75+0.125), 
                            Star_Rating %in% c(2.5,3)) %>%
                     mutate(treat=(Star_Rating==3.0),
                            score=raw_rating-2.75)))
star35 <- lm(mkt_share ~ treat + score,
             data=(ma.data.clean %>%
                     filter(raw_rating>=(3.25-0.125),
                            raw_rating <= (3.25+0.125), 
                            Star_Rating %in% c(3,3.5)) %>%
                     mutate(treat=(Star_Rating==3.5),
                            score=raw_rating-3.25)))

star40<-lm(mkt_share ~ treat + score,
           data=(ma.data.clean %>%
                   filter(raw_rating>=(3.75-0.125),
                          raw_rating <= (3.75+0.125), 
                          Star_Rating %in% c(3.5,4.0)) %>%
                   mutate(treat=(Star_Rating==4.0),
                          score=raw_rating-3.75)))
star45<-lm(mkt_share ~ treat + score,
           data=(ma.data.clean %>%
                   filter(raw_rating>=(4.0-0.125),
                          raw_rating <= (4.0+0.125), 
                          Star_Rating %in% c(4.0,4.5)) %>%
                   mutate(treat=(Star_Rating==4.5),
                          score=raw_rating-4.25)))

modelsummary(list("Star 3.0"=star30, "Star 3.5 "=star35, "Star 4.0"=star40), 
             title="Estimates", 
             coef_map=c('treatTRUE'="Treatment",
                        'score'="Score"), 
             gof_map=list(list("raw"="nobs","clean"="N", "fmt"=0),
                          list("raw"="r.squared", "clean"="R\\textsuperscript{2}", "fmt"=2))) 




#Q7 

for (h in seq(0.1, 0.15, 0.01)) {
  star30bw <- lm(mkt_share ~ treat +score, 
                 data= (ma.data.clean %>%
                          filter(raw_rating >= (2.75-h),
                                 raw_rating<= (2.75+h),
                                 Star_Rating %in% c(2.5,3.0)) %>%
                          mutate(treat= (Star_Rating == 3.0),
                                 score = raw_rating-2.75)))
  coef.30 <- tidy(star30bw, conf.int=TRUE) %>% mutate(rating=30)
  
  
  star35bw <- lm(mkt_share ~ treat+score, 
                 data=(ma.data.clean %>%
                         filter(raw_rating >= (3.25-h),
                                raw_rating<= (3.25+h),
                                Star_Rating %in% c(3.0,3.5)) %>%
                         mutate(treat= (Star_Rating == 3.5),
                                score = raw_rating-3.25)))
  coef.35 <- tidy(star35bw, conf.int=TRUE) %>% mutate(rating=35)
  
  star40bw <- lm(mkt_share ~ treat+score, 
                 data=(ma.data.clean %>%
                         filter(raw_rating >= (3.75-h),
                                raw_rating<= (3.75+h),
                                Star_Rating %in% c(3.5,4.0)) %>%
                         mutate(treat= (Star_Rating == 4.0),
                                score = raw_rating-3.75)))
  coef.40 <- tidy(star40bw, conf.int=TRUE) %>% mutate(rating=40)
  
  
  est.collect <- rbind(coef.30, coef.35, coef.40) %>%
    mutate(bandwidth=h)
  
  if(h==0.1) {
    est.final <- est.collect
  }
  else {
    est.final <- rbind(est.final, est.collect)
  }
  
}

rd.estimates <- est.final %>% filter (term== "treatTRUE") %>%
  ggplot(aes(x=as.factor(bandwidth), y=estimate, shape=as.factor(rating)))+
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width = 0.5))+
  labs(
    y="Estimate and 95 Percent Confidence Interval",
    x = "Bandwith",
    shape= "Star Rating") +
  geom_point(size=3, position=position_dodge(width=0.5)) + 
  theme_bw()
Q7<-rd.estimates
Q7

#Q8

Q8_1 <- ma.data.clean %>%
  filter(raw_rating>= (2.75-0.125),
         raw_rating<= (2.75+0.125),
         Star_Rating %in% c(2.5,3.0)) %>%
  ggplot(aes(x=raw_rating))+
  geom_density() + 
  geom_vline(xintercept=2.75, linetype='dashed') +
  labs(
    x="Star Rating",
    y = "Number of Plans",
    title="Distrubtion of of Plans on Star Rating"
  ) + scale_x_continuous(breaks=seq(2.6, 2.9, 0.05), limits=c(2.6, 2.9, 0.05))

Q8_1

Q8_2 <- ma.data.clean %>%
  filter((raw_rating>=3.25-0.125 & Star_Rating==3) |
           raw_rating<=3.25+.125 & Star_Rating==3.5) %>%
  ggplot(aes(x=raw_rating))+
  geom_density() + 
  geom_vline(xintercept=3.25, linetype='dashed') +
  labs(
    x="Star Rating",
    y = "Number of Plans",
    title="Distrubtion of of Plans on Star Rating"
  ) + scale_x_continuous(breaks=seq(3.10, 3.40, 0.05), limits=c(3.1, 3.40, 0.05))
Q8_2

Q8_3 <- ma.data.clean %>%
  filter((raw_rating>=3.75-0.125 & Star_Rating==3.5) |
           raw_rating<=3.75+.125 & Star_Rating==4) %>%
  ggplot(aes(x=raw_rating))+
  geom_density() + 
  geom_vline(xintercept=3.75, linetype='dashed') +
  labs(
    x="Star Rating",
    y = "Number of Plans",
    title="Distrubtion of of Plans on Star Rating"
  ) + scale_x_continuous(breaks=seq(3.6, 3.9, 0.05), limits=c(3.6, 3.9, 0.05))
Q8_3

Q8_4 <- ma.data.clean %>%
  filter((raw_rating>=4.25-0.125 & Star_Rating==4) |
           raw_rating<=4.25+.125 & Star_Rating==4.5) %>%
  ggplot(aes(x=raw_rating))+
  geom_density() + 
  geom_vline(xintercept=4.25, linetype='dashed') +
  labs(
    x="Star Rating",
    y = "Number of Plans",
    title="Distrubtion of of Plans on Star Rating"
  ) + scale_x_continuous(breaks=seq(4.1, 4.4, 0.05), limits=c(4.1, 4.4, 0.05))
Q8_4

#Q9

ma.data.clean <- ma.data.clean %>% mutate(HMO=str_detect(plan_type, "HMO"))

lp.vars <- ma.data.clean %>% ungroup() %>%
  filter( (raw_rating >= 2.75-0.125 & Star_Rating== 2.5) |
            (raw_rating  <= 2.75+0.125 & Star_Rating == 3) ) %>%
  mutate(rounded = (Star_Rating == 3)) %>% 
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.30 <- love.plot(bal.tab(lp.covs, treat= lp.vars$rounded), colors="black", title = "Compairson for 2.5 vs 3 stars (0.125 bandwidth)")+
  theme_bw() + theme(legend.position="none")

Q9_1<-plot.30

lp.vars <- ma.data.clean %>% ungroup() %>%
  filter((raw_rating >= 3.25-0.125 & Star_Rating==3.0) |
           (raw_rating <= 3.25+0.125 & Star_Rating==3.5)) %>%
  mutate(rounded=(Star_Rating==3.5)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.35 <- love.plot(bal.tab(lp.covs, treat=lp.vars$rounded), colors="black", title =  "Compairson balance for 3 vs 3.5 stars (0.125 bandwidth)") + 
  theme_bw() + theme(legend.position="none")

Q9_2<-plot.35

lp.vars <- ma.data.clean %>% ungroup() %>%
  filter((raw_rating >= 3.75-0.125 & Star_Rating==3.5) |
           (raw_rating <= 3.75+0.125 & Star_Rating==4.0)) %>%
  mutate(rounded=(Star_Rating==4.0)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.4 <- love.plot(bal.tab(lp.covs, treat=lp.vars$rounded), colors="black",title =  "Compairson balance for 3.5 vs 4 stars (0.125 bandwidth)") + 
  theme_bw() + theme(legend.position="none")

Q9_3<-plot.4










Q9_1




rm(list=c("ma.data", "ma.data.clean", "full.ma.data"))
save.image("Hwk4_workspace_4_3.Rdata")




