if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(tidyverse)
library(scales)
library(knitr)

library(rdrobust)

full.ma.data <- readRDS('data/output/full_ma_data.rds')
ma.data <- readRDS('data/output/final_ma_data.rds')
view(ma.data)
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
    y="Number of Plans"
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()
Final.plan.plot

#2 Provide bar graphs showing the distribution of star ratings in 2009, 2012, and 2015. How has this distribution changed over time?

rating.years <- ma.data %>%
  filter(year==2009 | year==2012 | year==2015)%>%
  ggplot(aes(x=as.factor(Star_Rating)))+
  geom_bar(aes(fill=as.factor(year)), position=position_dodge()) + 
  scale_fill_grey() + 
  labs(
    x="Star Rating",
    y="Count of Plans",
    fill="Year" 
  ) + 
  theme_bw() + 
  scale_y_continuous(labels=comma)
rating.years

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

benchmark_payment

#Q4
Q4<-ma.data%>%
  group_by(fips, year) %>%
  filter(year>=2009 & year<=2015) %>%
  summarize(shares=(avg_enrolled/avg_eligibles)) %>%
  ggplot(aes(x=as.factor(year), y=shares))+ 
  stat_summary(geom = "bar")  +
  labs(
    x="Year",
    y="Average Share of MA",
    title="Average share of Medicare Advantage 
    (relative to all Medicare eligibles) over time from 2009 through 2015"
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()
Q4


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
         bid, avg_ffscost, ma_rate, partd)
colnames(ma.data.clean)
ma.data.clean_q5<-ma.data.clean %>%
  filter(Star_Rating >=3)


Q5<- knitr::kable(table(ma.data.clean_q5$Star_Rating))
Q5


#Q6




ma.rd <- ma.data.clean %>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window1 = (score>=-.175 & score<=.175),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)


est1 <- rdrobust(y=ma.data.clean$avg_enrollment, x=ma.data.clean$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")
est2 <- rdrobust(y=ma.data.clean$avg_enrollment, x=ma.data.clean$score, c=0.5,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")
est3 <- rdrobust(y=ma.data.clean$avg_enrollment, x=ma.data.clean$score, c=1,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")
est4 <- rdrobust(y=ma.data.clean$avg_enrollment, x=ma.data.clean$score, c=1.5,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")


#Q7 
# c_values <- c(0, 0.5, 1, 1.5)
# h_values <- c(0.1, 0.12, 0.13, 0.14, 0.15)
# results <- list()
# 
# for (c in c_values) {
#   for (h in h_values) { 
#     est <- rdrobust::rdrobust(
#       y = ma.data.clean$avg_enrollment,
#       x = ma.data.clean$score, 
#       c = c, h = h, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off"
#     )
#     var_name <- paste0("h", h, "_c", c)
#     results[[var_name]] <- est
#   }
# }
# 
# # Extract the first coefficient for each model
# coefs <- sapply(results, function(model) model$coef[1])
# 
# # Create a bar plot of the coefficients
# barplot(coefs, names.arg = names(results))
# 
# 
# 
# as.numeric(h0.1_c0$(coef[3]))  # variable for h=1, c=1
# h2_c1  # variable for h=2, c=1
# h1_c2  # variable for h=1, c=2
# 
# results
# 
# h0.1_c0$coef[1]

rd_1 <- rdrobust(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0,
                  h=0.1, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

rd_2 <- rdrobust(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0,
                  h=0.12, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

rd_3 <- rdrobust(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0,
                  h=0.13, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

rd_4 <- rdrobust(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0,
                  h=0.14, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

rd_5 <- rdrobust(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0,
                  h=0.15, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")



graph_7a <- rdplot(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0, h=0.1, 
                   title = "RD Plot: h=0.1", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7b <- rdplot(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0, h=0.12, 
                   title = "RD Plot: h=0.12", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7c <- rdplot(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0, h=0.13, 
                   title = "RD Plot: h=0.13", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")
graph_7d <- rdplot(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0, h=0.14, 
                   title = "RD Plot: h=0.14", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")
graph_7e <- rdplot(y=ma.rd$avg_enrollment, x=ma.rd$score, c=0, h=0.15, 
                   title = "RD Plot: h=0.15", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

#Q8
Q8<-ma.rd %>%
  filter(avg_enrollment<500) %>%
  filter(raw_rating>2 & raw_rating<2.5)%>%
  ggplot(aes(x=raw_rating, y=avg_enrollment))+geom_point()
Q8

#Q9
below <- ma.data.clean %>%
  filter(raw_rating < 2.25 & raw_rating > 2) %>%
  summarise(count = n(),
            partd_yes = sum(partd == "Yes"),
            partd_prop = sum(partd == "Yes") / n())

above <- ma.data.clean %>%
  filter(raw_rating > 2.5 & raw_rating < 3) %>%
  summarise(count = n(),
            partd_yes = sum(partd == "Yes"),
            partd_prop = sum(partd == "Yes") / n())
df <- data.frame(Rating = c("Below 2.25", "Above 2.5"),
                 Count = c(below$count, above$count),
                 PartD_Yes = c(below$partd_yes, above$partd_yes),
                 PartD_Prop = c(below$partd_prop, above$partd_prop))
colnames(df) <- c("Rating Category", "Total Count", "PartD Yes Count", "PartD Yes Proportion")
Q9<-knitr::kable(df, caption = "Counts and proportion of plans with PartD equal to 'Yes'")
Q9



rm(list=c("ma.data", "ma.data.clean", "full.ma.data"))
save.image("Hwk4_workspace_4.Rdata")




