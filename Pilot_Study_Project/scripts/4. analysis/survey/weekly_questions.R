library(likert)
library(tidyverse)

labels <- data.frame(question_nbr = c(20,30,77,79:83),
                     label = c("Q20: How are you in control of your diabetes today?",
                               "Q30: Is the coach supporting your needs and goals?",
                               "Q77: How is has it been for you to access the Fitbit groups?",
                               "Q79: How likely are you to recommend the Fitbit group discussion to others?",
                               "Q80: How useful is the Fitbit group discussions?",
                               "Q81: How helpful were the Fitbit group discussions?",
                               "Q82: How are you satisfied with the Fitbit group discussions?",
                               "Q83: What aspects of group dicussions were helpful?"),
                     type=c("weekly","weekly","monthly","monthly","weekly","monthly","monthly","monthly"))

#

weekly_questions <- left_join(survey_spe2,labels,by=c("question_nbr")) %>%
  filter(type=="weekly")

survey_summary <- weekly_questions %>%
  arrange(savvy_id, week_no) %>%
  filter(question_nbr %not_in% c(78,83)) %>%
  select(savvy_id,week_no,label,answer) %>%
  spread(key = label, value = answer)

for(i in 1:length(names(survey_summary)[-c(1,2)])) {
  col <- names(survey_summary)[-c(1,2)][i]
  survey_summary[,col] <- factor(survey_summary[,col], levels = c("1", "2", "3", "4", "5"),order=TRUE)
}

survey_summary$week_no <- factor(survey_summary[,'week_no'], levels = c("9","8","7","6","5","4","3","2","1"),
                                   labels = c("Week 9", "Week 8","Week 7","Week 6","Week 5","Week 4","Week 3","Week 2","Week 1")) 


data <- likert(survey_summary[,c(3:5)],grouping=survey_summary[,2])

main <- plot(data,plot.percent.neutral=FALSE,plot.percent.low=T,plot.percent.high=T,
             centered=T,
             legend.position = "none",
             low.color = "#4897D8",
             neurtal.color = "#999999",
             high.color = "#F8A055",
             panel.strip.color = "#c9c9c9",wrap=100) +
  theme(strip.text.x=element_text(size=10, family = "sans", color = "#303030"))

# ###
# sample<-survey_spe2 %>%
#   group_by(week_no,question_nbr,answer) %>%
#   filter(question_nbr %not_in% c(78,83)) %>%
#   summarise(count=n())
# colnames(sample) <- c("question","group","response","count")
# licorice(sample,type="fill")


# weekly_count <- survey_summary %>%
#   arrange(savvy_id, week_no) %>%
#   filter(question_nbr %not_in% c(78,83)) %>%
#   select(savvy_id,week_no,question_nbr,answer) %>%
#   spread(key = question_nbr, value = answer)

week_no <- crossing(savvy_id = survey_summary$savvy_id,week_no = survey_summary$week_no %>% unique())
weekly_count <- left_join(week_no,survey_summary,by=c("savvy_id","week_no"))

weekly_count <- weekly_count %>%
  mutate_each(funs=funs(ifelse(is.na(.),"missing","complete")),starts_with("Q")) %>%
  gather(key = "questions",value="tag",-c(1:2)) %>% 
  group_by(week_no, questions) %>%
  summarise(complete = sum(tag == "complete"))

question_labels <- c(
  "Q20: How are you in control of your diabetes today?" = "Q20",
  "Q30: Is the coach supporting your needs and goals?" = "Q30",
  "Q80: How useful is the Fitbit group discussions?" = "Q80"
)

side <- ggplot(weekly_count,aes(x=week_no,y=complete)) +
  geom_bar(stat="identity",width=0.9) +
  facet_wrap(questions~.,strip.position="top",ncol=1,labeller = as_labeller(question_labels)) +
  geom_text(data=weekly_count,
            aes(label=complete,x=week_no,y=complete+0.5),
            position =  position_dodge(width = 1),
            vjust = 0.5,
            size=3) +
  coord_flip() +
  xlab("")+
  ylab("Number of Respondents") +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        strip.background = element_rect(fill=	"#c9c9c9"),
        text = element_text(family = "sans", color = "#303030"))

grid.arrange(main,side,ncol=2,widths=c(3/4,1/4),
             top = textGrob("WEEKLY QUESTIONS",gp=gpar(fontsize=17,fontfamily="sans",fontface="bold",color = "#303030")))
