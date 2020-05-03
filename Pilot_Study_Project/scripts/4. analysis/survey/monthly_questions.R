library(likert)
library(tidyverse)
'%not_in%' <- function(x,y)!('%in%'(x,y))

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

monthly_questions <- left_join(survey_spe2,labels,by=c("question_nbr")) %>%
  filter(type=="monthly")

survey_summary <- monthly_questions %>%
  arrange(savvy_id, week_no) %>%
  filter(question_nbr %not_in% c(78,83)) %>%
  select(savvy_id,week_no,label,answer) %>%
  spread(key = label, value = answer)

for(i in 1:length(names(survey_summary)[-c(1,2)])) {
  col <- names(survey_summary)[-c(1,2)][i]
  survey_summary[,col] <- factor(survey_summary[,col], levels = c("1", "2", "3", "4", "5"),order=TRUE)
}

survey_summary$week_no <- factor(survey_summary[,'week_no'], levels = c("6","4","3","9"),
                                 labels = c("Week 6","Week 4","Week 3","Week 9")) 

survey_summary$week_no <- fct_other(survey_summary$week_no, 
                                    drop = c("Week 4", "Week 3"), other_level = "Mid of Period") %>% 
                          factor(labels = c("End of Period", "End of Period", "Mid of Period"))

data <- likert(survey_summary[,-c(1:2)],grouping=survey_summary[,2])

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
monthly_count <- left_join(week_no,survey_summary,by=c("savvy_id","week_no"))

monthly_count <- monthly_count %>%
  mutate_each(funs=funs(ifelse(is.na(.),"missing","complete")),starts_with("Q")) %>%
  gather(key = "questions",value="tag",-c(1:2)) %>% 
  group_by(week_no, questions) %>%
  summarise(complete = sum(tag == "complete"))

question_labels <- c(
  "Q77: How is has it been for you to access the Fitbit groups?" = "Q77",
  "Q79: How likely are you to recommend the Fitbit group discussion to others?" = "Q79",
  "Q81: How helpful were the Fitbit group discussions?" = "Q81",
  "Q82: How are you satisfied with the Fitbit group discussions?" = "Q82"
)

side <- ggplot(monthly_count,aes(x=week_no,y=complete)) +
  geom_bar(stat="identity",width=0.9) +
  facet_wrap(questions~.,strip.position="top",ncol=1,labeller = as_labeller(question_labels)) +
  geom_text(data=monthly_count,
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
             top = textGrob("MONTHLY QUESTIONS",gp=gpar(fontsize=17,fontfamily="sans",fontface="bold",color = "#303030")))