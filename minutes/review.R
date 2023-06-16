
# Cleaning 2010 folder 
source("list_loop_for_data_frame.R")
df[which(is.na(df$supervisor) & df$status == "vacant"),][1] <- "NO VACANCY"
# This was picked up by mistake
df <- df[which(df$supervisor != ""),]
# He was actually present
df$status[which(df$date2 == "2009-08-13" & 
                  df$supervisor == "DIESTLER")] <- "present"
# Arriving late still counts as arriving
df$status[which(df$supervisor == "THOMPSONARRIVEDAT6:20")] <- "present"
df$supervisor[which(df$supervisor == "THOMPSONARRIVEDAT6:20")] <- "THOMPSON"
df$supervisor[which(df$supervisor == "THOMPSONARRIVEDLATE.SUPERVISORARNOLD")] <- "ARNOLD"
# Add row for THOMPSON's presence (late) since she was included in Arnold's Absence
tmp <- data.frame(matrix(c("THOMPSON", 
                           "AUGUST 13, 2009", 
                           "present"), ncol = 3)) %>% 
  rename(supervisor = X1, 
         date = X2, 
         status = X3)
df <- rbind(df, tmp)
# Add BEAVER for being present 
df[which(df$supervisor == "SUPERVISORBEAVERARRIVEDAT6:35PM.SUPERVISORSFIZZELL"),]
tmp <- data.frame(matrix(c("BEAVER", 
                           "SEPTEMBER 10, 2009", 
                           "present"), ncol = 3)) %>% 
  rename(supervisor = X1, 
         date = X2, 
         status = X3)
df <- rbind(df, tmp)
# Change string to just FIZZEL who was absent
df$supervisor[which(df$supervisor == "SUPERVISORBEAVERARRIVEDAT6:35PM.SUPERVISORSFIZZELL")] <- "FIZZEL"



source("do_the_loop.R")
df$date2 <- as.Date(df$date, "%B %d, %Y")
df$supervisor <- factor(df$supervisor)

library(ggplot2)
theme_set(theme_minimal())
library(dplyr)

# Length of Tenure by Supervisor
df %>% 
  filter(supervisor == "DAVIS" |
           supervisor == "PEER" |
           supervisor == "FOX" | 
           supervisor == "DIESTLER" | 
           supervisor == "ARNOLD" | 
           supervisor == "BEAVER") %>%
  ggplot(aes(date2, supervisor)) + geom_line() + 
  scale_x_date(date_breaks = "2 years")

# Absenteeism 
tmp <- df %>% 
  filter(supervisor == "DAVIS" |
           supervisor == "PEER" |
           supervisor == "FOX" | 
           supervisor == "DIESTLER" | 
           supervisor == "ARNOLD" | 
           supervisor == "BEAVER") %>% 
  group_by(supervisor, status) %>% 
  reframe(tally = n()) 
df %>% 
  filter(supervisor == "DAVIS" |
           supervisor == "PEER" |
           supervisor == "FOX" | 
           supervisor == "DIESTLER" | 
           supervisor == "ARNOLD" | 
           supervisor == "BEAVER") %>% 
  group_by(supervisor) %>% 
  reframe(total = n()) %>% 
  left_join(tmp, by = "supervisor", multiple = "all") %>% 
  mutate("percent" = (tally / total)*100) 

# Column chart of absences by supervisor
df %>% 
  # filter(supervisor == "BEAVER" |
  #          supervisor == "PEER" | 
  #          supervisor == "DIESTLER") %>% 
  filter(supervisor == "DAVIS" |
           supervisor == "PEER" |
           supervisor == "FOX" | 
           supervisor == "DIESTLER" | 
           supervisor == "ARNOLD" | 
           supervisor == "BEAVER") %>% 
  ggplot(aes(status, fill = status)) + 
  geom_bar() + facet_wrap(~supervisor)






