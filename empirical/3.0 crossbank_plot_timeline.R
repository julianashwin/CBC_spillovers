setwd("~/Documents/GitHub/CBC_spillovers")
rm(list=ls())

library(ggplot2)
library(scales)
library(lubridate)

### Define the directories where raw data is stored and clean will be saved
clean_dir <- "~/Documents/DPhil/Clean_Data/"
raw_dir <- "~/Documents/DPhil/Raw_Data/"
export_dir <- "/Users/julianashwin/Documents/GitHub/CBC_spillovers/empirical/figures/"

### Import the Federal Reserve data
meeting.df <- read.csv(paste0(clean_dir, "CBC/meetingtopicprops.csv"), encoding = "utf-8", stringsAsFactors = FALSE)
meeting.df$meet_date <- as.Date(meeting.df$meet_date)


dates.df <- meeting.df[which(meeting.df$meet_date >= "2000-01-01" & 
                               meeting.df$meet_date < "2001-01-01"),c("meeting_id", "central_bank", "meet_date")]
dates.df <- dates.df[with(dates.df, order(meet_date)),]


status_levels <- c("European Central Bank", "Bank of England", "Federal Reserve")
status_colors <- c("darkgoldenrod2",  "black","blue3")

dates.df$central_bank <- factor(dates.df$central_bank, levels = status_levels, ordered = TRUE)

# Set positions and directions
positions <- c(0.03, -0.03, 0.06, -0.06, 0.09, -0.09)
directions <- c(1, -1)

line_pos <- data.frame(
  "meet_date"=unique(dates.df$meet_date),
  "position"=rep(positions, length.out=length(unique(dates.df$meet_date))),
  "direction"=rep(directions, length.out=length(unique(dates.df$meet_date)))
)

dates.df <- merge(dates.df, line_pos, by = "meet_date", all = TRUE)
dates.df <- dates.df[with(dates.df, order(meet_date, central_bank)),]
head(dates.df)

# Offset the text in case there are multiple meetings on the same day
text_offset <- 0.05

dates.df$date_count <- ave(dates.df$meet_date==dates.df$meet_date, dates.df$meet_date, FUN=cumsum)
dates.df$text_position <- (dates.df$date_count * text_offset * dates.df$direction) + dates.df$position
head(dates.df)

# Set up the month labels
month_buffer <- 2

month_date_range <- seq(min(dates.df$meet_date) - months(month_buffer), max(dates.df$meet_date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

# And the year labels
year_date_range <- seq(min(dates.df$meet_date) - months(month_buffer), 
                       max(dates.df$meet_date) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)


# Set up the environment
timeline_plot<-ggplot(dates.df,aes(x=meet_date,y=0, col=central_bank, label=meeting_id))
timeline_plot<-timeline_plot+labs(col="Central Bank")
timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Horizontal line
timeline_plot<-timeline_plot+geom_hline(yintercept=0, color = "black", size=0.3)

# Vertical lines for meetings
timeline_plot<-timeline_plot+geom_segment(data=dates.df, 
  aes(y=position,yend=0,xend=meet_date), color='white', size=0.2)

# Points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3, alpha = 0.5)

# Remove axis and ticks and place legend below
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom")
#timeline_plot <- timeline_plot + scale_x_discrete(labels = month_date_range)

# Show text for months
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.05,label=month_format),
                                       size=2.5,vjust=0.0, color='black', angle=45)



month_dates <- as.Date(month_df$month_date_range)
timeline_plot<-timeline_plot + 
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[1], x = month_dates[1]), color='black' , size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[2], x = month_dates[2]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[3], x = month_dates[3]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[4], x = month_dates[4]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[5], x = month_dates[5]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[6], x = month_dates[6]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[7], x = month_dates[7]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[8], x = month_dates[8]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[9], x = month_dates[9]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[10], x = month_dates[10]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[11], x = month_dates[11]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[12], x = month_dates[12]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[13], x = month_dates[13]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[14], x = month_dates[14]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[15], x = month_dates[15]), color='black', size=0.2) +
  geom_segment(aes(y=-0.025,yend=0,xend=month_dates[16], x = month_dates[16]), color='black', size=0.2)



# Show year text
#timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, 
#                                                         fontface="bold"),size=2.5, color='black')


# Show text for each meeting
#timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=meeting_id),size=2.5)
timeline_plot
ggsave(paste0(export_dir, "meeting_timeline.pdf"), height = 1.5, width = 8)


###
