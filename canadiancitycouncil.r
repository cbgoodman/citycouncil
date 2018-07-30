library(tidyverse)

# read in data
canada <- read.csv("canadiancitycouncil.csv")

# calculate councilors per 100,000 population
canada$pc.council <- canada$council/(canada$pop16/100000)

annot <- read.table(text=
  "city|number|just|text
  33.5|2.7|0|Current number<br>of councilors - 44.
  41.5|2.6|1|Proposed number<br>of councilors - 25.
  16.5|5.4|0|Richmond Hill is the median city with<br>4.03 councilors per 100,000 residents.",
  sep="|", header=TRUE, stringsAsFactors=FALSE)
annot$text <- gsub("<br>", "\n", annot$text)

# plot
c <- ggplot(data=canada,
  aes(x=reorder(city, -pc.council), y=pc.council, width=0.9, fill=highlight))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("yes"="#E69F00", "no"="#999999" ), guide = FALSE )+
  scale_y_continuous(breaks=seq(0, 12, by=2))+
  # read in annotations
  geom_label(data=annot, aes(x=city, y=number, label=text, hjust=just),
    family="Open Sans Condensed Light", lineheight=0.95,
    size=3, label.size=0, color="#2b2b2b", inherit.aes = FALSE)+
  # Theming
  labs(
    title="Doug Ford's proposal will result in fewest city councilors per capita among the largest Canadian cities",
    subtitle="City councilors per 100,000 residents, 40 largest Canadian cities, 2016",
    caption="Author: Chris Goodman (@cbgoodman), Data: Statistics Canada & Author's calculations.",
    y=NULL,
    x=NULL) +
  theme_minimal(base_family="Open Sans Condensed Light") +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, angle=90, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  #theme(axis.text.y=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))

ggsave(plot=c, "canadiancouncilors.png", width=10, height=6, units="in", dpi="retina")
