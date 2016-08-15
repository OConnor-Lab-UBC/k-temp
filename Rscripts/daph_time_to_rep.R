### Time to first reproduction


time_to_clutch <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data-raw/time_to_reproduction.csv")

ggplot(data = time_to_clutch, aes(x = Temperature, y = `Days to first clutch`)) + geom_point(size = 4, color = "white") +
	geom_jitter(position = position_jitter(width = 1, height = 0.5), size = 8, color = "white") + 
theme_bw() +
	theme(
		# panel.border = element_blank(),
		# legend.key = element_blank(),
		axis.ticks = element_line(color = "white"),
		# axis.text.y = element_blank(),
		# axis.text.x = element_blank(),
		# panel.grid = element_blank(),
		panel.grid.minor = element_blank(), 
		panel.grid.major = element_blank(),
		panel.background = element_blank(),
		panel.border = element_rect(color = "white"),
		plot.background = element_rect(fill = "transparent",colour = NA)) + 
	theme(axis.text=element_text(size=40, color = "white"),
				axis.title=element_text(size=40,face="bold", color = "white"))

ggsave("time_to_rep.png", bg = "transparent", width = 10, height = 8)


ggsave("test.png", bg = "transparent")
