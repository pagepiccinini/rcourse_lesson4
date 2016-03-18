## LOAD PACKAGES ####
library(ggplot2)
library(babynames)
library(maps)
library(dplyr)


## READ IN DATA AND ORGANIZE ####
# Lesson example - 2014 most popular baby names
names2014 = read.table("data/rcourse_lesson4_data_prelesson.txt", header=T, sep="\t")

data_popnames_1901 = babynames %>%
  filter(year == 1901) %>%
  group_by(sex) %>%
  arrange(desc(prop)) %>%
  filter(row_number() <= 20) %>%
  ungroup() %>%
  select(name, sex)

data_names = babynames %>%
  inner_join(data_popnames_1901) %>%
  filter(year > 1900 & year <= 2000) %>%
  mutate(prop_log10 = log10(prop)) %>%
  mutate(century_half = ifelse(year <= 1950, "first", "second")) %>%
  mutate(sex = factor(sex, labels = c("female", "male"))) %>%
  group_by(name, sex, century_half) %>%
  summarise(prop_log10_mean = mean(prop_log10)) %>%
  ungroup()

# Lab example
state_sizes = map_data("state")

state_info = read.table("data/rcourse_lesson4_data_lesson.txt", header=T, sep="\t") %>%
  mutate(region = tolower(state))

data = inner_join(state_sizes, state_info)


## MAKE FIGURES ####
# Boxplot - year
year_boxplot.plot = ggplot(data_names, aes(x = century_half, y = prop_log10_mean)) +
  geom_point() +
  #geom_boxplot() +
  geom_smooth(aes(col = sex), method="lm") +
  ggtitle("Proportion of People with\n Popular Names for 1901") +
  xlab("Half of century") +
  ylab("Proportion of people\n(log base 10 transformed)") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

pdf("figures/lesson_year_dot.pdf")
year_boxplot.plot
dev.off()

# Boxplot - sex
sex_boxplot.plot = ggplot(data_names, aes(x = sex, y = prop_log10_mean)) +
  geom_point() +
  #geom_boxplot() +
  ggtitle("Proportion of People with\n Popular Names for 1901") +
  xlab("Sex") +
  ylab("Proportion of people\n(log base 10 transformed)") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

pdf("figures/lesson_sex_dot.pdf")
sex_boxplot.plot
dev.off()

# Boxplot
yearxsex_boxplot.plot = ggplot(data_names, aes(x = century_half, y = prop_log10_mean, fill = sex)) +
  geom_boxplot() +
  ggtitle("Proportion of People with\n Popular Names for 1901") +
  xlab("Half of century") +
  ylab("Proportion of people\n(log base 10 transformed)") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

pdf("figures/lesson_yearxsex.pdf")
yearxsex_boxplot.plot
dev.off()


## BUILD MODELS ####
popnames_year.lm = lm(prop_log10_mean ~ century_half, data = data_names)
summary(popnames_year.lm)

popnames_sex.lm = lm(prop_log10_mean ~ sex, data = data_names)
summary(popnames_sex.lm)

popnames.lm = lm(prop_log10_mean ~ century_half + sex, data = data_names)
summary(popnames.lm)

popnames_interaction.lm = lm(prop_log10_mean ~ century_half * sex, data = data_names)
summary(popnames_interaction.lm)

popnames.aov = aov(prop_log10_mean ~ century_half * sex, data = data_names)
summary(popnames.aov)

popnames_error.aov = aov(prop_log10_mean ~ century_half * sex + Error(name/century_half),
                         data = data_names)
summary(popnames_error.aov)


## LAB EXAMPLE ####
# Empty US map
us_empty.plot = ggplot(data, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color="black") +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

pdf("us_empty.pdf")
us_empty.plot
dev.off()

# Union map
us_union.plot = ggplot(subset(data, civil_war != "confederacy" | is.na(civil_war)), aes(x = long, y = lat, group = group)) +
  #geom_polygon(fill = "white", color="black") +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = "none")

png("us_union.png", bg = "transparent", width=800,height=500,units="px")
us_union.plot
dev.off()

# Confederate map
us_conf.plot = ggplot(subset(data, civil_war == "confederacy"), aes(x = long, y = lat, group = group)) +
  #geom_polygon(fill = "white", color="black") +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), legend.position = "none")

png("us_conf.png", bg = "transparent")
us_conf.plot
dev.off()


# Filled US map by Civil War
us_civilwar.plot = ggplot(data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(), legend.position = "none")

pdf("us_civilwar.pdf")
us_civilwar.plot
dev.off()

