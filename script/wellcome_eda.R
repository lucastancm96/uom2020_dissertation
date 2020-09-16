install.packages("gridExtra")
install.packages("finalfit")
install.packages("ggridges")
library(ggplot2)
library(naniar)
library(finalfit)
library("gridExtra")
source("http://peterhaschke.com/Code/multiplot.R")

#********** Visualization Packages ***********
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

df = read.csv('csv_data/Wellcome/full_data.csv')

#********** Missing Data Visualization **********#
# Create density plots for SC_SS and SC_UNI
x <- df %>%
  bind_shadow() %>%
  ggplot(aes(x = SC_PS,
             fill = SC_SS_NA)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()

y <- df %>%
  bind_shadow() %>%
  ggplot(aes(x = SC_PS,
             fill = SC_UNI_NA)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()

z <- df %>%
  bind_shadow() %>%
  ggplot(aes(x = SC_SS,
             fill = SC_UNI_NA)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()

# Create density plot for CH_RVAC
a <- df %>%
  bind_shadow() %>%
  ggplot(aes(x = ANY_CH,
             fill = CH_RVAC_NA)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()

# Choose either way to plot
multiplot(x,y,z)
grid.arrange(x, y, z, a, nrow=2, ncol=2, top = textGrob("Missing data distribution for SC_SS, SC_UNI, and CH_RVAC"))

# Missing data distribution
df %>%
  missing_plot(title="WGM 2018 Missing Data")

#********** Visualization for comparison of variables *********#
# AGE_CAT response to Vaccine Questions
df <- read.csv('csv_data/Wellcome/ridgedist_df.csv')
x <- ggplot(df, aes(x = VAC_IMPTCH, y = AGE_CAT, fill = AGE_CAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = AGE_CAT, fill = AGE_CAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = AGE_CAT, fill = AGE_CAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Age Category")

# GEN response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = GEN, fill = GEN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = GEN, fill = GEN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = GEN, fill = GEN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Gender")

# EDU response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = EDU, fill = EDU)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = EDU, fill = EDU)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = EDU, fill = EDU)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Education Level")

# LIV_AR response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = LIV_AR, fill = LIV_AR)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = LIV_AR, fill = LIV_AR)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = LIV_AR, fill = LIV_AR)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Living Area Type")

# HHI response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = HHI, fill = HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = HHI, fill = HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = HHI, fill = HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Household Income")

# RGN response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = RGN, fill = RGN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = RGN, fill = RGN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = RGN, fill = RGN)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across World Region")

# SUBJ_HHI response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = SUBJ_HHI, fill = SUBJ_HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = SUBJ_HHI, fill = SUBJ_HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = SUBJ_HHI, fill = SUBJ_HHI)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Subjective Household Income")

# CTRY_INC response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = CTRY_INC, fill = CTRY_INC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = CTRY_INC, fill = CTRY_INC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = CTRY_INC, fill = CTRY_INC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Distribution of Responses Towards Vaccine Questions Across Country Income Level")

# EMP_STAT response to Vaccine Questions
x <- ggplot(df, aes(x = VAC_IMPTCH, y = EMP_STAT, fill = EMP_STAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = EMP_STAT, fill = EMP_STAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = EMP_STAT, fill = EMP_STAT)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Vaccine Questions Responses Distribution Across Employment Status")

# ANY_CH with Vaccine Attitudes
df <- read.csv('csv_data/Wellcome/ridgedistvac_df.csv')
x <- ggplot(df, aes(x = VAC_IMPTCH, y = ANY_CH, fill = ANY_CH)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = ANY_CH, fill = ANY_CH)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = ANY_CH, fill = ANY_CH)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Vaccine Questions Responses Distribution Across Children Status")

# CH_RVAC with Vaccine Attitudes
x <- ggplot(df, aes(x = VAC_IMPTCH, y = CH_RVAC, fill = CH_RVAC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = VAC_SF, y = CH_RVAC, fill = CH_RVAC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
z <- ggplot(df, aes(x = VAC_EFF, y = CH_RVAC, fill = CH_RVAC)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Vaccine Questions Responses Distribution Across Children Vaccination Status")

#*********** Demographics: AGE **********#
df <- read.csv('csv_data/Wellcome/full_data.csv')
ggplot(df, aes(x=AGE)) + 
  geom_histogram(aes(y=..density..), fill="#4787A8", color="#e9ecef", alpha=0.9) +
  ggtitle('WGM 2018 Age Distribution') +
  geom_density() +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15)
  )

#********** AGE vs VAC **********#
df <- read.csv('csv_data/Wellcome/vac_age.csv')
x <- ggplot(df, aes(x = AGE, y = VAC_IMPTCH, fill = VAC_IMPTCH)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")
y <- ggplot(df, aes(x = AGE, y = VAC_SF, fill = VAC_SF)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

z <- ggplot(df, aes(x = AGE, y = VAC_EFF, fill = VAC_EFF)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

grid.arrange(x, y, z, nrow=3, ncol=1, top = "Vaccine Questions Responses Distribution Across Age")

#********** Test Code **********#
# Boxplot
ggplot(df, aes(x = VAC_IMPTCH, y = AGE, fill = VAC_IMPTCH)) + 
  geom_boxplot(size = .75) +   facet_grid(cols = vars(GEN)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(df, aes(x = VAC_IMPTCH, y = AGE, fill = VAC_IMPTCH)) + 
  geom_boxplot(size = .75) +   facet_grid(CTRY ~ GEN, margins = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(df, aes(x=GEN, y=VAC_IMPTCH, group=GEN)) + 
  geom_boxplot(aes(fill=GEN))

z <- ggplot(df, aes(x = VAC_EFF, y = GEN)) +
 geom_density_ridges(alpha = 0.1, fill = "blue", colour = "blue") +
 theme_ridges()
