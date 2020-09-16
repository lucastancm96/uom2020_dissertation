library(olsrr)
library(MASS)
library(nnet)

modtrain_ch <- read.csv("csv_data/Wellcome/modtrain_ch.csv")
modtest_ch <- read.csv("csv_data/Wellcome/modtest_ch.csv")

modtrain_ch$AGE
for (i in colnames(modtrain_ch)){
  modtrain_ch$i <- factor(modtrain_ch$i, levels=sort(unique(modtrain_ch$i)))
}
class(modtrain_ch$AGE)

lr <- multinom(VAC_LVL ~ AGE_CAT + GEN + EDU + LIV_AR + RGN + SUBJ_HHI + CTRY_INC + EMP_STAT, 
           data=modtrain_ch)
summary(lr)

z <- summary(lr)$coefficients/summary(lr)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

lr <- mlogit()

#********** Testing **********
# Train
modtrain_ch$VAC_LVL <- factor(modtrain_ch$VAC_LVL, levels=sort(unique(modtrain_ch$VAC_LVL)))
modtrain_ch$AGE_CAT <- factor(modtrain_ch$AGE_CAT, levels=sort(unique(modtrain_ch$AGE_CAT)))
modtrain_ch$GEN <- factor(modtrain_ch$GEN, levels=sort(unique(modtrain_ch$GEN)))
modtrain_ch$EDU <- factor(modtrain_ch$EDU, levels=sort(unique(modtrain_ch$EDU)))
modtrain_ch$LIV_AR <- factor(modtrain_ch$LIV_AR, levels=sort(unique(modtrain_ch$LIV_AR)))
modtrain_ch$RGN <- factor(modtrain_ch$RGN, levels=sort(unique(modtrain_ch$RGN)))
var <- c('VAC_LVL', 'AGE_CAT', 'GEN', 'EDU')
partial_modtrain_ch <- modtrain_ch[var]

# Test
modtest_ch$VAC_LVL <- factor(modtest_ch$VAC_LVL, levels=sort(unique(modtest_ch$VAC_LVL)))
modtest_ch$AGE_CAT <- factor(modtest_ch$AGE_CAT, levels=sort(unique(modtest_ch$AGE_CAT)))
modtest_ch$GEN <- factor(modtest_ch$GEN, levels=sort(unique(modtest_ch$GEN)))
modtest_ch$EDU <- factor(modtest_ch$EDU, levels=sort(unique(modtest_ch$EDU)))
modtest_ch$LIV_AR <- factor(modtest_ch$LIV_AR, levels=sort(unique(modtest_ch$LIV_AR)))
modtest_ch$RGN <- factor(modtest_ch$RGN, levels=sort(unique(modtest_ch$RGN)))
partial_modtest_ch <- modtest_ch[var]

lr <- multinom(VAC_LVL ~ ., data=partial_modtrain_ch)
summary(lr)
predicted_scores <- predict (lr, partial_modtest_ch, "probs") # predict on new data
predicted_class <- predict (lr, partial_modtest_ch)
mean(as.character(predicted_class) != as.character(partial_modtest_ch$VAC_LVL))

#********** Variables ***********
# 'SC_KNOWL', 'UND_SCSCI', 'SC_DZ', 'SC_POET', 'SC_PS', 'SC_SS', 'SC_UNI',
# 'SCINFO_30D', 'MDHINFO_30D', 'SC_INT', 'MDH_INT', 'CONF_NGO',
# 'CONF_HOSP', 'TRU_NEIGHB', 'TRU_SCI', 'TRU_JO', 'TRU_HCW', s'TRU_NGOPPL',
# 'TRU_TH', 'TRU_SC', 'TRUSCI_ACCINFO', 'TRUSCIUNI_BEN', 'TRUSCIUNI_HON',
# 'TRUSCICOM_BEN', 'TRUSCICOM_HON', 'SCI_BENPPL', 'SCI_BENRESP',
# 'SCTECH_IMPRLIFE', 'SCTECH_JOBS', 'TRU_PPLADV', 'TRU_HCWADV', 'VAC_LVL',
# 'CH_RVAC', 'TRUSCI_IDX', 'TRUSCI_LVL', 'VW_SC', 'AGE', 'AGE_CAT', 'GEN',
# 'EDU', 'LIV_AR', 'RGN', 'SUBJ_HHI', 'CTRY_INC', 'EMP_STAT'