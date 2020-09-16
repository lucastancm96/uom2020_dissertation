install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

df <-read.csv('csv_data/mfa_test.csv')
df <- subset(df[6:10])
for(i in 1:ncol(df)){
  
  df[,i] <- as.factor(df[,i])
  
}

for (i in 1:ncol(df)){
  print(class(i))
}

# Must at least two variables in a group. Cannot group = c(1,1,1)
res.mfa <- MFA(df, 
               group = c(3, 2),
               type = c("n", "n"),
               name.group = c("vacatt","child"),
               num.group.sup = NULL,
               graph = FALSE)
print(res.mfa)
fviz_screeplot(res.mfa)
fviz_mfa_var(res.mfa, "group")
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)
quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

#df <- sapply(df, as.factor)

save.image('r_data/factor_analysis.RData')