library(dplyr)
library(ggplot2)

df <- read.csv("D:/AZ/adni/objects/clusters_mci.csv", stringsAsFactors = F, row.names = 1)

df$cluster <- factor(df$cluster, levels = c("MCI_Healthy", "MCI_Middle", "MCI_AD"))


tiff("D:/AZ/adni/pics/pulse.tiff", units="in", width=5, height=3, res=300)
ggplot(df, aes(cluster, VSPULSE)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=12))
dev.off()

kruskal.test(VSPULSE ~ cluster, df)
FSA::dunnTest(VSPULSE ~ cluster, df)



tiff("D:/AZ/adni/pics/MHPSYCH_bar.tiff", units="in", width=5, height=3, res=300)
df %>%
  group_by(cluster) %>%
  summarise(GDDROP = round(sum(NXGAIT == 1) / n() * 100,2) ) %>%
  ggplot(aes(cluster, GDDROP)) + 
  geom_bar(stat = 'identity') +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=12))

dev.off()


table(for_bxplots$cluster, for_bxplots$GDTOTAL)
chisq.test(table(df$cluster, df$PTHOME), simulate.p.value = T)



contingency_table <- table(df$cluster, df$GDDROP)


# Omnibus chi-squared test
chisq_test <- chisq.test(contingency_table)
print(chisq_test)

groups <- dimnames(contingency_table)[[1]]
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    pairwise_table <- contingency_table[c(i, j), ]
    test_result <- chisq.test(pairwise_table)
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- test_result
  }
}



pairwise_pvalues <- sapply(pairwise_results, function(x) x$p.value)
sapply(pairwise_results, function(x) x$statistic)
adjusted_pvalues <- p.adjust(pairwise_pvalues, method = "bonferroni")  # Or "BH"
names(adjusted_pvalues) <- names(pairwise_pvalues)
print(adjusted_pvalues)



# Progressors

diag <- read.csv("D:/AZ/adni/raw/outcome.csv", stringsAsFactors = F)
mci_df <- read.csv("D:/AZ/adni/objects/df_imputed_not_complete_812_p.csv", row.names = 1)

outcome <- diag %>%
  dplyr::select(RID, EXAMDATE, DIAGNOSIS) %>%
  mutate(EXAMDATE = as.Date(EXAMDATE)) %>%
  group_by(RID) %>%
  arrange(EXAMDATE, .by_group = T) %>%
  select(RID, DIAGNOSIS) %>%
  summarise(
    first_diagnosis = first(DIAGNOSIS),  # Get the first diagnosis
    last_diagnosis = last(DIAGNOSIS),    # Get the last diagnosis
    .groups = 'drop'  # Ungroup the data after summarizing
  ) %>%
  filter(first_diagnosis %in% c(1,2,3) & last_diagnosis %in% c(1,2,3)) %>%
  mutate(status = case_when(
    first_diagnosis < last_diagnosis ~ "Progressor",
    first_diagnosis >= last_diagnosis ~ "Stable",
    TRUE ~ "Unknown"  # Handle any unexpected cases
  )) %>%
  mutate(status2 = case_when(first_diagnosis == 1 & last_diagnosis == 2 ~ "CtrToMCI",
                             first_diagnosis == 1 & last_diagnosis == 3 ~ "CtrToAD",
                             first_diagnosis == 1 & last_diagnosis == 1 ~ "CtrStable",
                             first_diagnosis == 2 & last_diagnosis == 3 ~ "MCIToAD",
                             first_diagnosis == 2 & last_diagnosis == 1 ~ "MCIToCtr",
                             first_diagnosis == 2 & last_diagnosis == 2 ~ "MCIStable",
                             first_diagnosis == 3 & last_diagnosis == 2 ~ "ADToMCI",
                             first_diagnosis == 3 & last_diagnosis == 1 ~ "ADToCtr",
                             first_diagnosis == 3 & last_diagnosis == 3 ~ "ADStable") )%>%
  #filter(RID %in% df$RID) %>%
  select(RID, status2, status)


outcome_mci <- outcome %>%
  inner_join(df[,c("RID", "cluster")], by="RID")

table(outcome_mci$cluster, outcome_mci$status2)
prop.table(table(outcome_mci$cluster, outcome_mci$status2), 1)



contingency_table <- table(outcome_mci$cluster, outcome_mci$status2)


# Omnibus chi-squared test
chisq_test <- chisq.test(contingency_table)
print(chisq_test)

groups <- dimnames(contingency_table)[[1]]
pairwise_results <- list()

for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    pairwise_table <- contingency_table[c(i, j), ]
    test_result <- chisq.test(pairwise_table)
    pairwise_results[[paste(groups[i], "vs", groups[j])]] <- test_result
  }
}



pairwise_pvalues <- sapply(pairwise_results, function(x) x$p.value)
sapply(pairwise_results, function(x) x$statistic)
adjusted_pvalues <- p.adjust(pairwise_pvalues, method = "bonferroni")  # Or "BH"
names(adjusted_pvalues) <- names(pairwise_pvalues)
print(adjusted_pvalues)









for_plot_AD <- data.frame(Cluster = c("MCI_Healthy", "MCI_Middle", "MCI_AD"),
                          Prop = c(19, 41, 53) )

for_plot_stable <- data.frame(Cluster = c("MCI_Healthy", "MCI_Middle", "MCI_AD"),
                              Prop = c(63, 54, 44) )

for_plot_rev <- data.frame(Cluster = c("MCI_Healthy", "MCI_Middle", "MCI_AD"),
                           Prop = c(19, 5, 3) )


tiff("D:/AZ/adni/pics/progressors_stable.tiff", units="in", width=5, height=3, res=300)
for_plot_stable %>%
  mutate(Cluster = factor(Cluster, levels = c("MCI_Healthy",
                                              "MCI_Middle",
                                              "MCI_AD")) ) %>%
  ggplot(aes(y = Prop, x = Cluster))+
  geom_bar(stat = "identity") + 
  labs(y = "Progressors, %") +
  #scale_y_continuous(breaks=c(1,3,5,7,9))+
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=12))
dev.off()


for_plot_comb <- data.frame(Cluster = rep(c("MCI_Healthy", "MCI_Middle", "MCI_AD"),3),
                            Status = c(rep("MCItoAD",3), rep("Stable", 3), rep("MCItoHC", 3)),
                            Prop = c(19, 41, 53, 63, 54, 44, 19, 5, 3) )


for_plot_comb %>%
  mutate(Cluster = factor(Cluster, levels = c("MCI_Healthy",
                                              "MCI_Middle",
                                              "MCI_AD")),
         Status = factor(Status, level = c("Stable", "MCItoHC", "MCItoAD") )) %>%
  ggplot(aes(x = Cluster, y = Prop, fill = Status))+
  geom_bar(stat = "identity") +
  labs(y = "Progressors, %") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=12))
