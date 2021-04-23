
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, gtsummary)

df_sum <- df %>%
	dplyr::select(lnwtp, lnyear, local, prov, reg, cult, lninc, forest, volunt,
		   lumpsum, ce, nrev, lnq_change, us) 
#Summary Stats for whole data
sum_whole <-  df_sum %>% descr(stats = "common") %>% tb()

# Grouped summary statis
grouped_sum <- df_sum %>% group_by(us) %>% descr(stats = "common") %>% tb()

#saving data
write_csv(sum_whole, "output/tables/sum_whole_23_3_21.csv")
write_csv(grouped_sum, "output/tables/grouped__23_3_21.csv")
