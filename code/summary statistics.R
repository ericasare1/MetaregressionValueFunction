
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


#plot lnwtp vrs lnacres 
df <- df %>% 
  mutate(
  Country = ifelse(us ==1, "US", "Canada")
)
df %>% ggplot(aes(x=lnq_change,y=lnwtp, color= Country)) + 
  geom_point() +
  stat_smooth(method = "lm", 
              color = "black",
              se = FALSE,
              size = 0.2) +
  labs(
    x = "Log (Willingness to Pay)",
    y = "Log (Wetland Acreage Change)"
  ) +
  coord_flip() +
  theme(text =element_text(size = 14)) +
  theme_bw()

ggsave("output/lnwtp_vrs_lnchange.png")
