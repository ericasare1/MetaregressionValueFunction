
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, gtsummary)

df_sum <- df %>%
	dplyr::select(lnwtp, wtp_original, lnyear, q0, q1, lnq0, lnq_change, us, local, prov, reg, cult, lninc, forest, volunt, lumpsum, ce, nrev) 
#Summary Stats for whole data
sum_whole <-  df_sum %>% descr(stats = "common") %>% tb()

# Grouped summary statis
grouped_sum <- df_sum %>% group_by(us) %>% descr(stats = "common") %>% tb()
grouped_sum %>% View()
#saving data
write_csv(sum_whole, "output/sum_whole_5_5_21.csv")
write_csv(grouped_sum, "output/grouped_5_5_21.csv")


#plot lnwtp vrs lnacres 
df <- df %>% 
  mutate(
  Country = ifelse(us ==1, "US", "Canada")
)
df %>% ggplot(aes(x=lnq_change,y=lnwtp, color= Country)) + 
  geom_point() +
  labs(
    x = "Log (Willingness to Pay)",
    y = "Log (Wetland Acreage Change)"
  ) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  theme(legend.position="bottom")
ggsave("output/lnwtp_vrs_lnchange.png", , width=11, height=8.5)
