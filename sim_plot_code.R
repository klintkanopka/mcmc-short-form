library(tidyverse)

#setwd('~/Dropbox/ShortForming MCMC')

d_float <- read_csv('results_float_k_500_tiebreak.csv')
d_3 <- read_csv('results_10_3.csv')
d_5 <- read_csv('results_10_5.csv')

d_3 |>
  pivot_longer(starts_with('item'), names_to='item', names_prefix='item_',
               values_to='value') |>
  group_by(J_star, item) |>
  summarize(p = mean(value),
            p_se = sqrt(p*(1-p)/length(value))) |>
  ggplot(aes(x=as.numeric(item), y=p, color=as.factor(J_star))) +
  geom_point() + geom_errorbar(aes(ymin = p-2*p_se, ymax = p + 2*p_se), width=0)+
  labs(title = "MCMC Fixed K, iter = 1e3", x = "item", y = "p") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "J*") +
  theme_minimal()

d_5 |>
  pivot_longer(starts_with('item'), names_to='item', names_prefix='item_',
               values_to='value') |>
  group_by(J_star, item) |>
  summarize(p = mean(value),
            p_se = sqrt(p*(1-p)/length(value)),
            .groups='drop') |>
  ggplot(aes(x=as.numeric(item), y=p, color=as.factor(J_star))) +
  geom_vline(aes(xintercept = 10), color='#66BBDD', lty=2, alpha=0.6) +
  geom_vline(aes(xintercept = 7), color='#FF0000', lty=2, alpha=0.6) +
  geom_vline(aes(xintercept = 14), color='#9954E6', lty=2, alpha=0.6) +
  geom_point() +
  geom_errorbar(aes(ymin = p-2*p_se, ymax = p + 2*p_se), width=0)+
  labs(x = "Item", y = "Proportion of short forms including item") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "J*") +
  #facet_grid(.~round(q,2)) +
  theme_minimal()

ggsave('fig/fixed-k-forms-retained.png', height=5, width=6)


d_float |>
  pivot_longer(starts_with('item'), names_to='item', names_prefix='item_',
               values_to='value') |>
  group_by(J_star, item) |>
  summarize(p = mean(value),
            p_se = sqrt(p*(1-p)/length(value)),
            .groups='drop') |>
  ggplot(aes(x=as.numeric(item), y=p, color=as.factor(J_star))) +
  geom_vline(aes(xintercept = 10), color='#66BBDD', lty=2, alpha=0.6) +
  geom_vline(aes(xintercept = 7), color='#FF0000', lty=2, alpha=0.6) +
  geom_vline(aes(xintercept = 14), color='#9954E6', lty=2, alpha=0.6) +
  geom_point() +
  geom_errorbar(aes(ymin = p-2*p_se, ymax = p + 2*p_se), width=0)+
  labs(x = "Item", y = "Proportion of short forms including item") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "J*") +
  theme_minimal()

ggsave('fig/float-k-forms-retained.png', height=5, width=6)


d_3 |>
  group_by(J_star, rep, q) |>
  filter(bw_accuracy == max(bw_accuracy)) |>
  ungroup() |>
  mutate(info_count = case_when(J_star==0.33 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7,
                                J_star==0.50 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10,
                                J_star==0.67 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10 + item_11 + item_12 + item_13 + item_14,
                                TRUE ~ 6666666666)) |>
  mutate(info_prop = info_count/k) |>
  ggplot(aes(x=as.factor(k), y=info_prop)) +
  geom_boxplot(color = "darkblue", fill = "#66BBDD", alpha = 0.7) +
  labs(title = "MCMC Fixed K, iter = 1e3", x = "k", y = "proportion informative") +
  theme_minimal()

d_5 |>
  group_by(J_star, rep, q) |>
  filter(bw_accuracy == max(bw_accuracy)) |>
  filter(k == min(k)) |>
  ungroup() |>
  mutate(info_count = case_when(J_star==0.33 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7,
                                J_star==0.50 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10,
                                J_star==0.67 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10 + item_11 + item_12 + item_13 + item_14,
                                TRUE ~ 6666666666)) |>
  mutate(info_prop = case_when(J_star==0.33 ~ info_count/7,
                               J_star==0.50 ~ info_count/10,
                               J_star==0.67 ~ info_count/14,
                               TRUE ~ 6666666666)) |>
  group_by(J_star, k) |>
  summarize(info_mean = mean(info_prop),
            info_sd = sd(info_prop),
            .groups='drop') |>
  ggplot(aes(x=k, y=info_mean, color=as.factor(J_star), fill=as.factor(J_star))) +
  geom_line() +
  labs(x = "Form length",
       y = "Proportion of informative items in best form") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "J*") +
  scale_x_continuous(breaks=4:20) +
  theme_minimal()

ggsave('fig/fixed-k-info-prop.png', height=5, width=6)



d_float |>
  group_by(J_star, rep, q) |>
  filter(bw_accuracy == max(bw_accuracy)) |>
  filter(k == min(k)) |>
  ungroup() |>
  mutate(info_count = case_when(J_star==0.33 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7,
                                J_star==0.50 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10,
                                J_star==0.67 ~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6 + item_7 + item_8 + item_9 + item_10 + item_11 + item_12 + item_13 + item_14,
                                TRUE ~ 6666666666)) |>
  mutate(info_prop = case_when(J_star==0.33 ~ info_count/7,
                               J_star==0.50 ~ info_count/10,
                               J_star==0.67 ~ info_count/14,
                               TRUE ~ 6666666666)) |>
  group_by(J_star, k) |>
  summarize(info_mean = mean(info_prop),
            info_sd = sd(info_prop),
            .groups='drop') |>
  ggplot(aes(x=k, y=info_mean, color=as.factor(J_star), fill=as.factor(J_star))) +
  geom_line() +
  labs(x = "Form length",
       y = "Proportion of informative items in best form") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "J*") +
  scale_x_continuous(breaks=4:20) +
  theme_minimal()

ggsave('fig/float-k-info-prop.png', height=5, width=6)


d_3 |>
  ggplot(aes(x=prop_agree))+
  geom_bar(color = "darkblue", fill = "#66BBDD", alpha = 0.7)+
  labs(title = "MCMC Fixed K, iter = 1e3") +
  theme_minimal()

d_5|>
  ggplot(aes(x=prop_agree))+
  geom_bar(color = "darkblue", fill = "#66BBDD", alpha = 0.7)+
  labs(x = 'Proportion of five chains that agree on best solution',
       y = 'Number of simulation conditions') +
  scale_x_continuous(breaks=seq(0,1, by=0.2)) +
  theme_minimal()

ggsave('fig/fixed-k-agree.png', height=5, width=6)


d_float |>
  ggplot(aes(x=prop_agree))+
  geom_bar(color = "darkblue", fill = "#66BBDD", alpha = 0.7)+
  labs(x = 'Proportion of five chains that agree on best solution',
       y = 'Number of simulation conditions') +
  scale_x_continuous(breaks=seq(0,1, by=0.2)) +
  theme_minimal()

ggsave('fig/float-k-agree.png', height=5, width=6)



best_d5 <- d_5 |>
  group_by(rep, q, J_star) |>
  filter(bw_accuracy == max(bw_accuracy)) |>
  filter(k == min(k)) |>
  ungroup() |>
  select(k_full = k, bw_accuracy_full = bw_accuracy, q, J_star, rep)

eval <- d_float |>
  left_join(best_d5, by=c('q', 'J_star', 'rep'))

eval |>
  mutate(delta_bwa = bw_accuracy - bw_accuracy_full,
         delta_k = k - k_full) |>
  filter(delta_k > 0 | delta_bwa < 0) |>
  ggplot(aes(x=delta_k, y=delta_bwa, color=as.factor(J_star))) +
  geom_point(size=4) +
  labs(x = "Difference in form length", y='Difference in weighted balanced accuracy') +
  scale_color_manual(values=c('0.33'='#FF0000',
                             '0.5'='#66BBDD',
                             '0.67' = '#9954E6'),
                    name = "J*") +
  scale_x_continuous(breaks=-3:9) + ylim(-0.005, 0) +
  theme_minimal()


ggsave('fig/float-vs-fixed.png', height=5, width=6)







# in the new simulation with tie breaking there were 8 values that were different
# but there were fewer that found different k given the same bwa?? (20-4 vs. 21-8)


d_3 |>
  select(k, q, J_star, bw_accuracy, rep) |>
  group_by(k, q, J_star) |>
  summarize(mean_bwa = mean(bw_accuracy),
            sd_bwa = sd(bw_accuracy),
            .groups='drop') |>
  ggplot(aes(x=k, y=mean_bwa, ymax=mean_bwa+1.96*sd_bwa,
             ymin=mean_bwa-1.96*sd_bwa)) +
  geom_line(aes(color=as.factor(round(q,2)))) +
  geom_ribbon(aes(fill=as.factor(round(q,2))), alpha=0.3) +
  facet_grid(q~J_star) +
  labs(title = "MCMC Fixed K, iter = 1e3") +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "q") +
  scale_fill_manual(values=c('0.33'='#FF0000',
                             '0.5'='#66BBDD',
                             '0.67' = '#9954E6'),
                    name = "q") +
  theme_minimal()

d_5 |>
  select(k, q, J_star, bw_accuracy, rep) |>
  group_by(k, q, J_star) |>
  summarize(mean_bwa = mean(bw_accuracy),
            sd_bwa = sd(bw_accuracy),
            .groups='drop') |>
  ggplot(aes(x=k, y=mean_bwa, ymax=mean_bwa+1.96*sd_bwa,
             ymin=mean_bwa-1.96*sd_bwa)) +
  geom_line(aes(color=as.factor(round(q,2)))) +
  geom_ribbon(aes(fill=as.factor(round(q,2))), alpha=0.3) +
  facet_grid(round(q,2)~J_star) +
  labs(x='Form length',
       y='Weighted balanced accuracy') +
  scale_color_manual(values=c('0.33'='#FF0000',
                              '0.5'='#66BBDD',
                              '0.67' = '#9954E6'),
                     name = "q") +
  scale_fill_manual(values=c('0.33'='#FF0000',
                             '0.5'='#66BBDD',
                             '0.67' = '#9954E6'),
                    name = "q") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name=expression('J*'), breaks = NULL, labels = NULL)) +
  #scale_y_continuous(sec.axis = sec_axis(~ . , name=expression('q'), breaks = NULL, labels = NULL)) +
  theme_minimal()

ggsave('fig/fixed-k-wba.png', height=5, width=6)

