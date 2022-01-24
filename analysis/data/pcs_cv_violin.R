library(tidyverse)
library(readr)

#変動係数CVはすべての正またはすべての負でいずれかの数値で計算された場合に最も情報量が多いことから、PCスコアを1から10の範囲に正規化する。

pca_small_combine2 <- readr::read_rds(here("analysis/data/pca_small_combine2.rds"))
pca_large_combine2 <- readr::read_rds(here("analysis/data/pca_large_combine2.rds"))

pca_small_combine2_tmp <-
  pca_small_combine2$x %>%
  as_tibble() %>%
  mutate_all(~scales::rescale(., to = c(1, 10))) %>%
  as.matrix() 

pca_large_combine2_tmp <- 
  pca_large_combine2$x %>% 
  as_tibble() %>% 
  mutate_all(~scales::rescale(., to = c(1, 10))) %>%
  as.matrix() 

row.names(pca_small_combine2_tmp) <- row.names(pca_small_combine2$x )
row.names(pca_large_combine2_tmp) <- row.names(pca_large_combine2$x )

#念のためコピーしたもので計算
pca_small_combine3 <- pca_small_combine2
pca_small_combine3$x <- pca_small_combine2_tmp

pca_large_combine3 <- pca_large_combine2
pca_large_combine3$x <- pca_large_combine2_tmp

# データがいくつあるか確認
len_small <- length(pca_small_combine3$x[,1])
len_large <- length(pca_large_combine3$x[,1])

# 第1〜第3主成分を時代毎に抽出
pc_plot_small <- tibble(pc = c(pca_small_combine3$x[,1],
                               pca_small_combine3$x[,2],
                               pca_small_combine3$x[,3]),
                        Phase= rep(pca_small_combine3$fac$Phase, 3),
                        pcn = as.character(str_glue(
                          'PC{sort(rep(1:3, len_small))}')))

pc_plot_large <- tibble(pc = c(pca_large_combine3$x[,1],
                               pca_large_combine3$x[,2],
                               pca_large_combine3$x[,3]),
                        Phase= rep(pca_large_combine3$fac$Phase, 3),
                        pcn = as.character(str_glue(
                          'PC{sort(rep(1:3, len_large))}')))

# 変動係数CVを計算し一覧表にする。
##group_by()でグループ化する 
##raster::cv();変動係数を計算する関数
##tidyr::pivot_wider();縦長(long-format)から幅広(wide-format)に変形する関数。names_fromで新しく列名になる列、values_fromで動かしたい値が入っている列を設定する。
pc_cv_table <- pc_plot_small %>%
  group_by(Phase, pcn) %>%
  summarise(cvs = raster::cv(pc)) %>% 
  pivot_wider(names_from = pcn, values_from = cvs) 

pc_cv_table_2 <- pc_plot_large %>% 
  group_by(Phase, pcn) %>% 
  summarise(cvs = raster::cv(pc)) %>% 
  pivot_wider(names_from = pcn, values_from = cvs)

# pc_plotを横幅の表に変換
pc_all_wide_small  <- pc_plot_small %>%
  group_by(pcn) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = pcn, 
              values_from = pc) %>% 
  select(-row)

pc_all_wide_large  <- pc_plot_large %>% 
  group_by(pcn) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = pcn, 
              values_from = pc) %>% 
  select(-row)

# 各段階の主成分毎に変動係数CVを計算
##変動係数CV=標準偏差SD / 平均mean で求められる。
##mutate;この場合新しい列を作成している。
##dplyr::distinct();重複した行を削除する関数。
cv_pcs_all_small <-
  pc_all_wide_small %>%
  group_by(Phase) %>%
  mutate(cv_pc1 = round(sd(PC1)/ mean(PC1), digits = 2)*100,
         cv_pc2 = round(sd(PC2)/ mean(PC2), digits = 2)*100,
         cv_pc3 = round(sd(PC3)/ mean(PC3), digits = 2)*100)%>% 
  mutate(mean_pc1 = mean(PC1),
         mean_pc2 = mean(PC2),
         mean_pc3 = mean(PC3)) %>% 
  distinct(cv_pc1, cv_pc2, cv_pc3, mean_pc1, mean_pc2, mean_pc3)

cv_pcs_all_large <-
  pc_all_wide_large %>% 
  group_by(Phase) %>%
  mutate(cv_pc1 = round(sd(PC1)/ mean(PC1), digits = 2)*100,
         cv_pc2 = round(sd(PC2)/ mean(PC2), digits = 2)*100,
         cv_pc3 = round(sd(PC3)/ mean(PC3), digits = 2)*100)%>% 
  mutate(mean_pc1 = mean(PC1),
         mean_pc2 = mean(PC2),
         mean_pc3 = mean(PC3)) %>% 
  distinct(cv_pc1, cv_pc2, cv_pc3, mean_pc1, mean_pc2, mean_pc3)

# CVのラベルを作成（cv_labelというデータフレームを作成した後ベクトルにしている）
cv_label_small <- cv_pcs_all_small %>% 
  mutate(cv_pc1 = paste("CV = ", cv_pc1, "%"),
         cv_pc2 = paste("CV = ", cv_pc2, "%"),
         cv_pc3 = paste("CV = ", cv_pc3, "%"))
cv_label_small <- c(cv_label_small$cv_pc1, cv_label_small$cv_pc2, cv_label_small$cv_pc3) 

cv_label_large <- cv_pcs_all_large %>% 
  mutate(cv_pc1 = paste("CV = ", cv_pc1, "%"),
         cv_pc2 = paste("CV = ", cv_pc2, "%"),
         cv_pc3 = paste("CV = ", cv_pc3, "%"))
cv_label_large <- c(cv_label_large$cv_pc1, cv_label_large$cv_pc2, cv_label_large$cv_pc3) 

# 各段階の主成分スコアをプロットし、得られたCVの結果を貼り付ける。
# プロットの各段階とラベルの順番がズレている点に注意。
ggplot(pc_plot_small,aes(Phase,pc)) +
  geom_violin() + 
  ggforce::geom_sina() +
  xlab("") +
  facet_wrap(~ pcn, ncol = 1) +
  geom_text(data = cv_pcs_all_small,
            mapping = aes(x = c(1.0, 2.0, 3.0, 4.0, 6.1, 7.2, 8.125, 5.0, 
                                1.1, 2.0, 3.0, 4.0, 6.1, 7.0, 8.0, 5.1, 
                                1.0, 2.0, 3.0, 4.0, 6.1, 7.0, 8.0, 5.0), 
                          y = 9.0, 
                          label = cv_label_small),
            size = 2.5,
            color= "red",
            hjust = -0.1,
            vjust= -1)+
  theme_minimal(base_family = "IPAexGothic")

ggsave(here("analysis/figures/PCs_CV_small.jpg"),
       h = 5, w = 10)

ggplot(pc_plot_large, aes(Phase, pc)) +
  geom_violin() + 
  ggforce::geom_sina() +
  xlab("") +
  facet_wrap(~ pcn, ncol = 1) +
  geom_text(data = cv_pcs_all_large,
            mapping = aes(x = c(1.0, 2.0, 3.0, 4.0, 7.1, 6.0, 5.1, 8.0,  
                                1.0, 2.1, 3.1, 4.0, 7.1, 6.0, 5.0, 8.1, 
                                1.0, 2.0, 3.1, 4.0, 7.1, 6.0, 5.1, 8.0), 
                          y = 9.0, 
                          label = cv_label_large),
            size = 2.5,
            color= "red",
            hjust = -0.1,
            vjust= -1)+
  theme_minimal(base_family = "IPAexGothic")

ggsave(here("analysis/figures/PCs_CV_large.jpg"),
       h = 5, w = 10)