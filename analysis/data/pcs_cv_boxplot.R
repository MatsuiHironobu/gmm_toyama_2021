#箱髭図と散布図を作る工程
library(ggpubr)
library(dplyr)

#元となる一覧表を作成

##small
pc1_small <- data.frame(x = pca_small_combine3$x[,1],
                        y = as.character(pca_small_combine3$fac$Phase),
                        stringsAsFactors = F)

pc2_small <- data.frame(x = pca_small_combine3$x[,2],
                        y = as.character(pca_small_combine3$fac$Phase),
                        stringsAsFactors = F)

pc3_small <- data.frame(x = pca_small_combine3$x[,3],
                        y = as.character(pca_small_combine3$fac$Phase),
                        stringsAsFactors = F)

##large
pc1_large <- data.frame(x = pca_large_combine3$x[,1],
                        y = as.character(pca_large_combine3$fac$Phase),
                        stringsAsFactors = F)

pc2_large <- data.frame(x = pca_large_combine3$x[,2],
                        y = as.character(pca_large_combine3$fac$Phase),
                        stringsAsFactors = F)

pc3_large <- data.frame(x = pca_large_combine3$x[,3],
                        y = as.character(pca_large_combine3$fac$Phase),
                        stringsAsFactors = F)


#基になるデータを読み込み
pca_small_combine2 <- readr::read_rds(here("analysis/data/pca_small_combine2.rds"))
pca_large_combine2 <- readr::read_rds(here("analysis/data/pca_large_combine2.rds"))

##bind_cols;列の左側に列を追加する関数
##dplyr::full_join;指定した列名でデータフレームを結合
##dplyr::anti_join(x,y,by=~);yとマッチしなかったxの行を返します。

##データフレームを抽出
size_df_small <- pca_small_combine2$fac
size_df_large <- pca_large_combine2$fac



#主成分スコアをデータフレームに結合
##small
pc1_filename_small <- 
  bind_cols(File_name = rownames(pc1_small), pc1_small) %>% 
  rename(c("pc1" = "x")) 

pc2_filename_small <- 
  bind_cols(File_name = rownames(pc2_small), pc2_small) %>% 
  rename(c("pc2" = "x")) 

pc3_filename_small <- 
  bind_cols(File_name = rownames(pc3_small), pc3_small) %>% 
  rename(c("pc3" = "x")) 

size_pcs_df_small <-
  size_df_small %>% 
  left_join(pc1_filename_small) %>% 
  left_join(pc2_filename_small) %>%
  left_join(pc3_filename_small) %>%
  select(-y)

##large
pc1_filename_large <- 
  bind_cols(File_name = rownames(pc1_large), pc1_large) %>% 
  rename(c("pc1" = "x"))

pc2_filename_large <- 
  bind_cols(File_name = rownames(pc2_large), pc2_large) %>% 
  rename(c("pc2" = "x"))

pc3_filename_large <- 
  bind_cols(File_name = rownames(pc3_large), pc3_large) %>% 
  rename(c("pc3" = "x"))

size_pcs_df_large <-
  size_df_large %>% 
  left_join(pc1_filename_large) %>% 
  left_join(pc2_filename_large) %>%
  left_join(pc3_filename_large) %>%
  select(-y)



#cvの情報を統合する
##dplyr::group_by;ある変数の値でデータセットをグループ化して、グループ単位で処理を行えるようにする。
##raster::cv;変動係数を計算する関数
##small
size_pcs_cv_small <-
  size_pcs_df_small %>% 
  group_by(Phase) %>% 
  mutate(cv_rim_diameter = round(raster::cv(rim_diameter), 2),
         cv_hight = round(raster::cv(hight), 2),
         rim_diameter_mean = round(mean(rim_diameter), 2)) %>% 
  mutate(Phase = as.factor(Phase)) %>% 
  left_join(cv_pcs_all_small)

##large
size_pcs_cv_large <-
  size_pcs_df_large %>% 
  group_by(Phase) %>% 
  mutate(cv_rim_diameter = round(raster::cv(rim_diameter), 2),
         cv_hight = round(raster::cv(hight), 2),
         rim_diameter_mean = round(mean(rim_diameter), 2)) %>% 
  mutate(Phase = as.factor(Phase)) %>% 
  left_join(cv_pcs_all_large)



# データフレームを作成し、ラベルを作成
size_cv_df_small <-
  size_pcs_cv_small %>%
  #重複する行を削除する
  distinct(Phase, rim_diameter_mean, cv_rim_diameter, cv_pc1) %>% 
  mutate(cv_rim_diameter_label = paste("CV =", cv_rim_diameter, "%"),
         rim_diameter_mean_label = paste("MV=", rim_diameter_mean,"cm"))

cv_diameter_label_small <- size_cv_df_small$cv_rim_diameter_label
rim_diameter_mean_label_small <- size_cv_df_small$rim_diameter_mean
rim_diameter_mean_label_small_2 <- size_cv_df_small$rim_diameter_mean_label

size_cv_df_large <-
  size_pcs_cv_large %>% 
  #重複する行を削除する
  distinct(Phase, rim_diameter_mean, cv_rim_diameter, cv_pc1) %>% 
  mutate(cv_rim_diameter_label = paste("CV =", cv_rim_diameter, "%"),
         rim_diameter_mean_label = paste("MV=", rim_diameter_mean,"cm"))

cv_diameter_label_large <- size_cv_df_large$cv_rim_diameter_label
rim_diameter_mean_label_large <- size_cv_df_large$rim_diameter_mean
rim_diameter_mean_label_large_2  <- size_cv_df_large$rim_diameter_mean_label

#箱髭図で各段階毎の口径を比較
#ラベルの位置がズレているので注意（size_cv_df_を確認しながら修正）
#\nで改行
##small
boxplot_small <-
  ggplot(size_pcs_df_small,
         aes(rim_diameter, Phase)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "rim diameter\n(cm)", y = "") +
  annotate("text",
           x = c(8.0, 7.8, 7.6, 7.6, 8.0, 6.9, 8.0, 7.8), 
           y = c(1.0, 2.0, 3.0, 4.0, 6.0, 7.0, 8.0, 5.0),
           size = 1.5, label = cv_diameter_label_small) + #annotate;注釈を追加する
  annotate("text",
           x = c(7.8, 7.6, 7.4, 7.4, 7.8, 6.7, 7.8, 7.6), 
           y = c(1.0, 2.0, 3.0, 4.0, 6.0, 7.0, 8.0, 5.0),
           size = 1.5, label = rim_diameter_mean_label_small_2) +
  theme_minimal(base_family = "IPAexGothic")+
  ###x軸,y軸の文字の大きさをx倍にする
  theme(axis.text.x = element_text(size = rel(0.5),angle = 10),
        axis.text.y = element_text(size = rel(0.9))) 

##large
boxplot_large <-
  ggplot(size_pcs_df_large,
         aes(rim_diameter, Phase)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "rim diameter\n(cm)", y = "") +
  annotate("text", 
           x = c(16.7,10.5,10.5,16.2,10.4,11.0,11.1,10.4), 
           y = c(1.0, 2.0, 3.0, 4.0, 7.0, 6.0, 5.0, 8.0),
           size = 1.3, label = cv_diameter_label_large) +
  annotate("text", 
           x = c(16.4,10.2,10.2,15.9,10.1,10.7,10.8,10.1), 
           y = c(1.0, 2.0, 3.0, 4.0, 7.0, 6.0, 5.0, 8.0),
           size = 1.3, label = rim_diameter_mean_label_large_2) +
  theme_minimal(base_family = "IPAexGothic")+
  ###x軸,y軸の文字の大きさをx倍にする
  theme(axis.text.x = element_text(size = rel(0.5),angle = 10),
        axis.text.y = element_text(size = rel(0.9))) 


#散布図を描写
library(ggrepel)
##geom_point();散布図を作成する
##プロット領域に日本語を打ち込む場合は、geom_text_repel()でフォントを決めなければならない
###small
par(family = "IPAexGothic")
scatterplot_small <-
  size_cv_df_small %>% 
  ggplot(aes(cv_rim_diameter, cv_pc1)) +
  geom_point() +
  geom_text_repel(label = size_cv_df_small$Phase, 
                  size = 1.5,
                  family = "IPAexGothic") + 
  labs(x = "CV of rim diameter (%)",
       y = "CV of PC1\n(%)") +
  theme(legend.position = "none") +
  theme_minimal(base_family = "IPAexGothic")+
  #x軸,y軸の文字の大きさを0.9倍にする
  theme(axis.text.x = element_text(size = rel(0.9)),
        axis.text.y = element_text(size = rel(0.9)))

###large
scatterplot_large <-
  size_cv_df_large %>% 
  ggplot(aes(cv_rim_diameter, cv_pc1)) +
  geom_point() +
  geom_text_repel(label = size_cv_df_large$Phase, 
                  size = 1.5,
                  family = "IPAexGothic") + 
  labs(x = "CV of rim diameter (%)",
       y = "CV of PC1\n(%)") +
  theme(legend.position = "none") +
  theme_minimal(base_family = "IPAexGothic")+
  #x軸,y軸の文字の大きさを0.9倍にする
  theme(axis.text.x = element_text(size = rel(0.9)),
        axis.text.y = element_text(size = rel(0.9)))



#作成した図をまとめる
library(cowplot)
boxplot_scatterplot_small_large <-
  plot_grid(scatterplot_small,boxplot_small,
            scatterplot_large,boxplot_large,
            labels="AUTO",
            rel_widths = c(1,2,1,2),
            ncol=2)

ggsave(plot=boxplot_scatterplot_small_large,
       here("analysis", "figures","boxplot_scatterplot_small_large.jpg"),
       width = 180, height = 100, dpi = 300,
       units = "mm")