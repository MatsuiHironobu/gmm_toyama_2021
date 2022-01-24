#1データの統合
library(Momocs)
##読み込み
shape_small <- readr::read_rds(here("analysis/data/shape_small.rds"))
shape_large <- readr::read_rds(here("analysis/data/shape_large.rds"))
shape_small_add <- readr::read_rds(here("analysis/data/shape_small_add.rds"))
shape_large_add <- readr::read_rds(here("analysis/data/shape_large_add.rds"))
shape_add <- readr::read_rds(here("analysis/data/shape_add.rds"))

##別々に作成・編集したshapeのデータを統合する。
shape_all_combine <- 
  combine(shape_small,shape_large,shape_small_add,shape_large_add,shape_add)%>%
  coo_slide(ldk=2) %>%
  fgProcrustes() %>%
  filter(Site!="山科寺内町遺跡")

##facのPhaseを修正、レベルを設定
shape_all_combine$fac <-
  shape_all_combine$fac %>%
  dplyr::mutate(Phase = case_when(
    str_detect(remains,"SE0922")~"京都9C段階",
    str_detect(remains,"SK2185")~"京都10A段階",
    str_detect(remains,"土壙170")~"京都9B段階",
    str_detect(remains,"SK415")~"京都9C段階",
    str_detect(remains,"土坑170")~"京都9C段階",
    str_detect(remains,"SK499")~"京都10A段階",
    str_detect(remains,"SD166上層")~"京都10B段階",
    str_detect(remains,"土坑2134")~"京都10A段階",
    str_detect(remains,"SD7003")~"富山2段階",
    str_detect(remains,"SD7030")~"富山1段階",
    str_detect(remains,"2SK705")~"富山4段階",
    str_detect(remains,"3SD54")~"富山3段階",
    str_detect(remains,"SK0366")~"京都9B段階",
    str_detect(remains,"SK769")~"京都9B段階",
    str_detect(remains,"SK145")~"京都9B段階",
    str_detect(remains,"SK144B")~"京都10A段階",
    str_detect(remains,"SK8-18")~"京都10B段階",
    str_detect(remains,"溝5下層")~"京都10B段階",))

shape_all_combine$fac$Phase <-
  factor(shape_all_combine$fac$Phase,
         levels=c("富山1段階", "富山2段階","富山3段階","富山4段階",
                  "京都9B段階","京都9C段階","京都10A段階","京都10B段階"))

saveRDS(shape_all_combine,here("analysis/data/shape_all_combine.rds"))



#2小型品と中大型品の設定（富山）
shape_all_combine <- readr::read_rds(here("analysis/data/shape_all_combine.rds"))

shape_toyama_combine <-
  shape_all_combine %>%
  Momocs::filter(Prefecture == "Toyama")

##調整の違いを基に小型品と中大型品を分ける。
###SD7030
shape_toyama_small_1 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "SD7030") %>%
  Momocs::filter(Report_ID < 4906)

shape_toyama_large_1 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "SD7030") %>%
  Momocs::filter(Report_ID >= 4906)

###SD7003
shape_toyama_small_2 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "SD7003") %>%
  Momocs::filter(Report_ID < 4504)

shape_toyama_large_2 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "SD7003") %>%
  Momocs::filter(Report_ID >= 4504)

###3SD54
shape_toyama_small_3 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "3SD54") %>%
  Momocs::filter(Report_ID < 594)

shape_toyama_large_3 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "3SD54") %>%
  Momocs::filter(Report_ID >= 594)

###2SK705
shape_toyama_small_4 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "2SK705") %>%
  Momocs::filter(rim_diameter < 10.9)

shape_toyama_large_4 <-
  shape_toyama_combine %>%
  Momocs::filter(remains == "2SK705") %>%
  Momocs::filter(rim_diameter >= 10.9)

##富山の小型品
shape_toyama_combine_small <- 
  combine(shape_toyama_small_1,shape_toyama_small_2,shape_toyama_small_3,shape_toyama_small_4)%>%
  coo_slide(ldk=2) %>%
  fgProcrustes()

##富山の中大型品
shape_toyama_combine_large <- 
  combine(shape_toyama_large_1,shape_toyama_large_2,shape_toyama_large_3,shape_toyama_large_4)%>%
  coo_slide(ldk=2) %>%
  fgProcrustes()

##保存
saveRDS(shape_toyama_combine_small,here("analysis/data/shape_toyama_combine_small.rds"))
saveRDS(shape_toyama_combine_large,here("analysis/data/shape_toyama_combine_large.rds"))



#3小型品と中大型品の設定（京都）
shape_all_combine <- readr::read_rds(here("analysis/data/shape_all_combine.rds"))

shape_kyoto_combine <-
  shape_all_combine %>%
  Momocs::filter(Prefecture == "Kyoto")

##京都の小型品（9C段階を参考に機械的に10.5以下を小型品とする）
shape_kyoto_combine_small <-
  shape_kyoto_combine %>%
  Momocs::filter(rim_diameter <= 10.5)

##京都の中大型品
shape_kyoto_combine_large <-
  shape_kyoto_combine %>%
  Momocs::filter(rim_diameter > 10.5)

##保存
saveRDS(shape_kyoto_combine_small,here("analysis/data/shape_kyoto_combine_small.rds"))
saveRDS(shape_kyoto_combine_large,here("analysis/data/shape_kyoto_combine_large.rds"))



#4京都と富山のデータを再統合
shape_small_combine <- Momocs::combine(shape_toyama_combine_small,shape_kyoto_combine_small)
shape_large_combine <- Momocs::combine(shape_toyama_combine_large,shape_kyoto_combine_large)

##保存
saveRDS(shape_small_combine,here("analysis/data/shape_small_combine.rds"))
saveRDS(shape_large_combine,here("analysis/data/shape_large_combine.rds"))



#5楕円フーリエ解析
f_small_combine <- efourier(shape_small_combine,nb.h=16,norm=FALSE)
f_large_combine <- efourier(shape_large_combine,nb.h=17,norm=FALSE)

##保存
saveRDS(f_small_combine,here("analysis/data/f_small_combine.rds"))
saveRDS(f_large_combine,here("analysis/data/f_large_combine.rds"))



#6主成分分析
##主成分分析
pca_small_combine <- f_small_combine %>% PCA
pca_large_combine <- f_large_combine %>% PCA

##外れ値の特定
which_out(pca_small_combine$x[,1],0.5)
which_out(pca_large_combine$x[,1],0.5)

##外れ値を赤くプロット
cols_small_combine <- rep("black", nrow(pca_small_combine$x))
outliers_small_combine <- which_out(pca_small_combine$x[, 1], conf = 0.5)
cols_small_combine[outliers_small_combine] <- "red"
plot(pca_small_combine, col=cols_small_combine)

cols_large_combine <- rep("black", nrow(pca_large_combine$x))
outliers_large_combine <- which_out(pca_large_combine$x[, 1], conf = 0.5)
cols_large_combine[outliers_large_combine] <- "red"
plot(pca_large_combine, col=cols_large_combine)

##外れ値を除外し再度主成分分析を実施
pca_small_combine2<-f_small_combine %>% slice(-outliers_small_combine) %>%PCA
PCcontrib(pca_small_combine2,nax=1:3) 

pca_large_combine2<-f_large_combine %>% slice(-outliers_large_combine) %>%PCA
PCcontrib(pca_large_combine2,nax=1:3) 

##保存
saveRDS(pca_small_combine2,here("analysis/data/pca_small_combine2.rds"))
saveRDS(pca_large_combine2,here("analysis/data/pca_large_combine2.rds"))
