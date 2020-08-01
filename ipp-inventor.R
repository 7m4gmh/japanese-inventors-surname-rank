#setwd("D:/R")

library(tidyverse)
library(stringr)

## 空白が１文字含まれる氏名のみ抽出
# 空白が２文字以上，空白がゼロ，点，カンマ，中黒，数字，判読できない記号などが含まれる氏名は，氏の部分と名の部分の判別がつかないので除外。
# ただし，三角とカギカッコは除外しない。
#   **  inventor.txt のファイルには個人情報が含まれているため，
#       githubにはアップロードしていません。
#       データの入手については，代理人又は
japanese_inventor <- read_tsv("IPP/inventor.txt", locale = locale(encoding = "utf8")) %>% 
 filter(!grepl("　.*　", name) & grepl("　", name) & !grepl("，", name) & !grepl("、", name)  & !grepl("□", name)  & !grepl("・", name)  & !grepl("≡", name) & !grepl("[[A-zＡ-ｚ0-9０-９]", name) )

# ここまでで 25,281,019件

## 住所が不明又は外国のレコードを除外
japanese_inventor <- japanese_inventor %>% drop_na(address)  %>%
 filter(
   !grepl("国$", address) &
   !grepl("^.{0,2}$", address) &
   !grepl("^[A-zＡ-ｚァ-ヴー・]", address) &
   !grepl("^台湾", address) &
   !grepl("^大韓民国", address)
   ) 

# ここまでで  24,143,610件

#　重複した氏名（同姓同名）を集約 
library(dplyr)
## japanese_name <- japanese_inventor %>% distinct(name, .keep_all = FALSE) 
japanese_name <- japanese_inventor %>% distinct(name, .keep_all = TRUE) 

#  氏と名を分割
Surname_Givenname <- 
  separate(japanese_name, name, c("Surname", "Givenname"), sep ="　") 
  
# 異体字（「高橋」と「▲高▼橋」など）を集約対象とする
Surname_Givenname2 <- 
  bind_cols(
  str_remove_all(Surname_Givenname$Surname, "[▼▲「」]"),
  Surname_Givenname$Givenname, 
  Surname_Givenname$address
  )  
colnames(Surname_Givenname2) <- c("Surname", "GivenName", "Address")

#ここまでで 1,785,896名

# いったんファイルに保存
library(openxlsx)
write_csv(Surname_Givenname2, "Surname_Givenname2.csv")
write.xlsx(Surname_Givenname2, "Surname_Givenname2.xlsx")


#氏ごとに出現回数を計上
inventor_surname_rank <- Surname_Givenname2 %>% 
  drop_na(Surname) %>% group_by(Surname) %>%
  summarise(count=n(), percent = count / nrow(.) * 100)  %>%
  arrange(desc(count)) %>% 
  mutate(min_rank = min_rank(desc(count)), cume_dist = 100 * cume_dist(desc(count))) 

# ここまでで，氏の種類 68,228種類


# ランクを確認。　「梅本」は745位
umemoto_rank = subset(inventor_surname_rank, Surname =="梅本") %>%
  select(min_rank) %>% as.numeric()
umemoto_count = subset(inventor_surname_rank, Surname =="梅本") %>%
  select(count) %>% as.numeric()
umemoto_label = paste(as.character(umemoto_rank), " - - 梅本")

## A tibble: 1 x 5
#  Surname count percent min_rank cume_dist
#  <chr>   <int>   <dbl>    <int>     <dbl>
#1 梅本      436  0.0244      745      1.09


## ファイルに保存 (CSVとXSL)
write_csv(inventor_surname_rank, "inventor_surname_rank.csv")
write.xlsx(inventor_surname_rank, "inventor_surname_rank.xlsx")

## 上記ファイルを読み込み 
#setwd("D:/R")
#library(readr)
#inventor_surname_rank <- read_csv("inventor_surname_rank.csv")


# 「梅本」の位置を示す図をプロット
ggplot(data=inventor_surname_rank) + 
   geom_point(mapping = aes (x = (min_rank), y = (count))) +
   annotate("text", label = umemoto_label, x = (umemoto_rank), y= (umemoto_count))
ggsave("umemoto_rank_normal.png")
ggplot(data=inventor_surname_rank) + 
   geom_point(mapping = aes (x = log10(min_rank), y = log10(count))) + 
   annotate("text", label = umemoto_label, x = log10(umemoto_rank), y = log10(umemoto_count))
ggsave("umemoto_rank_log10.png")


