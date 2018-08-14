# 実行前に
## RStudio - Menu - Session - Set Working Directory - To Source file location
## を実行してください
## RAWDATA/IDFフォルダに「verYYYYMMDD」（YYYYMMDDは年月日）フォルダを格納して
## ください
## input/ProhibitedConcomitant.txtに併用禁止薬を列挙してください

# inputフォルダ内の情報から併用禁止薬をリスト化する処理
# initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
if (!exists("parent_path")) {
  setwd("../")
  parent_path <- getwd()
}
input_path <- paste0(parent_path, "/input")
rawdata_path <- paste0(parent_path, "/RAWDATA")
idf_parent_path <- paste0(rawdata_path, "/IDF")
# 併用禁止薬情報
kInputFileName <- "ProhibitedConcomitant.txt"
input_file_path <- paste(input_path, kInputFileName, sep="/")
# IDFフォルダ直下には一つだけフォルダが格納されている前提で処理
# バージョン日付を取得
idf_version <- list.files(idf_parent_path)[1]
idf_version_YMD <- substr(idf_version, 4, nchar(idf_version))
# 日本語：IDF/verYYYYMMDD/医薬品名データファイル/YYYYMMDD提供/全件.txt
idf_japanese_dir_path <- paste(idf_parent_path, idf_version,
                                "医薬品名データファイル",
                                paste0(idf_version_YMD, "提供"), sep="/")
idf_japanese_file_path <- paste(idf_japanese_dir_path, "全件.txt", sep="/")
# 英語：IDF/verYYYYMMDD/英名/英名.txt
idf_english_dir_path <- paste(idf_parent_path, idf_version, "英名", sep="/")
idf_english_file_path <- paste(idf_english_dir_path, "英名.txt", sep="/")
# 出力フォルダが存在しなければ作成
output_path <- paste0(parent_path, "/output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# load input files ------
# 併用禁止薬リスト取り込み
# エンコードがshift-jisの前提で処理
file_connection <- file(input_file_path, "r", encoding="cp932")
input_ProhibitedConcomitant <- readLines(con=file_connection)
close(file_connection)
if (exists("ProhibitedConcomitant")) {
  rm(ProhibitedConcomitant)
}
for (i in 1:length(input_ProhibitedConcomitant)) {
  # "#"で始まる行はコメントとして読み飛ばす。#の前の空白は許容しない
  if ((substring(input_ProhibitedConcomitant[i], 1, 1) != "#")
      && (input_ProhibitedConcomitant[i] != "")) {
    # カンマで切り分けて前後の空白除去、大文字に変換
    temp_input <- unlist(strsplit(input_ProhibitedConcomitant[i], ","))
    for (j in 1:length(temp_input)) {
      temp_input[j] <- gsub("^[ ]+|[ ]+$", "", temp_input[j])
      temp_input[j] <- toupper(temp_input[j])
      if (!exists("ProhibitedConcomitant")) {
        ProhibitedConcomitant <- temp_input[j]
      } else {
        ProhibitedConcomitant <- append(ProhibitedConcomitant, temp_input[j])
      }
    }
  }
}
# 日本語ファイル取り込み
rawdata_idf_japanese <- read.csv(idf_japanese_file_path, as.is=T,
                                 fileEncoding="cp932", stringsAsFactors=F,
                                 header=F)
names(rawdata_idf_japanese) <- c("薬剤コード", "慣用区分", "使用区分1",
                                 "使用区分2", "基準名コード", "薬剤名",
                                 "薬剤名カナ", "一般名", "一般名カナ",
                                 "メーカーコード", "メーカーの略称",
                                 "剤形コード", "薬剤コード区分1",
                                 "メンテナンスSEQ", "メンテナンスFLG",
                                 "メンテ年月", "薬剤コード区分2")
# 英名ファイル取り込み
rawdata_idf_english <- read.csv(idf_english_file_path, as.is=T,
                                fileEncoding="CP932", stringsAsFactors=F,
                                header=F)
names(rawdata_idf_english) <- c("メンテナンスSEQ", "薬剤コード", "英名",
                                "JANフラグ", "INNフラグ", "予備",
                                "英メンテ年月", "英メンテナンスFLG")
# idf_data merge, extraction ------
idf_japanese <- rawdata_idf_japanese[ , c("薬剤コード", "薬剤名", "一般名")]
idf_english <- rawdata_idf_english[ , c("薬剤コード", "英名")]
idf_english <- subset(idf_english, 英名!="")
# 薬剤コードで日本語ファイルと英名ファイルをマージ
idf <- merge(idf_japanese, idf_english, by="薬剤コード", all.x=T)
# 一般名：薬剤コードが7桁のレコードを抽出
idf_generic_name <- subset(idf, sapply(idf[ ,"薬剤コード"], nchar) == 7)
# 商品名：一般名以外のレコードを抽出
idf_product_name <- subset(idf, !(idf[ ,"薬剤コード"]
                                  %in% idf_generic_name[ ,"薬剤コード"]))
# Prohibited Concomitant_data extraction ------
# 英名一般名で併用禁忌薬を検索
generic_english_list <- subset(idf_generic_name, (idf_generic_name[ ,"英名"]
                                                  %in% ProhibitedConcomitant))
# 日本語一般名で併用禁忌薬を検索
generic_japanese_list <- subset(idf_generic_name, (idf_generic_name[ ,"一般名"]
                                                   %in% ProhibitedConcomitant))
# 薬品コードの上n桁で併用禁忌薬を検索、一般名のみ抽出する
# 抽出結果格納のため空のデータフレームを作成
therapeutic_category_list <- data.frame(matrix(rep(NA, ncol(idf_generic_name)),
                                               nrow=1))[numeric(0), ]
names(therapeutic_category_list) <- colnames(idf_generic_name)
for (i in 1:length(ProhibitedConcomitant)) {
  # 文字列から数値変換時のワーニングをsuppressWarnings関数で抑制する
  if (!is.na(suppressWarnings(as.numeric(ProhibitedConcomitant[i])))) {
    temp_therapeutic_category_list <- subset(idf_generic_name,
                                             substring(idf_generic_name[ ,"薬剤コード"],
                                                       1, nchar(ProhibitedConcomitant[i]))
                                                          == ProhibitedConcomitant[i])
    therapeutic_category_list <- rbind(therapeutic_category_list,
                                       temp_therapeutic_category_list)
  }
}
# マージして完全に重複するレコードは削除する、薬剤コードの重複はそのまま残す
merge_list <- rbind(generic_english_list, generic_japanese_list,
                    therapeutic_category_list)
sort_list <- order(merge_list$薬剤コード)
sort_df <- merge_list[sort_list, ]
generic_ProhibitedConcomitant_list <- sort_df[!duplicated(sort_df), ]

# 併用禁忌の商品名リストを取得

# Output prohibited_concomitant_list ------
# shift-jisで出力する
