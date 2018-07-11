# 実行前に
## RStudio - Menu - Session - Set Working Directory - To Project Directory を実行してください
## RAWDATA/IDFフォルダに「verYYYYMMDD」（YYYYMMDDは年月日）フォルダを格納してください
## inputフォルダに併用禁止薬のリストを格納してください
# 併用禁止薬をリスト化する

# initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
if (!exists("parent_path")) {
  parent_path <- getwd()
}
input_path <- paste0(parent_path, "/input")
rawdata_path <- paste0(parent_path, "/RAWDATA")
idf_parent_path <- paste0(rawdata_path, "/IDF")
# 併用禁止薬情報
input_file_path <- paste(input_path, "ProhibitedConcomitant.txt", sep="/")
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
# read rawfile ------
# 併用禁止薬リスト取り込み
ProhibitedConcomitant <- read.csv(input_file_path, as.is=T,
                                  fileEncoding="CP932", stringsAsFactors=F,
                                  header=F)
# 前後の空白除去、大文字に変換

# 日本語ファイル取り込み
rawdata_idf_japanese <- read.csv(idf_japanese_file_path, as.is=T,
                                 fileEncoding="CP932", stringsAsFactors=F,
                                 header=F)
names(rawdata_idf_japanese) <- c("薬剤コード", "慣用区分", "使用区分1",
                                 "使用区分2", "基準名コード", "薬剤名",
                                 "薬剤名カナ", "一般名", "一般名カナ",
                                 "メーカーコード", "メーカーの略称",
                                 "剤形コード", "薬剤コード区分1",
                                 "メンテナンスSEQ", "メンテナンスFLG",
                                 "メンテ年月", "薬剤コード区分2")
idf_japanese <- rawdata_idf_japanese[ , c("薬剤コード", "薬剤名", "一般名")]
# 英名ファイル取り込み
rawdata_idf_english <- read.csv(idf_english_file_path, as.is=T,
                                fileEncoding="CP932", stringsAsFactors=F,
                                header=F)
names(rawdata_idf_english) <- c("メンテナンスSEQ", "薬剤コード", "英名",
                                "JANフラグ", "INNフラグ", "予備",
                                "英メンテ年月", "英メンテナンスFLG")
idf_english <- rawdata_idf_english[ , c("薬剤コード", "英名")]
idf_english <- subset(idf_english, 英名!="")
# 薬剤コードで日本語ファイルと英名ファイルをマージ
idf <- merge(idf_japanese, idf_english, by="薬剤コード", all.x=T)
#  ------
# 一般名：薬剤コードが7桁のレコードを抽出
idf_generic_name <- subset(rawdata_idf_japanese,
                           sapply(rawdata_idf_japanese[ ,"薬剤コード"], nchar) == 7)
# 商品名：一般名以外のレコードを抽出し、一般名コードをセット
idf_product_name <- subset(rawdata_idf_japanese,
                           !(rawdata_idf_japanese[ ,"薬剤コード"]
                             %in% idf_generic_name[ ,"薬剤コード"]))
# 英名で併用禁忌薬を検索
