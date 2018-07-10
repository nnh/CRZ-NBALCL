# 実行前に
## RStudio - Menu - Session - Set Working Directory - To Project Directory を実行してください
## RAWDATA/IDFフォルダに「verYYYYMMDD」（YYYYMMDDは年月日）フォルダを格納してください

# 併用禁止薬をリスト化する

# initialize ------
Sys.setenv("TZ" = "Asia/Tokyo")
parent_path <- getwd()
rawdata_path <- paste0(parent_path, "/RAWDATA")
idf_parent_path <- paste0(rawdata_path, "/IDF")
# IDFフォルダ直下には一つだけフォルダが格納されている前提で処理
# バージョン日付を取得
idf_version <- list.files(idf_parent_path)[1]
idf_version_YMD <- substr(idf_version, 4, nchar(idf_version))
idf_japanese_dir_path <- paste(idf_parent_path, idf_version,
                                "医薬品名データファイル",
                                paste0(idf_version_YMD, "提供"), sep="/")
idf_japanese_file_path <- paste(idf_japanese_dir_path, "全件.txt", sep="/")
output_path <- paste0(parent_path, "/output")
# 出力フォルダが存在しなければ作成
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# read rawfile ------
# ファイルが一件のみの前提で処理
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
#  ------
# 一般名：薬剤コードが7桁のレコードを抽出
idf_generic_name <- subset(rawdata_idf_japanese,
                           sapply(rawdata_idf_japanese[ ,"薬剤コード"], nchar) == 7)
# 商品名：一般名以外のレコードを抽出し、一般名コードをセット
idf_product_name <- subset(rawdata_idf_japanese,
                           !(rawdata_idf_japanese[ ,"薬剤コード"]
                             %in% idf_generic_name[ ,"薬剤コード"]))
