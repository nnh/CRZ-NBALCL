# 実行前に
## RStudio - Menu - Session - Set Working Directory - To Source file location
## を実行してください
## RAWDATA/IDFフォルダに「verYYYYMMDD」（YYYYMMDDは年月日）フォルダを格納して
## ください
## RAWDATAフォルダ直下に「CRZ-NBALCL_CM_...csv」を格納してください
## input/ProhibitedConcomitant.txtに併用禁止薬を列挙してください
kCM_file_name <- "CRZ-NBALCL_CM_"
# CM_field3に併用禁止薬が存在するかチェックする
# 初期処理の実行
if (exists("save_wd")) {
  setwd(save_wd)
}
source("./CRZ-NBALCL_common.R")
initCRZ_NBALCL()
# CRZ-NBALCL_CM_...csvの取り込み
# 対象ファイルが一件の前提で処理する
file_list <- list.files(rawdata_path)
cm_index <- grep(kCM_file_name, file_list)
if (length(cm_index) > 0) {
  rawdata_cm <- read.csv(paste(rawdata_path, file_list[cm_index], sep="/"), as.is=T, fileEncoding="cp932",
                         stringsAsFactors=F, header=T)
}
# 併用禁止薬リストの取り込み
ProhibitedConcomitant <- parseInputFile("ProhibitedConcomitant.txt", "cp932")
# 医薬品データファイルの取り込み
idf <- mergeIdf()
# 医薬品データファイルから一般名：薬剤コードが7桁のレコードを抽出
idf_generic_name <- extractGenericName(idf)
# 医薬品データファイルから商品名：一般名以外のレコードを抽出
  ## x %in% y ベクトルyに対しベクトルxの全ての要素を調べ、yの中に存在すればTRUEを返す
  ## 下記のRドキュメンテーションに仕様が記載されている
  ## https://stat.ethz.ch/R-manual/R-devel/library/base/html/match.html
idf_product_name <- subset(idf, !(idf[ ,"薬剤コード"] %in% idf_generic_name[ ,"薬剤コード"]))
# 英名一般名で併用禁忌薬を検索
generic_english_list <- subset(idf_generic_name, (idf_generic_name[ ,"英名"] %in% ProhibitedConcomitant))
# 日本語一般名で併用禁忌薬を検索
generic_japanese_list <- subset(idf_generic_name, (idf_generic_name[ ,"一般名"] %in% ProhibitedConcomitant))
# 抽出結果をマージして完全に重複するレコードは削除する
merge_list <- rbind(generic_english_list, generic_japanese_list)
sort_list <- order(merge_list$薬剤コード)
sort_df <- merge_list[sort_list, ]
generic_ProhibitedConcomitant_list <- sort_df[!duplicated(sort_df), ]
# 商品名を含めた併用禁忌のリストを取得
output_ProhibitedConcomitant_list <- subset(idf, substring(idf[ ,"薬剤コード"], 1, 7)
                                            %in% generic_ProhibitedConcomitant_list[ ,"薬剤コード"])
# Output prohibited_concomitant_list ------
# 確認用 併用禁忌薬リスト shift-jisで出力する
write.csv(output_ProhibitedConcomitant_list, paste0(output_path, "/output_ProhibitedConcomitant.csv"), row.names=F,
          fileEncoding="cp932")
# ------
# CM併用薬チェック
