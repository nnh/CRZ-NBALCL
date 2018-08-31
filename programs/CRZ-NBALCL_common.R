# CRZ-NBALCL central monitoring common program
# initialize ------
initCRZ_NBALCL = function(){
  Sys.setenv("TZ" = "Asia/Tokyo")
  # parent_pathが設定されていなければ現在の作業ディレクトリの一階層上を設定する
  if (!exists("parent_path")) {
    save_wd <<- getwd()
    setwd("../")
    parent_path <<- getwd()
  }
  # 入出力フォルダの設定
  input_path <<- paste0(parent_path, "/input")
  rawdata_path <<- paste0(parent_path, "/RAWDATA")
  # 出力フォルダが存在しなければ作成
  output_path <<- paste0(parent_path, "/output")
  if (file.exists(output_path) == F) {
    dir.create(output_path)
  }
}
# load IDF ------
loadIdf = function(){
  if (!exists("parent_path")) {
    initCRZ_NBALCL()
  }
  idf_parent_path <- paste0(rawdata_path, "/IDF")
  # 医薬品データファイルの取り込み
  # IDFフォルダ直下には一つだけフォルダが格納されている前提で処理
  # バージョン日付を取得
  idf_version <- list.files(idf_parent_path)[1]
  idf_version_YMD <- substr(idf_version, 4, nchar(idf_version))
  # 日本語：IDF/verYYYYMMDD/医薬品名データファイル/YYYYMMDD提供/全件.txt
  idf_japanese_dir_path <- paste(idf_parent_path, idf_version, "医薬品名データファイル", paste0(idf_version_YMD, "提供"),
                                  sep="/")
  idf_japanese_file_path <- paste(idf_japanese_dir_path, "全件.txt", sep="/")
  # 英語：IDF/verYYYYMMDD/英名/英名.txt
  idf_english_dir_path <- paste(idf_parent_path, idf_version, "英名", sep="/")
  idf_english_file_path <- paste(idf_english_dir_path, "英名.txt", sep="/")
  # 日本語ファイル取り込み
  rawdata_idf_japanese <<- read.csv(idf_japanese_file_path, as.is=T, fileEncoding="cp932", stringsAsFactors=F, header=F)
  names(rawdata_idf_japanese) <<- c("薬剤コード", "慣用区分", "使用区分1", "使用区分2", "基準名コード", "薬剤名",
                                    "薬剤名カナ", "一般名", "一般名カナ", "メーカーコード", "メーカーの略称",
                                     "剤形コード", "薬剤コード区分1", "メンテナンスSEQ", "メンテナンスFLG",
                                     "メンテ年月", "薬剤コード区分2")
  # 英名ファイル取り込み
  rawdata_idf_english <<- read.csv(idf_english_file_path, as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
  names(rawdata_idf_english) <<- c("メンテナンスSEQ", "薬剤コード", "英名", "JANフラグ", "INNフラグ", "予備",
                                    "英メンテ年月", "英メンテナンスFLG")
}
# merge IDF ------
mergeIdf = function(){
  # 英名IDFと日本語IDFを薬剤コードでマージし、必要列のみにして返す
  # IDFファイル取り込み
  if (!exists("rawdata_idf_japanese")) {
    loadIdf()
  }
  idf_japanese <- rawdata_idf_japanese[ , c("薬剤コード", "薬剤名", "一般名")]
  idf_english <- rawdata_idf_english[ , c("薬剤コード", "英名")]
  idf_english <- subset(idf_english, 英名!="")
  # 薬剤コードで日本語ファイルと英名ファイルをマージ
  idf <- merge(idf_japanese, idf_english, by="薬剤コード", all.x=T)
  return(idf)
}

# search IDF
extractGenericName = function(idf){
  # idfから一般名：薬剤コードが7桁のレコードを抽出して返す
  idf_generic_name <- subset(idf, sapply(idf[ ,"薬剤コード"], nchar) == 7)
  return(idf_generic_name)
}

# load input file ------
loadInputFile = function(input_file_name, file_encode){
  # input_file_nameを取り込んで返す
  input_file_path <- paste(input_path, input_file_name, sep="/")
  file_connection <- file(input_file_path, "r", encoding=file_encode)
  input_data <- readLines(con=file_connection)
  close(file_connection)
  return(input_data)
}

parseInputFile = function(input_file_name, file_encode){
  # input_file_nameを取り込んで整形して返す
  input_data <- loadInputFile(input_file_name, file_encode)
  # "#"で始まる行はコメントとして読み飛ばす。#の前の空白は許容しない
  temp_input_data <- subset(input_data, substring(input_data, 1, 1) != "#")
  for (i in 1:length(temp_input_data)) {
    # カンマで切り分けて前後の空白除去、大文字に変換
    temp_input <- unlist(strsplit(input_data[i], ","))
    for (j in 1:length(temp_input)) {
      temp_input[j] <- gsub("^[ ]+|[ ]+$", "", temp_input[j])
      temp_input[j] <- toupper(temp_input[j])
      if (!exists("parse_data")) {
        parse_data <- temp_input[j]
      } else {
        parse_data <- append(parse_data, temp_input[j])
      }
    }
  }
  return(parse_data)
}
