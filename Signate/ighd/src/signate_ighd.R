# Signate
# 背景
# 土地の販売価格は様々な要因によって決定します。土地そのものの特性はもちろん、面している道路などの周辺環境や駅からの距離といった利便性なども関係しています。
# 今回のコンペでは、埼玉県における過去の土地の販売実績データを用い、土地の販売価格を予測するアルゴリズムの作成に挑戦していただきます。
# タスク説明
# データは大きく「現場」「号棟」の2種類に分かれています。「現場」データには、"PJ番号"で管理される区画毎に土地の情報が記録されています。この「現場」データで管理される区画に対して、1つ以上の号棟が割り当てられ、その情報が「号棟」データに記録されています。（下図参照）
# 今回は、この「号棟」単位の販売価格を予測していただきます。
# 評価関数
# ・平均絶対誤差率「MAPE（Mean Absolute Percentage Error）」を使用します。
# ・評価値は0以上の値をとり、精度が高いほど小さな値となります。 
# 最終順位の決定
# 1. コンテスト最終日までの評価（暫定評価）は評価用データセットの一部で評価し、コンテスト終了後の評価（最終評価）は評価用データセットの残りの部分で評価します。
# スコアボードはコンテスト終了時に自動的に最終評価に切り替わり、それを元に最終順位を決定します。このため、開催中と終了後では順位が大きく変動する場合もあります。
# 2. スコアが同値の場合は、早い日時でご応募いただいた参加者を上位とします。
# 3. 入賞候補者の方には順位確定の際に下記の情報を提出していただきます。
# ・予測モデルのソースコード
# ・学習済モデル
# ・予測結果の再現の為の手順書（前処理部分、学習部分、予測部分が分かるよう明記）
# ・実行環境（OSのバージョン、使用ソフトウェア及び解析手法）　
# ・乱数シード（Random Forest等の乱数を利用した手法の場合）
# ・各説明変数の予測モデルへの寄与度（寄与度の算出が可能な手法を用いた場合）
# ・データの解釈、工夫点、モデリングから得られる示唆等
# 4. 再現性検証期間中、入賞候補者及び、その提出モデルが下記いずれかに該当する場合は懸賞の獲得資格を失います。
# ・事務局からの手続き上の連絡・要求に対して指定された期限内に対応しない
# ・参加条件やルールを満たしていない
# ・モデルの予測結果を再現できない

# tryするpackage
# tidymodels
# mlr
# esquisse
# https://suryu.me/post/osm_nominatim/
setwd("/Users/hanadate/work/hana_train/Signate/ighd")
library(OpenStreetMap)
library(rgdal)
map <- openmap(c(50,125), c(25,145))
plot(map)
library(tidyverse)
library(mlr)
library(esquisse)
library(DT)
list.files("dat")

# data def
dat.def <- read_delim("dat/data_definition.txt", delim="\t")
dat.def %>% datatable(., options = list(pageLength=1000))
# sample submit
sample.submit <- read_tsv("dat/sample_submit.tsv", col_names = FALSE)
sample.submit %>% glimpse
# test goto
test.goto <- read_tsv("dat/test_goto.tsv")
test.goto %>% glimpse
# test genba
test.genba <- read_tsv("dat/test_genba.tsv")
test.genba %>% glimpse
# train goto
train.goto <- read_tsv("dat/train_goto.tsv")
train.goto %>% glimpse
# train genba
train.genba <- read_tsv("dat/train_genba.tsv")
train.genba %>% glimpse

# geocoding
# 向きをone hot
# 階と間取りをone hot



# mlr tutorial
## 1) タスクの定義 ----
# ここではデータと応答変数、問題の種類を定義する。
task = makeClassifTask(data = iris, target = "Species")

## 2) 学習器の定義 ----
# アルゴリズムを選択する。
lrn = makeLearner("classif.lda")

# (データを訓練セットとテストセットに分割する)
n = nrow(iris)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) 訓練 ----
# 訓練セットのランダムなサブセットを用いて学習器を訓練し、モデルを作成
model = train(lrn, task, subset = train.set)

## 4) 予測 ----
# モデルに基いてテストセットの入力変数に対応する出力を予測する
pred = predict(model, task = task, subset = test.set)

## 5) 評価 ----
# 誤分類率並びに精度を計算する
performance(pred, measures = list(mmce, acc))



