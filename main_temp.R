# Package ----
library(dplyr)
library(quanteda)
library(topicmodels)
library(LSX)
library(stopwords)
library(ggplot2)
library(showtext)
showtext_auto()

# Data ----
# 读取文本数据。
gm_review <- read.csv("data_raw/gg_review_park_tsukuba.csv") %>% 
  rename_with(~ tolower(.x)) %>% 
  mutate(rating_count = as.numeric(rating_count)) %>% 
  tibble() %>% 
  mutate(rating = as.numeric(gsub(" つ星", "", rating))) %>% 
  # 对时间进行分类。
  mutate(review_time = case_when(
    review_time %in% c(
      "5 日前", "6 日前", "1 週間前", "2 週間前", "3 週間前", 
      "1 か月前", "2 か月前", "3 か月前", "4 か月前", "5 か月前", "6 か月前", 
      "7 か月前", "8 か月前", "9 か月前", "10 か月前", "11 か月前"
    ) ~ "1 year", 
    review_time %in% c("1 年前", "2 年前") ~ "2-3 year", 
    review_time %in% c("3 年前", "4 年前", "5 年前") ~ "3-5 year", 
    review_time %in% c("6 年前", "7 年前", "8 年前") ~ "6-8 year"
  )) %>% 
  mutate(name = case_when(
    name == "赤塚富士住建パーク（赤塚公園）" ~ "赤塚公園", 
    TRUE ~ name
  ))

# 构建语料库。
corp <- corpus(gm_review, text_field = "review")

# 词典。
dict <- dictionary(list(
  biodiversity = "生物 多様 性", 
  diversity = "多様 性", 
  global_warming = "地球 温暖 化", 
  warming = "温暖 化", 
  asoushi = "麻生 氏", 
  asoutarou = "麻生 太郎", 
  climate_change = "気候 変動", 
  ll = "地産 地 消",
  lc = "地 消",
  oa = "有機 農業"
))

# 定义停止词。
stopword <- c(
  stopwords("ja", source = "marimo"), 
  "amp", "ます", "です", "こと", "って", "てい", "という", "んで", "ので", 
  "なく", "など", "なる", "せん", "しま", "とか", "しょう", "ろう", "けど", 
  "さん", "あっ", "られる", "ぜひ"
)

# Token。
tok <- tokens(
  corp, 
  remove_symbols = TRUE, 
  remove_numbers = TRUE, 
  remove_url = TRUE, 
  remove_separators = TRUE, 
  remove_punct = TRUE
) %>% 
  # Bug: Should delete text with just a few words? 
  tokens_compound(pattern = dict, concatenator = "") %>% 
  # Keep tokens in Japanese. 
  # Bug: Need to keep the words in other language? 
  tokens_select(
    pattern = "^[ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE
  ) %>% 
  tokens_keep(min_nchar = 2) %>% 
  tokens_remove(pattern = stopword)

# 构建Document-term matrix。
df_matrix <- dfm(tok)

# 定义种子词汇。
seed_word <- 
  c(rep(1, 6), rep(-1, 4)) %>% 
  setNames(c(
    c("恵み", "絶賛", "創出", "綺麗", "きれい", "雄大"), 
    c("破壊", "危険", "壊滅", "悪い")
  ))

# 情感分析。
lss <- textmodel_lss(
  df_matrix, 
  seeds = seed_word, 
  # terms = context_word, 
  k = 300, 
  include_data = TRUE, 
  group_data = TRUE
)

lss_score <- 
  docvars(df_matrix) %>% 
  mutate(fit = predict(lss, newdata = df_matrix))

ggplot(lss_score %>% filter(
  review_time == "1 year" | review_time == "2-3 year" | review_time == "4-6 year"
), aes(name, fit)) + 
  geom_violin() + 
  geom_jitter(alpha = 0.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_cartesian(ylim = c(-2.5, 2.5))

# 计算中位数。
lss_score %>% 
  filter(
    review_time == "1 year" | 
      review_time == "2-3 year" | 
      review_time == "4-6 year"
  ) %>% 
  group_by(name) %>% 
  summarise(mid = median(fit, na.rm = TRUE), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(name, mid)) + 
  theme(axis.text.x = element_text(angle = 90))

# 主题分析。
lda_model <- LDA(
  convert(df_matrix, to = "topicmodels"), k = 2, control = list(seed = 1234)
) 
# 每个主题下最重要的 5 个词。
terms(lda_model, 10)  

# 分地点主题分析。
lda_model_loc <- split(tok, docvars(df_matrix)$name) %>% 
  lapply(., function(x) {
    dfm(x) %>% 
      convert(to = "topicmodels") %>% 
      LDA(., k = 3, control = list(seed = 1234))
  })
lapply(lda_model_loc, terms, k = 10)

# 差评主题分析。
lda_model_loc_neg <- split(
  tokens_subset(tok, rating <= 3), 
  docvars(df_matrix)[which(docvars(df_matrix)$rating <= 3), ]$name
) %>% 
  lapply(., function(x) {
    dfm(x) %>% 
      convert(to = "topicmodels") %>% 
      LDA(., k = 3, control = list(seed = 1234))
  })
lapply(lda_model_loc_neg, terms, k = 10)
