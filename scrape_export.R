# system.time(source("scrape_export.R"))

library(XML)
library(dplyr)

# 注：部分相邻章节的网页名数字不连续
# 37048
# 1. Get the URL from Outline page
outline <- "http://www.qbkan.com/mindiaojuyiwenluzhimianchuan/"
doc <- htmlTreeParse(outline, useInternal = TRUE, trim = TRUE) 

hrefs <- xpathSApply(doc, "//dl[@class='chapterlist cate']//a", xmlGetAttr, "href")
chapters <- xpathSApply(doc, "//dl[@class='chapterlist cate']//a", xmlValue)
target <- data.frame(chapter = chapters, href = hrefs, stringsAsFactors = F)
target <- target %>%
  mutate(chapter = sapply(chapter, function(x) gsub("第一八", "第一百", x)),
         chapter = sapply(chapter, function(x) gsub("第三包", "第三百", x))
  )

# 宋梦桥崖 第一章 怪客来访: "/book/35380.html"
smqy_target <- target[grep("35380", target$href):nrow(target), ]
smqy_target <- smqy_target %>% filter(!grepl("3524[7-8]", href)) %>%
  mutate(no = sapply(chapter, function(x) substr(x, regexpr("第", x)[1]+1, nchar(x))),
         no = sapply(no, function(x) substr(x, 1, regexpr(" ", x)[1]-2))
  ) %>%
  arrange(href, no)
cn_num <- read.delim("cn_num.txt", as.is = T)
smqy_target <- smqy_target %>% left_join(cn_num, by = c("no" = "cn")) %>%
  arrange(num)

pages <- paste0("http://www.qbkan.com", smqy_target$href)


ExtractText <- function(x) {
  doc <- htmlTreeParse(x, useInternal = TRUE)
  text <- sapply(getNodeSet(doc, '//div[@id="BookText"]'), xmlValue)
  text <- gsub("全本看  百度搜索 全本看更新最快的小说站！", "", text)
  gsub("全本看", "", text)
}

for (i in 1:length(pages)) {
    tryCatch(
    out <- ifelse(i == 1,
                paste0(i, "@\n\r", sapply(pages[i], ExtractText)),
                paste0(out, "\n\r", i, "@\n\r", sapply(pages[i], ExtractText))
    ),
    error = function(e) {cat("ERROR :", pages[i], "\n")}
  )
}

writeLines(out, "宋梦桥崖.txt")
save.image("ready.Rda")
