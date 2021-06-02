# 1. 텍스트마이닝
install.packages("tm")
library(tm)

data(crude)
summary(crude)[1:6,]                       # summary함수를 통해 저장된 데이터 형태를 파악할 수 있음

inspect(crude[1])                          # inspect함수로 문서의 character 수 등을 확인할 수 있음

crude[[1]]$content

install.packages("tm")
library(tm)

news<-readLines("키워드_뉴스.txt")               # text 파일을 readLines 함수로 불러오기
news

news.corpus<-VCorpus(VectorSource(news))         # 문서화와 함께 Corpus로 변환
news.corpus[[1]]$content

clean_txt<-function(txt){
  txt<-tm_map(txt, removeNumbers)                # 숫자제거
  txt<-tm_map(txt, removePunctuation)            # 문장부호 제거
  txt<-tm_map(txt, stripWhitespace)              # 공백제거
  return(txt)
}
clean.news<-clean_txt(news.corpus)
clean.news[[1]]$content                          # ',와 같은 부호는 제거되지 않음 
txt2<-gsub("[[:punct:]]", "", clean.news[[1]])   # gsub 함수는 엑셀 등에서의 찾아바꾸기의 기능을 함.

install.packages("KoNLP")
library(KoNLP)

useSejongDic()                                   # 세종사전 사용
sentence<-'아버지가 방에 스르륵 들어가신다.'
extractNoun(sentence)                            # '스르륵'은 명사가 아님

