library (plyr)
library(rvest)
library(dplyr)
library(xlsx)
library(XML)
library(purrr)
library(rvest)
library(purrr)
library(dplyr)
library(lubridate)
library(stringr)
library(jiebaR)
library(wordcloud) # 非互動式文字雲
library(wordcloud2)
library(ggplot2)
library(httr)
library(RCurl)
library(XML)

pttfinal <- map(pttcode,ptturl)
pttunlist <- unlist(pttfinal)
pttunlist[grep("疫苗",pttunlist)]


######################################################################################################

#文章標題＆網址
pttCode <- paste0("https://www.ptt.cc/bbs/HatePolitics/index",4013:4037,".html")

pttFucTitle <- function(url){
  urlText <-  url %>% GET( set_cookies(over18 = 1))%>% read_html () %>% 
    html_nodes(css = ".title a") %>% html_text()
  return(urlText)
}
pttFucCode <- function(url){
  urlText <-  url %>% GET( set_cookies(over18 = 1)) %>% read_html () %>% 
    html_nodes(css = ".title a") %>% html_attr('href') 
  
  return(urlText)
}
pttfinalTitle <- map(pttCode,pttFucTitle)
pttfinalCode <-  map(pttCode,pttFucCode)
TitleUnlist <- unlist(pttfinalTitle)
CodeUnlist  <- unlist(pttfinalCode)

print(pttfinalCode)
pttDF <- data.frame(Title=TitleUnlist[grep("",TitleUnlist)],
                    Code = CodeUnlist[grep("",CodeUnlist)])
pttDF$Code <- paste0("https://www.ptt.cc/",pttDF$Code)

pttcontent <- pttDF$Title
toString(pttcontent)
paste(pttcontent,collapse=" ")
cutter <- worker(bylines = F)


#pttFunction

articleF <- function(url){
  #文章內容

  articleUrlText <-  url %>% GET( set_cookies(over18 = 1))%>% read_html () %>% 
    html_nodes(css = "#main-content") %>% html_text()
  article_clean <- articleUrlText %>% gsub(pattern = "作者.+:[0-9]{2}\\s[0-9]{4}?",.,replacement = "")%>% 
    gsub(pattern = "(\n--\n※).+",., replacement = "") %>% 
    gsub(pattern = "(http|https)://[a-zA-Z0-9./?=_-]+",., replacement = "") %>%
    gsub(pattern = "引述《[a-zA-Z0-9./_()].+》之銘言",., replacement = "") %>% #
    gsub(pattern = "Sent from [a-zA-Z0-9 -./_()]+",., replacement = "") %>% 
    gsub(pattern = "<U[a-zA-Z0-9 +]+>",., replacement = "") %>%
    gsub(pattern = "\n",., replacement = "") %>% data.frame(url)
  return(article_clean)
  
  #文章留言
}
messageF <- function(url){

  MessageUrlText <-  url %>% GET( set_cookies(over18 = 1))%>% read_html () %>% 
    html_nodes(css = ".push-content") %>% html_text() %>% cbind(url)
  UserUrlText <-  url %>% GET( set_cookies(over18 = 1))%>% read_html () %>% 
    html_nodes(css = ".push-userid") %>% html_text() %>% cbind(MessageUrlText)
  return(UserUrlText)
}
userF <-function(url){
  UserUrlText <-  url %>% GET( set_cookies(over18 = 1))%>% read_html () %>% 
    html_nodes(css = ".push-userid") %>% html_text()
  return(UserUrlText)
}
pttArticleCode <- pttDF$Code[1:length(pttDF$Code)-1]
print(pttArticleCode[1])
#資料整合                 
messageResult <- map(pttArticleCode,messageF)
messageResultF <- ldply(messageResult,data.frame)
messageResult <- unlist(messageResult)
#userResult  <- map(pttArticleCode,userF)
#userResult<-unlist(userResult)

#MessageDF <- data.frame(User =userResult,Message =messageResultF)
#MessageDF <- cbind(userResult,messageResultF)
colnames(messageResultF) <- c("使用者","留言","Code")


articleResult <- map(pttArticleCode,articleF)
articleResultF <- ldply(articleResult,data.frame)
articleResult <- unlist(articleResult)
colnames(articleResultF) <- c("文章","Code")

finalResult <- paste0(articleResult,pttcontent,messageResult)
finalResult <- unlist(finalResult)

#最後資料合併
finalDF <- merge(messageResultF,pttDF,by="Code",all = T) %>% group_by(Code)
finalDF <- merge(finalDF,articleResultF,by="Code",all = T) %>% group_by(Code)

#文字雲
filePath <- "/Users/laichunming/Desktop/work/data.txt"
write.table(finalResult,file = filePath,row.names = FALSE)
result <- read.table(filePath,header = FALSE,stringsAsFactors = FALSE)
result <-toString(result)
cutter[result]
new_words<- c("黃國昌","快篩","姚文智","性侵","莫德納","蔡壁如","柯文哲","賴清德","AZ","蔡英文","黃偉哲",
              "郭婞淳","為什麼","五倍卷","三倍卷","五倍券","三倍券","下一任","輩份","掉鏈子","江啟臣",
              "盧秀燕","綠共","朱立倫","陳時中","為什麼","高端","紓困","台灣人","人民")
for(i in 1:length(new_words)){
  new_user_word(cutter,new_words[i])
}
result <- str_remove_all(result,"[0-9a-zA-Z]+?")
cutter[result]
writeLines(new_words, "new_words.txt")
stop_words <- c("在","的","下","個","來","至","座","亦","與","或","日","月","年","週","了","就","了","都",
                "快","　","討論","新聞","他","她","問","呢","啦","是","啊","這","被","你","吧","的","我",
                "要","就是","有","不","也","嗎","耶","阿","啥","只","再","喔","才","又","很","這","跟",
                "會","說","去","到","那","但","問卦","︰","和","後","誰","把","而","且","由","人")
writeLines(stop_words, "stop_words.txt")
cutter <- worker(user = "new_words.txt", stop_word = "stop_words.txt", bylines = FALSE)
seg_words <- cutter[result]
txt_freq <- freq(seg_words)
txt_freq <- arrange(txt_freq, desc(freq))
txt_freq
#wordcloud2(filter(txt_freq, freq > 20), 
#           minSize = 2, fontFamily = "Microsoft YaHei", size = 1)
par(family=("Heiti TC Light")) 
wordcloud(txt_freq$char, txt_freq$freq, min.freq = 50 , random.order = F,
          ordered.colors = F,colors = rainbow(nrow(txt_freq)))

#輸出資料
csvPath <- "/Users/laichunming/Desktop/work/20210817_HatePolitics.csv"
write.csv(finalDF,file = csvPath,row.names = F)














  