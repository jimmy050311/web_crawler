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
library(devtools)
library(githubinstall)
library(Rfacebook)
library(RSelenium)
#java -jar selenium-server-standalone-3.141.59.jar
############################################################################################################

url <- "https://www.facebook.com/"

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")
remDr$open()
remDr$navigate(url)

emailPath <- "//*[@id='email']"
passWordPath <- "//*[@id='pass']"
loginPath <- "div._6ltg"

btn1 <- remDr$findElement(using = "xpath",value = emailPath)
btn1$clickElement()
text <- list("chintan5311@gmail.com")
btn1$sendKeysToElement(text)

btn2 <- remDr$findElement(using = "xpath",value = passWordPath)
btn2$clickElement()
text <- list("jimmy25821117")
btn2$sendKeysToElement(text)

btn3 <- remDr$findElement(using = "css",value = loginPath)
btn3$clickElement()



urlVector1 <- "https://www.facebook.com/chengwentsan/posts/385499692939242"
urlVector2 <- "https://www.facebook.com/chengwentsan/posts/384749373014274"
urlVector3 <- "https://www.facebook.com/chengwentsan/posts/384222643066947"
urlVector4 <- "https://www.facebook.com/chengwentsan/posts/383886243100587"
urlVector5 <- "https://www.facebook.com/chengwentsan/posts/383465973142614"
fbDfFinal <- NULL
#方法執行
map(urlVector1,MessageF)
map("",MessageFullF)
fbText1 <- map("",MessageFunctionFinal)
fbM <- map("",DfFunctionM)
fbN <- map("",DfFunctionN)
fbDF <- data.frame(fbM,fbN)
colnames(fbDF) <- c("Message","Name")
fbDfFinal <- rbind(fbDfFinal,fbDF)
colnames(fbDfFinal) <- c("Message","Name")

#-------------------------------------------------
map(urlVector2,MessageF)
map("",MessageFullF)
fbText2 <- map("",MessageFunctionFinal)
fbM <- map("",DfFunctionM)
fbN <- map("",DfFunctionN)
fbDF <- data.frame(fbM,fbN)
colnames(fbDF) <- c("Message","Name")
fbDfFinal <- rbind(fbDfFinal,fbDF)
colnames(fbDfFinal) <- c("Message","Name")
#-------------------------------------------------
map(urlVector3,MessageF)
map("",MessageFullF)
fbText3 <- map("",MessageFunctionFinal)
fbM <- map("",DfFunctionM)
fbN <- map("",DfFunctionN)
fbDF <- data.frame(fbM,fbN)
colnames(fbDF) <- c("Message","Name")
fbDfFinal <- rbind(fbDfFinal,fbDF)
colnames(fbDfFinal) <- c("Message","Name")
#-------------------------------------------------
map(urlVector4,MessageF)
map("",MessageFullF)
fbText4 <- map("",MessageFunctionFinal)
fbM <- map("",DfFunctionM)
fbN <- map("",DfFunctionN)
fbDF <- data.frame(fbM,fbN)
colnames(fbDF) <- c("Message","Name")
fbDfFinal <- rbind(fbDfFinal,fbDF)
colnames(fbDfFinal) <- c("Message","Name")
#-------------------------------------------------
map(urlVector5,MessageF)
map("",MessageFullF)
fbText5 <- map("",MessageFunctionFinal)
fbM <- map("",DfFunctionM)
fbN <- map("",DfFunctionN)
fbDF <- data.frame(fbM,fbN)
colnames(fbDF) <- c("Message","Name")
fbDfFinal <- rbind(fbDfFinal,fbDF)
colnames(fbDfFinal) <- c("Message","Name")
#-------------------------------------------------

fbText1 <- as.character(fbText1)
fbText2 <- as.character(fbText2)
fbText3 <- as.character(fbText3)
fbText4 <- as.character(fbText4)
fbText5 <- as.character(fbText5)
fbTextFinal <- paste0(fbText1,fbText2,fbText3,fbText4,fbText5)
fbTextFinal <- as.character(fbTextFinal)
map(fbTextFinal,wordClouldFunction)
csvPath <- "/Users/laichunming/Desktop/work/20210817_市長.csv"
write.csv(fbDfFinal,file = csvPath,row.names = F)
fbTextFinal
#--------------------------------------FUNCTION-------------------------------------------------------------

MessageF <- function(url){
    remDr$navigate(url)  
    Sys.sleep(2)
    btnComfirm <- remDr$findElement(using = "css",value = "body._6s5d._71pn._-kb.sf")
    btnComfirm$clickElement()
    Sys.sleep(2)
    #  btn4 <- remDr$findElement(using = "xpath",value = "//*[(@id = 'jsc_c_u')]")
    #  btn4$clickElement()
    btn5 <- remDr$findElement(using = "css",value = "div.rq0escxv.l9j0dhe7.du4w35lb.nc684nl6.g0qnabr5")
    btn5$clickElement()
    
    Sys.sleep(2)
    btn6 <- remDr$findElement(using = "css",value = "div.oajrlxb2.g5ia77u1.qu0x051f.esr5mh6w.e9989ue4.r7d6kgcz.rq0escxv.nhd2j8a9.j83agx80.p7hjln8o.kvgmc6g5.oi9244e8.oygrvhab.h676nmdw.pybr56ya.dflh9lhu.f10w8fjw.scb9dxdr.i1ao9s8h.esuyzwwr.f1sip0of.lzcic4wl.l9j0dhe7.abiwlrkh.p8dawk7l.bp9cbjyn.dwo3fsh8.btwxx1t3.pfnyh3mw.du4w35lb")
    btn6$clickElement()
    while(TRUE){
      tryCatch({
        Sys.sleep(2)
        btn_more = remDr$findElement(using = "css",value = 'span.j83agx80.fv0vnmcu.hpfvmrgz')
        print(t)
        remDr$mouseMoveToLocation(webElement = btn_more)
        btn_more$clickElement()
        print("按一下")
        Sys.sleep(3)
      },warning = function(warn){
        print("warning")
      },error = function(err){
        print("沒得按")
        break
      },finally = {
        print("")
      }
      )
    }
    return(NULL)
  }
  
MessageFullF <- function(a){
  while(TRUE){
    tryCatch({
      Sys.sleep(2)
      
      btn_more = remDr$findElement(using = "css",value = 'div.oajrlxb2.g5ia77u1.qu0x051f.esr5mh6w.e9989ue4.r7d6kgcz.rq0escxv.nhd2j8a9.nc684nl6.p7hjln8o.kvgmc6g5.cxmmr5t8.oygrvhab.hcukyx3x.jb3vyjys.rz4wbd8a.qt6c0cv9.a8nywdso.i1ao9s8h.esuyzwwr.f1sip0of.lzcic4wl.oo9gr5id.gpro0wi8.lrazzd5p')
      remDr$mouseMoveToLocation(webElement = btn_more)
      btn_more$clickElement()
      
    },warning = function(warn){
      print("warning")
    },error = function(err){
      print("沒得按")
      break
    },finally = {
      print("按一下")
      
    }
    )}
  return(NULL)
}
  
MessageFunctionFinal <- function(a){
  fbCraw <- remDr$findElement(using = "xpath",value = "//div")
  fbText <- fbCraw$getElementText()
  
  fbText <- fbText %>% gsub(pattern =  "(\n--\n※).+",.,replacement = "") %>%
    gsub(pattern = "\n",.,replacement = "")
  fbText
  
  return(fbText)
}
DfFunctionM <- function(a){
  PageArray <- NULL
  page <- read_html(remDr$getPageSource()[[1]]) %>% html_nodes("div.tw6a2znq.sj5x9vvc.d1544ag0.cxgpxx05") %>% html_text()
  page <- page %>% gsub(pattern = "回覆鄭文燦......",.,replacement = "撰寫回覆……")
  page <- page %>% gsub(pattern = "回覆 Nash，神之領域......",.,replacement = "撰寫回覆……")
  grepNum <- grep("撰寫回覆……",page)
  if(length(grepNum) != 0){
    page <- page[-grepNum]
  }
  page
  grepString <-  grep('`回覆 Nash，神之領域......',page)
  grepString
  return(page)
}
DfFunctionN <- function(a){
  NameArray <- NULL
  pageName <- read_html(remDr$getPageSource()[[1]]) %>% html_nodes("span.pq6dq46d") %>% html_text()
  
  pageName <- pageName %>% gsub(pattern = "[0-9]",.,replacement = "")
  pageName <- pageName %>% gsub(pattern = "+",.,replacement = "")
  pageName
  
  for(x in pageName){
    if(x == ""|| x =="+" || x==" ")next
    
    NameArray <- c(NameArray,x)
  }
  
  NameArray <- c("留言",NameArray)
  NameArray
  
  pageMessage <- read_html(remDr$getPageSource()[[1]]) %>% html_nodes("div.tw6a2znq.sj5x9vvc.d1544ag0.cxgpxx05") %>% html_text()
  pageMessage
  return(NameArray)
}

#fb <- remDr$findElement(using = "css",value = "div.b3i9ofy5.e72ty7fz.qlfml3jp.inkptoze.qmr60zad.rq0escxv.oo9gr5id.q9uorilb.kvgmc6g5.cxmmr5t8.oygrvhab.hcukyx3x.d2edcug0.jm1wdb64.l9j0dhe7.l3itjdph.qv66sw1b")
#fbT <- fb$getElementText()

wordClouldFunction <- function(fbText){
  #寫入txt
  filePath <- "/Users/laichunming/Desktop/work/dataFB.txt"
  write.table(fbText,file = filePath,row.names = FALSE)
  
  #文字雲
  cutter <- worker(bylines = F)
  result <- read.table(filePath,header = FALSE,stringsAsFactors = FALSE)
  result <-toString(result)
  cutter[result]
  new_words<- c("黃國昌","快篩","姚文智","性侵","莫德納","蔡壁如","柯文哲","賴清德","AZ","蔡英文","黃偉哲",
                "郭婞淳","為什麼","五倍卷","三倍卷","五倍券","三倍券","下一任","輩份","掉鏈子","江啟臣",
                "盧秀燕","綠共","朱立倫","AZ","az","紓困","能力","時機","人民")
  for(i in 1:length(new_words)){
    new_user_word(cutter,new_words[i])
  }
  result <- str_remove_all(result,"[0-9]+?")
  cutter[result]
  writeLines(new_words, "new_words.txt")
  stop_words <- c("在","的","下","個","來","至","座","亦","與","或","日","月","年","週","了","就","了","都",
                  "快","　","討論","新聞","他","她","問","呢","啦","是","啊","這","被","你","吧","的","我",
                  "要","就是","有","不","也","嗎","耶","阿","啥","只","再","喔","才","又","很","打","這","跟",
                  "會","說","去","到","那","但","讚","天","回覆","留言","小時","欸","讓","人")
  writeLines(stop_words, "stop_words.txt")
  cutter <- worker(user = "new_words.txt", stop_word = "stop_words.txt", bylines = FALSE)
  seg_words <- cutter[result]
  txt_freq <- freq(seg_words)
  txt_freq <- arrange(txt_freq, desc(freq))
  txt_freq
  
  par(family=("Heiti TC Light")) 
  wordcloud(txt_freq$char, txt_freq$freq, min.freq = 2 ,random.order = F,
            ordered.colors = F,colors = rainbow(nrow(txt_freq)))
  
}



