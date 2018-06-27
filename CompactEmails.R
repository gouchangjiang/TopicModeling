setwd("C:/Users/JoAnn/Desktop/topicModeling")
rm(list=ls())

basedir<-list.dirs("./maildir",full.names = TRUE,recursive = FALSE)

Get_Email_content<-function(dir){
  mail<-readLines(dir)
  #mailID<-mail[1]
  content<-mail[(which(mail=="")[1]+1):length(mail)]
  content<-noquote(content)
  content<-paste(content,sep='',collapse = '')
  return(content)
}

Get_Email_ID<-function(dir){
  mail<-readLines(dir)
  mailID<-mail[1]
  return(mailID)
}

#----------------compact emails in all_documents-----------------------
all_documents_dir<-unlist(lapply(basedir,function(x) {paste(x,'/all_documents',sep='')}))
all_mails<-lapply(all_documents_dir, function(x){list.files(x,full.names=TRUE)})

checklist<-unlist(lapply(all_mails,function(x){length(x)==0}))
checkdir<-basedir[checklist]

all_Email_content<-lapply(all_mails, function(x){lapply(x, Get_Email_content)})
all_Email_ID<-lapply(all_mails, function(x){lapply(x, Get_Email_ID)})

EmailDF<-as.data.frame(unlist(all_Email_content))
EmailIDDF<-as.data.frame(unlist(all_Email_ID))
EmailDF$id<-EmailIDDF$`unlist(all_Email_ID)`
colnames(EmailDF)[1]<-"content"

#----------------compact emails in sent_items, inbox-----------------------

#nextdir<-'/sent_items'
nextdir<-'/inbox'

mail_dir<-unlist(lapply(checkdir,function(x) {paste(x,nextdir,sep='')}))
all_mails<-lapply(mail_dir, function(x){list.files(x,pattern='[0-9]',recursive = TRUE,full.names=TRUE)})

#checklist<-unlist(lapply(all_mails,function(x){length(x)==0}))
#checkdir<-basedir[checklist]

all_Email_content<-lapply(all_mails, function(x){lapply(x, Get_Email_content)})
all_Email_ID<-lapply(all_mails, function(x){lapply(x, Get_Email_ID)})

EmailDF1<-as.data.frame(unlist(all_Email_content))
EmailIDDF1<-as.data.frame(unlist(all_Email_ID))
EmailDF1$id<-EmailIDDF1$`unlist(all_Email_ID)`
colnames(EmailDF1)[1]<-"content"
Emailout<-rbind(EmailDF,EmailDF1)

length(unique(Emailout$id))

write.table(Emailout$content,"allEmails.txt",row.names = FALSE,col.names = TRUE,quote = TRUE)
