rm(list=ls())
gc()
setwd('')

require(stringr)
require(C50)

# Reading data
train = read.csv('../Data/data.csv')
blindData = read.csv('../Data/blindset_table_out.csv')

train$table.text = as.character(train$table.text)
blindData$table.text = as.character(blindData$table.text)
train$url = as.character(train$url)
blindData$url = as.character(blindData$url)
blindData$site = as.character(blindData$site)

# Adding prefix 'www' wherever it is not present
blindData$site[substr(blindData$site,0,3)!='www'] = paste('www.',blindData$site[substr(blindData$site,0,3)!='www'],sep='')
train$url[substr(train$url,0,3)!='www'] = paste('www.',train$url[substr(train$url,0,3)!='www'],sep='')

# Creating training and eval data with length as an attribute
model_data = data.frame(target=train$label,length=nchar(train$table.text))
eval_data = data.frame(length=nchar(blindData$table.text))

# extracting website from train and eval data
model_data$webSite = substr(train$url,5,str_locate(train$url,'.com')-1)
model_data$webSite1 = substr(train$url,5,str_locate(train$url,'.co.')-1)
model_data$webSite[is.na(model_data$webSite)] = model_data$webSite1[is.na(model_data$webSite)]
model_data$webSite1 = NULL
model_data$webSite[model_data$webSite==''] = 'competitiveedgeproducts'

eval_data$webSite = substr(blindData$site,5,str_locate(blindData$site,'.com')-1)
eval_data$webSite[eval_data$webSite==''] = 'competitiveedgeproducts'

# Feature Generation
# Number of div elements in the page
model_data$divElements = str_count(train$table.text,'/div')
eval_data$divElements = str_count(blindData$table.text,'/div')

# Number of tr elements in the page
model_data$trElements = str_count(train$table.text,'/tr')
eval_data$trElements = str_count(blindData$table.text,'/tr')

# Number of h1 elements in the page
model_data$h1Elements = str_count(train$table.text,'/h1')
eval_data$h1Elements = str_count(blindData$table.text,'/h1')

# Number of h2 elements in the page
model_data$h2Elements = str_count(train$table.text,'/h2')
eval_data$h2Elements = str_count(blindData$table.text,'/h2')

# Number of h3 elements in the page
model_data$h3Elements = str_count(train$table.text,'/h3')
eval_data$h3Elements = str_count(blindData$table.text,'/h3')

# Number of h4 elements in the page
model_data$h4Elements = str_count(train$table.text,'/h4')
eval_data$h4Elements = str_count(blindData$table.text,'/h4')

# Number of h5 elements in the page
model_data$h5Elements = str_count(train$table.text,'/h5')
eval_data$h5Elements = str_count(blindData$table.text,'/h5')

# Number of p elements in the page
model_data$paraElements = str_count(train$table.text,'/p')
eval_data$paraElements = str_count(blindData$table.text,'/p')

# Number of links in the page
model_data$linksCount = str_count(train$table.text,'href')
eval_data$linksCount = str_count(blindData$table.text,'href')

# Number of jpg images in the page
model_data$imagesCount = str_count(train$table.text,'.jpg')
eval_data$imagesCount = str_count(blindData$table.text,'.jpg')

# Number of input elements in the page
model_data$inputsCount = str_count(train$table.text,'/input')
eval_data$inputsCount = str_count(blindData$table.text,'/input')

# Doing a 5 fold validation
set.seed(123)
recalls = vector()
precisions = vector()
for(i in 1:5)
{
  rows = sample(nrow(model_data),round(0.2*nrow(model_data)))
  train_new = model_data[-rows,]
  test_new = model_data[rows,]
  dt = C5.0(target~.,train_new)
  pred = predict(dt,test_new)
  recalls = c(recalls,round(sum(pred==test_new$target & pred=='yes' & test_new$target=='yes')*100/sum(test_new$target=='yes'),2))
  precisions = c(precisions,round(sum(pred==test_new$target & pred=='yes' & test_new$target=='yes')*100/sum(pred=='yes'),2))
  rm(rows,train_new,test_new,dt)
}
rm(i)
cat('Recall :',mean(recalls))
cat('Precision :',mean(precisions))
cat('F1 Score :',mean(round(2*precisions*recalls/(precisions+recalls),2)))

dt = C5.0(target~., model_data)
submit = predict(dt,eval_data)
write.csv(submit,'../Submissions/Submit1.csv',row.names=F)