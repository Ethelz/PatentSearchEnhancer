
library(rvest)
library(xml2)

url <- "http://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO2&Sect2=HITOFF&p=1&u=%2Fnetahtml%2FPTO%2Fsearch-bool.html&r=0&f=S&l=50&TERM1=electric&FIELD1=&co1=AND&TERM2=&FIELD2=&d=PTXT"

web <- read_html(url)

patent <- web %>% html_nodes('td a')
count <- 0
repeat{
  count <- count+1
  patent<-patent[-1]
  if (count>4){
    break
  }
  patent<-patent[-length(patent)]
}

p_text <- patent %>% html_text()
i=1
for (i in seq_along(p_text)) {
  if(i%%2==0)
    p_title[length(p_title)] <-p_text[i] else
      p_num[length(p_title)] <- p_text[i]
}
p_title<-na.omit(p_title)
p_num<-na.omit(p_num)

p_title<-p_title[-1]
p_num<-p_num[-1]

p_link <- patent %>% html_attrs()

for (i in seq_along(p_link)) {
  p_link[i]<-paste("http://patft.uspto.gov",p_link[[i]],sep="")
}
p_link <- unique(p_link)

for (i in seq_along(p_link)) {
  web_result <- paste(unlist(p_link[i]),collapse = "")
  p_abstract[i] <- read_html(web_result) %>%
    html_nodes('body p') %>%
    html_text()
}

library(dplyr)
library(tidytext)
library(text2vec)
library(readr)
library(stringr)

p_data<- data.frame(Number=p_num,Title=p_title,Abstract=p_abstract,Link=unlist(p_link))

not_blank<-grep('a',p_data$Abstract)

p_clear<-" "
p_data_fil<-p_clear
p_data_fil<-p_data_fil[-1]
for (i in seq_along(not_blank)) {
  p_data_save<-p_data[not_blank[i],]
  p_data_fil<-rbind(p_data_fil,p_data_save)
}
  


# data cleaning function
prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ") 
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")}

# clean the job description data and create a new column
p_data_fil$abstract_clean =
  prep_fun(p_data_fil$Abstract)

# use vocabulary_based vectorization
it_patent = itoken(p_data_fil$abstract_clean,
                   progressbar = FALSE)
v_patent = create_vocabulary(it_patent)
v_patent = prune_vocabulary(v_patent, doc_proportion_max
                            = 0.1, term_count_min = 5)
vectorizer_patent = vocab_vectorizer(v_patent)

# apply TF-IDF transformation
dtm_patent = create_dtm(it_patent, vectorizer_patent)
tfidf = TfIdf$new()
dtm_tfidf_patent = fit_transform(dtm_patent, tfidf)

# compute similarity-score against each row
patent_tfidf_cos_sim = sim2(x = dtm_tfidf_patent, method = "cosine", norm = "l2")
patent_tfidf_cos_sim[1:5,1:5]

# create a new column for similarity_score of dataframe
p_data_fil["similarity_score"] =
  patent_tfidf_cos_sim[1:50]
# sort the dataframe by similarity score
p_output<-p_data_fil[order(-p_data_fil$similarity_score),]
