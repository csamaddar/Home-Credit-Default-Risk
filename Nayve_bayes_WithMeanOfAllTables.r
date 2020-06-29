library (ROCR)
library(plyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(naivebayes)

####EDA
## Number of NA
library(data.table)

numeric_list <- unlist(lapply(tr, is.numeric))
dt1_num <- setDT(tr)[,..numeric_list]
mv <- as.data.frame(apply(dt1_num, 2, function(col)sum(is.na(col))/length(col)))
colnames(mv)[1] <- "missing_values"


#Function to change index to column
index_to_col <- function(data, Column_Name){
  data <- cbind(newColName = rownames(data), data)
  rownames(data) <- 1:nrow(data)
  colnames(data)[1] <- Column_Name
  return (data)
}
mv <- index_to_col(mv,'Column')
mv <- setDT(mv)[order (missing_values,decreasing = T)]



ggplot (mv[1:40,], aes (reorder(Column, missing_values), missing_values)) + geom_bar (position = position_dodge(), stat = "identity") + coord_flip () + xlab('Columns') + ylab('Missing Value %')
ggplot (mv[1:40,], aes (reorder(Column, missing_values),  missing_values)) + geom_bar ( position = position_dodge(), stat = "identity", fill="steelblue") + xlab('Columns') + ylab('NAs in %') +theme(axis.text.x = element_text(angle = 30))

?reorder


  
  ########## Barplot for categorical columns

doPlots(dt1_non_num, plotBar, ii = 1:9)
View(tr)
ggplot(tr, aes(x=factor(TARGET)))+    geom_bar(stat="count", width=0.7, fill="steelblue")+    theme_minimal()

ggplot(tr, aes(x=factor(tr$NAME_CONTRACT_TYPE)))+    geom_bar(stat="count", width=0.7, fill="steelblue")+    theme_minimal()

ggplot(tr, aes(x=factor(tr$NAME_INCOME_TYPE)))+    geom_bar(stat="count", width=0.7, fill="steelblue")+    theme_minimal()

str(tr)
summary(tr)





##################### model fitting - naive bayes ############
getwd()
library(magrittr)

# "C:/Users/Charu/Documents"

setwd("E:/Sem3 - 218256/Machine Learning 1 - ANLY 530 - O/Project/all")
setwd("C:/Users/Charu/Documents")
 tr <- read.csv("application_train.csv")
te <- read.csv("application_test.csv")

 bureau <- read.csv("bureau.csv") %>% 
     mutate_if(is.character, funs(factor(.) %>% as.integer()))
 
 cred_card_bal <-  read_csv("credit_card_balance.csv") %>% 
     mutate_if(is.character, funs(factor(.) %>% as.integer()))
 

 pos_cash_bal <- read_csv("POS_CASH_balance.csv") %>% 
     mutate_if(is.character, funs(factor(.) %>% as.integer()))

 prev <- read_csv("previous_application.csv") %>% 
     mutate_if(is.character, funs(factor(.) %>% as.integer()))

 avg_bureau <- bureau %>% 
     group_by(SK_ID_CURR) %>% 
     summarise_all(funs(sum()), na.rm = TRUE) %>% 
    mutate(buro_count = bureau %>%  
                           group_by(SK_ID_CURR) %>% 
                           count() %$% n)
 

 avg_cred_card_bal <- cred_card_bal %>% 
     group_by(SK_ID_CURR) %>% 
     summarise_all(funs(sum()), na.rm = TRUE) %>% 
     mutate(card_count = cred_card_bal %>%  
                           group_by(SK_ID_CURR) %>% 
                           count() %$% n)
 avg_pos_cash_bal <- pos_cash_bal %>% 
    group_by(SK_ID_CURR) %>% 
     summarise_all(funs(sum()), na.rm = TRUE) %>% 
   mutate(pos_count = pos_cash_bal %>%  
                          group_by(SK_ID_PREV, SK_ID_CURR) %>% 
                           group_by(SK_ID_CURR) %>% 
                           count() %$% n)
 

  avg_prev <- prev %>% 
   group_by(SK_ID_CURR) %>% 
      summarise_all(funs(sum()), na.rm = TRUE) %>% 
      mutate(nb_app = prev %>%  
                           group_by(SK_ID_CURR) %>% 
                           count() %$% n)
 
tr_all <- tr %>% 
     left_join(avg_bureau, by = "SK_ID_CURR") %>% 
     left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>% 
   left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>% 
     left_join(avg_prev, by = "SK_ID_CURR") %>% 
     mutate_if(is.character, funs(factor(.) %>% as.integer()))



tr_all$TARGET <- factor(tr_all$TARGET)
 n = nrow(tr_all)
 trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = tr_all[trainIndex ,]
 val = tr_all[-trainIndex ,]
 prop.table(table(train$TARGET))

 table(train$TARGET)


 prop.table(table(val$TARGET))


 table(val$TARGET)


 naive_model <- naive_bayes(TARGET ~ ., data= train)
 pred = predict(naive_model, val)
 conf_mat <- table(pred, val$TARGET)
conf_mat

##pred     0     1
##      0 27596  1181
##      1 57090  6386

predictions <- predict(naive_model, val)

#recall/sensitivity =tp/ tp+fn
recall = 6386/(6386+1181)
recall
#0.8439276

## specificity = tn/fp+tn
(spec = 27596/(57090+27596))
pred <- prediction(predictions, val$TARGET);
#0.3258626
2*(.32*.84)/(.32+.84)

## precision = tp/tp+fp
(prec = 6386/(6386+57090))
# 0.100605

RP.perf <- performance(pred, "prec", "rec")
library(caret)


########### precision/recall
precision <- posPredValue(predictions, val$TARGET, positive="0")
recall <- sensitivity(predictions, val$TARGET, positive="0")
F1 <- (2 * precision * recall) / (precision + recall)

###################

accuracy = diag(conf_mat)/sum(conf_mat)


############# creating test set

te_all <- te %>% left_join(avg_bureau, by = "SK_ID_CURR") %>% left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>% left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>% left_join(avg_prev, by = "SK_ID_CURR") %>% mutate_if(is.character, funs(factor(.) %>% as.integer()))

read_csv("../input/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(naive_model, te_all)) %>%
  write_csv(paste0("tidy_xgb_", round(m_xgb$best_score, 4), ".csv"))


te_all$TARGET <- predict(naive_model, te_all)
submitfile <- te_all[,c("SK_ID_CURR", "TARGET")]
write.csv(submitfile, "submitfile.csv")
table(submitfile$TARGET)

# .565
