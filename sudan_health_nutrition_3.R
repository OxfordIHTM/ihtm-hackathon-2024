
View(child)
dim(child)


#################Create a table of na values per variable####################

# create a loop to determine number of na values per variable
na_list<-c()
for (i in 1:ncol(child)) {
  na_count<- sum(is.na(child[,i])) 
  print(na_count)
  na_list<-c(na_list,na_count)
}

colname_list<-colnames(child)

##create a table of na values
na_tbl <- data.frame(colname_list, na_list)

##naming cols
colnames(na_tbl) = c("variable", "values missing") 

##calc percentage of na variables
na_tbl[,3]<-round( na_tbl$`values missing`/nrow(child) *100,2)

na_tbl[,4]<-nrow(child)-na_tbl$`values missing`

##naming cols
colnames(na_tbl) = c("variable", "missing","missing %","present")

View(na_tbl)