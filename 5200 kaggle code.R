scoringdf$rating = -1
df_all = rbind(df,scoringdf)
df_all = separate_rows(df_all,genre,sep=",")
df_all$genre = str_remove(df_all$genre,"\\[|\\]")
df_all$genre = str_remove(df_all$genre,"]")
df_all$genre = str_trim(df_all$genre)
df_all["genre"][df_all["genre"]==""] <- NA
df_all$genre= replace_na(df_all$genre,"Others")
df_all$genre = str_remove(df_all$genre,"\\'|\\'")
df_all$genre = str_remove(df_all$genre,"'")
df_all$genre=str_remove(df_all$genre," ")
df_all_long=
  df_all %>%
  mutate(v=1) %>%
  pivot_wider(names_from=genre,values_from = v,values_fill = 0) 
df_all_long=separate_rows(df_all_long, performer,sep= "featuring")
df_all_long$performer=str_trim(df_all_long$performer)
df_all_long['performer'][df_all_long['performer']==""] = NA
df_all_long1= 
  df_all_long %>%
  mutate(v=1) %>%
  pivot_wider(names_from=performer,values_from=v,values_fill = 0)
basic_info=df_all_long1[,1:1060]
pe=df_all_long1[,-1:-1060]
pe2=pe[colSums(pe)>10]
df_all_long1=cbind(basic_info,pe2)
df_all_long1=clean_names(df_all_long1)

df_long =
  df_all_long1 %>%
  filter(rating!=-1) %>%
  select(-1:-2)

scdf_long=
  df_all_long1 %>%
  filter(rating==-1) %>%
  select(-1:-2) 
library(ranger)
df_long1=clean_names(df_long)
scdf_long1=clean_names(scdf_long)
forest_ranger = ranger(rating~.,data=df_long1,num.trees = 5000)