# install.packages("crqa")
# library('crqa')
# install.packages('XQuartz')
# data(crqa) 


library(crqa)
library(ggplot2)
library(reshape2)
library(tseriesChaos)
library(gridExtra)
library(tidyverse)
library(dplyr)

getwd()
setwd('YOUR Working Directory')

#### import data, create df #######
full_data <- read.csv('YOUR FILE IN CSV')


#create an empty RQA table - data frame with 0 rows and 12 columns
df <- data.frame(matrix(ncol = 12, nrow = 0))
#provide column names
colnames(df) <- c('jup', 'max_word_length', 'RR', 'DET', 'NRLINE', 'maxL', 'L', 'ENTR', 'rENTR',
                  'LAM', 'TT', 'catH')


##### append the df #######

## parameter setting 
delay = 1; embed = 1; rescale = 0; radius = 0.0001;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 1; whiteline = FALSE; recpt = FALSE; 
side = "both"; method = 'rqa'; metric = 'euclidean';  
datatype = "categorical"


## generate the RQA indices and write them in df
for (conv_id in unique(full_data$jup)){
  # conv_id = 3023299
  # conv_id = 3078456
  student_char = full_data[full_data$user_role == 'student' &
                             full_data$jup == conv_id, 
                                 "num_list_equal"]
  fac_char = full_data[full_data$user_role == 'facilitator' &
                           full_data$jup == conv_id, 
                            "num_list_equal"]
  # print(fac_char)
  # print(student_char)
  student_num_list <- as.numeric(strsplit(substr(student_char,2, nchar(student_char)-1), ",")[[1]])
  facilitator_num_list <- as.numeric(strsplit(substr(fac_char,2, nchar(fac_char)-1), ",")[[1]])
  rqaAns <- crqa(student_num_list, facilitator_num_list, delay, embed , radius , rescale = 0, normalize = 0, mindiagline = 2, minvertline = 2)
  rqaMetrics <- c(conv_id, length(student_num_list), rqaAns[[1]], rqaAns[[2]], rqaAns[[3]],rqaAns[[4]],
                  rqaAns[[5]],rqaAns[[6]],rqaAns[[7]],rqaAns[[9]],rqaAns[[9]],rqaAns[[10]])
  # Append the RQA indices to df
  df[nrow(df) + 1,] = rqaMetrics

}


write_csv(df, 'RQA_indiced.csv')

##### Generate recurrent plots ### 
# Plot the recurrence plots one by one (on a 1s interval)

for (conv_id in unique(df_select$jup)){
  # conv_id = 3023299
  # conv_id = 2476052
  student_char = full_data[full_data$user_role == 'student' &
                             full_data$jup == conv_id, 
                           "num_list_equal"]
  fac_char = full_data[full_data$user_role == 'facilitator' &
                         full_data$jup == conv_id, 
                       "num_list_equal"]
  student_num_list <- as.numeric(strsplit(substr(student_char,2, nchar(student_char)-1), ",")[[1]])
  facilitator_num_list <- as.numeric(strsplit(substr(fac_char,2, nchar(fac_char)-1), ",")[[1]])
  rqaAns <- crqa(student_num_list, facilitator_num_list, delay, embed , radius , rescale = 0, normalize = 0, mindiagline = 2, minvertline = 2)

  # plot
  mRP <- melt(as.matrix(rqaAns$RP), varnames=c("TimeV1", "TimeV2"), value.name="Recurrence")
  
  binary <- ggplot(mRP, aes(x=TimeV1, y=TimeV2, fill=Recurrence)) + 
    geom_raster() + 
    theme(
      # axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.background = element_blank(),
     # axis.text  = element_blank(),
     # axis.ticks = element_blank(),
      axis.title = element_blank()) +
    ggtitle(conv_id) +
    scale_fill_manual(values=c("#00008b","#ffffff"), 
                      breaks=c(TRUE, FALSE))+
    # scale_x_continuous(name="conversation length", limits=c(0, 281))+
    theme(legend.position="none", plot.title = element_text(size = 18, face = "bold"))
  print(binary)

  # If you are generating all plots at the same time, you can comment this out so that you can browse the generated plots one by one on a 1s interval
  Sys.sleep(1)
  
  # save the plots 
  # ggsave(binary, file=paste0(conv_id,".png"), width = 40, height = 40, units = "cm", dpi=300)
  
}






