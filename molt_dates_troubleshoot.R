# molt troubleshoot
# investigating the birds that appeared to have low levels of diving when should have been molting

library(tidyverse)
library(data.table)
library(aws.s3)
library(diveMove)

# Set creditials for aws access
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIPIS43MIHABD4CVA", "AWS_SECRET_ACCESS_KEY" = "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", 
           "AWS_DEFAULT_REGION" = "us-west-2")

# read in gdr deploy data

gdr_depl <-
  read_csv(
    "Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv"
  )

molt_tab <-
  read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2022-01-27.csv")
plot(gdr_depl$season)

# 70449 in 2018
# read in processed dive data

b70449_18 <-  s3readRDS(object = "GDR_1819_zoc/000070449_1801027c18_zoc.Rda",
            bucket = "pb-adelie",
            silent = TRUE)




plotTDR(b70449_18,diveNo=20000:20005)  

# most of these "dives" during Feb don't appear to be real dives, noisy sensor?
# how should we handle the dive stats for this bird?


b71865<- s3readRDS(object = "GDR_1819_zoc/000071865_1801020c18_zoc.Rda",
                   bucket = "pb-adelie",
                   silent = TRUE)

plotTDR(b71865,diveNo=40010:40026) 


# also check 46645 in 2018
b46645<-  s3readRDS(object = "GDR_1819_zoc/000046645_1801011c18_zoc.Rda",
                        bucket = "pb-adelie",
                        silent = TRUE)


plotTDR(b46645,diveNo=29510:30326) 
# looks like molt actually went 02/11-3/5
