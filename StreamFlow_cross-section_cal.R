#Calculatiing cross-section, wetted perimetre and hydrauic radius of a stream 
#This script should work for stream cross section shapes including rectangle, trapezoid and triangle.
#Width across the bank (W) in m and depth (D) in m for n number of transects along the stream are required
#Site - a local stream
#Script written by Gayan Gunaratne
#Date 24/10/2019

#install R packages as required
library(dplyr)#A Grammar of Data Manipulation

library(here)#finds your R project files, based on the current working directory at the time when the package is loaded


#load input dataset

#This data set includes diffrent depth and width measurements across a stream along a few transects.
#D1 to D8 represent depth measurements in meters.
#W1 to W8 represent width measurements in meters.

crosssection<- read.csv("Stream_input_D&W.csv",header=T,na.strings=c("","NA"))
head(crosssection,10)

#install dplyr package as required
library(dplyr)

#Cross section
#Stream is split into polygons (traingles and rectangles/squares) and 
#area is calculated for each smaller portion (Ex:A1 to A8)
crosssection<-crosssection%>%
  mutate(A1 = ifelse(D1 == 0,0,ifelse(D1>0,0.5*W1*D1, 0)))%>%
  mutate(A2 = ifelse(D2 == D1,D2*W2,ifelse(D2>D1,((0.5*W2*(D2-D1))+D1*W2), ((0.5*W2*(D1-D2))+D2*W2))))%>%
  mutate(A3 = ifelse(D3 == D2,D3*W3,ifelse(D3>D2,((0.5*W3*(D3-D2))+D2*W3), ((0.5*W3*(D2-D3))+D3*W3))))%>%
  mutate(A4 = ifelse(D4 == D3,D4*W4,ifelse(D4>D3,((0.5*W4*(D4-D3))+D3*W4), ((0.5*W4*(D3-D4))+D4*W4))))%>%
  mutate(A5 = ifelse(D5 == D4,D5*W5,ifelse(D5>D4,((0.5*W5*(D5-D4))+D4*W5), ((0.5*W5*(D4-D5))+D5*W5))))%>%
  mutate(A6 = ifelse(D6 == D5,D6*W6,ifelse(D6>D5,((0.5*W6*(D6-D5))+D5*W6), ((0.5*W6*(D5-D6))+D6*W6))))%>%
  mutate(A7 = ifelse(D7 == D6,D7*W7,ifelse(D7>D6,((0.5*W7*(D7-D6))+D6*W7), ((0.5*W7*(D6-D7))+D7*W7))))%>%
  mutate(A8 = ifelse(D8 == D7,D8*W8,ifelse(D8>D7,((0.5*W8*(D8-D7))+D7*W8), ((0.5*W8*(D7-D8))+D8*W8))))%>%
  rowwise() %>% 
  
 #Calculate cross section of the stream 
  mutate(cross_section=sum(A1,A2,A3,A4,A5,A6,A7,A8,na.rm=TRUE))

#Wetted perimeter
#Calculation of stream bottom length
crosssection<-crosssection%>%
  mutate(B1 = ifelse(D1 == 0,0,ifelse(D1>0,sqrt(D1^2+W1^2), NA)))%>%
  mutate(B2 = ifelse(D2 == D1,W2,ifelse(D2>D1,sqrt((D2-D1)^2+W2^2), sqrt((D1-D2)^2+W2^2))))%>%
  mutate(B3 = ifelse(D3 == D2,W3,ifelse(D3>D2,sqrt((D3-D2)^2+W3^2), sqrt((D2-D3)^2+W3^2))))%>%
  mutate(B4 = ifelse(D4 == D3,W4,ifelse(D4>D3,sqrt((D4-D3)^2+W4^2), sqrt((D3-D4)^2+W4^2))))%>%
  mutate(B5 = ifelse(D5 == D4,W5,ifelse(D5>D4,sqrt((D5-D4)^2+W5^2), sqrt((D4-D5)^2+W5^2))))%>%
  mutate(B6 = ifelse(D6 == D5,W6,ifelse(D6>D5,sqrt((D6-D5)^2+W6^2), sqrt((D5-D6)^2+W6^2))))%>%
  mutate(B7 = ifelse(D7 == D6,W7,ifelse(D7>D6,sqrt((D7-D6)^2+W7^2), sqrt((D6-D7)^2+W7^2))))%>%
  mutate(B8 = ifelse(D8 == D7,W8,ifelse(D8>D7,sqrt((D8-D7)^2+W8^2), sqrt((D7-D8)^2+W8^2))))%>%
  
#Calculate wetted perimeter and Hydraulic radius
  rowwise() %>% 
  mutate(WP=sum(B1,B2,B3,B4,B5,B6,B7,B8,na.rm=TRUE))%>% 
  mutate(stream_width=sum(W1,W2,W3,W4,W5,W6,W7,W8,na.rm=TRUE))%>% 
  mutate(HR=cross_section/WP)
 
crosssection<-as.data.frame(crosssection)
head(crosssection)
crosssection<-crosssection[,c(1,26,35,36,37)]
head(crosssection)

write.csv(crosssection,file="stream_output.csv", row.names=F)

