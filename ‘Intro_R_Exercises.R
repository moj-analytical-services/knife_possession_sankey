y = 17
y* 78


# Make sure the latest package versions are used
options(repos = "https://cloud.r-project.org/")
# install renv if it doesn't exist on your system
if(!"renv" %in% installed.packages()[, "Package"]) install.packages("renv")
# Remove bare = TRUE if you'd like to move your existing packages over to
# renv. This is not a good idea if you're migrating from s3tools as
# renv will attempt to install that library.
renv::init(bare = TRUE)
# Tell renv to use Python and set up a virtual environment.
# If you get an error here, remove the python path argument and
# manually select the version of python you require.
renv::use_python(python='/usr/bin/python3')
# Install reticulate so we can make calls to Python libraries, required by
# botor
renv::install('reticulate')
# Install the Python library, boto3, used by botor to access S3
reticulate::py_install('boto3')
# Install botor itself
renv::install('botor')

install (botor)

offenders <- botors3read
chicoff<-botor::s3_read("s3://alpha-r-training/intro-r-training/Offenders_Chicago_Police_Dept_Main.csv",read.csv)

summary(chicoff)

chicoff[(500:502),4]

#Exercises 2

#1
mean(chicoff$AGE)
median(chicoff$AGE)

#2
max(chicoff$WEIGHT)
min(chicoff$WEIGHT)

#3
chicoff$SENTENCE<-as.factor(chicoff$SENTENCE)
levels(chicoff$SENTENCE)

#Exercises 3

install.packages("systemfonts", dependencies = TRUE)
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

#1

genregave <- chicoff %>%
  dplyr:: group_by(REGION,GENDER) %>%
  dplyr:: summarise(Mean=mean(AGE),Median=median(AGE))

#2

over2m <- chicoff %>%
  filter (HEIGHT>=200) %>%
select(GENDER)

#3

heightcount <- chicoff %>%
  group_by(HEIGHT) %>%
  summarise(n())

#4

totconv <- chicoff %>%
  mutate(NUM_CONVICTIONS = PREV_CONVICTIONS+1) %>%
select(SENTENCE, PREV_CONVICTIONS, NUM_CONVICTIONS) %>%
rename(SENTENCE_TYPE = SENTENCE)

#Excercises 4

install.packages("lubridate")
library(lubridate)

#1

ftse <- ftse %>%
  dplyr::mutate(date_formatted=lubridate::dmy(ftse$Date))

#2
ftse <- ftse %>%
  dplyr::mutate(day=lubridate::wday(date_formatted))%>%
  dplyr::mutate(daily_performance=Close-Open)

#3

ftse %>%
  group_by(day) %>%
summarise(mean(daily_performance))

#Exercises 5

chictri<-botor::s3_read("s3://alpha-r-training/intro-r-training/Offenders_Chicago_Police_Dept_Trial.csv",read.csv)

#1

chicoff <- rename(chicoff, DoB = BIRTH_DATE)

offenders_trial_age <- dplyr::inner_join(chicoff, chictri, by=c("LAST","DoB"))

#2

write.csv(offenders_trial_age,file="offenders_trial_age.csv")

#r charting training

#Exercises one

ggplot(data=mpg)
?mpg
ggplot(data=(mpg),aes(x=hwy, y=cty))+ geom_point()
ggplot(data=(mpg),aes(x=class, y=drv))+ geom_point()
ggplot(data=(mpg))+ geom_point(aes(x=class, y=cty, colour = factor(drv)))
      
#Exercises two

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(color="blue")

ggplot(data=mpg) +
  geom_point(aes(x=displ, y=hwy, colour=factor(cty)))

ggplot(data=mpg) +
  geom_point(aes(x=displ, y=hwy, size=factor(cty)))

ggplot(data=mpg) +
  geom_point(aes(x=displ, y=hwy, shape=factor(cty)))


ggplot(data=mpg) +
  geom_point(aes(x=displ, y=hwy, shape=factor(cty)))

#Exercises three

ggplot(data=mpg,aes(x=displ, y=hwy, colour=class))+
       geom_point()+
      geom_smooth(colour="blue", linetype="dashed")

#Exercises four

ggplot(data=mpg, aes(x=class),y=mean(displ))+
 geom_bar()

#Exercises five

ggplot(data=mpg) +
  geom_point(mapping=aes(x=hwy, y=cty, colour=as.factor(cyl)))+
theme_minimal()+
  labs(title="Highway versus City efficiency by number of cylinders",x="Highway", y="City")


library(openxlsx)
data(mtcars)

header_style <- createStyle(halign = "center", textDecoration = "bold")

wb <- createWorkbook()

addWorksheet(wb, "'Data")
writeData(wb, "Data", mtcars, headerStyle = header_style)
freezePane(wb, "Data", firstRow = TRUE)
setColWidths(wb, "Data", cols = 1, widths = 15)
setColWidths(wb, "Data", cols = 2:ncol(mtcars), widths = 8)
setRowHeights(wb, "Data", rows = 1, heights = 24)
setRowHeights(wb, "Data", rows = 2:nrow(mtcars), heights = 12)
saveWorkbook(wb, file = "mtcars.xlsx", overwrite = TRUE)

Table7_quarterly<-Table7_quarterly%>%
  select(-'...1')

library(openxlsx)
data('Table 7 Template')

wb <- loadWorkbook(system.file("Table7", "Table 7 Template.xlsx", package = "openxlsx"))

install.packages('xlsx')
library("xlsx")

qrow1=summarise(Table7_annual,num=n())+5
lastrow=summarise(Table7_annual,num=n())+summarise(Table7_quarterly,num=n())+4

wb <- createWorkbook()

addWorksheet(wb, "Tab7")

writeData(wb,"Tab7", Table7_annual,
          startRow = 4,
          startCol = 1)

writeData(wb,"Tab7", Table7_quarterly,
          startRow = qrow1,
          startCol = 1,
          colNames=FALSE)

setColWidths(wb,"Tab7",cols= 1:16, widths=20)
setRowHeights(wb,"Tab7",rows=4, heights=70)

headstyle <- createStyle(fontName='Ariel',fontSize = 10, halign = "LEFT", valign="CENTER",textDecoration ="BOLD", wrapText = TRUE
                         , border=c("TOP","BOTTOM"),borderColour="black",borderStyle=c("MEDIUM","THIN"))
rowheadstyle1 <- createStyle(fontName='Ariel',fontSize = 10, halign = "LEFT", textDecoration = "BOLD")
rowheadstyle2 <- createStyle(fontName='Ariel',fontSize = 10, halign = "LEFT", textDecoration = "BOLD"
                         , border="BOTTOM",borderColour="black",borderStyle="THIN")
colheadstyle <- createStyle(fontName='Ariel',fontSize = 10, halign = "RIGHT", valign="CENTER",textDecoration ="BOLD", wrapText = TRUE
                         , border=c("TOP","BOTTOM"),borderColour="black",borderStyle=c("MEDIUM","THIN"))
totstyle1 <- createStyle(fontName='Ariel',fontSize = 10, halign="RIGHT",textDecoration = "BOLD", numFmt="COMMA")
totstyle2 <- createStyle(fontName='Ariel',fontSize = 10, halign="RIGHT",textDecoration = "BOLD", numFmt="COMMA"
                         , border="BOTTOM",borderColour="black",borderStyle="THIN")
bodystyle1 <- createStyle(fontName='Ariel',fontSize = 10, halign="RIGHT", numFmt="COMMA")
bodystyle2 <- createStyle(fontName='Ariel',fontSize = 10, halign="RIGHT", numFmt="COMMA"
                        , border="BOTTOM",borderColour="black",borderStyle="THIN")

addStyle(wb,"Tab7",style=headstyle, rows=4, cols=1, gridExpand = TRUE)
addStyle(wb,"Tab7",style=rowheadstyle1, rows=6:lastrow[1,1]-1, cols=1, gridExpand = TRUE)
addStyle(wb,"Tab7",style=rowheadstyle2, rows=lastrow[1,1], cols=1, gridExpand = TRUE)
addStyle(wb,"Tab7",style=colheadstyle, rows=4, cols=2:16, gridExpand = TRUE)
addStyle(wb,"Tab7",style=totstyle1, rows=6:lastrow[1,1]-1, cols=c(2:3,11),gridExpand = TRUE)
addStyle(wb,"Tab7",style=totstyle2, rows=lastrow[1,1], cols=c(2:3,11),gridExpand = TRUE)
addStyle(wb,"Tab7",style=bodystyle1, rows=6:lastrow[1,1]-1, cols=c(4:10,12:16), gridExpand = TRUE)
addStyle(wb,"Tab7",style=bodystyle2, rows=lastrow[1,1], cols=c(4:10,12:16), gridExpand = TRUE)


saveWorkbook(wb, file = "Tab7.xlsx", overwrite = TRUE)