


#install.packages("exams")
library(exams)


#Create a PDF for the exam
set.seed(403)
test_exam <- exams2nops(exercises_exam,
                  dir = "nops_pdf",
                  name = "demo", date = "2015-07-29",
                  points = c(1, 1, 1, 2, 2, 3),
                  n = 2)

#Use example images to check out how it works
scan_image <- dir(system.file("nops", package = "exams"),
           pattern = "nops_scan",
           full.names = TRUE)

#Create a folder and copy images
dir.create("nops_scan")
file.copy(scan_image, to = "nops_scan")

#Scan images
nops_scan(dir = "nops_scan")

#Who participates the exam? Load data or use example data
write.table(data.frame(
  registration = c("1501090", "9901071"),
  name = c("Jane Doe", "Ambi Dexter"),
  id = c("jane_doe", "ambi_dexter")),
  file = "Exam-2015-07-29.csv", sep = ";", quote = FALSE, row.names = FALSE)

#Extract/Eval information from images
exam_results <- nops_eval(
  register = "Exam-2015-07-29.csv",
  solutions = "nops_pdf/demo.rds",
  scans = Sys.glob("nops_scan/nops_scan_*.zip"),
  eval = exams_eval(partial = FALSE, negative = FALSE),
  interactive = TRUE
)

#Inspect results
exam_results

#Load data
library(readr)
exam_df <- read_delim("nops_eval.csv",
                        ";", escape_double = FALSE, trim_ws = TRUE)

#Fake example data
library(tidyverse)
exam_df <- tribble(
  ~ID, ~points,
   1, 57,
   2, 60,
   3, 84,
   4, 45,
   5, 82
)

exam_df <- exam_df  %>% 
  select(ID, points)
exam_df


#Give grades according to points
exam_df <- exam_df %>% 
  mutate(
    grade = (
      case_when(
        points >= 95 ~ 1.0,
        points >= 90 ~ 1.3,
        points >= 85 ~ 1.7,
        points >= 80 ~ 2.0,
        points >= 75 ~ 2.3,
        points >= 70 ~ 2.7,
        points >= 65 ~ 3.0,
        points >= 60 ~ 3.3,
        points >= 55 ~ 3.7,
        points >= 50 ~ 4.0,
        points <= 49 ~ 5.0
      )
    )
  )

exam_df

#Arrange and check if data preparation steps really worked
exam_df %>% 
  mutate(grade_system = grade * 100) %>%
  arrange(ID)

#In case you need to save your results
#write_csv(final_results, "final_results.csv")

#Inspect the results visually
mean_grade <- exam_df %>% 
  pull(grade) %>% 
  mean() %>% 
  round(2)

#Plot it
ggplot(exam_df, aes(x=grade)) +
  geom_histogram(colour="black", fill="white", bins = 11)+
  geom_vline(xintercept=mean_grade, size=1.5, color="red")+
  geom_text(aes(x=mean_grade+0.5, label=paste0("Mean\n",mean_grade), y=8))+
  theme_minimal(base_size = 14)
