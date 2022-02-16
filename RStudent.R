#Author : Gopal Chakravarthy
#ModLog : 11/22/2020 Using R functins for Visualization and data analysis

remove(list = ls())
setwd("C:/RLab/RStudentLab")
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
library(DBI)
library(odbc)
library(dplyr)
library(calibrate)
library(scatterplot3d)
library(MASS)
library(rgl)

sort(unique(odbcListDrivers()[[1]]))
##conn is a DBIConnection
conn <- dbConnect(odbc(),
                  Driver = "SQL Server", 
                  Server = "LT-GCHAKRA",  
                  Port = 1433,
                  Database = "R_Db", 
                  Trusted_Connection = "True")

odbcListObjects(conn)
odbcListObjects(conn, catalog="R_Db", schema="dbo")
odbcListObjects(conn, catalog="R_Db", schema="dbo", type="table")
odbcListObjects(conn, catalog="R_Db", schema="dbo", type="view")
odbcListObjects(conn, catalog="R_Db", schema="dbo", name="vw_studentRpt")
odbcListObjects(conn, catalog="R_Db", schema="dbo", view="vw_studentRpt")
odbcListObjectTypes(conn)
dbListFields(conn, "vw_studentRpt")
res1 <- dbSendQuery(conn,"SELECT student_name,subject_name,marks FROM vw_studentRpt")
data <- dbFetch(res1)
##res <- dbGetQuery(conn, "SELECT student_name,subject_name,marks FROM vw_studentRpt")
dbDisconnect(conn)

resultsByStudent <- res %>% group_by(student_name) %>% summarise(
  mean = mean(marks),
  median = median(marks),
  mode = Mode(marks)
)

 # persp(res$student_name,res$subject_name, res$marks,
 #       main="Student Performance  Perspective",
 #       zlab= "Marks",
 #       theta = 30, phi=15,
 #       col = "springgreen", shade = 0.5)

# with(data = res,
#      scatterplot3d(x = res[,student_name],
#                    y = res[,subject_name],
#                    z = res[,marks],
#      )
# )

pdf(file = "resultsByStudent.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 8) # The height of the plot in inches


plot(resultsByStudent$mean, 
     ylim = c(0,100),
     xaxt = "n",
     main = "Results by Student",
     col = "red",
     pch = 21,
     xlab = "Student",
     ylab = "Score")
axis(1, at=1:4, labels=resultsByStudent$student_name)
lines(x = 1:4, y = resultsByStudent$mean, col = "red")
points(x = 1:4, y = resultsByStudent$mode, col = "green")
lines(x = 1:4, y = resultsByStudent$mode, col = "green")

legend("topleft", 
       c("mean", "mode"), 
       col = c("red","green"),
       pch = 21
       )

dev.off()

write.csv(resultsByStudent, 
          row.names = F,
          file = "resultsByStudent.csv")
write.csv(res, 
          row.names = F,
          file = "rawStudentData.csv")

print(res,quote = FALSE, row.names = FALSE)

dbClearResult(res1)

