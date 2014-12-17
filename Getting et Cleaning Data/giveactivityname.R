give_activity_name <- function (x) {
  
n <- length(x)
for (i in 1:n) { 
                if (x[i] == 1) {x[i] <- activity_label[1,2]}
                if (x[i] == 2) {x[i] <- activity_label[2,2]}
                if (x[i] == 3) {x[i] <- activity_label[3,2]}
                if (x[i] == 4) {x[i] <- activity_label[4,2]}
                if (x[i] == 5) {x[i] <- activity_label[5,2]}
                if (x[i] == 6) {x[i] <- activity_label[6,2]}
  
               }  
x
}