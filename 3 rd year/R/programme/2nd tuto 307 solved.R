data1<- data.frame(
        Serial=c(1:5),
        Name=c("Hamid","Himel", "Sayed", "Polash", "Hasan"),
        GPA=c(5,5,5,5,5),
        Monthly_Expenditure=c(5,7,10,50,80),
        Expected_home_district=c("Meherpur", "Dinajpur", "Kushtia", "Chapainganj", "Mymensing"),
        stringsAsFactors = FALSE
)


data1$department<-c("stat", "math", "stat", "Finance", "stat")
data1
data1$jobs<-c("a", "b", "c", "d", "e")
data1


str(data1)
summary(data1)
print(data1$Name)
print(data1$GPA)

new.data<-data.frame()

list.data<-c("A", c(1,2,3), 5,6, "stat" ,c(1,2,3),
             5,6, "TRUE",c(3,7,9), 55,60, "stat"
             ,c(1,2,3), 5,6, "stat")
print(list.data)




# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2, byrow = T),
                  list("green",12.3), TRUE)

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list", "logical values")

# Show the list.
print(list_data)

