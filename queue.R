# REFERENCES:
#
# * http://adv-r.had.co.nz/R5.html [reference classes]
# * http://stackoverflow.com/questions/11789038/does-r-have-a-priority-queue-like-javas-priorityqueue
#
# ---------------------------------------------------------------------------------------------------------------------

# HAVING SOME TROUBLE CREATING THE PUSH() AND POP() METHODS...

test <- setRefClass("TEST",
                    fields = list( a = "numeric"),
                    methods = list(
                      addone = function(){
                        a <<- a+1
                      },
                      initialize = function(){
                        a <<- 1
                      }
                    )
)

# this does show "addone" as a method!!!
test$methods()

example <- test$new()
example$addone()

# BASIC QUEUE ---------------------------------------------------------------------------------------------------------

# Create a generator object
#
Queue <- setRefClass(Class = "Queue",
                     fields = list(
                       Keys = "vector",
                       Data = "list",
                       Name = "character"
                       ),
                     methods = list(
                       length <- function() {return(length(Keys))},
                       push <- function(item) {},
                       pop <- function() {}
                       )
                     )
# Check out methods of generator object
Queue$new()
Queue$methods()
Queue$fields()
Queue$help()
Queue$accessors("Name")

q1 <- Queue$new()
q1$Name <- "test queue"
q1$Name

q2 <- Queue$new(Name = "another test")
q2$Name
q2$getName()

q2$push("first")
q2$push("second")
q2$push("third")

# PRIORITY QUEUE ------------------------------------------------------------------------------------------------------

PriorityQueue <- setRefClass("PriorityQueue", contains = "Queue")