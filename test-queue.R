source("queue.R")

# Check out methods of generator object
Queue$new()
Queue$methods()
Queue$fields()
Queue$help()
Queue$help(push)

q1 <- Queue$new()
q1$name <- "test queue"
q1$name

q1

q2 <- Queue$new(name = "another test")
q2$name

# Set up some accessors for the name field
#
Queue$accessors("name")

q2$getName()
q2$setName("a new name")
q2$getName()

q2$push("item number one")
q2$push(2)
q2$push("third item")
q2$size()
q2$data

q2$pop()
q2$pop()
q2$size()

q2$peek()
q2$size()

q2$poll()
q2$size()
try(q2$pop())
q2$peek()
q2$poll()

q3 <- Queue$new(name = "letter queue")

sapply(letters, function(n) {q3$push(n)})

q3$size()
q3$peek()
q3$peek(2)
q3$peek(4:7)
q3$peek(24:28)

PriorityQueue$fields()

q4 <- PriorityQueue$new()

q4$push("first", 1)
q4$push("second", 2)
q4$push("third", 1)
q4$push("fourth", 3)

q4$priorities
q4$data

q4$pop()
q4$priorities

q4$pop()
q4$peek()
q4$poll()

q4$pop()
q4$priorities
q4$poll()

# Test iterator functionality
#
q5 <- Queue$new()
#
for (n in 1:10) q5$push(n)
#
i5 <- q5$iterator()
#
while (hasNext(i5)) {
  print(nextElem(i5))
}