context("Priority Queue")

test_that("new() creates a PriorityQueue", {
  expect_is(PriorityQueue$new(), "PriorityQueue")
})

test_that("push items onto PriorityQueue", {
  queue <<- PriorityQueue$new()
  queue$push("first", 1)
  queue$push("second", 2)
  queue$push("third", 1)
  queue$push("fourth", 3)
  expect_equal(queue$size(), 4)
})

test_that("retrieve priorities", {
  expect_equal(queue$priorities, c(3, 2, 1, 1))
})

test_that("retrieve data", {
  expect_equal(queue$data, as.list(c("fourth", "second", "first", "third")))
})

test_that("pop highest priority", {
  expect_equal(queue$pop(), "fourth")
})

library(iterators)

test_that("iterator functionality", {
  queue <<- Queue$new()
  for (n in 1:10) queue$push(n)
  #
  items = c()
  #
  i <- queue$iterator()
  #
  while (hasNext(i)) {
    items <- c(items, nextElem(i))
  }
  expect_equal(items, 1:10)
})

test_that("calculated priority (abs)", {
  queue <- PriorityQueue$new(prioritise = abs)
  #
  queue$push(5)
  queue$push(3)
  queue$push(-6)
  queue$push(1)
  #
  expect_equal(unlist(queue$data), c(-6, 5, 3, 1))
})

test_that("calculated priority (x^2)", {
  queue <- PriorityQueue$new(prioritise = function(x) x^2)
  #
  queue$push(5)
  queue$push(3)
  queue$push(-6)
  queue$push(1)
  #
  expect_equal(unlist(queue$data), c(-6, 5, 3, 1))
})

test_that("calculated priority (nchar)", {
  queue <- PriorityQueue$new(prioritise = nchar)
  #
  queue$push("xxx")
  queue$push("x")
  queue$push("xxxxxxxxxx")
  queue$push("xx")
  #
  expect_equal(unlist(queue$data), c("xxxxxxxxxx", "xxx", "xx", "x"))
})
