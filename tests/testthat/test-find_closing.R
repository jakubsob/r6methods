test_that("one parathensis", {
  expect_equal(find_closing("(xxx)"), 5)
})

test_that("multiple parathenses", {
  expect_equal(find_closing("(((xxx)))"), 9)
})

test_that("no match", {
  expect_identical(find_closing("((xxxx)"), NA_integer_)
})

test_that("match on arbitrary characters", {
  expect_equal(find_closing("[xxxx X", "\\[", "X"), 7)
})
