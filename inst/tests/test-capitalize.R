context("capitalize")

test_that("capitalize_first_letter_words", {

  expect_that(capitalize_first_letter_words("The"), is_a("character"))
  expect_error(capitalize_first_letter_words(1))
  expect_identical(capitalize_first_letter_words("The"), "The")
  expect_identical(capitalize_first_letter_words("The dog"), "The Dog")
  expect_identical(capitalize_first_letter_words("The dOg "), "The DOg ")
  expect_identical(capitalize_first_letter_words(" he dog "), " He Dog ")
  
  str <- c(" he dog ", " snoop doggy", "2rf")
  names(str) <- c("name1", "2name", "33")
  
  expect_equivalent(capitalize_first_letter_words(str), c(" He Dog ", " Snoop Doggy", "2rf"))
  expect_identical(names(capitalize_first_letter_words(str)), c("name1", "2name", "33"))
  
  expect_identical(capitalize_first_letter_words("/he DOG/dog fu "), 
    "/He DOG/Dog Fu ")
})
