context("is-git-repository")

test_that("is_git_repository", {
  expect_false(is_git_repository(tempdir()))
})
