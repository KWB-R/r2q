#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2023-07-14 11:50:22.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("assess_all_hazards() works", {

  f <- r2q:::assess_all_hazards

  expect_error(
    r2q:::assess_all_hazards()
    # argument "hazard_list" is missing, with no default
  )

})