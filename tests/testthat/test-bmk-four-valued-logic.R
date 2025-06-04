#
# Kleene's strong three‐valued logic extended with a separate missing (NA) that
# propagates except when overridden by a dominant false (for AND) or true (for
# OR).
#
test_that("Biomarker four valued logic: conjunction", {

  # Conjunction
  expect_identical(bmk("-") & bmk("-"), bmk("-"))
  expect_identical(bmk("-") & bmk("~"), bmk("-"))
  expect_identical(bmk("-") & bmk("+"), bmk("-"))
  expect_identical(bmk("-") & bmk(NA), bmk("-"))

  expect_identical(bmk("~") & bmk("-"), bmk("-"))
  expect_identical(bmk("~") & bmk("~"), bmk("~"))
  expect_identical(bmk("~") & bmk("+"), bmk("~"))
  expect_identical(bmk("~") & bmk(NA), bmk(NA))

  expect_identical(bmk("+") & bmk("-"), bmk("-"))
  expect_identical(bmk("+") & bmk("~"), bmk("~"))
  expect_identical(bmk("+") & bmk("+"), bmk("+"))
  expect_identical(bmk("+") & bmk(NA), bmk(NA))

  expect_identical(bmk(NA) & bmk("-"), bmk("-"))
  expect_identical(bmk(NA) & bmk("~"), bmk(NA))
  expect_identical(bmk(NA) & bmk("+"), bmk(NA))
  expect_identical(bmk(NA) & bmk(NA), bmk(NA))

})

test_that("Biomarker four valued logic: disjunction", {

  # Disjunction
  expect_identical(bmk("-") | bmk("-"), bmk("-"))
  expect_identical(bmk("-") | bmk("~"), bmk("~"))
  expect_identical(bmk("-") | bmk("+"), bmk("+"))
  expect_identical(bmk("-") | bmk(NA), bmk(NA))

  expect_identical(bmk("~") | bmk("-"), bmk("~"))
  expect_identical(bmk("~") | bmk("~"), bmk("~"))
  expect_identical(bmk("~") | bmk("+"), bmk("+"))
  expect_identical(bmk("~") | bmk(NA), bmk(NA))

  expect_identical(bmk("+") | bmk("-"), bmk("+"))
  expect_identical(bmk("+") | bmk("~"), bmk("+"))
  expect_identical(bmk("+") | bmk("+"), bmk("+"))
  expect_identical(bmk("+") | bmk(NA), bmk("+"))

  expect_identical(bmk(NA) | bmk("-"), bmk(NA))
  expect_identical(bmk(NA) | bmk("~"), bmk(NA))
  expect_identical(bmk(NA) | bmk("+"), bmk("+"))
  expect_identical(bmk(NA) | bmk(NA), bmk(NA))

})

test_that("Biomarker four valued logic: not", {

  # NOT operation
  expect_identical(!bmk("-"), bmk("+"))
  expect_identical(!bmk("~"), bmk("~"))
  expect_identical(!bmk("+"), bmk("-"))
  expect_identical(!bmk(NA), bmk(NA))

})
