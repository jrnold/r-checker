context("proto_data_frame")

test_that("proto_date_frame works", {
  checks <-
    ColumnCheckList(a = ColumnChecks(classtype="numeric"),
                    b = ColumnChecks(classtype="ANY"))
  expect_equal(check_constraints(proto_data_frame(checks),
                                 TableChecks(columns=checks)),
               TRUE)
})
