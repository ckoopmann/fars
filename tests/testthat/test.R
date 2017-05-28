testthat::test_that('make_filename generates correct filename',{
      testthat::expect_equal(fars::make_filename(2014),"accident_2014.csv.bz2")
})

