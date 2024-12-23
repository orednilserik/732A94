data('iris')
df <- data_dl()

test_that('Check that the data is correct', { 

  expect_true('kpi' %in% colnames(df))        
  expect_true('municipality' %in% colnames(df))
  expect_true('period' %in% colnames(df))
  expect_true('values' %in% colnames(df))
  
  })


test_that('Checking inputs for muniLines',{
  expect_error(p <- muniLines('Götebörg', df))
  expect_error(p <- muniLines('Göteborg',iris))
})


test_that('Checking inputs for muniCompare',{
  expect_error(p <- muniLines('Götebörg', df,'care_spend' ))
  expect_error(p <- muniLines('Göteborg',iris,'care_spend'))
  expect_error(p <- muniLines('Göteborg',df,'care_spenddrups'))
})


test_that('Checking inputs for muniCorr',{
  expect_error(p <- muniCorr(iris))

})














