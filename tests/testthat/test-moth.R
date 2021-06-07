test_that("Moth attributes are correct", {
  
  #Test that moths only ever have colour 0 (white) or 1 (black)
  moth_colours <- sapply(init_sim$population$individuals, function(x){x$colour})
    
  expect_true(all(moth_colours %in% c(0L, 1L)))
  
})