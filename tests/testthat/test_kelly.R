context('Kelly functions')

test_that('Kelly criterion for fair bets is 0', {
    for (n in seq(2,20)){
        expect_equal(kelly_criterion(1/n, n-1 ,1), 0)
    }

})

test_that('More certain bets should change fraction',
          {
              for (p in seq(0.02,0.98 )){
                  expect_gt(kelly_back_dec(2,p+0.01, 0), kelly_back_dec(2,p, 0))
              }

              for (p in seq(0.02,0.98 )){
                  expect_lt(kelly_lay_dec(2,p+0.01, 0), kelly_lay_dec(2,p, 0))
              }
          })

test_that('Commisions decrease betting fractions',{
    for (n in seq(2,20)){
        expect_lt(kelly_back_dec(n,1/n+0.01, 0.01),kelly_back_dec(n,1/n+0.01, 0.00))
        expect_lt(kelly_lay_dec(n,1/n+0.01, 0.01),kelly_lay_dec(n,1/n+0.01, 0.00))
    }
})
