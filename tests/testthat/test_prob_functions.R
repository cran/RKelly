context('Testing probability functions')

test_that('Probabilities sum up to one', {
    probabilities <- seq(0,1, 0.1)
    for (p in probabilities){
        for (n in seq(1,10)){
            expect_equal(chance_to_draw_n_games(p, n)+
                             chance_to_win_n_games(p, n)+
                             chance_to_win_n_games(1-p, n),1)
        }
    }
})

test_that('More games give more certain results',{
    p <- 0.51
    for (n in seq(1,10)){
        # Draws are only valid for even games
        expect_gt(chance_to_win_n_games(p, n+2), chance_to_win_n_games(p, n))
    }
})

test_that('Draws are impossible for odd number of games',{
    for (p in seq(0,1, 0.1)){
        for (n in seq(1,10)){
            expect_equal(chance_to_draw_n_games(p, 2*n+1),0)
        }
    }
})

test_that('Draws are monotonically less likely', {
    p <- 0.51
    for (n in seq(1,10)){
        expect_lt(chance_to_draw_n_games(p,2*(n+1)),chance_to_draw_n_games(p,2*n))
    }
})

test_that('Certain events are certain',{
   p <- 0
   for (n in seq(1,10)){
       expect_equal(chance_to_draw_n_games(p, n),0)
       expect_equal(chance_to_win_n_games(p, n),0)
   }

   p <- 1
   for (n in seq(1,10)){
       expect_equal(chance_to_draw_n_games(p, n),0)
       expect_equal(chance_to_win_n_games(p, n),1)
   }

})
