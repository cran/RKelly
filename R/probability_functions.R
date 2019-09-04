#' Calculate win chance after multiple matches
#'
#' Chance of a player winning the majority of n matches. Draws count not as a
#' win
#'
#' @param p probability for player to win a single match
#' @param n number of total matches playes
#'
#' @return The decimal chance of winning a game
#' @export
#'
#' @examples
#' chance_to_win_n_games(0.55,5) # Chance for player with p=0.55 to win best of 5 matches
chance_to_win_n_games <-
function(p, n){
    is_even <- (n %% 2) == 0

    if (is_even){
        wins = n/2+1
    } else {
        wins = (n+1)/2
    }

    q <- 1-p

    total_chance <- 0

    if (!is_even){
        for (i in seq(0,wins-1)){
            total_chance <- total_chance + p**wins*(q**i)*choose(i+wins-1,i)
        }
    } else{
        for (i in seq(0,wins-2)){
            total_chance <- total_chance + p**wins*(q**i)*choose(i+wins-1,i)
        }
    }

    return(total_chance)
}


#' Calculates the chance to draw out of n matches
#'
#' @param p probability of first (or second) player winning match
#' @param n number of matches
#'
#' @return The decimal chance for a draw
#' @export
#'
#' @examples
#' chance_to_draw_n_games(0.4, 4) # Draw chance if one player has p=0.4 in four matches
chance_to_draw_n_games <-
function(p, n){
    is_even <- (n %% 2) == 0

    if(!is_even){
        return(0.0)
    } else {
        n_draws <- n/2
        q <- 1-p
        chance <- q**(n_draws)*p**(n_draws)*choose(n,n_draws)
        return(chance)
    }
}


#' The Kelly criterion
#'
#' @param p The objective probability of the event
#' @param alpha_w The return multiplier in case of the event happening
#' @param alpha_l The return multiplier in case of the event not happening
#'
#' @return The Kelly optimised fraction of the bankroll that should be bet
#' @export
#'
#' @examples
#' kelly_criterion(0.5,1,1)
#'
#' @references  Thorp, Edward O. (1997; revised 1998). The Kelly Criterion in Blackjack, Sports Betting, and the Stock Market. \url{http://www.eecs.harvard.edu/cs286r/courses/fall12/papers/Thorpe_KellyCriterion2007.pdf}
kelly_criterion <-
function(p, alpha_w, alpha_l){
    q <- 1-p
    numerator <- p*alpha_w-q*alpha_l
    denominator <- alpha_w*alpha_l
    return(numerator/denominator)
}

#' Kelly for back bet
#'
#' @param price Price to back in decimal odds
#' @param p Probability of event to to materialise
#' @param commision_rate Rate of commision charged on WINNINGS
#'
#' @return Kelly optimised fraction of stake relative to bank
#' @export
#'
#' @examples
#' kelly_back_dec(2,0.5,0.05)
kelly_back_dec <- function(price,p, commision_rate){
    alpha_w = (1-commision_rate)*(price-1)
    alpha_l = 1
    fraction = kelly_criterion(p, alpha_w = alpha_w, alpha_l = alpha_l)
    return(fraction)
}

#' Kelly for lay bet
#'
#' @param price Price at which to lay
#' @param p Base probability of event that is being laid
#' @param commision_rate Rate of commision charged on WINNINGS
#'
#' @return Kelly optimised fraction of stake relative to bank
#' @export
#'
#' @examples
kelly_lay_dec <- function(price, p, commision_rate){
    p <- 1-p
    alpha_w <- (1-commision_rate)
    alpha_l <- price-1
    fraction <- kelly_criterion(p, alpha_w = alpha_w, alpha_l = alpha_l)
    return(fraction)
}
