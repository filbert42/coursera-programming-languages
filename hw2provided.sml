(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun append_option (x, lst) = 
    case lst of
      NONE    => NONE
    | SOME xs => SOME (x::xs)
(* put your solutions for problem 1 here *)

fun all_except_option (str, lst) =
    case lst of
        []    => NONE
      | x::xs => 
            if same_string(x, str)
            then SOME xs
            else append_option(x, all_except_option(str, xs))
  
fun get_substitutions1([], str) = []
  | get_substitutions1(hd::rest, str) = 
        case all_except_option (str, hd) of
          NONE     => get_substitutions1(rest, str)
        | SOME lst => lst @ get_substitutions1(rest, str)

fun get_substitutions2(lls, str) = 
    let 
        fun helper([], acc) = acc
          | helper(hd::rest, acc) =
            case all_except_option (str, hd) of
                  NONE     => helper(rest, acc)
                | SOME lst => helper(rest, acc @ lst)  
    in
        helper(lls, [])
    end

fun similar_names (names, full_name) =
    let
      val {first=f, middle=m, last=l} = full_name
      val substitutions = get_substitutions2(names, f)
      fun build_fname (name, {first=f, middle=m, last=l}) = 
          {first=name, middle=m, last=l}

      fun build_fnames [] = []
        | build_fnames (hd::rest) =
          build_fname(hd, full_name) :: build_fnames(rest)
    in
      full_name :: build_fnames(substitutions)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(suit, rank) = 
    case suit of
      Spades => Black
    | Clubs  => Black
    | _      => Red 

fun card_value(_, rank) = 
    case rank of
      Num n => n
    | Ace   => 11
    | _     => 10

fun remove_card(cs, c, e) =
    case cs of
      [] => raise e
    | hd::rest => if hd = c then rest else hd::remove_card(rest, c, e)

fun all_same_color(cs) = 
    case cs of
      [] => true
    | (hd::rest) =>
        (case rest of
          [] => true
        | (hd2::rest2) => if card_color hd2 = card_color hd 
                          then true andalso all_same_color(rest)
                          else false)

fun sum_cards(cs) = 
    let 
      fun helper(cs, prev) = 
        case cs of
          [] => 0 + prev
        | hd::rest => helper(rest, card_value hd + prev)
    in 
      helper(cs, 0)
    end

fun score(cs, goal) =
    let
      val sum = sum_cards cs
      val preliminary_score = if sum > goal 
                              then 3 * (sum - goal)
                              else goal - sum 
    in 
      if all_same_color(cs) 
      then preliminary_score div 2
      else preliminary_score
    end

fun officiate(cards, mvs, goal) = 
    let
      fun game_round(cards, mvs, held) = 
        case mvs of
          [] => score(held, goal)
        | (hd::rest) =>
            case hd of 
              Discard c => game_round(cards, rest, remove_card(held, c, IllegalMove))
            | Draw      => 
                case cards of
                  []      => score(held, goal)
                | cr::css =>
                    if sum_cards(cr::held) > goal
                    then score(cr::held, goal)
                    else game_round(css, rest, cr::held)
    in
      game_round(cards, mvs, [])
    end
              

fun score_challenge(cs, goal) = 
  let
    fun count_aces(cs, num) =
        case cs of
          []     => num
        | c::rest => 
            case c of
              (_, Ace) => count_aces(rest, num+1)
            | _        => count_aces(rest, num)

    fun color_modifyer(score) = 
      if all_same_color(cs) 
      then score div 2
      else score

    fun prepare_score(sum) =
      if sum > goal 
      then 3 * (sum - goal)
      else goal - sum 

    fun min(lst, start) = 
      case lst of
        [] => start
      | x::xs => if x < start then min(xs, x) else min(xs, start)

    fun get_sums(cs, num_aces, acc) = 
      let 
        val sum = sum_cards cs
      in
        if num_aces = 0
        then acc
        else get_sums(cs, num_aces-1, color_modifyer(prepare_score(sum-10))::acc)
      end
    
    val aces_num = count_aces(cs, 0) 
    val init_sum = color_modifyer(prepare_score(sum_cards cs))
    val sums = get_sums(cs, aces_num, [init_sum])
  in 
    min(sums, 99999)
  end

fun officiate_challenge(cards, mvs, goal) = 
  let
    fun game_round(cards, mvs, held) = 
      case mvs of
        [] => score_challenge(held, goal)
      | (hd::rest) =>
          case hd of 
            Discard c => game_round(cards, rest, remove_card(held, c, IllegalMove))
          | Draw      => 
              case cards of
                []      => score_challenge(held, goal)
              | cr::css =>
                  if sum_cards(cr::held) > goal
                  then score_challenge(cr::held, goal)
                  else game_round(css, rest, cr::held)
  in
    game_round(cards, mvs, [])
  end