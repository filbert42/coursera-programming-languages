fun is_older(fst : int*int*int, snd : int*int*int) =
    if #1 fst = #1 snd andalso #2 fst = #2 snd
    then if #3 fst < #3 snd then true else false
    else if #1 fst = #1 snd andalso #2 fst < #2 snd then true
         else if #1 fst < #1 snd then true
              else false

fun is_in_month(dt : int*int*int, m : int) = 
    #2 dt = m

fun number_in_month(lod : (int*int*int) list, m : int) =
    if null lod
    then 0
    else if is_in_month (hd lod, m) 
        then 1 + number_in_month(tl lod, m)
        else number_in_month(tl lod, m)
    

fun number_in_months(lod : (int*int*int) list, lom : int list) =
    if null lom
    then 0
    else number_in_month(lod, hd lom) + number_in_months(lod, tl lom)

fun dates_in_month(lod : (int*int*int) list, m : int) = 
    if null lod
    then []
    else if is_in_month(hd lod, m)
         then (hd lod) :: dates_in_month(tl lod, m)
         else dates_in_month(tl lod, m)

fun dates_in_months(lod : (int*int*int) list, lom : int list) =
    if null lom
    then []
    else dates_in_month(lod, hd lom) @ dates_in_months(lod, tl lom)

fun get_nth(los : string list, n : int) = 
    if n = 1 
    then hd los 
    else get_nth(tl los, n-1) 

fun date_to_string(date : int*int*int) = 
let
  val months = [
      "January", "February", "March", "April", 
      "May", "June", "July", "August", "September",
      "October", "November", "December"
  ]

  val get_year = Int.toString (#1 date)
  
  val get_month = get_nth (months, (#2 date))
  
  val get_day = Int.toString (#3 date)
in
  get_month ^ " " ^ get_day ^ ", " ^ get_year
end

fun number_before_reaching_sum(sum : int, lon : int list) = 
    if null lon 
    then 0
    else if sum - hd lon <= 0 
         then 0 + number_before_reaching_sum(sum - hd lon, tl lon)
         else 1 + number_before_reaching_sum(sum - hd lon, tl lon)

fun what_month(day : int) = 
let
  val months_days = [31,28,31,30,31,30,31,31,30,31,30,31]
in
  number_before_reaching_sum(day, months_days) + 1
end

fun month_range(day1 : int, day2 : int) = 
    if day1 = day2 
    then [what_month day1]
    else [what_month day1] @ month_range(day1+1, day2)

fun oldest(lod : (int*int*int) list) = 
    if null lod
    then NONE 
    else if null (tl lod) 
         then SOME (hd lod)
         else if is_older(hd lod, hd (tl lod))
              then oldest(hd lod :: tl (tl lod))
              else oldest(tl lod)

fun date_in_list(date : int*int*int, lod : (int*int*int) list) =
    if null lod
    then false
    else if #1 date = #1 (hd lod)
            andalso #2 date = #2 (hd lod) 
            andalso #3 date = #3 (hd lod)
         then true
         else date_in_list(date, tl lod)

fun remove_duplicates(lod : (int*int*int) list) = 
    let
      fun helper(lod : (int*int*int) list, mem : (int*int*int) list) = 
        if null lod
        then mem
        else if date_in_list(hd lod, mem)
             then helper(tl lod, mem)
             else helper(tl lod, mem @ [hd lod])
    in
      helper(lod, [])
    end


fun number_in_months_challenge(lod : (int*int*int) list, lom : int list) = 
    number_in_months((remove_duplicates lod), lom)

fun dates_in_months_challenge(lod : (int*int*int) list, lom : int list) =
    dates_in_months((remove_duplicates lod), lom)

fun reasonable_date(date : int*int*int) = 
    let
      fun is_leap() = 
        (((#1 date) mod 400) = 0)
        orelse
        ((((#1 date) mod 4) = 0) andalso (((#1 date) mod 100) <> 0))
      
      val months_days = if is_leap() 
                        then [31,29,31,30,31,30,31,31,30,31,30,31]
                        else [31,28,31,30,31,30,31,31,30,31,30,31]  
      
      fun get_month_days(month : int, md : int list) = 
        if month = 1
        then hd md
        else get_month_days(month-1, tl md)

      val check_year  = #1 date > 0
      val check_month = #2 date >= 1 andalso #2 date <=12
      val check_day   = #3 date >= 1 andalso #3 date <= get_month_days(#2 date, months_days)
    in
      check_year andalso check_month andalso check_day
    end