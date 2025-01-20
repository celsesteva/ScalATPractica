include "globals.mzn";

%%%%%% llegir variables
int: nmachesperday;
int: nmatchesperstadium;
int: ndays;
int: nteams;
int: nstadiums;

set of int: NDAYS = 1..ndays;
set of int: TEAMS = 1..nteams;

int: matchesNotPlayedPerDay = ndays-nmachesperday;
int: matchesNotPlayedPerStadium = nstadiums-nmatchesperstadium;


array[1..ndays,1..nstadiums] of set of int: fixes;
%files ->dies;  estadis -> columnes
array[1..nstadiums,1..nstadiums] of int: distancies;
array[1..nteams] of int: tifosi;
array[1..nstadiums] of int: capacitats;


array[1..nteams] of int: increasingOrderedTifosi = sort(tifosi);
array[1..nteams] of int: drecreasingOrderedTifosi = reverse(sort(tifosi));
array[1..nstadiums] of int: increasingOrderedCapacitats = sort(capacitats);
array[1..nstadiums] of int: drecreasingOrderedCapacitats = reverse(sort(capacitats));

%Variables.
array[1..ndays,1..nstadiums] of var set of 1..nteams: matches; %Equip - Equip.

constraint forall(i in 1..ndays, j in 1..nstadiums) (
  card(matches[i,j]) in {0,2} /\
  fixes[i,j] subset matches[i,j]
);


%constraints
%a team can only play once per day:
constraint forall(day in 1..ndays)(
  partition_set(matches[day,1..nstadiums],TEAMS)
);

%a team can only play in one stadium
constraint forall(estadi in 1..nstadiums)(
  partition_set(matches[1..ndays,estadi],TEAMS)
);








int: maxDistance = sum(estadi in 1..nstadiums)(
  sum(playersPerMatch in 1..2)(
    reverse(sort([distancies[estadi, j] | j in 1..nstadiums where j != estadi]))[playersPerMatch]
  )
) * nmatchesperstadium;


int: minDistance = sum(estadi in 1..nstadiums)(
    sum(playersPerMatch in 1..2) (
      sort([distancies[estadi, j] | j in 1..nstadiums where j != estadi])[playersPerMatch]
    )
) * nmatchesperstadium;

var minDistance..maxDistance: kilometers;

array[1..nteams,1..(ndays-1)] of var 0..max(distancies): arrayDistancies;

constraint forall(team in 1..nteams, day in 1..(ndays-1)) (
  let {
    var int: stadium_day = sum(stadium in 1..nstadiums where team in matches[day, stadium]) (stadium),
    var int: stadium_next_day = sum(stadium in 1..nstadiums where team in matches[day+1, stadium]) (stadium)
  } in
  arrayDistancies[team, day] = distancies[stadium_day, stadium_next_day]
);

constraint kilometers = sum(arrayDistancies);

solve minimize kilometers;



function string: formatSet(var set of int: s) =
  if(card(fix(s)) == 0) then 
    "     "
  else
  join("-", [show_int(2,i) | i in fix(s)])
  endif;


output 
["\t:" ++ concat([" Es" ++ show_int(2,team) ++ "  |" | team in 1..nstadiums]) ++ "\n"] 
++
["-" | i in 1..((8*nstadiums)+9)]
++ ["\n"] ++ 
["Dia   " ++ show_int(2,dia) ++ ": " ++ concat([formatSet(matches[dia, estadi]) ++ " | " | estadi in 1..nstadiums]) ++ "\n" | dia in 1..ndays]
++
["-" | i in 1..((8*nstadiums)+9)]
++ ["\n"] ++
["KMs TOTALS: " ++ show(kilometers)]
++ ["\n"] ++

["SEGUIDORS FORA: " ++ show(
  sum(day in 1..ndays, estadi in 1..nstadiums) (
    max((sum(team in matches[day, estadi]) (tifosi[team])) - capacitats[estadi],0)
  ))
 ]