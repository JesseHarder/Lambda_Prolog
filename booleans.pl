/* Predicate: ifthenelse(Condition, T1, T2, Result)
* If Condition = tru, then Result = T1
* If Condition = fls, then Result = T2
*/
ifthenelse(tru, T1, _, T1).
ifthenelse(fls, _, T2, T2).

and(tru, tru, tru).
and(fls, _, fls).
and(_, fls, fls).

or(fls, fls, fls).
or(tru, _, tru).
or(_, tru, tru).

xor(fls, fls, fls).
xor(tru, fls, tru).
xor(fls, tru, tru).
xor(tru, tru, fls).

not(tru, fls).
not(fls, tru).
