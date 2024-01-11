def scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t = 
   scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)

def equal_test 't [n] (hash: t -> i64)(A: [n]t): [n]t =
   let l:i64 = 4
   let keys = loop A = copy A for i < n do
         if any(\x -> (hash x) == (hash A[0])) (drop 1 A) then drop 1 A else rotate 1 A 
   let k = length keys
   let hash_table (v:t):i64 = let Searched = map(\i -> if (hash keys[i]) == (hash v) then i else -1) (iota k)
                              let Filtered = filter (>=0) Searched
                              in if (length Filtered == 0) then 0 else Filtered[0]

   let C = map(\i -> loop arr = (replicate k (0:i64)) for j < l do
                           if (j+i*l) > (n-1) then copy arr else
                           let id = hash_table A[j+(i*l)]
                           in arr with [id] = arr[id] + 1) (iota ((n/l)+1))

   let X = transpose C |> flatten
                       |> scanExc (+) 0
                       |> unflatten
                       |> transpose
   
   let (T_Idx, _) = map(\i -> loop arr = ((replicate l (0:i64)), (flatten (copy X))) for j < l do 
                           if (j+(i*l)) > n-1 then copy arr else 
                           let id = hash_table A[j+(i*l)]
                           let (Idx, X') = copy arr
                           let Idxnew = Idx with [j] = X'[i*k + id]
                           let Xnew = X' with [i*k + id] = X'[i*k + id] + 1
                           in (Idxnew, Xnew)) (iota ((n/l)+1))
                        |> unzip

   let T_Idx_flattened = map(\i -> (flatten T_Idx)[i])(iota n)
   in scatter (copy A) T_Idx_flattened A
