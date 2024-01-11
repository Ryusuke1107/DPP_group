import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "equal_test"

let nl:i64 = 1024 -- we might have to change this value according to our PC
let a:i64 = 1024 -- we might have to change this value according to our PC

module rng_engine = minstd_rand
module rand_i64 = uniform_int_distribution i64 rng_engine

def log10 (x:i64):i64 = 
   let (_, result) = loop v = (1, 0) while (\(x,y) -> x)v < x do 
               let (a,b) = copy v 
               let a_new = 10*a
               let b_new = b+1 
               in (a_new, b_new)
   in result

def scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t = 
   scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)

def less_than_test 't [n] (hash: t -> i64)(A: [n]t): [n]t =
   let keys = loop A = copy A for i < n do 
      if any(\x -> (hash x) == (hash A[0])) (drop 1 A) then drop 1 A else rotate 1 A 
   let A_hashed = map(\key -> hash key) A
   let A_hashed_sorted = radix_sort 64 i64.get_bit A_hashed
   let A_sorted = map(\i -> let idx = map(\j -> if (hash keys[j]) == i then j else -1)(iota (length keys))
                                       |> filter (>=0)
                                       |> head
                            in keys[idx]
                  ) A_hashed_sorted
   in A_sorted

def semisort 't [n] (hash: t -> i64)(is_equal_test: bool)(A: [n]t): [n]t = 
   let Basecase [m] (hash: t -> i64)(A: [m]t):[m]t = 
      if is_equal_test then equal_test hash A else less_than_test hash A
    
   in if n < a then Basecase hash A else

   let l:i64 = n/500 -- we might have to change this value according to our PC
   let semisort_step 't [n'] (hash: t -> i64)(A: [n']t): ([n']t, []i64) =
      let nllogn' = nl * (log10 n')
      let samplerngs = minstd_rand.rng_from_seed [(i32.i64 (2023*n'+124))] |> minstd_rand.split_rng nllogn'
      let sample:[]t = []
      let (sample, _) = loop A_with_idx = (copy sample, copy A) for i < nllogn' do
                        let (_, idx) = rand_i64.rand(0i64, (n'-i-1)) samplerngs[i]
                        let (sample', A') = copy A_with_idx
                        let sample_new = concat sample' [A'[idx]]
                        let A_new = if idx == (n'-i-1) then A'[0:idx] else concat A'[0:idx] A'[(idx+1):(n'-i)]
                        in (sample_new, A_new)

      let keys = loop sample = copy sample for i < nllogn' do
         if any(\x -> (hash x) == (hash sample[0])) (drop 1 sample) then drop 1 sample else rotate 1 sample 

      let logn = log10 n
      let count:[]i64 = map(\j -> reduce (+) 0 (map (\x -> if (hash x) == (hash j) then 1 else 0) sample)) keys
      let Heavy_Index = map(\i -> if count[i] > logn then i+1 else 0) (iota (length keys)) 
                           |> filter (!=0) 
                           |> map (\i -> i-1)
      let Heavy_keys = if (length Heavy_Index) == 0 then [head keys] else map(\i -> keys[i]) Heavy_Index
      let Heavy_keys_hashed = map(\i -> hash i) Heavy_keys
      let nH = length Heavy_keys

      let Heavy (v:t) : i64 = 
         let Searched = map(\i -> if Heavy_keys_hashed[i] == (hash v) then i else -1) (iota (length Heavy_keys))
         let Filtered = filter (>=0) Searched
         in Filtered[0] + nl

      let Light (v:t) : i64 = (hash v) % nl

      let GetBucketId (v:t) : i64 = if any(\x -> x == (hash v)) Heavy_keys_hashed then Heavy v else Light v

      let C = map(\i -> loop arr = (replicate (nl + nH) (0:i64)) for j < l do
                           if (j+i*l) > (n'-1) then copy arr else
                           let id = GetBucketId A[j+(i*l)]
                           in arr with [id] = arr[id] + 1) (iota ((n'/l)+1))

      let X = transpose C |> flatten
                          |> scanExc (+) 0
                          |> unflatten
                          |> transpose

      let offsets = X[0]

      let (T_Idx, _) = map(\i -> loop arr = ((replicate l (0:i64)), (flatten (copy X))) for j < l do 
                           if (j+(i*l)) > n'-1 then copy arr else 
                           let id = GetBucketId A[j+(i*l)]
                           let (Idx, X') = copy arr
                           let Idxnew = Idx with [j] = X'[i*(nl+nH) + id]
                           let Xnew = X' with [i*(nl+nH) + id] = X'[i*(nl+nH) + id] + 1
                           in (Idxnew, Xnew)) (iota ((n'/l)+1))
                        |> unzip

      let T_Idx_flattened = map(\i -> (flatten T_Idx)[i])(iota n')
      in (scatter (copy A) T_Idx_flattened A, offsets[:1025])

   let (Light_keys, offsets) = semisort_step hash A
   let sorted = Light_keys[(last offsets):]
   let dummy_ne = head A
   let A_matrix = 
      map(\i -> let A' = Light_keys[offsets[i]:offsets[i+1]]
                let (rest, A'_sorted) =
                  loop A = (copy A', []) while (length ((\(x,y) -> x)A)) > a do
                        let (arr, sorted) = copy A
                        let (arr', offsets') = semisort_step hash arr
                        let new_arr = arr'[:(last offsets')]
                        let new_sorted = concat arr'[(last offsets'):] sorted
                        in (new_arr, new_sorted)
                let rest_sorted = Basecase hash rest
                let A'_sorted_new = concat A'_sorted (replicate ((last offsets) - offsets[i+1]) dummy_ne) 
                                         |> concat rest_sorted
                                         |> concat (replicate offsets[i] dummy_ne)
                in map(\i -> A'_sorted_new[i])(iota (last offsets))
      )(iota ((length offsets)-1))
   let Add(x:t)(y:t) :t = if (hash x) == (hash dummy_ne) then y else x 
   let Light_sorted = transpose A_matrix |> map(\arr -> [reduce Add dummy_ne arr])
                                         |> transpose 
                                         |> head
   let result = concat Light_sorted sorted
   in map(\i -> result[i])(iota n)
