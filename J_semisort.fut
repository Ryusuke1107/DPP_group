import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"

module rng_engine = minstd_rand
module rand_i64 = uniform_int_distribution i64 rng_engine

def scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t = 
   scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)

-- In equal test, similar to semisort_step, the number of occurrences of the key in the input is 
-- counted and recorded in C. Then, X is computed as the prefix sum of C, and the T matrix is computed 
-- using X. Finally, the flattened T matrix is used to obtain the sorted values as output by scattering.
def equal_test 't [n] (hash: t -> i64)(A: [n]t): [n]t =
   let l:i64 = 20
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

-- We use radix sort of the hashed value of the keys in less_than_test
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
    --input=10000, nl=8, a=128
    --input=100000, nl=16, a=256
    --inpu=1000000, nl=32, a=1024
   let nl:i64 = 16 -- assuming the input size is 100000
   let a:i64 = 4096 -- assuming the input size is 100000
   let Basecase [m] (hash: t -> i64)(A: [m]t):[m]t = 
      if is_equal_test then equal_test hash A else less_than_test hash A
    
   in if n < a then Basecase hash A else

   let l:i64 = n/20 -- assuming the input size is 100000
   let semisort_step 't [n'] (hash: t -> i64)(A: [n']t): ([n']t, []i64) =

   -- Step 1: Sampling and Bucketing
      let nllogn' = i64.f64 (f64.i64 nl * (intrinsics.log10_64 (f64.i64 n')))

      -- Get nllogn' random rngs here
      let samplerngs = minstd_rand.rng_from_seed [(i32.i64 (2023*n'+124))] |> minstd_rand.split_rng nllogn'
      
      -- Get nllogn' random samples. In order to avoid sampling the same index key in duplicate, we use for loop
      -- and in each loop we take one random key from the array.
      let (sample, _) = loop A_with_idx = ([], copy A) for i < nllogn' do
                        let (_, idx) = rand_i64.rand(0i64, (n'-i-1)) samplerngs[i]
                        let (sample', A') = copy A_with_idx
                        let sample_new = concat sample' [A'[idx]]
                        let A_new = if idx == (n'-i-1) then A'[0:idx] else concat A'[0:idx] A'[(idx+1):(n'-i)]
                        in (sample_new, A_new)

      -- Collect the information about what kinds of keys the sample include. In each loop, if the head key
      -- of the sample is included in another index in the sample, drop the head value. If not, rotate 1 sample.
      let keys = loop sample = copy sample for i < nllogn' do
         if any(\x -> (hash x) == (hash sample[0])) (drop 1 sample) then drop 1 sample else rotate 1 sample 

      -- Count the occurence of each key in the sample.
      let logn = i64.f64 (intrinsics.log10_64 (f64.i64 n))
      let count:[]i64 = map(\j -> reduce (+) 0 (map (\x -> if (hash x) == (hash j) then 1 else 0) sample)) keys
      
      -- If the occurence of each key is more than logn, the key is stored in Heavy_keys. 
      let Heavy_Index = map(\i -> if count[i] > logn then i+1 else 0) (iota (length keys)) 
                           |> filter (!=0) 
                           |> map (\i -> i-1)
      let Heavy_keys = if (length Heavy_Index) == 0 then [head keys] else map(\i -> keys[i]) Heavy_Index
      let Heavy_keys_hashed = map(\i -> hash i) Heavy_keys
      let nH = length Heavy_keys

      -- Heavy function takes a heavy key as input and assigns a corresponding bucket number to each key.
      let Heavy (v:t) : i64 = 
         let Searched = map(\i -> if Heavy_keys_hashed[i] == (hash v) then i else -1) (iota nH)
         let Filtered = filter (>=0) Searched
         in Filtered[0] + nl

      -- Light function takes a light key as input and assigns a corresponding bucket number to each key.
      let Light (v:t) : i64 = (hash v) % nl

      -- GetBucketId takes an arbitrary key as input and applies Heavy function to the key if the key 
      -- is a heavy key, otherwise it applies light function to the key.
      let GetBucketId (v:t) : i64 = if any(\x -> x == (hash v)) Heavy_keys_hashed then Heavy v else Light v

   -- Step 2: Blocked Distributing
      
      -- We count the number of records in each bucket in a (n'/l+1) × (nl + nH) matrix. We count the
      -- occurence of the keys in each subarray and store in the corresponding location in C
      -- C[i][id]: records stored into bucket id in subarray 
      let C = map(\i -> loop arr = (replicate (nl + nH) (0:i64)) for j < l do
                           if (j+i*l) > (n'-1) then copy arr else
                           let id = GetBucketId A[j+(i*l)]
                           in arr with [id] = arr[id] + 1) (iota ((n'/l)+1))
      
      -- We get the prefix sum of C and store as X
      let X = transpose C |> flatten
                          |> scanExc (+) 0
                          |> unflatten
                          |> transpose

      -- For each subarray, execute the following loop. First, get the bucket ID of the key. Then,
      -- get the value stored in the corresponding place of the X matrix (this value indicates which 
      -- index the key should be stored in at output). Then, increase the value stored in the corresponding
      -- location of the X matrix by 1.
      let (T_Idx, _) = map(\i -> loop arr = ((replicate l (0:i64)), (copy X)[i]) for j < l do 
                           if (j+(i*l)) > n'-1 then copy arr else 
                           let id = GetBucketId A[j+(i*l)]
                           let (Idx, X') = copy arr
                           let Idxnew = Idx with [j] = X'[id]
                           let Xnew = X' with [id] = X'[id] + 1
                           in (Idxnew, Xnew)) (iota ((n'/l)+1))
                        |> unzip

      let T_Idx_flattened = map(\i -> (flatten T_Idx)[i])(iota n')
      in (scatter (copy A) T_Idx_flattened A, X[0])

   let (Light_keys, offsets) = semisort_step hash A
   let sorted = Light_keys[offsets[nl]:]

   -- Step 3: Local Refining
   -- Take a light key for each bucket id, sort it, and merge it into temp_A_matrix 
   let Light_sorted = 
      loop temp_A_matrix = [] for i < nl do
        let A' = Light_keys[offsets[i]:offsets[i+1]]

        -- Recursively apply the semisort_step function to the first element. The sorted part are
        -- combined into the second element, leaving the unsorted portions in the first element.
        -- Iterating this until the length of the first element is less than a.
        let (rest, A'_sorted) =
            loop A = (copy A', []) while (length ((\(x,y) -> x)A)) > a do
                let (arr, sorted) = copy A
                let (arr', offsets') = semisort_step hash arr
                let new_arr = arr'[:offsets'[nl]]
                let new_sorted = concat arr'[offsets'[nl]:] sorted
                in (new_arr, new_sorted)

         -- When the unsorted part is less than a, then apply it in Basecase.
         let rest_sorted = Basecase hash rest
         let A'_sorted_new = concat rest_sorted A'_sorted 
        in concat temp_A_matrix A'_sorted_new
   let result = concat Light_sorted sorted
   in map(\i -> result[i])(iota n)

---- Making a sequential loop out of the map----------
   

--original irregular nesting
--let A_matrix = 
--      map(\i -> let A' = Light_keys[offsets[i]:offsets[i+1]]
--                let (rest, A'_sorted) =
--                  loop A = (copy A', []) while (length ((\(x,y) -> x)A)) > a do
--                        let (arr, sorted) = copy A
--                        let (arr', offsets') = semisort_step hash arr
--                        let new_arr = arr'[:(last offsets')]
--                        let new_sorted = concat arr'[(last offsets'):] sorted
--                        in (new_arr, new_sorted)
--                let rest_sorted = Basecase hash rest
--                let A'_sorted_new = concat A'_sorted (replicate ((last offsets) - offsets[i+1]) dummy_ne) 
--                                         |> concat rest_sorted
--                                         |> concat (replicate offsets[i] dummy_ne)
--               in map(\i -> A'_sorted_new[i])(iota (last offsets))
--      )(iota ((length offsets)-1))
