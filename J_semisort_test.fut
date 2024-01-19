-- Benchmarking semisort with sample arrays
-- ==
-- input @ example_dataset_10000.txt
-- auto output

import "semisort"
import "gen_data"
def is_sorted 't (hash: t -> i64)(A:[]t): bool =
    let n = length A
    let tmp = map(\i -> map(\j -> if (hash A[j]) == (hash i) then j else -1)(iota n)) A
              |> map(\arr -> let filtered = filter (>=0) arr
                             let differences = map(\i -> filtered[i+1] - filtered[i])(iota ((length filtered) - 1))
                             in reduce (*) 1 differences)
    in (reduce (*) 1 tmp) == 1

def hash_for_i32 (v:i32): i64 = i64.i32 v
def is_sorted_i32 't (A:[]i32):bool = is_sorted hash_for_i32 A

-- ==
-- entry: test_is_sorted
-- compiled input { [1,1,2,2,2,7,7,7,3,3,3,3,5,5,5,5]} output { true }
-- compiled input { [2,2,1,1,1,3,3,3,4,4,4,2,2,2] } output { false }
-- compiled input { [10,10,1,1,2,2,3,3,3,-1,-1,-1,20,20,7,7,7,8] } output { true }
-- compiled input { [4,4,4,4,5,5,5,6,5,5,3,3,3,7,7,8,8,9] } output { false }

entry test_is_sorted (A:[]i32) = is_sorted_i32 A

-- ==
-- entry: test_equal10000
-- random input {i32} output{ true }
entry test_equal10000 (a:i32) = let d = gen_norm_rand 10000 a 0 2
                                 in semisort hash_for_i32 true d |> is_sorted_i32

-- ==
-- entry: test_equal100000
-- random input {i32} output{ true }
entry test_equal100000 (a:i32) = let d = gen_norm_rand 100000 a 10 5
                                 in semisort hash_for_i32 true d |> is_sorted_i32

-- ==
-- entry: test_less_than10000
-- random input {i32} output{ true }
entry test_less_than10000 (a:i32) = let d = gen_norm_rand 10000 a 5 10
                                 in semisort hash_for_i32 false d |> is_sorted_i32

-- ==
-- entry: test_less_than100000
-- random input {i32} output{ true }
entry test_less_than100000 (a:i32) = let d = gen_norm_rand 100000 a 20 1
                                 in semisort hash_for_i32 false d |> is_sorted_i32

entry main (B:[]i32) = semisort hash_for_i32 false B