import "lib/github.com/diku-dk/cpprandom/random"



module rand_i32 = uniform_int_distribution i32 minstd_rand
module norm_rand = normal_distribution f32 minstd_rand





def gen_uniform_rand (num_elems : i64) (seed : i32) : []i32 = 

    let (minval, maxval) = (-1000000, 1000000)
    let rng = minstd_rand.rng_from_seed [seed]

    let rngs = minstd_rand.split_rng num_elems rng
    let (_, vals) = unzip( map(\rng -> rand_i32.rand (minval,maxval) rng) rngs)
    in vals

def gen_norm_rand (num_elems : i64) (seed : i32) (m: f32) (sdev: f32) : []i32 = 

    let rng = minstd_rand.rng_from_seed [seed]

    let rngs = minstd_rand.split_rng num_elems rng
    let (_, vals) = unzip( map(\rng -> norm_rand.rand {mean=m, stddev=sdev} rng) rngs)
    in map i32.f32 vals

def main (num_elems : i64) (seed : i32) (m: f32) (sdev: f32) : []i32 = 
    gen_norm_rand num_elems seed m sdev