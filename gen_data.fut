import "lib/github.com/diku-dk/cpprandom/random"

let seed = 100000
let output_size = 100000

module rand_i32 = uniform_int_distribution i32 minstd_rand
module norm_rand = normal_distribution f32 minstd_rand



def main : []i32 = 
    let (minval, maxval) = (-1000000, 1000000)
    let rng = minstd_rand.rng_from_seed [seed]

    let rngs = minstd_rand.split_rng output_size rng
    let (_, vals) = unzip( map(\rng -> rand_i32.rand (minval,maxval) rng) rngs)
    in vals

def gen_uniform_rand : []i32 = 

    let (minval, maxval) = (-1000000, 1000000)
    let rng = minstd_rand.rng_from_seed [seed]

    let rngs = minstd_rand.split_rng output_size rng
    let (_, vals) = unzip( map(\rng -> rand_i32.rand (minval,maxval) rng) rngs)
    in vals

def gen_norm_rand : []i32 = 

    let sdev = 2000000
    let rng = minstd_rand.rng_from_seed [seed]

    let rngs = minstd_rand.split_rng output_size rng
    let (_, vals) = unzip( map(\rng -> norm_rand.rand {mean=0, stddev=sdev} rng) rngs)
    in map i32.f32 vals

