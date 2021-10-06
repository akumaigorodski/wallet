/**********************************************************************
 * Copyright (c) 2016 Andrew Poelstra                                 *
 * Distributed under the MIT software license, see the accompanying   *
 * file COPYING or http://www.opensource.org/licenses/mit-license.php.*
 **********************************************************************/
#ifndef SECP256K1_MODULE_SURJECTION_MAIN
#define SECP256K1_MODULE_SURJECTION_MAIN

#include <assert.h>
#include <string.h>

#if defined HAVE_CONFIG_H
#include "libsecp256k1-config.h"
#endif

#include "include/secp256k1_rangeproof.h"
#include "include/secp256k1_surjectionproof.h"
#include "modules/rangeproof/borromean.h"
#include "modules/surjection/surjection_impl.h"
#include "hash.h"

#ifdef USE_REDUCED_SURJECTION_PROOF_SIZE
#undef SECP256K1_SURJECTIONPROOF_MAX_USED_INPUTS
#define SECP256K1_SURJECTIONPROOF_MAX_USED_INPUTS 16
#endif

typedef struct {
    unsigned char state[32];
    size_t state_i;
} secp256k1_surjectionproof_csprng;

static void secp256k1_surjectionproof_csprng_init(secp256k1_surjectionproof_csprng *csprng, const unsigned char* state) {
    memcpy(csprng->state, state, 32);
    csprng->state_i = 0;
}

static size_t secp256k1_surjectionproof_csprng_next(secp256k1_surjectionproof_csprng *csprng, size_t rand_max) {
    /* The number of random bytes to read for each random sample */
    const size_t increment = rand_max > 256 ? 2 : 1;
    /* The maximum value expressable by the number of random bytes we read */
    const size_t selection_range = rand_max > 256 ? 0xffff : 0xff;
    /* The largest multiple of rand_max that fits within selection_range */
    const size_t limit = ((selection_range + 1) / rand_max) * rand_max;

    while (1) {
        size_t val;
        if (csprng->state_i + increment >= 32) {
            secp256k1_sha256 sha;
            secp256k1_sha256_initialize(&sha);
            secp256k1_sha256_write(&sha, csprng->state, 32);
            secp256k1_sha256_finalize(&sha, csprng->state);
            csprng->state_i = 0;
        }
        val = csprng->state[csprng->state_i];
        if (increment > 1) {
            val = (val << 8) + csprng->state[csprng->state_i + 1];
        }
        csprng->state_i += increment;
        /* Accept only values below our limit. Values equal to or above the limit are
         * biased because they comprise only a subset of the range (0, rand_max - 1) */
        if (val < limit) {
            return val % rand_max;
        }
    }
}

int secp256k1_surjectionproof_initialize(const secp256k1_context* ctx, secp256k1_surjectionproof* proof, size_t *input_index, const secp256k1_fixed_asset_tag* fixed_input_tags, const size_t n_input_tags, const size_t n_input_tags_to_use, const secp256k1_fixed_asset_tag* fixed_output_tag, const size_t n_max_iterations, const unsigned char *random_seed32) {
    secp256k1_surjectionproof_csprng csprng;
    size_t n_iterations = 0;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(proof != NULL);
    ARG_CHECK(input_index != NULL);
    ARG_CHECK(fixed_input_tags != NULL);
    ARG_CHECK(fixed_output_tag != NULL);
    ARG_CHECK(random_seed32 != NULL);
    ARG_CHECK(n_input_tags <= SECP256K1_SURJECTIONPROOF_MAX_N_INPUTS);
    ARG_CHECK(n_input_tags_to_use <= SECP256K1_SURJECTIONPROOF_MAX_USED_INPUTS);
    ARG_CHECK(n_input_tags_to_use <= n_input_tags);
    (void) ctx;

    secp256k1_surjectionproof_csprng_init(&csprng, random_seed32);
    memset(proof->data, 0, sizeof(proof->data));
    proof->n_inputs = n_input_tags;

    while (1) {
        int has_output_tag = 0;
        size_t i;

        /* obtain a random set of indices */
        memset(proof->used_inputs, 0, sizeof(proof->used_inputs));
        for (i = 0; i < n_input_tags_to_use; i++) {
            while (1) {
                size_t next_input_index;
                next_input_index = secp256k1_surjectionproof_csprng_next(&csprng, n_input_tags);
                if (memcmp(&fixed_input_tags[next_input_index], fixed_output_tag, sizeof(*fixed_output_tag)) == 0) {
                    *input_index = next_input_index;
                    has_output_tag = 1;
                }

                if (!(proof->used_inputs[next_input_index / 8] & (1 << (next_input_index  % 8)))) {
                    proof->used_inputs[next_input_index / 8] |= (1 << (next_input_index % 8));
                    break;
                }
            }
        }

        /* Check if we succeeded */
        n_iterations++;
        if (has_output_tag) {
#ifdef VERIFY
            proof->initialized = 1;
#endif
            return n_iterations;
        }
        if (n_iterations >= n_max_iterations) {
#ifdef VERIFY
            proof->initialized = 0;
#endif
            return 0;
        }
    }
}

#endif
