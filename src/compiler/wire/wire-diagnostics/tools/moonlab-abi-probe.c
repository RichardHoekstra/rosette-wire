/* moonlab-abi-probe.c -- isolated consumer of Moonlab's installed public ABI. */
#define _POSIX_C_SOURCE 200809L
#include <complex.h>
#include <dlfcn.h>
#include <inttypes.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void (*version_fn)(int *, int *, int *);
typedef void *(*create_fn)(int);
typedef void (*destroy_fn)(void *);
typedef double complex (*amplitude_fn)(const void *, uint64_t);
typedef int (*gate_1_fn)(void *, int);
typedef int (*gate_r_fn)(void *, int, double);
typedef int (*gate_2_fn)(void *, int, int);

struct api {
    version_fn version;
    create_fn create;
    destroy_fn destroy;
    amplitude_fn amplitude;
    gate_1_fn h, x, y, z, s, t;
    gate_r_fn rx, ry, rz;
    gate_2_fn cnot, cz, swap;
};

typedef double (*measurement_probability_fn)(const void *, int);
typedef int (*measurement_single_fn)(void *, int, double);
typedef double (*channel_deviation_fn)(int, double);
typedef void (*qgt_callback_fn)(const double[2], void *, double complex *);
typedef void *(*qgt_create_nband_fn)(qgt_callback_fn, void *, size_t, size_t);
typedef void (*qgt_free_nband_fn)(void *);
typedef struct { size_t n; double *berry; double chern; } qgt_grid;
typedef int (*qgt_grid_nband_fn)(const void *, size_t, qgt_grid *);
typedef void (*qgt_grid_free_fn)(qgt_grid *);
typedef int (*qwz_chern_fn)(double, size_t, double *);
typedef void *(*pauli_create_fn)(size_t, size_t);
typedef int (*pauli_add_fn)(void *, double, const char *, size_t);
typedef void (*opaque_free_fn)(void *);
typedef void *(*ansatz_create_fn)(size_t, size_t);
typedef void *(*optimizer_create_fn)(int);
typedef void *(*entropy_create_fn)(void);
typedef void *(*solver_create_fn)(void *, void *, void *, void *);
typedef int (*gradient_fn)(void *, const double *, double *, size_t);
typedef double (*energy_fn)(void *, const double *);
typedef int (*gpu_create_fn)(size_t, void **);
typedef int (*gpu_sync_fn)(void *);

struct surface_api {
    measurement_probability_fn probability_one;
    measurement_single_fn measure;
    channel_deviation_fn channel_deviation;
    qgt_create_nband_fn qgt_create;
    qgt_free_nband_fn qgt_free;
    qgt_grid_nband_fn qgt_grid;
    qgt_grid_free_fn qgt_grid_free;
    qwz_chern_fn qwz_chern;
    pauli_create_fn pauli_create;
    pauli_add_fn pauli_add;
    opaque_free_fn pauli_free;
    ansatz_create_fn ansatz_create;
    opaque_free_fn ansatz_free;
    optimizer_create_fn optimizer_create;
    opaque_free_fn optimizer_free;
    entropy_create_fn entropy_create;
    opaque_free_fn entropy_free;
    solver_create_fn solver_create;
    opaque_free_fn solver_free;
    gradient_fn gradient;
    energy_fn energy;
    gpu_create_fn gpu_create;
    gpu_sync_fn gpu_sync_to_host;
};

static int bind_symbol(void *handle, const char *name, void *slot, size_t size) {
    void *symbol = dlsym(handle, name);
    if (!symbol || size != sizeof(symbol)) return 0;
    memcpy(slot, &symbol, size);
    return 1;
}

#define BIND(api, field, symbol) \
    do { if (!bind_symbol(handle, symbol, &(api).field, sizeof((api).field))) { \
        fprintf(stderr, "missing public ABI symbol: %s\n", symbol); return 66; \
    } } while (0)

static int read_unitary_header(int *n, uint64_t *basis, int *gate_count) {
    char key[32];
    if (scanf("%31s %d", key, n) != 2 || strcmp(key, "QUBITS") != 0) return 0;
    if (scanf("%31s %" SCNu64, key, basis) != 2 || strcmp(key, "BASIS") != 0) return 0;
    if (scanf("%31s %d", key, gate_count) != 2 || strcmp(key, "GATES") != 0) return 0;
    return *n >= 1 && *n <= 20 && *gate_count >= 0 && *gate_count <= 256 &&
           *basis < (UINT64_C(1) << *n);
}

struct surface_request {
    double measurement_draw;
    double channel_parameter;
    double qgt_mass;
    double gradient[2];
};

static int read_surface_request(struct surface_request *request) {
    char key[32], end[16];
    return scanf("%31s %lf", key, &request->measurement_draw) == 2 &&
           strcmp(key, "MEASUREMENT") == 0 &&
           scanf("%31s %lf", key, &request->channel_parameter) == 2 &&
           strcmp(key, "CHANNEL") == 0 &&
           scanf("%31s %lf", key, &request->qgt_mass) == 2 &&
           strcmp(key, "QGT-MASS") == 0 &&
           scanf("%31s %lf %lf", key, &request->gradient[0],
                 &request->gradient[1]) == 3 &&
           strcmp(key, "GRADIENT") == 0 &&
           scanf("%15s", end) == 1 && strcmp(end, "END") == 0 &&
           request->measurement_draw >= 0.0 && request->measurement_draw < 1.0 &&
           request->channel_parameter >= 0.0 && request->channel_parameter <= 1.0 &&
           isfinite(request->qgt_mass) && isfinite(request->gradient[0]) &&
           isfinite(request->gradient[1]);
}

struct qgt_context { double mass; int calls; };

static void qwz_callback(const double k[2], void *user, double complex *h) {
    struct qgt_context *context = (struct qgt_context *)user;
    const double x = sin(k[0]);
    const double y = sin(k[1]);
    const double z = context->mass + cos(k[0]) + cos(k[1]);
    context->calls++;
    h[0] = z; h[1] = x - I * y; h[2] = x + I * y; h[3] = -z;
}

static int qgt_expected(double mass) {
    if (mass > -2.0 && mass < 0.0) return 1;
    if (mass > 0.0 && mass < 2.0) return -1;
    return 0;
}

static int bind_surface_api(void *handle, struct surface_api *api) {
    BIND((*api), probability_one, "measurement_probability_one");
    BIND((*api), measure, "measurement_single_qubit");
    BIND((*api), channel_deviation, "noise_kraus_completeness_deviation");
    BIND((*api), qgt_create, "qgt_create_nband");
    BIND((*api), qgt_free, "qgt_free_nband");
    BIND((*api), qgt_grid, "qgt_berry_grid_nband");
    BIND((*api), qgt_grid_free, "qgt_berry_grid_free");
    BIND((*api), qwz_chern, "moonlab_qwz_chern");
    BIND((*api), pauli_create, "pauli_hamiltonian_create");
    BIND((*api), pauli_add, "pauli_hamiltonian_add_term");
    BIND((*api), pauli_free, "pauli_hamiltonian_free");
    BIND((*api), ansatz_create, "vqe_create_hardware_efficient_ansatz");
    BIND((*api), ansatz_free, "vqe_ansatz_free");
    BIND((*api), optimizer_create, "vqe_optimizer_create");
    BIND((*api), optimizer_free, "vqe_optimizer_free");
    BIND((*api), entropy_create, "quantum_entropy_ctx_create_hw");
    BIND((*api), entropy_free, "quantum_entropy_ctx_destroy");
    BIND((*api), solver_create, "vqe_solver_create");
    BIND((*api), solver_free, "vqe_solver_free");
    BIND((*api), gradient, "moonlab_vqe_gradient");
    BIND((*api), energy, "vqe_compute_energy");
    BIND((*api), gpu_create, "quantum_state_create_gpu");
    BIND((*api), gpu_sync_to_host, "quantum_state_sync_to_host");
    return 1;
}

static int run_surface_probe(void *handle, const struct api *base) {
    struct surface_request request = {0};
    struct surface_api api = {0};
    if (!read_surface_request(&request)) return 65;
    int bind_status = bind_surface_api(handle, &api);
    if (bind_status != 1) return bind_status;

    void *state = base->create(1);
    int created = state != NULL;
    if (!state || base->h(state, 0) != 0) return 70;
    double probability = api.probability_one(state, 0);
    int outcome = api.measure(state, 0, request.measurement_draw);
    double collapsed = api.probability_one(state, 0);
    int invalid_measurement = api.measure(state, 1, 0.25);
    int invalid_target = base->x(state, 1);
    base->destroy(state);
    void *zero_state = base->create(0);
    int zero_rejected = zero_state == NULL;
    if (zero_state) base->destroy(zero_state);

    double max_channel_deviation = 0.0;
    for (int channel = 0; channel < 6; ++channel) {
        double deviation = api.channel_deviation(channel,
                                                   request.channel_parameter);
        if (!isfinite(deviation) || deviation < 0.0) return 71;
        if (deviation > max_channel_deviation) max_channel_deviation = deviation;
    }
    double invalid_channel = api.channel_deviation(0, -0.25);

    struct qgt_context context = { request.qgt_mass, 0 };
    void *qgt = api.qgt_create(qwz_callback, &context, 2, 1);
    qgt_grid grid = {0};
    int qgt_status = qgt ? api.qgt_grid(qgt, 16, &grid) : -1;
    double chern = qgt_status == 0 ? grid.chern : 0.0;
    if (grid.berry) api.qgt_grid_free(&grid);
    if (qgt) api.qgt_free(qgt);
    void *bad_qgt = api.qgt_create(NULL, NULL, 2, 1);
    int null_callback_rejected = bad_qgt == NULL;
    if (bad_qgt) api.qgt_free(bad_qgt);
    double reference_chern = NAN;
    int reference_status = api.qwz_chern(request.qgt_mass, 16,
                                         &reference_chern);

    void *hamiltonian = api.pauli_create(1, 1);
    void *ansatz = api.ansatz_create(1, 1);
    void *optimizer = api.optimizer_create(3);
    void *entropy = api.entropy_create();
    int gradient_status = -99;
    int count_status = -99;
    double native[2] = {NAN, NAN}, finite_difference[2] = {NAN, NAN};
    double residual = INFINITY;
    int nondegenerate = 0;
    if (hamiltonian && ansatz && optimizer && entropy &&
        api.pauli_add(hamiltonian, 1.0, "Z", 0) == 0) {
        void *solver = api.solver_create(hamiltonian, ansatz, optimizer, entropy);
        if (solver) {
            const double h = 1e-6;
            gradient_status = api.gradient(solver, request.gradient, native, 2);
            count_status = api.gradient(solver, request.gradient, native, 1);
            for (int i = 0; i < 2; ++i) {
                double plus[2] = { request.gradient[0], request.gradient[1] };
                double minus[2] = { request.gradient[0], request.gradient[1] };
                plus[i] += h; minus[i] -= h;
                finite_difference[i] = (api.energy(solver, plus) -
                                        api.energy(solver, minus)) / (2.0 * h);
            }
            residual = fmax(fabs(native[0] - finite_difference[0]),
                            fabs(native[1] - finite_difference[1]));
            const double anchor[2] = {0.37, -0.22};
            double anchor_gradient[2] = {0.0, 0.0};
            if (api.gradient(solver, anchor, anchor_gradient, 2) == 0)
                nondegenerate = fmax(fabs(anchor_gradient[0]),
                                     fabs(anchor_gradient[1])) > 1e-9;
            api.solver_free(solver);
        }
    }
    int null_status = api.gradient(NULL, request.gradient, native, 2);
    if (entropy) api.entropy_free(entropy);
    if (optimizer) api.optimizer_free(optimizer);
    if (ansatz) api.ansatz_free(ansatz);
    if (hamiltonian) api.pauli_free(hamiltonian);

    const char *gpu_kind = "UNAVAILABLE";
    int gpu_status = 0;
    double gpu_error = 0.0;
    void *gpu_state = NULL;
    gpu_status = api.gpu_create(2, &gpu_state);
    if (gpu_status == 0 && gpu_state) {
        int gate_status = base->h(gpu_state, 0);
        if (gate_status == 0) gate_status = base->cnot(gpu_state, 0, 1);
        if (gate_status == 0) gate_status = api.gpu_sync_to_host(gpu_state);
        if (gate_status == 0) {
            const double target = 0.70710678118654752440;
            gpu_error = fmax(cabs(base->amplitude(gpu_state, 0) - target),
                             cabs(base->amplitude(gpu_state, 3) - target));
            gpu_error = fmax(gpu_error, cabs(base->amplitude(gpu_state, 1)));
            gpu_error = fmax(gpu_error, cabs(base->amplitude(gpu_state, 2)));
            gpu_kind = "PASS";
        } else {
            gpu_status = gate_status;
            gpu_kind = "FAIL";
        }
        base->destroy(gpu_state);
    } else if (gpu_state) {
        base->destroy(gpu_state);
        gpu_kind = "FAIL";
    }

    int major = 0, minor = 0, patch = 0;
    base->version(&major, &minor, &patch);
    printf("ROSETTE-MOONLAB-SURFACES/2\nABI %d %d %d\n", major, minor, patch);
    printf("MEASUREMENT %.17e %d %.17e %d\n", probability, outcome,
           collapsed, invalid_measurement);
    printf("CHANNELS %.17e %.17e\n", max_channel_deviation, invalid_channel);
    printf("QGT %d %d %.17e %d %d\n", qgt_status, context.calls, chern,
           qgt_expected(request.qgt_mass), null_callback_rejected);
    printf("QGT-REFERENCE %d %.17e\n", reference_status, reference_chern);
    printf("GRADIENT %d %.17e %.17e %.17e %.17e %.17e %d %d %d\n",
           gradient_status, native[0], native[1], finite_difference[0],
           finite_difference[1], residual, null_status, count_status,
           nondegenerate);
    printf("OWNERSHIP %d %d %d\n", created, invalid_target, zero_rejected);
    printf("GPU %s %d %.17e\nEND\n", gpu_kind, gpu_status, gpu_error);
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 2) return 64;
    void *handle = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!handle) { fprintf(stderr, "Moonlab library unavailable\n"); return 69; }

    struct api api = {0};
    BIND(api, version, "moonlab_abi_version");
    BIND(api, create, "quantum_state_create");
    BIND(api, destroy, "quantum_state_destroy");
    BIND(api, amplitude, "quantum_state_get_amplitude");
    BIND(api, h, "gate_hadamard"); BIND(api, x, "gate_pauli_x");
    BIND(api, y, "gate_pauli_y"); BIND(api, z, "gate_pauli_z");
    BIND(api, s, "gate_s"); BIND(api, t, "gate_t");
    BIND(api, rx, "gate_rx"); BIND(api, ry, "gate_ry"); BIND(api, rz, "gate_rz");
    BIND(api, cnot, "gate_cnot"); BIND(api, cz, "gate_cz"); BIND(api, swap, "gate_swap");

    char version[64];
    if (!fgets(version, sizeof(version), stdin)) return 65;
    if (!strcmp(version, "ROSETTE-MOONLAB-SURFACES/2\n")) {
        int status = run_surface_probe(handle, &api);
        dlclose(handle);
        return status;
    }
    if (strcmp(version, "ROSETTE-MOONLAB/1\n") != 0) return 65;

    int n = 0, gate_count = 0;
    uint64_t basis = 0;
    if (!read_unitary_header(&n, &basis, &gate_count)) return 65;
    void *state = api.create(n);
    if (!state) return 70;

    int error_path = api.x(state, n) != 0;
    int status = 0;
    for (int q = 0; q < n && status == 0; ++q)
        if ((basis >> q) & UINT64_C(1)) status = api.x(state, q);

    char op[16];
    for (int i = 0; i < gate_count && status == 0; ++i) {
        if (scanf("%15s", op) != 1) { status = -999; break; }
        if (!strcmp(op, "H") || !strcmp(op, "X") || !strcmp(op, "Y") ||
            !strcmp(op, "Z") || !strcmp(op, "S") || !strcmp(op, "T")) {
            int q; if (scanf("%d", &q) != 1) { status = -999; break; }
            gate_1_fn fn = !strcmp(op, "H") ? api.h : !strcmp(op, "X") ? api.x :
                !strcmp(op, "Y") ? api.y : !strcmp(op, "Z") ? api.z :
                !strcmp(op, "S") ? api.s : api.t;
            status = fn(state, q);
        } else if (!strcmp(op, "RX") || !strcmp(op, "RY") || !strcmp(op, "RZ")) {
            int q; double angle;
            if (scanf("%d %lf", &q, &angle) != 2) { status = -999; break; }
            gate_r_fn fn = !strcmp(op, "RX") ? api.rx : !strcmp(op, "RY") ? api.ry : api.rz;
            status = fn(state, q, angle);
        } else if (!strcmp(op, "CNOT") || !strcmp(op, "CZ") || !strcmp(op, "SWAP")) {
            int a, b; if (scanf("%d %d", &a, &b) != 2) { status = -999; break; }
            gate_2_fn fn = !strcmp(op, "CNOT") ? api.cnot : !strcmp(op, "CZ") ? api.cz : api.swap;
            status = fn(state, a, b);
        } else { status = -998; }
    }
    char end[16] = {0};
    if (status == 0 && (scanf("%15s", end) != 1 || strcmp(end, "END") != 0)) status = -999;
    if (status != 0 || !error_path) { api.destroy(state); dlclose(handle); return 67; }

    int major = 0, minor = 0, patch = 0;
    api.version(&major, &minor, &patch);
    printf("ROSETTE-MOONLAB/1\nABI %d %d %d\nOWNERSHIP 1\nERROR-PATH 1\nSTATE %" PRIu64 "\n",
           major, minor, patch, UINT64_C(1) << n);
    for (uint64_t i = 0; i < (UINT64_C(1) << n); ++i) {
        double complex value = api.amplitude(state, i);
        printf("AMP %.17e %.17e\n", creal(value), cimag(value));
    }
    printf("END\n");
    api.destroy(state);
    dlclose(handle);
    return 0;
}
