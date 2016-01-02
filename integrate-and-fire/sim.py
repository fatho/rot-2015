import numpy as np
import matplotlib.pyplot as plt

def simulate(tau_m, mu_0, V_rest, V_threshold, sigma_0, dt=0.00001, N=10000):
    # t-Achse
    V = np.zeros(N)
    V_noreset = np.zeros(N)
    dV = np.zeros(N)
    dV_noreset = np.zeros(N)
    t = np.linspace(0, N*dt, N, endpoint=False)
    eta = np.random.normal(0, 1. / dt, N)

    num_spikes = 0
    last_spike_time = 0.

    for i in range(1, N):
        dV[i-1] = ((- V[i-1] + mu_0) * dt + sigma_0 * np.sqrt(tau_m) * eta[i-1]) / tau_m
        dV_noreset[i-1] = ((- V_noreset[i-1] + mu_0) * dt + sigma_0 * np.sqrt(tau_m) * eta[i-1]) / tau_m
        V[i] = V[i-1] + dV[i-1]
        V_noreset[i] = V_noreset[i-1] + dV_noreset[i-1]
        if V[i] >= V_threshold - V_rest:
            V[i] = 0
            num_spikes += 1
            last_spike_time = i * dt
    V += V_rest
    V_noreset += V_rest
    if num_spikes > 0:
        spike_freq = num_spikes / last_spike_time
    else:
        spike_freq = 0
    return (t, V, dV, eta, spike_freq, V_noreset)


def explicit_result(tau_m, mu_0, V_rest, V_threshold, N=100):
    print(V_rest - mu_0)
    print(V_threshold - mu_0)
    print((V_rest - mu_0) / (V_threshold - mu_0))
    fire_rate = 1 / (tau_m * np.log((V_rest - mu_0) / (V_threshold - mu_0)))
    t = np.linspace(0, fire_rate, N)
    return (t, None, None, None, fire_rate)

N_rates = 100
mean = np.linspace(0, 0.1, N_rates)
fire_rate = np.zeros(N_rates)

for i in range(N_rates):
    fire_rate[i] = simulate(0.02, mean[i], -0.072, -0.0421, 0)[4]
    # fire_rate[i] = explicit_result(20, mean[i], 0, 30)[4]

# plt.plot(mean, fire_rate)
# plt.show()

# Fast-spiking Interneuron
results = simulate(0.018, 199e6 * 400e-12, -0.072, -0.0421, 0)
# results = simulate(40, 80, -72, -42.1, 0)

# Normal Interneuron
# results = simulate(20.1, 351e6 * 400e-12 * 1e3, -67, -41, 0)

fig = plt.figure()
fig.suptitle('Integrate-and-Fire', fontsize=14, fontweight='bold')

ax = fig.add_subplot(2,1,1)
# fig.subplots_adjust(top=0.85)
ax.set_title('...')

ax.set_xlabel('time [s]')
ax.set_ylabel('membrane potential [V]')

ax.text(0.8, 1.01, "fire rate: %.2f Hz" % results[4],
        transform=ax.transAxes, fontsize=14)

ax.plot(results[0], results[1])
ax.plot(results[0], results[5])
# ax.plot(results[0], results[2])

ax2 = fig.add_subplot(2,1,2)
ax2.plot(mean, fire_rate)

ax2.set_xlabel('$\mu_0$ [V]')
ax2.set_ylabel('fire rate [Hz]')

plt.show()
