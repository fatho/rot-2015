import numpy as np
import numpy.random as rand
import scipy as sp

import matplotlib.pyplot as plt


def gen_training(func, x, var=1):
    y = func(x)
    y += rand.normal(0, var, y.shape)
    return y


def const(x):
    return lambda y: x


def lms(funcs, x, y, alpha):
    (m,) = x.shape
    (n,) = funcs.shape
    w = np.zeros(n)

    for c in range(10000):
        for i in range(m):
            for j in range(n):
                val = 0
                for k in range(n):
                    val += funcs[k](x[i]) * w[k]
                w[j] = w[j] + alpha * x[i] * (y[i] - val)
    return w


def build_func(funcs, w):
    (n,) = funcs.shape

    def f(x):
        val = 0
        for i in range(n):
            val += funcs[i](x) * w[i]
        return val
    return f

# (x, y) = gen_training(lambda x: x, 0, 2*np.pi, 100)
x = np.linspace(0, 2*np.pi, 100)
y = gen_training(lambda x: np.sin(x) + (x/np.pi)**2, x, 0.2)

funcs = np.array([const(1), np.sin, lambda x:x**2])
w = lms(funcs, x, y, 0.0001)
f = build_func(funcs, w)
print(w)
plt.scatter(x, y)
plt.plot(x, f(x))
plt.show()
