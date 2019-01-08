import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from math import exp, ceil, log
from functools import reduce


def mul(a, b):
    return a*b


def ncr(n, r):
    if r == 0:
        return 1
    else:
        numerator = reduce(mul, range(n - r + 1, n + 1))
        denominator = reduce(mul, range(1, r + 1))
        return numerator//denominator


def gamma(epsilon, xi):
    return (exp(epsilon) - 1 + (1.0/xi)) / (exp(epsilon))


def f(j, n, xi):
    return ncr(n, j) * (1.0 / xi) ** j * (1 - 1.0 / xi) ** (n - j)


def F(j, n, xi, start, end):
    sum = 0
    for i in range(start, end + 1):
        sum += f(i, n, xi)
    return sum


def d(k, xi, epsilon):
    ls = []
    num_gamma = gamma(epsilon, xi)
    for i in range(1, 101):
        n = ceil((k + i) / num_gamma - 1)
        j = ceil(num_gamma * n)
        ls.append(F(j, n, xi, j, n))
    return max(ls)


def plot_k_1_5_fix_xi():
    # Fig1
    ls_k = [5, 10, 20, 30, 50]
    arr_epsilon = np.arange(0.25, 2.05, 0.05)
    arr_inv_delta = np.zeros(shape=arr_epsilon.shape)
    plt.xlabel(r'$\varepsilon$')
    plt.ylabel(r'$\log({\frac{1}{\delta}})$', fontsize=14)
    for k in ls_k:
        for i, epsilon in zip(range(arr_epsilon.shape[0]), arr_epsilon):
            arr_inv_delta[i] = log(1 / d(k=k, xi=10.0, epsilon=epsilon))
        plt.plot(arr_epsilon, arr_inv_delta,
                 label=r'k={k}, $\xi$=10'.format(k=k))
    plt.legend()
    plt.show()


def plot_k_5_50_fix_xi():
    # Fig2
    iter_k = range(5, 0, -1)
    arr_epsilon = np.arange(0.25, 2.05, 0.05)
    arr_inv_delta = np.zeros(shape=arr_epsilon.shape)
    plt.xlabel(r'\epsilon')
    plt.ylabel(r'$\log({\frac{1}{\delta}})$')
    for k in iter_k:
        for i, epsilon in zip(range(arr_epsilon.shape[0]), arr_epsilon):
            arr_inv_delta[i] = log(1 / d(k=k, xi=10, epsilon=epsilon))
        plt.plot(arr_epsilon, arr_inv_delta,
                 label=r'k={k}, $\xi$=10'.format(k=k))
    plt.legend()
    plt.show()


def plot_xi_fix_k():
    ls_xi = [50.0, 20.0, 10.0, 5.0]
    # Fig3
    arr_epsilon = np.arange(0.25, 2.05, 0.05)
    arr_inv_delta = np.zeros(shape=arr_epsilon.shape)
    plt.xlabel(r'$\epsilon$')
    plt.ylabel(r'$\log({\frac{1}{\delta}})$')
    for xi in ls_xi:
        for i, epsilon in zip(range(arr_epsilon.shape[0]), arr_epsilon):
            arr_inv_delta[i] = log(1 / d(k=20, xi=xi, epsilon=epsilon))
        plt.plot(arr_epsilon, arr_inv_delta,
                 label=r'k=20, $\xi$={xi}'.format(xi=int(xi)))
    plt.legend()
    plt.show()


def plot_epsilon():
    # Fig4
    plt.xlabel(r'$k$', fontsize=14)
    plt.ylabel(r'$\epsilon$', fontsize=14)
    ls_xi = [5.0, 10.0, 20.0, 50.0]
    arr_k = np.arange(9, 51, 2)
    iter_epsilon = np.arange(0.2, 2.5, 0.05)
    for xi in ls_xi:
        # print('xi:', xi)
        arr_epsilon = np.zeros(arr_k.shape[0])
        for i, k in zip(range(arr_k.shape[0]), arr_k):
            # print('k', k)
            for e in iter_epsilon:
                delta = d(k=k, xi=xi, epsilon=e)
                # print(delta)
                arr_epsilon[i] = e
                if delta <= 1e-6:
                    break
        plt.plot(arr_k, arr_epsilon, label=r'$\xi$={:d}'.format(int(xi)))
    plt.legend()
    plt.show()


if __name__ == "__main__":
    plot_k_1_5_fix_xi()
    plot_k_5_50_fix_xi()
    plot_xi_fix_k()
    plot_epsilon()
