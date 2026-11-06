"""Independent NumPy Mamba-1 fixture, including full-stack finite differences.

No production code is imported. The scalar time/channel recurrence follows the
pinned equations in inst/sequence-reference.json. NumPy is a fixture-generation
tool only; LKT has no Python runtime dependency.
"""
import json
from pathlib import Path
import numpy as np


def payload(x):
    x = np.asarray(x)
    return {"shape": list(x.shape), "values": x.flatten(order="F").tolist()}


def forward(w, x, conv, memory):
    result, memories, buffers = [], [], []
    for t in range(x.shape[1]):
        base = x[:, t] @ w["input.weight"].T + w["input.bias"]
        q = base / np.sqrt(np.mean(base ** 2, axis=1, keepdims=True) + 1e-5) * w["layers.0.norm"]
        v, z = np.split(q @ w["layers.0.in_proj.weight"].T, 2, axis=1)
        conv = np.concatenate([conv[:, :, 1:], v[:, :, None]], axis=2)
        u = np.sum(conv * w["layers.0.conv.weight"][:, 0, :], axis=2) + w["layers.0.conv.bias"]
        u = u / (1 + np.exp(-u))
        terms = u @ w["layers.0.x_proj.weight"].T
        delta = np.logaddexp(0, terms[:, :1] @ w["layers.0.dt_proj.weight"].T + w["layers.0.dt_proj.bias"])
        A = -np.exp(w["layers.0.A_log"])
        next_memory = np.empty_like(memory)
        for b in range(x.shape[0]):
            for c in range(2):
                for n in range(2):
                    next_memory[b, c, n] = np.exp(delta[b, c] * A[c, n]) * memory[b, c, n] + delta[b, c] * terms[b, 1+n] * u[b, c]
        memory = next_memory
        y = np.sum(memory * terms[:, None, 3:5], axis=2) + w["layers.0.D"] * u
        y = y * z / (1 + np.exp(-z))
        result.append(base + y @ w["layers.0.out_proj.weight"].T)
        memories.append(memory.copy()); buffers.append(conv.copy())
    return np.stack(result, axis=1), memories, buffers


def main():
    shapes = {"input.weight": (2,3), "input.bias": (2,), "layers.0.norm": (2,),
              "layers.0.A_log": (2,2), "layers.0.D": (2,), "layers.0.in_proj.weight": (4,2),
              "layers.0.conv.weight": (2,1,2), "layers.0.conv.bias": (2,),
              "layers.0.x_proj.weight": (5,2), "layers.0.dt_proj.weight": (2,1),
              "layers.0.dt_proj.bias": (2,), "layers.0.out_proj.weight": (2,2)}
    w = {key: .3 * np.sin(np.arange(np.prod(shape)).reshape(shape) + i + .7) for i, (key, shape) in enumerate(shapes.items())}
    w["layers.0.norm"] = np.array([.9, 1.1]); w["layers.0.A_log"] = np.log([[1.,2.],[1.,2.]])
    w["layers.0.D"] = np.array([.8,1.2]); w["layers.0.dt_proj.bias"] = np.array([-2.,-1.])
    x = np.cos(np.arange(30).reshape(2,5,3)/4)
    conv = .1 * np.sin(np.arange(8).reshape(2,2,2))
    memory = .1 * np.cos(np.arange(8).reshape(2,2,2))
    output, states, buffers = forward(w,x,conv,memory)
    def loss():
        return np.sum(forward(w,x,conv,memory)[0] ** 2)/2
    grads = {}
    for key, a in list(w.items()) + [("input", x)]:
        g = np.zeros_like(a)
        for index in np.ndindex(a.shape):
            old = a[index]; eps = 1e-5
            a[index] = old + eps; plus = loss()
            a[index] = old - eps; minus = loss(); a[index] = old
            g[index] = (plus-minus)/(2*eps)
        grads[key] = payload(g)
    document = {"weights": {k: payload(v) for k,v in w.items()}, "input": payload(x),
                "initial_conv": payload(conv), "initial_ssm": payload(memory), "output": payload(output),
                "ssm_steps": [payload(v) for v in states], "conv_steps": [payload(v) for v in buffers], "gradients": grads}
    dest = Path(__file__).resolve().parents[1] / "tests" / "fixtures" / "sequence-mamba.json"
    dest.parent.mkdir(exist_ok=True)
    dest.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")
    print("Wrote independent Mamba fixture and finite-difference gradients")


if __name__ == "__main__":
    main()
