
* **Attractor**: the set of points to which the trajectories of a
  dynamical system converges over time.
  
* **Lagged variables plot**: a plot of a time series as a function of 
the same time series lagged by multiples of a length of time $\tau$.
For instance, the axes of a three-dimensional lagged variable plot are 
the original time series $X(t)$, the lagged time series $X(t + \tau)$,
and $X(t + 2\tau)$.

* **State space reconstruction**: the process of making a
representation of a state space of interest that preserves some
important properties. In the context of EDM, the reconsctructed state
space corresponds to the lagged variables plot of a time series.

* **Shadow Manifold**: the projection of the original manifold (which
  is an attractor in our case) into an Euclidean space. This space has
  a dimension (the embedding dimension) that ensures a one-to-one map to the
  original manifold. 

* **Embedding dimension**: for our purposes, the dimension of the
Euclidean space used to build the shadow manifold. In the case of a
state space reconstruction, the embedding dimension is the dimension
of this space, that is, the number of axes of the lagged variables
plot.
