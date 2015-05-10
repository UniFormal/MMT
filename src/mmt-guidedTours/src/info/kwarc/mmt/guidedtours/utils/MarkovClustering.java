package info.kwarc.mmt.guidedtours.utils;

/**
 * This file is part of the Java Machine Learning Library
 * 
 * The Java Machine Learning Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * The Java Machine Learning Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the Java Machine Learning Library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Copyright (c) 2006-2009, Thomas Abeel
 * 
 * Project: http://java-ml.sourceforge.net/
 * 
 */

/**
 * MarkovClustering implements the Markov clustering (MCL) algorithm for graphs,
 * using a HashMap-based sparse representation of a Markov matrix, i.e., an
 * adjacency matrix m that is normalised to one. Elements in a column / node can
 * be interpreted as decision probabilities of a random walker being at that
 * node. Note: whereas we explain the algorithms with columns, the actual
 * implementation works with rows because the used sparse matrix has row-major
 * format.
 * <p>
 * The basic idea underlying the MCL algorithm is that dense regions in sparse
 * graphs correspond with regions in which the number of k-length paths is
 * relatively large, for small k in N, which corresponds to multiplying
 * probabilities in the matrix appropriately. Random walks of length k have
 * higher probability (product) for paths with beginning and ending in the same
 * dense region than for other paths.
 * <p>
 * The algorithm starts by creating a Markov matrix from the graph, for which
 * first the adjacency matrix is added diagonal elements to include self-loops
 * for all nodes, i.e., probabilities that the random walker stays at a
 * particular node. After this initialisation, the algorithm works by
 * alternating two operations, expansion and inflation, iteratively recomputing
 * the set of transition probabilities. The expansion step corresponds to matrix
 * multiplication (on stochastic matrices), the inflation step corresponds with
 * a parametrized inflation operator Gamma_r, which acts column-wise on (column)
 * stochastic matrices (here, we use row-wise operation, which is analogous).
 * <p>
 * The inflation operator transforms a stochastic matrix into another one by
 * raising each element to a positive power p and re-normalising columns to keep
 * the matrix stochastic. The effect is that larger probabilities in each column
 * are emphasised and smaller ones deemphasised. On the other side, the matrix
 * multiplication in the expansion step creates new non-zero elements, i.e.,
 * edges. The algorithm converges very fast, and the result is an idempotent
 * Markov matrix, M = M * M, which represents a hard clustering of the graph
 * into components.
 * <p>
 * Expansion and inflation have two opposing effects: While expansion flattens
 * the stochastic distributions in the columns and thus causes paths of a random
 * walker to become more evenly spread, inflation contracts them to favoured
 * paths.
 * <p>
 * Description is based on the introduction of Stijn van Dongen's thesis Graph
 * Clustering by Flow Simulation (2000); for a mathematical treatment of the
 * algorithm and the associated MCL process, see there.
 * 
 * @author gregor :: arbylon . net
 */
public class MarkovClustering {

    /**
     * run the MCL process.
     * 
     * @param a
     *            matrix
     * @param maxResidual
     *            maximum difference between row elements and row square sum
     *            (measure of idempotence)
     * @param pGamma
     *            inflation exponent for Gamma operator
     * @param loopGain
     *            values for cycles
     * @param maxZero
     *            maximum value considered zero for pruning operations
     * @return the resulting matrix
     */
    public SparseMatrix run(SparseMatrix a, double maxResidual, double pGamma, double loopGain, double maxZero) {
        // add cycles
        addLoops(a, loopGain);
        a.normaliseRows();
        double residual = 1.;
        //int i = 0;

        // main iteration
        while (residual > maxResidual) {
          //  i++;
            a = expand(a);
            residual = inflate(a, pGamma, maxZero);
        }
        return a;

    }

    /**
     * inflate stochastic matrix by Hadamard (elementwise) exponentiation,
     * pruning and normalisation :
     * <p>
     * result = Gamma ( m, p ) = normalise ( prune ( m .^ p ) ).
     * <p>
     * By convention, normalisation is done along rows (SparseMatrix has
     * row-major representation)
     * 
     * @param m
     *            matrix (mutable)
     * @param p
     *            exponent as a double
     * @param zeromax
     *            below which elements are pruned from the sparse matrix
     * @return residuum value, m is modified.
     */
    public double inflate(SparseMatrix m, double p, double zeromax) {
        double res = 0.;

        // matlab: m = m .^ p
        m.hadamardPower(p);
        // matlab: m(find(m < threshold)) = 0
        m.prune(zeromax);
        // matlab [for cols]: dinv = diag(1./sum(m)); m = m * dinv; return
        // sum(m)
        SparseVector rowsums = m.normalise(1.);

        // check if done: if the maximum element
        for (int i : rowsums.keySet()) {
            SparseVector row = m.get(i);
            double max = row.max();
            double sumsq = row.sum(2.);
            res = Math.max(res, max - sumsq);
        }
        return res;
    }

    /**
     * expand stochastic quadratic matrix by sqaring it with itself: result = m *
     * m. Here normalisation is rowwise.
     * 
     * @param matrix
     * @return new matrix (pointer != argument)
     */
    public SparseMatrix expand(SparseMatrix m) {
        m = m.times(m);
        return m;
    }

    /**
     * add loops with specific energy, which corresponds to adding loopGain to
     * the diagonal elements.
     * 
     * @param a
     * @param loopGain
     */
    private void addLoops(SparseMatrix a, double loopGain) {
        if (loopGain <= 0) {
            return;
        }
        for (int i = 0; i < a.size(); i++) {
            a.add(i, i, loopGain);
        }
    }

}