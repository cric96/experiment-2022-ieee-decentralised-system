package it.unibo.geo.altitude;

import smile.neighbor.KDTree;

import java.util.List;

public class KDTreeFacade {
    static KDTree<Double> createTree(List<double[]> elements, Double ... data) {
        final double[][] wrapper = new double[elements.size()][elements.get(0).length];
        return new KDTree<Double>(elements.toArray(wrapper), data);
    }
}
