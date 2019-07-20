# Algebraic Graphs

It's always convenient to use graphs, but they are inherently unsafe data structures in a functional
world. For instance, how do you avoid creating graphs that have edges referencing unknown vertices ?

This is where **algebraic graphs** come into play. They define an **algebraic data type** that lets
you describe any graph, and only produce valid graphs.

This library is based on the paper [Algebraic Graphs with Class](https://github.com/snowleopard/alga-paper)
by @snowleopard.