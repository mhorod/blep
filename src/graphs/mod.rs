use std::hash::Hash;

use std::collections::{HashMap, HashSet, VecDeque};

pub fn get_transitive_closure<T: Eq + Hash + Clone>(
    graph: &HashMap<T, HashSet<T>>,
) -> HashMap<T, HashSet<T>> {
    graph
        .keys()
        .map(|source| (source.clone(), reachable_from(source, graph)))
        .collect()
}

fn reachable_from<T: Eq + Hash + Clone>(source: &T, graph: &HashMap<T, HashSet<T>>) -> HashSet<T> {
    let mut result: HashSet<T> = HashSet::new();
    let mut queue: VecDeque<&T> = VecDeque::from([source]);

    while let Some(current) = queue.pop_front() {
        if result.contains(current) {
            continue;
        }
        result.insert(current.clone());

        queue.extend(graph.get(current).into_iter().flat_map(|x| x.iter()))
    }

    result
}
